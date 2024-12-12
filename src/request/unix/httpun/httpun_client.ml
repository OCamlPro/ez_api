open Httpun
open Httpun_lwt_unix

let (let>) = Lwt.bind

let error_handler cb error =
  let s = match error with
    | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
    | `Invalid_response_body_length _ -> "Invalid body length"
    | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn) in
  cb @@ Error (-1, Some s)

let response_handler cb response body =
  let b = Buffer.create 0x1000 in
  let open Response in
  match response with
  | { status = `OK; _ } ->
    let on_eof () = cb @@ Ok (Buffer.contents b) in
    let rec on_read bs ~off ~len =
      Buffer.add_string b (Bigstringaf.substring ~off ~len bs);
      Body.Reader.schedule_read body ~on_read ~on_eof in
    Body.Reader.schedule_read body ~on_read ~on_eof;
  | r -> cb @@ Error (Status.to_code r.status, Some r.reason)

let stream_handler_lwt handler finish acc response body =
  let open Response in
  match response with
  | { status = `OK; _ } ->
    let on_eof acc = finish @@ Ok acc in
    let rec on_read acc bs ~off ~len =
      Fun.flip Lwt.dont_wait (fun exn -> finish (Error (-1, Some (Printexc.to_string exn)))) @@ fun () ->
      Fun.flip Lwt.map (handler acc (Bigstringaf.substring ~off ~len bs)) @@ function
      | `continue acc -> Body.Reader.schedule_read body ~on_read:(on_read acc) ~on_eof:(fun () -> on_eof acc)
      | `stop x -> finish x in
    Body.Reader.schedule_read body ~on_read:(on_read acc) ~on_eof:(fun () -> on_eof acc);
  | r -> finish @@ Error (Status.to_code r.status, Some r.reason)

let parse url =
  let uri = Uri.of_string url in
  let p = Option.value ~default:80 (Uri.port uri) in
  match Uri.host uri, Uri.scheme uri with
  | Some h, Some sch -> Ok (h, sch, p, Uri.path_and_query uri)
  | _ -> Error (-1, Some "invalid url")

let perform ?meth ?content ?content_type ?(headers=[]) handler url =
  let meth = match meth, content with
    | Some `PATCH, _ -> `Other "PATCH"
    | Some (#Method.t as m), _ -> m
    | _, None -> `GET
    | _ -> `POST in
  match parse url with
  | Error e -> Lwt.return_error e
  | Ok (host, scheme, port, path) ->
    let w, notify = Lwt.wait () in
    let> addresses = Lwt_unix.getaddrinfo host (Int.to_string port) [Unix.(AI_FAMILY PF_INET)] in
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let> () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
    let headers = Headers.of_list @@
      [ "host", host ] @ headers @
      Option.fold ~none:[] ~some:(fun c -> [ "content-length", string_of_int (String.length c)]) content @
      Option.fold ~none:[] ~some:(fun c -> [ "content-type", c]) content_type in
    let req = Request.create ~headers meth path in
    let error_handler = error_handler (Lwt.wakeup notify) in
    let response_handler = handler notify in
    let> body =
      if scheme = "https" then
        let> connection = Client.TLS.create_connection_with_default socket in
        Lwt.return @@ Client.TLS.request connection req ~error_handler ~response_handler
      else
        let> connection = Client.create_connection socket in
        Lwt.return @@ Client.request connection req ~error_handler ~response_handler in
    Option.iter (fun c -> Body.Writer.write_string body c) content;
    Body.Writer.close body;
    w

let call ?meth ?content ?content_type ?headers url =
  let handler n = response_handler (Lwt.wakeup n) in
  perform ?meth ?content ?content_type ?headers handler url

let stream ?meth ?content ?content_type ?headers ~url cb acc =
  let handler n = stream_handler_lwt cb (Lwt.wakeup n) acc in
  perform ?meth ?content ?content_type ?headers handler url
