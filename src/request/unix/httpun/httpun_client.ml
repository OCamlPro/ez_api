open Httpun

let (let>) = Lwt.bind
let (let>?) p f = Lwt.bind p (function Error e -> Lwt.return_error e | Ok x -> f x)

let log ?(meth="GET") url = function
  | None -> if !Verbose.v <> 0 then Format.printf "[ez_api] %s %s@." meth url
  | Some msg -> Format.printf "[>%s %s %s ]@." msg meth url

let error_handler cb error =
  let s = match error with
    | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
    | `Invalid_response_body_length _ -> "Invalid body length"
    | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn) in
  cb @@ Error (-1, Some s)

let response_handler ?msg ~url cb response body =
  let b = Buffer.create 0x1000 in
  let open Response in
  match response with
  | { status = `OK; _ } ->
    log ~meth:("RECV " ^ (string_of_int @@ Status.to_code response.status)) url msg;
    let on_eof () =
      let data = Buffer.contents b in
      if !Verbose.v land 1 <> 0 && data <> "" then Format.printf "[ez_api] received:\n%s@." data;
      cb @@ Ok data in
    let rec on_read bs ~off ~len =
      Buffer.add_string b (Bigstringaf.substring ~off ~len bs);
      Body.Reader.schedule_read body ~on_read ~on_eof in
    Body.Reader.schedule_read body ~on_read ~on_eof;
  | r ->
    cb @@ Error (Status.to_code r.status, Some r.reason)

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
  match Uri.host uri, Uri.scheme uri with
  | Some h, Some sch ->
    let p = Option.value ~default:(if sch = "https" then 443 else 80) (Uri.port uri) in
    Ok (h, sch, p, Uri.path_and_query uri)
  | _ -> Error (-1, Some "invalid url")

let perform ?msg ?meth ?content ?content_type ?(headers=[]) handler url =
  let meth = match meth, content with
    | Some `PATCH, _ -> `Other "PATCH"
    | Some (#Method.t as m), _ -> m
    | _, None -> `GET
    | _ -> `POST in
  log ~meth:(Method.to_string meth) url msg;
  (match !Verbose.v land 2 <> 0, content with
   | true, Some content when content <> "" -> Format.printf "[ez_api] sent:\n%s@." content;
   | _ -> ());
  match parse url with
  | Error e -> Lwt.return_error e
  | Ok (hostname, scheme, port, path) ->
    let w, notify = Lwt.wait () in
    let> addresses = Lwt_unix.getaddrinfo hostname (Int.to_string port) [Unix.(AI_FAMILY PF_INET)] in
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let> () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
    let headers = Headers.of_list @@
      [ "host", hostname ] @ headers @
      Option.fold ~none:[] ~some:(fun c -> [ "content-length", string_of_int (String.length c)]) content @
      Option.fold ~none:[] ~some:(fun c -> [ "content-type", c]) content_type in
    let req = Request.create ~headers meth path in
    let error_handler = error_handler (Lwt.wakeup notify) in
    let response_handler = handler ?msg ~url notify in
    let>? body =
      Lwt.catch (fun () ->
          if scheme = "https" then
            Httpun_tls.request ~hostname ~socket ~error_handler ~response_handler req
          else
            let> connection = Httpun_lwt_unix.Client.create_connection socket in
            Lwt.return_ok @@ Httpun_lwt_unix.Client.request connection req ~error_handler ~response_handler)
        (fun exn -> Lwt.return_error (-1, Some (Printexc.to_string exn))) in
    Option.iter (fun c -> Body.Writer.write_string body c) content;
    Body.Writer.close body;
    w

let call ?meth ?content ?content_type ?headers url =
  let handler ?msg ~url n = response_handler ?msg ~url (Lwt.wakeup n) in
  perform ?meth ?content ?content_type ?headers handler url

let stream ?meth ?content ?content_type ?headers ~url cb acc =
  let handler ?msg:_ ~url:_ n = stream_handler_lwt cb (Lwt.wakeup n) acc in
  perform ?meth ?content ?content_type ?headers handler url
