open Httpun
open Httpun_common

let (let>) = Lwt.bind
let (let>?) p f = Lwt.bind p (function Error e -> Lwt.return_error e | Ok x -> f x)

let timeout_fail cb = function
  | None -> Lwt.return_unit
  | Some timeout ->
    let> () = Lwt_unix.sleep timeout in
    cb @@ Error (`interval_timeout timeout);
    Lwt.return_unit

let stream_handler_lwt ?timeout handler (finish: (_, [> _ stream_error]) result -> unit) acc response body =
  let open Response in
  match response.status with
  | #Status.successful ->
    let on_eof acc = finish @@ Ok acc in
    let stop = timeout_fail finish timeout in
    let rec on_read stop acc bs ~off ~len =
      Fun.flip Lwt.dont_wait (function
          | Lwt.Canceled -> ()
          | exn -> finish (Error (`exn exn))) @@ fun () ->
      Lwt.bind (handler acc (Bigstringaf.substring ~off ~len bs)) @@ function
      | `continue acc ->
        Lwt.cancel stop;
        let stop = timeout_fail finish timeout in
        Body.Reader.schedule_read body ~on_read:(on_read stop acc) ~on_eof:(fun () -> on_eof acc);
        stop
      | `stop r ->
        let r = Result.map_error (fun e -> `cb e) r in
        finish r; Lwt.return_unit in
    Body.Reader.schedule_read body ~on_read:(on_read stop acc) ~on_eof:(fun () -> on_eof acc)
  | _ ->
    let b = Buffer.create 0x1000 in
    let on_eof () = finish @@ Error (`http (Status.to_code response.status, Buffer.contents b)) in
    let rec on_read bs ~off ~len =
      Buffer.add_string b (Bigstringaf.substring ~off ~len bs);
      Body.Reader.schedule_read body ~on_read ~on_eof in
    Body.Reader.schedule_read body ~on_read ~on_eof

let perform ?msg ?meth ?content ?content_type ?(headers=[]) ?timeout handler url =
  let meth = match meth, content with
    | Some `PATCH, _ -> `Other "PATCH"
    | Some (#Method.t as m), _ -> m
    | _, None -> `GET
    | _ -> `POST in
  log ~meth:(Method.to_string meth) url msg;
  (if !Verbose.v land 4 <> 0 then
     Format.printf "[ez_api] headers\n  %s@." @@
     String.concat "\n  " @@ List.map (fun (k, v) -> k ^ " : " ^ v) headers);
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
    let host = if (port <> 80 && port <> 443) then Format.sprintf "%s:%d" hostname port else hostname in
    let headers = Headers.of_list @@
      [ "host", host ] @ headers @
      Option.fold ~none:[ "content-length", "0" ] ~some:(fun c -> [ "content-length", string_of_int (String.length c)]) content @
      Option.fold ~none:[] ~some:(fun c -> [ "content-type", c]) content_type in
    let req = Request.create ~headers meth path in
    let error_handler = error_handler (Lwt.wakeup_later notify) in
    let response_handler = handler (Lwt.wakeup_later notify) in
    let>? body, shutdown =
      Lwt.catch (fun () ->
          if scheme = "https" then
            Httpun_lwt_tls.request ~hostname ~socket ~error_handler ~response_handler req
          else
            let> connection = Httpun_lwt_unix.Client.create_connection socket in
            let shutdown () = Httpun_lwt_unix.Client.shutdown connection in
            Lwt.return_ok (Httpun_lwt_unix.Client.request connection req ~error_handler ~response_handler, shutdown))
        (fun exn -> Lwt.return_error (`exn exn)) in
    Option.iter (fun c -> Body.Writer.write_string body c) content;
    Body.Writer.close body;
    let> r = match timeout with
      | None -> w
      | Some timeout ->
        let timeout () =
          let> () = Lwt_unix.sleep timeout in
          Lwt.return_error (`timeout timeout) in
        Lwt.pick [ w; timeout () ] in
    let> () = shutdown () in
    Lwt.return r

let call ?msg ?meth ?content ?content_type ?headers ?timeout url : (_, [> perform_error | http_error ]) result Lwt.t =
  let handler n = response_handler ?msg ~url ~res:Simple n in
  perform ?meth ?content ?content_type ?headers ?timeout handler url

let stream ?meth ?content ?content_type ?headers ?timeout ?interval_timeout ~url cb acc :
  (_, [> error | `cb of _]) result Lwt.t =
  let handler n = stream_handler_lwt ?timeout:interval_timeout cb n acc in
  perform ?meth ?content ?content_type ?headers ?timeout handler url
