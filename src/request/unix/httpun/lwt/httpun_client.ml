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

let with_timeout timeout p = match timeout with
  | None -> p
  | Some t -> Lwt.pick [ p; (let> () = Lwt_unix.sleep t in Lwt.return_error (`timeout t)) ]

let close_socket socket =
  Lwt.catch (fun () -> Lwt_unix.close socket) (fun _ -> Lwt.return_unit)

let connect ?timeout ~hostname ~port () =
  with_timeout timeout @@
  Lwt.catch (fun () ->
      let> addresses = Lwt_unix.getaddrinfo hostname (Int.to_string port) [Unix.(AI_FAMILY PF_INET)] in
      match addresses with
      | [] -> Lwt.return_error (`msg (Format.sprintf "cannot resolve host %s" hostname))
      | { Unix.ai_addr; _ } :: _ ->
        let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Lwt.catch
          (fun () -> let> () = Lwt_unix.connect socket ai_addr in Lwt.return_ok socket)
          (fun exn -> let> () = close_socket socket in Lwt.reraise exn))
    (fun exn -> Lwt.return_error (`exn exn))

let perform ?msg ?meth ?content ?content_type ?(headers=[]) ?timeout handler url =
  let meth = match meth, content with
    | Some `PATCH, _ -> `Other "PATCH"
    | Some (#Method.t as m), _ -> m
    | _, None -> `GET
    | _ -> `POST in
  Verbose.request ?msg ~meth:(Method.to_string meth) ?content ~headers url;
  match parse url with
  | Error e -> Lwt.return_error e
  | Ok (hostname, scheme, port, path) ->
    let w, notify = Lwt.wait () in
    let>? socket = connect ?timeout ~hostname ~port () in
    let host = if (port <> 80 && port <> 443) then Format.sprintf "%s:%d" hostname port else hostname in
    let headers = Headers.of_list @@
      [ "host", host ] @ headers @
      Option.fold ~none:[ "content-length", "0" ] ~some:(fun c -> [ "content-length", string_of_int (String.length c)]) content @
      Option.fold ~none:[] ~some:(fun c -> [ "content-type", c]) content_type in
    let req = Request.create ~headers meth path in
    let resolve r = if Lwt.is_sleeping w then Lwt.wakeup_later notify r in
    let error_handler = error_handler resolve in
    let response_handler = handler resolve in
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
    let> r = with_timeout timeout w in
    let> () = Lwt.catch shutdown (fun _ -> Lwt.return_unit) in
    Lwt.return r

let call ?msg ?meth ?content ?content_type ?headers ?timeout url : (_, [> perform_error | http_error ]) result Lwt.t =
  let handler n = response_handler ?msg ~url ~res:Simple n in
  perform ?meth ?content ?content_type ?headers ?timeout handler url

let stream ?meth ?content ?content_type ?headers ?timeout ?interval_timeout ~url cb acc :
  (_, [> error | `cb of _]) result Lwt.t =
  let handler n = stream_handler_lwt ?timeout:interval_timeout cb n acc in
  perform ?meth ?content ?content_type ?headers ?timeout handler url
