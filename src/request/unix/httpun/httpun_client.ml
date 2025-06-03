open Httpun

type 'content with_headers = { content: 'content; headers: (string * string) list }

type _ response =
  | Simple : string response
  | WithHeaders : string with_headers response

type http_error = [ `http of (int * string) ]
type perform_error = [
  | `msg of string
  | `exn of exn
  | `timeout of float
  | `invalid_url of string
]
type stream_error_base = [ `interval_timeout of float | `exn of exn | http_error ]
type 'e stream_error = [ stream_error_base | `cb of 'e ]
type error = [ http_error | perform_error | stream_error_base ]

let (let>) = Lwt.bind
let (let>?) p f = Lwt.bind p (function Error e -> Lwt.return_error e | Ok x -> f x)

let log ?(meth="GET") url = function
  | None -> if !Verbose.v <> 0 then Format.printf "[ez_api] %s %s@." meth url
  | Some msg -> Format.printf "[>%s %s %s ]@." msg meth url

let pp_error fmt (e: [< error]) = match e with
  | `msg s -> Format.fprintf fmt "%s" s
  | `exn exn -> Format.fprintf fmt "exn: %s" (Printexc.to_string exn)
  | `timeout f -> Format.fprintf fmt "timeout %.1fs" f
  | `interval_timeout f -> Format.fprintf fmt "interval timeout %.1fs" f
  | `http (code, content) -> Format.fprintf fmt "http error %d: %s" code content
  | `invalid_url url -> Format.fprintf fmt "invalid url: %s" url

let error_handler cb error =
  cb (Error (match error with
      | `Malformed_response err -> `msg (Format.sprintf "malformed response: %s" err)
      | `Invalid_response_body_length _ -> `msg "invalid body length"
      | `Exn exn -> `exn exn))

let response_handler :
  type r. ?msg:string -> res:r response -> url:string -> ((r, [> http_error]) result -> 'a) ->
  Response.t -> Body.Reader.t -> 'a = fun ?msg ~res ~url cb response body ->
  let b = Buffer.create 0x1000 in
  let open Response in
  log ~meth:("RECV " ^ (string_of_int @@ Status.to_code response.status)) url msg;
  let on_eof f () =
    let content = Buffer.contents b in
    if !Verbose.v land 1 <> 0 && content <> "" then Format.printf "[ez_api] received:\n%s@." content;
    cb @@ f res response content in
  let rec on_read f bs ~off ~len =
    Buffer.add_string b (Bigstringaf.substring ~off ~len bs);
    Body.Reader.schedule_read body ~on_read:(on_read f) ~on_eof:(on_eof f) in
  match response.status with
  | #Status.successful ->
    let f : type r. r response -> _ -> string -> (r, _) result = fun res response content -> match res with
      | Simple -> Ok content
      | WithHeaders ->
        let headers = Headers.to_list response.headers in
        (Ok { content; headers }) in
    let on_eof, on_read = on_eof f, on_read f in
    Body.Reader.schedule_read body ~on_read ~on_eof
  | _ ->
    let f _ _ content = Error (`http (Status.to_code response.status, content)) in
    let on_eof, on_read = on_eof f, on_read f in
    Body.Reader.schedule_read body ~on_read ~on_eof

let timeout_fail cb = function
  | None -> Lwt.return_unit
  | Some timeout ->
    let> () = EzLwtSys.sleep timeout in
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

let parse url =
  let uri = Uri.of_string url in
  match Uri.host uri, Uri.scheme uri with
  | Some h, Some sch ->
    let p = Option.value ~default:(if sch = "https" then 443 else 80) (Uri.port uri) in
    Ok (h, sch, p, Uri.path_and_query uri)
  | _ -> Error (`invalid_url url)

let perform ?msg ?meth ?content ?content_type ?(headers=[]) ?timeout handler url =
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
    let response_handler = handler (Lwt.wakeup notify) in
    let>? body =
      Lwt.catch (fun () ->
          if scheme = "https" then
            Httpun_tls.request ~hostname ~socket ~error_handler ~response_handler req
          else
            let> connection = Httpun_lwt_unix.Client.create_connection socket in
            Lwt.return_ok @@ Httpun_lwt_unix.Client.request connection req ~error_handler ~response_handler)
        (fun exn -> Lwt.return_error (`exn exn)) in
    Option.iter (fun c -> Body.Writer.write_string body c) content;
    Body.Writer.close body;
    match timeout with
    | None -> w
    | Some timeout ->
      let timeout () =
        let> () = EzLwtSys.sleep timeout in
        Lwt.return_error (`timeout timeout) in
      Lwt.pick [ w; timeout () ]

let call ?msg ?meth ?content ?content_type ?headers ?timeout url : (_, [> perform_error | http_error ]) result Lwt.t =
  let handler n = response_handler ?msg ~url ~res:Simple n in
  perform ?meth ?content ?content_type ?headers ?timeout handler url

let stream ?meth ?content ?content_type ?headers ?timeout ?interval_timeout ~url cb acc :
  (_, [> error | `cb of _]) result Lwt.t =
  let handler n = stream_handler_lwt ?timeout:interval_timeout cb n acc in
  perform ?meth ?content ?content_type ?headers ?timeout handler url
