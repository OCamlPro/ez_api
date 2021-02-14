open StringCompat
open Lwt
open EzAPI.TYPES
open EzAPIServerUtils
open Resto1

module Header = Cohttp.Header
module Request = Cohttp.Request
module Server = Cohttp_lwt_unix.Server

let set_debug () = Cohttp_lwt_unix.Debug.activate_debug ()

let of_cohttp_meth = function
  | `GET -> Resto1.GET
  | `HEAD -> HEAD
  | `POST -> POST
  | `PUT -> PUT
  | `DELETE -> DELETE
  | `CONNECT -> CONNECT
  | `OPTIONS -> OPTIONS
  | `TRACE -> TRACE
  | `PATCH -> PATCH
  | `Other s -> OTHER s

(* Resolve handler matching request and run it *)
let dispatch ~require_method ?catch s (io, _conn) req body =
  set_req_time ();
  begin
    match io with
    | Conduit_lwt_unix.TCP tcp ->
      begin
        match[@warning "-42"] Lwt_unix.getpeername
                                tcp.Conduit_lwt_unix.fd with
        | Lwt_unix.ADDR_INET (ip,_port) ->
          let ip = Ipaddr.to_string (Ipaddr_unix.of_inet_addr ip) in
          let ip =
            match Header.get (Cohttp.Request.headers req) "x-forwarded-for"
            with
            | None -> ip
            | Some ip -> ip
          in
          register_ip ip
        | Lwt_unix.ADDR_UNIX _path -> ()
      end
    | Conduit_lwt_unix.Domain_socket _
    | Conduit_lwt_unix.Vchan _ -> ()
  end;
  Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Debug;
  let local_path =
    req
    |> Cohttp.Request.uri
    |> Uri.path
  in
  let path =
    local_path
    |> String.split_on_char '/'
    |> list_trim
  in
  let req_params = req |> Request.uri |> Uri.query in
  let headers =
    let headers = ref StringMap.empty in
    Header.iter (fun s v ->
        headers :=
          StringMap.add (String.lowercase_ascii s) v !headers)
      (Request.headers req);
    !headers
  in
  let version = match Request.version req with
    | `HTTP_1_0 -> HTTP_1_0
    | `Other _ | `HTTP_1_1 -> HTTP_1_1
  in
  Cohttp_lwt.Body.to_string body >>= fun content ->
  let content_type =
    try
      match StringMap.find "content-type" headers with
      | s :: _ -> Some s
      | [] -> None
    with _ -> None
  in
  let body = BodyString (content_type, content) in
  let request = EzAPI.request ~version
      ~headers
      ~body
      req_params
  in
  debug "[%t] REQUEST: %s %S"
    pp_time
    (req |> Cohttp.Request.meth |> Cohttp.Code.string_of_method)
    (req |> Cohttp.Request.uri |> Uri.path_and_query);
  debugf ~v:1 (fun () ->
      StringMap.iter (fun s v ->
          List.iter (fun v -> EzDebug.printf "  %s: %s" s v) v)
        headers);
  let req_meth = of_cohttp_meth @@ Cohttp.Request.meth req in
  Lwt.catch
    (fun () ->
       if path = ["debug"] then
         reply_json 200
           (`O
              ["headers", `A
                 (Cohttp.Request.headers req
                  |> Header.to_lines
                  |> List.map (fun s -> `String s));
               "params", `O
                 (List.map (fun (arg,list) ->
                      arg, `A (List.map (fun s -> `String s) list)
                    ) req_params)
              ]
           )
       else
         let meth = if require_method then Some req_meth else None in
         match s.server_kind, req_meth, request.req_body with
         | API dir, OPTIONS, _ ->
           RestoDirectory1.lookup dir.meth_OPTIONS request path
           >>= fun (handler, _) -> handler None >>= fun _answer ->
           (* Note: this path always fails with EzReturnOPTIONS *)
           reply_none 200
         | API dir, _, BodyString (_, "") ->
           RestoDirectory1.lookup ?meth dir.meth_GET request path >>= fun (handler, _) ->
           handler None >>= reply_answer
         | API dir, _, BodyString (Some mime, content) when mime = EzUrl.content_type ->
           debug ~v:2 "Request params:\n  %s" content;
           EzAPI.add_params request ( EzUrl.decode_args content );
           RestoDirectory1.lookup ?meth dir.meth_GET request path >>= fun (handler, _) ->
           handler None >>= reply_answer
         | API dir, _, BodyString (Some mime, content) ->
           RestoDirectory1.lookup ?meth dir.meth_GET request path >>= fun (handler, allowed_mimes) ->
           if mime = "application/json" && EzAPIServerUtils.is_mime_allowed allowed_mimes mime then (
             debug ~v:2 "Request content:\n  %s" content;
             handler (Some (Ezjsonm.from_string content)) >>= reply_answer)
           else if EzAPIServerUtils.is_mime_allowed allowed_mimes mime then
             handler (Some (`String content)) >>= reply_answer
           else reply_none 415
         | API _, _, _ ->
           reply_none 415
         | Root (root, default), meth, _ ->
           reply_file ~meth root ?default path
    )
    (fun exn ->
       match exn with
       | EzReturnOPTIONS headers ->
         request.rep_headers <- headers @ request.rep_headers;
         reply_none 200
       | EzRawReturn s -> reply_raw_json 200 s
       | EzRawError code -> reply_none code
       | EzContentError (code, json) -> reply_raw_json code json
       | Not_found -> reply_none 404
       | RestoDirectory1.Cannot_parse (descr, msg, rpath) ->
         reply_answer (RestoDirectory1.response_of_cannot_parse descr msg rpath)
       | exn ->
         EzDebug.printf "In %s: exception %s" local_path @@ Printexc.to_string exn;
         match catch with
         | None -> reply_none 500
         | Some c -> c local_path exn)
  >>= fun (code, reply) ->
  let headers =
    Header.add_list
      (Header.add_list
         (Header.init_with
            "access-control-allow-origin" "*")
         [
           ("access-control-allow-headers", "Accept, Content-Type");
           ("access-control-allow-methods", "POST, GET, OPTIONS, PATCH, PUT, DELETE")
         ]) request.rep_headers in
  let status = Cohttp.Code.status_of_code code in
  debug ~v:(if code = 200 then 1 else 0) "Reply computed to %S: %d" local_path code;
  let body, headers = match reply with
    | ReplyNone ->
      Cohttp_lwt__Body.empty, headers
    | ReplyJson json ->
      let content = Ezjsonm.to_string (json_root json) in
      debug ~v:3 "Reply content:\n  %s" content;
      Cohttp_lwt__Body.of_string content,
      Header.add headers "Content-Type" "application/json"
    | ReplyString (content_type, content) ->
      debug ~v:3 "Reply content:\n  %s" content;
      Cohttp_lwt__Body.of_string content,
      Header.add headers "Content-Type" content_type
  in
  Cohttp_lwt__Body.to_string body >>= fun body ->
  Server.respond_string ~headers ~status ~body ()

(*********************************************************************)
(* HTTP Server                                                       *)
(*********************************************************************)

let server ?(require_method=false) ?catch servers =
  let create_server port kind =
    let s = { server_port = port;
              server_kind = kind;
            } in
    if not (EzAPI.all_services_registered ()) then (* exit 2 *) ();
    init_timings (EzAPI.nservices());
    let callback conn req body = dispatch ~require_method ?catch s conn req body in
    let dont_crash_on_exn exn =
      try
        raise exn
      with
      (* Broken Pipe -> do nothing *)
      | Unix.Unix_error (Unix.EPIPE, _, _) -> ()
      | exn ->
        EzDebug.printf "Server Error: %s" (Printexc.to_string exn)
    in
    (*  Cache.set_error_handler (fun e -> return @@ dont_crash_on_exn e); *)
    Server.create
      ~on_exn:dont_crash_on_exn
      ~mode:(`TCP (`Port port))
      (Server.make ~callback ())
  in
  Lwt.join (List.map (fun (port,kind) ->
      create_server port kind) servers)
