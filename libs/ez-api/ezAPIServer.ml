open StringCompat
open Lwt
open EzAPI.TYPES
open EzAPIServerUtils

module Header = Cohttp.Header
module Request = Cohttp.Request
module Server = Cohttp_lwt_unix.Server

(* Resolve handler matching request and run it *)
let dispatch s (io, _conn) req body =
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
  if verbose > 0 then
    Printf.eprintf "REQUEST: %s %S\n%!"
      (req |> Cohttp.Request.meth |> Cohttp.Code.string_of_method)
      local_path;
  if verbose > 1 then
    StringMap.iter (fun s v ->
        List.iter (fun v ->
            Printf.eprintf "  %s: %s\n%!" s v;
          ) v
      ) headers;
  let meth = Cohttp.Request.meth req in
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
         match s.server_kind, meth with
         | API dir, `OPTIONS ->
           RestoDirectory1.lookup dir.meth_OPTIONS request path
           >>= fun handler -> handler None >>= fun _answer ->
           (* Note: this path always fails with EzReturnOPTIONS *)
           reply_none 200
         | API dir, _ ->
           let content =
             match meth, request.req_body with
             | `GET, BodyString (_, "") -> None
             | _, BodyString (Some "application/x-www-form-urlencoded", content) ->
                EzAPI.add_params request ( EzUrl.decode_args content );
                None
             | _, BodyString (Some "application/json", content) ->
                Some (Ezjsonm.from_string content)
             | _, BodyString (_, content) ->
                try
                  Some (Ezjsonm.from_string content)
                with _ -> None
           in
           RestoDirectory1.lookup dir.meth_GET request path >>= fun handler ->
           handler content
           >>= reply_answer
         | Root (root, default), meth ->
           reply_file ~meth root ?default path
    )
    (fun exn ->
       Printf.eprintf "Exception %s\n%!" (Printexc.to_string exn);
       match exn with
       | EzReturnOPTIONS headers ->
         request.rep_headers <- headers @ request.rep_headers;
         reply_none 200
       | EzRawReturn s -> reply_raw_json 200 s
       | EzRawError code -> reply_none code
       | EzContentError (code, json) ->
         reply_raw_json code json
       | Not_found -> reply_none 404
       | exn ->
         Printf.eprintf "In %s: exception %s\n%!"
           local_path (Printexc.to_string exn);
         reply_none 500)
  >>= fun (code, reply) ->
  let headers =
    Header.add_list
      (Header.add_list
         (Header.init_with
            "access-control-allow-origin" "*")
         [
           ("access-control-allow-headers", "Accept, Content-Type");
           ("access-control-allow-methods", "POST, GET, OPTIONS, PUT, DELETE")
         ]) request.rep_headers in
  let status = Cohttp.Code.status_of_code code in
  if verbose > 1 then
    Printf.eprintf "Reply computed %d\n%!" code;
  let body, headers = match reply with
    | ReplyNone ->
      Cohttp_lwt__Body.empty, headers
    | ReplyJson json ->
      let content = Ezjsonm.to_string (json_root json) in
      if verbose > 2 then
        Printf.eprintf "Content:\n%s\n%!" content;
      Cohttp_lwt__Body.of_string content,
      Header.add headers "Content-Type" "application/json"
    | ReplyString (content_type, content) ->
      if verbose > 2 then
        Printf.eprintf "Content:\n%s\n%!" content;
      Cohttp_lwt__Body.of_string content,
      Header.add headers "Content-Type" content_type
  in
  Cohttp_lwt__Body.to_string body >>= fun body ->
  Server.respond_string ~headers ~status ~body ()

(*********************************************************************)
(* HTTP Server                                                       *)
(*********************************************************************)

let server servers =
  let create_server port kind =
    let s = { server_port = port;
              server_kind = kind;
            } in
    if not (EzAPI.all_services_registered ()) then (* exit 2 *) ();
    init_timings (EzAPI.nservices());
    let callback conn req body = dispatch s conn req body in
    let dont_crash_on_exn exn =
      try
        raise exn
      with
      (* Broken Pipe -> do nothing *)
      | Unix.Unix_error (Unix.EPIPE, _, _) -> ()
      | exn ->
        Printf.eprintf "Server Error: %s\n%!" (Printexc.to_string exn)
    in
    (*  Cache.set_error_handler (fun e -> return @@ dont_crash_on_exn e); *)
    Server.create
      ~on_exn:dont_crash_on_exn
      ~mode:(`TCP (`Port port))
      (Server.make ~callback ())
  in
  Lwt.join (List.map (fun (port,kind) ->
      create_server port kind) servers)
