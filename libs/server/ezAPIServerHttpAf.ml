open EzAPI.TYPES
open Lwt
open StringCompat
open Httpaf
open EzAPIServerUtils

type lwt_server = {
  shutdown : unit Lwt.t Lazy.t;
}

let of_httpaf_meth = function
  | `GET -> Resto1.GET
  | `HEAD -> HEAD
  | `POST -> POST
  | `PUT -> PUT
  | `DELETE -> DELETE
  | `CONNECT -> CONNECT
  | `OPTIONS -> OPTIONS
  | `TRACE -> TRACE
  | `Other "patch" -> PATCH
  | `Other s -> OTHER s


let () =
  Lwt.async_exception_hook := (fun exn -> EzDebug.printf "Exception %s" (Printexc.to_string exn))

let nb_live_connections = ref 0

let listen_cond = Lwt_condition.create ()

let incr_connections () =
  incr nb_live_connections

let decr_connections nb_max =
  if !nb_live_connections = nb_max - 1 then
    Lwt_condition.signal listen_cond () ;
  decr nb_live_connections

let close_socket fd =
  Lwt.finalize
    (fun () ->
       Lwt.catch
         (fun () ->
            Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
            Lwt.return_unit)
         (function
           (* Occurs if the peer closes the connection first. *)
           | Unix.Unix_error (Unix.ENOTCONN, _, _) -> Lwt.return_unit
           | exn -> Lwt.fail exn) [@ocaml.warning "-4"])
    (fun () ->
       Lwt_unix.close fd)

(* There are several variants of establish_server that have accumulated over the
   years in Lwt_io. This is their underlying implementation. The functions
   exposed in the API are various wrappers around this one. *)
let establish_server_generic
    bind_function
    ?fd:preexisting_socket_for_listening
    ?(backlog = 5)
    listening_address
    connection_handler_callback
    nb_max_connections =

  let listening_socket =
    match preexisting_socket_for_listening with
    | None ->
      Lwt_unix.socket
        (Unix.domain_of_sockaddr listening_address) Unix.SOCK_STREAM 0
    | Some socket ->
      socket
  in
  Lwt_unix.setsockopt listening_socket Unix.SO_REUSEADDR true;

  (* This promise gets resolved with `Should_stop when the user calls
     Lwt_io.shutdown_server. This begins the shutdown procedure. *)
  let should_stop, notify_should_stop =
    Lwt.wait () in

  (* Some time after Lwt_io.shutdown_server is called, this function
     establish_server_generic will actually close the listening socket. At that
     point, this promise is resolved. This ends the shutdown procedure. *)
  let wait_until_listening_socket_closed, notify_listening_socket_closed =
    Lwt.wait () in

  let rec accept_loop () =
    let try_to_accept =
      incr_connections () ;
      Lwt_unix.accept listening_socket >|= fun x ->
      `Accepted x
    in
    Lwt.pick [ try_to_accept; should_stop ] >>= function
    | `Accepted (client_socket, client_address) ->
      begin
        try Lwt_unix.set_close_on_exec client_socket
        with Invalid_argument _ -> ()
      end;
      connection_handler_callback client_address client_socket;
      if !nb_live_connections >= nb_max_connections then
        Lwt_condition.wait listen_cond >>= fun () ->
        accept_loop ()
      else accept_loop ()
    | `Should_stop ->
      Lwt_unix.close listening_socket >>= fun () ->

      begin match listening_address with
        | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
          Unix.unlink path
        | _ ->
          ()
      end [@ocaml.warning "-4"];

      Lwt.wakeup_later notify_listening_socket_closed ();
      Lwt.return_unit
  in

  let server =
    {shutdown =
       lazy begin
         Lwt.wakeup_later notify_should_stop `Should_stop;
         wait_until_listening_socket_closed
       end}
  in

  (* Actually start the server. *)
  let server_has_started =
    bind_function listening_socket listening_address >>= fun () ->
    Lwt_unix.listen listening_socket backlog;

    Lwt.async accept_loop;

    Lwt.return_unit
  in

  server, server_has_started

let establish_server_with_client_socket
    ?server_fd ?backlog ?(no_close = false) ~nb_max_connections sockaddr f =
  let handler client_address client_socket =
    Lwt.async begin fun () ->
      (* Not using Lwt.finalize here, to make sure that exceptions from [f]
         reach !Lwt.async_exception_hook before exceptions from closing the
         channels. *)
      Lwt.catch
        (fun () -> f client_address client_socket)
        (fun exn ->
           !Lwt.async_exception_hook exn;
           Lwt.return_unit)
      >>= fun () ->
      decr_connections nb_max_connections ;
      if no_close then Lwt.return_unit
      else
      if Lwt_unix.state client_socket = Lwt_unix.Closed then
        Lwt.return_unit
      else
        Lwt.catch
          (fun () -> close_socket client_socket)
          (fun exn ->
             !Lwt.async_exception_hook exn;
             Lwt.return_unit)
    end
  in

  let server, server_started =
    establish_server_generic
      Lwt_unix.bind ?fd:server_fd ?backlog sockaddr handler nb_max_connections
  in
  server_started >>= fun () ->
  Lwt.return server

let mk_uri  { Request.meth ; Request.target ; Request.headers ; _ } =
  match target with
  | "*" ->
    begin match Headers.get headers "host" with
      | None -> Uri.of_string ""
      | Some host ->
        let host_uri = Uri.of_string ("//"^host) in
        let uri = Uri.(with_host (of_string "") (host host_uri)) in
        Uri.(with_port uri (port host_uri))
    end
  | authority when meth = `CONNECT -> Uri.of_string ("//" ^ authority)
  | path ->
    let uri = Uri.of_string path in
    begin match Uri.scheme uri with
      | Some _ -> (* we have an absoluteURI *)
        Uri.(match path uri with "" -> with_path uri "/" | _ -> uri)
      | None ->
        let empty = Uri.of_string "" in
        let empty_base = Uri.of_string "///" in
        let pqs = match Stringext.split ~max:2 path ~on:'?' with
          | [] -> empty_base
          | [path] ->
            Uri.resolve "http" empty_base (Uri.with_path empty path)
          | path::qs::_ ->
            let path_base =
              Uri.resolve "http" empty_base (Uri.with_path empty path)
            in
            Uri.with_query path_base (Uri.query_of_encoded qs)
        in
        let uri = match Headers.get headers "host" with
          | None -> Uri.(with_scheme (with_host pqs None) None)
          | Some host ->
            let host_uri = Uri.of_string ("//"^host) in
            let uri = Uri.with_host pqs (Uri.host host_uri) in
            Uri.with_port uri (Uri.port host_uri)
        in
        uri
    end

let af_headers_from_string_map m =
  StringMap.fold (fun k v acc ->
      Headers.add acc k @@ List.hd v)
    m Headers.empty

let add_headers_response headers =
  let h = StringMap.add "access-control-allow-origin" [ "*" ] headers in
  let h =
    StringMap.add "access-control-allow-headers" [ "Accept, Content-Type" ] h in
  StringMap.add "access-control-allow-methods" [ "POST, GET, OPTIONS, PATCH, PUT, DELETE" ] h

let read_body body =
  let body_str = ref "" in
  Httpaf.Body.schedule_read
    body
    ~on_eof:(fun () -> ())
    ~on_read:(fun request_data ~off ~len ->
        body_str := !body_str ^ Bigstringaf.substring request_data ~off ~len) ;
  !body_str


let connection_handler :
  require_method:bool -> server -> Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  fun ~require_method s sockaddr fd ->
    let module Body = Httpaf.Body in
    let module Headers = Httpaf.Headers in
    let module Reqd = Httpaf.Reqd in
    let module Response = Httpaf.Response in
    let module Status = Httpaf.Status in
    (* Lwt_unix.sleep 5. >>= fun () -> *)
    let request_handler client_address request_descriptor =
      set_req_time () ;
      let request = Reqd.request request_descriptor in
      let uri = mk_uri request in
      let req_params = Uri.query uri in
      debug "REQUEST: %s %S"
        (Method.to_string request.meth)
        request.target;
      debugf ~v:1 (fun () ->
          List.iter (fun (name, value) -> EzDebug.printf "  %s: %s" name value)
            (Headers.to_list request.headers));
      begin match client_address with
        | Unix.ADDR_INET (iaddr, _port) ->
          let ip = Unix.string_of_inet_addr iaddr in
          let ip =
            match Headers.get (request.Request.headers) "x-forwarded-for" with
            | None -> ip
            | Some ip -> ip
          in
          register_ip ip
        | Unix.ADDR_UNIX _ -> ()
      end ;
      let local_path = Uri.path uri in
      let path = local_path |> String.split_on_char '/' |> list_trim in

      let headers =
        let headers = ref StringMap.empty in
        Headers.iter ~f:(fun s v ->
            headers :=
              StringMap.add (String.lowercase_ascii s) [v] !headers)
          (request.Httpaf.Request.headers);
        !headers
      in
      let version =
        if request.Request.version.Version.minor = 0
        then HTTP_1_0
        else HTTP_1_1 in

      let response_content_type =
        match Headers.get request.Request.headers "Content-Type" with
        | Some request_content_type -> request_content_type
        | None -> "application/octet-stream"
      in
      let req_body = Reqd.request_body request_descriptor in
      let body = BodyString (Some response_content_type, read_body req_body) in
      let ez_request =
        EzAPI.request ~version ~headers ~body req_params in

      Lwt.async (fun () ->
          begin
            if path = ["debug"] then
              reply_json 200
                (`O
                   ["headers", `A []
                    (* (Cohttp.Request.headers req
                     *  |> Header.to_lines
                     *  |> List.map (fun s -> `String s)) *);
                    "params", `O
                      (List.map (fun (arg,list) ->
                           arg, `A (List.map (fun s -> `String s) list)
                         ) req_params)
                   ]
                )
            else
              Lwt.catch (fun () ->
                  let meth = of_httpaf_meth request.Request.meth in
                  match s.server_kind, meth with
                  | API dir, OPTIONS ->
                    RestoDirectory1.lookup dir.meth_OPTIONS ez_request path >>= fun handler ->
                    handler None >>= fun _answer -> reply_none 200
                  | API dir, _ ->
                    let content =
                      match request.Request.meth, ez_request.req_body with
                      | `GET, BodyString (_, "") -> None
                      | _, BodyString (Some "application/x-www-form-urlencoded", content) ->
                        debug ~v:2 "Request params:\n  %s" content;
                        EzAPI.add_params ez_request ( EzUrl.decode_args content );
                        None
                      | _, BodyString (Some mime, content) when
                          Re.Str.(string_match (regexp "image") mime 0)
                          || mime = "multipart/form-data" ->
                        Some (`String content)
                      | _, BodyString (_, content) ->
                        if content = "" then None
                        else (
                          debug ~v:2 "Request content:\n  %s" content;
                          Some (Ezjsonm.from_string content))
                    in
                    let meth = if require_method then Some meth else None in
                    RestoDirectory1.lookup ?meth dir.meth_GET ez_request path >>= fun handler ->
                    (* Lwt.pick [ *)
                    handler content >>= reply_answer ;
                    (* Lwt_unix.sleep 0.1 >>= fun () -> reply_none 408 *) (* ] *)
                  | Root (root, default), meth ->
                    reply_file ~meth root ?default path)
                (fun exn ->
                   match exn with
                   | EzReturnOPTIONS _ -> reply_none 200
                   | EzRawReturn s -> reply_raw_json 200 s
                   | EzRawError code -> reply_none code
                   | EzContentError (code, json) ->
                     reply_raw_json code json
                   | Not_found -> reply_none 404
                   | RestoDirectory1.Cannot_parse (descr, msg, rpath) ->
                     reply_answer
                       (RestoDirectory1.response_of_cannot_parse
                          descr msg rpath)
                   | exn ->
                     EzDebug.printf "In %s: exception %s"
                       local_path (Printexc.to_string exn);
                     reply_none 500)
          end >>= fun (code, reply) ->
          let status = Httpaf.Status.unsafe_of_code code in
          debug ~v:(if code = 200 then 1 else 0) "Reply computed to %S: %d" local_path code;
          let headers = add_headers_response ez_request.req_headers in
          let headers = af_headers_from_string_map headers in
          let body_str, headers = match reply with
            | ReplyNone -> "", headers
            | ReplyJson json ->
              let content = Ezjsonm.to_string (json_root json) in
              debug ~v:3 "Reply content:\n  %s" content;
              content,
              Headers.add headers "Content-Type" "application/json"
            | ReplyString (content_type, content) ->
              debug ~v:3 "Reply content:\n  %s" content;
              content,
              Headers.add headers "Content-Type" content_type
          in
          let len = String.length body_str in
          let headers = Headers.add headers "content-length" (string_of_int len) in
          let response = Response.create ~headers status in
          let body = Reqd.respond_with_streaming request_descriptor response in
          Body.write_string body body_str ;
          begin
            try
              Body.flush body (fun () ->
                  Body.close_writer body)
            with exn -> raise exn
          end ;
          Lwt.return_unit)
    in

    let error_handler :
      Unix.sockaddr ->
      ?request:Httpaf.Request.t ->
      _ ->
      (Headers.t -> [`write] Body.t) ->
      unit =
      fun _client_address ?request:_ error start_response ->

        let response_body = start_response Headers.empty in

        begin match error with
          | `Exn exn ->
            Body.write_string response_body (Printexc.to_string exn);
            Body.write_string response_body "\n";
          | #Status.standard as error ->
            Body.write_string response_body (Status.default_reason_phrase error)
        end;
        Body.flush response_body (fun () ->
            Body.close_writer response_body
          );
    in

    Httpaf_lwt_unix.Server.create_connection_handler
      ?config:None
      ~request_handler
      ~error_handler
      sockaddr
      fd


(*********************************************************************)
(* HTTP Server                                                       *)
(*********************************************************************)

let server ?(require_method=false) servers =
  let create_server port kind =
    let s = { server_port = port;
              server_kind = kind;
            } in
    if not (EzAPI.all_services_registered ()) then (* exit 2 *) ();
    init_timings (EzAPI.nservices());
    let listen_address = Unix.(ADDR_INET (inet_addr_any, port)) in
    let nb_max_connections = match ExtUnixAll.(getrlimit RLIMIT_NOFILE) with
      | Some soft64 , _ ->
        let soft = Int64.to_int soft64 in
        let nb = soft - 100 in
        EzDebug.printf "Setting max_connection to %d" nb  ;
        nb
      | _ -> assert false in
    establish_server_with_client_socket
      ~nb_max_connections
      listen_address (fun sockaddr fd ->
          connection_handler ~require_method s sockaddr fd) >>= fun _server ->
    Lwt.return_unit
  in
  Lwt.join (List.map (fun (port,kind) ->
      create_server port kind) servers)
