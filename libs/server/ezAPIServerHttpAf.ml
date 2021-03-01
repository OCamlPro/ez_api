open EzAPI.TYPES
open Lwt
open StringCompat
open Httpaf
open EzAPIServerUtils

external limit_open_file : unit -> int = "rlimit_no_file_c"

type lwt_server = {
  shutdown : unit Lwt.t Lazy.t;
}

let of_httpaf_meth = function
  | `Other "patch" | `Other "PATCH" | `Other "Patch" -> `PATCH
  | #Resto1.method_type as m -> m

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
      Lwt_unix.close listening_socket >|= fun () ->
      begin match listening_address with
        | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
          Unix.unlink path
        | _ -> ()
      end [@ocaml.warning "-4"];
      Lwt.wakeup_later notify_listening_socket_closed ()
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
    Lwt.async @@ fun () ->
    Lwt.catch
      (fun () -> f client_address client_socket)
      (fun exn ->
         !Lwt.async_exception_hook exn;
         Lwt.return_unit)
    >>= fun () ->
    decr_connections nb_max_connections ;
    if no_close then
      Lwt.return_unit
    else if Lwt_unix.state client_socket = Lwt_unix.Closed then
      Lwt.return_unit
    else
      Lwt.catch
        (fun () -> close_socket client_socket)
        (fun exn ->
           !Lwt.async_exception_hook exn;
           Lwt.return_unit) in

  let server, server_started =
    establish_server_generic
      Lwt_unix.bind ?fd:server_fd ?backlog sockaddr handler nb_max_connections in
  server_started >|= fun () ->
  server

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
  StringMap.fold (fun k v acc -> Headers.add acc k @@ List.hd v)
    m Headers.empty

let af_headers_to_string_map m =
  Headers.fold ~f:(fun k v acc -> StringMap.add (String.lowercase_ascii k) [v] acc)
    ~init:StringMap.empty m

let add_headers_response headers =
  let h = StringMap.add "access-control-allow-origin" [ "*" ] headers in
  let h =
    StringMap.add "access-control-allow-headers" [ "Accept, Content-Type" ] h in
  StringMap.add "access-control-allow-methods" [ "POST, GET, OPTIONS, PATCH, PUT, DELETE" ] h

let read_body body =
  let w, n = Lwt.wait () in
  let body_str = ref "" in
  let on_eof () = Lwt.wakeup n !body_str in
  let rec on_read bs ~off ~len =
    body_str := !body_str ^ Bigstringaf.substring bs ~off ~len;
    Body.schedule_read body ~on_read ~on_eof in
  Body.schedule_read
    body
    ~on_eof
    ~on_read;
  w

let connection_handler :
  require_method:bool -> ?catch:(string -> exn -> (int * reply) Lwt.t) ->
  server -> Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  fun ~require_method ?catch s sockaddr fd ->
  let request_handler client_address request_descriptor =
    set_req_time () ;
    let request = Reqd.request request_descriptor in
    let uri = mk_uri request in
    let req_params = Uri.query uri in
    debug "[%t] REQUEST: %s %S" pp_time
      (Method.to_string request.Request.meth)
      request.Request.target;
    debugf ~v:1 (fun () ->
        List.iter (fun (name, value) -> EzDebug.printf "  %s: %s" name value)
          (Headers.to_list request.Request.headers));
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

    let headers = af_headers_to_string_map request.Request.headers in
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
    Lwt.async (fun () ->
        begin
          if path = ["debug"] then
            reply_json 200
              (`O
                 ["headers", `A [];
                  "params", `O
                    (List.map (fun (arg,list) ->
                         arg, `A (List.map (fun s -> `String s) list)
                       ) req_params)
                 ]
              )
          else
            Lwt.catch (fun () ->
                read_body req_body >>= fun body ->
                let body = BodyString (Some response_content_type, body) in
                let ez_request =
                  EzAPI.request ~version ~headers ~body req_params in
                let req_meth = of_httpaf_meth request.Request.meth in
                let meth = if require_method then Some req_meth else None in
                match s.server_kind, req_meth, ez_request.req_body with
                | API dir, `OPTIONS, _ ->
                  RestoDirectory1.lookup dir.meth_OPTIONS ez_request path
                  >>= fun (handler, _) -> handler None >>= fun _answer ->
                  reply_none 200
                | API dir, _, BodyString (_, "") ->
                  RestoDirectory1.lookup ?meth dir.meth_GET ez_request path >>= fun (handler, _) ->
                  handler None >>= reply_answer
                | API dir, _, BodyString (Some mime, content) when mime = EzUrl.content_type ->
                  debug ~v:2 "Request params:\n  %s" content;
                  EzAPI.add_params ez_request ( EzUrl.decode_args content );
                  RestoDirectory1.lookup ?meth dir.meth_GET ez_request path >>= fun (handler, _) ->
                  handler None >>= reply_answer
                | API dir, _, BodyString (Some mime, content) ->
                  RestoDirectory1.lookup ?meth dir.meth_GET ez_request path >>= fun (handler, allowed_mimes) ->
                  if mime = "application/json" && EzAPIServerUtils.is_mime_allowed allowed_mimes mime then (
                    debug ~v:2 "Request content:\n  %s" content;
                    handler (Some (Ezjsonm.from_string content)) >>= reply_answer)
                  else if EzAPIServerUtils.is_mime_allowed allowed_mimes mime then
                    handler (Some (`String content)) >>= reply_answer
                  else reply_none 415
                | API _, _, _ -> reply_none 415
                | Root (root, default), meth, _ ->
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
                   EzDebug.printf "In %s: exception %s" local_path @@ Printexc.to_string exn;
                   match catch with
                   | None -> reply_none 500
                   | Some c -> c local_path exn)
        end >>= fun (code, reply) ->
        let status = Httpaf.Status.unsafe_of_code code in
        debug ~v:(if code = 200 then 1 else 0) "Reply computed to %S: %d" local_path code;
        let headers = add_headers_response headers in
        let headers = af_headers_from_string_map headers in
        let body_str, headers = match reply with
          | ReplyNone -> "", headers
          | ReplyJson json ->
            let headers = Headers.remove headers "content-type" in
            let content = Ezjsonm.to_string (json_root json) in
            debug ~v:3 "Reply content:\n  %s" content;
            content,
            Headers.add headers "content-type" "application/json"
          | ReplyString (content_type, content) ->
            let headers = Headers.remove headers "content-type" in
            debug ~v:3 "Reply content:\n  %s" content;
            content,
            Headers.add headers "content-type" content_type
        in
        let len = String.length body_str in
        let headers = Headers.remove headers "content-length" in
        let headers = Headers.add headers "content-length" (string_of_int len) in
        let response = Response.create ~headers status in
        Reqd.respond_with_string request_descriptor response body_str;
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
      Body.flush response_body (fun () -> Body.close_writer response_body)
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

let server ?(require_method=false) ?catch servers =
  let create_server port kind =
    let s = { server_port = port;
              server_kind = kind;
            } in
    if not (EzAPI.all_services_registered ()) then (* exit 2 *) ();
    init_timings (EzAPI.nservices());
    let listen_address = Unix.(ADDR_INET (inet_addr_any, port)) in
    let nb_max_connections = limit_open_file () - 100 in
    EzDebug.printf "Setting max_connection to %d" nb_max_connections;
    establish_server_with_client_socket
      ~nb_max_connections
      listen_address (fun sockaddr fd ->
          connection_handler ~require_method ?catch s sockaddr fd) >>= fun _server ->
    Lwt.return_unit
  in
  let waiter, _ = Lwt.wait () in
  Lwt.join (List.map (fun (port,kind) -> create_server port kind) servers) >>= fun () ->
  waiter (* keep the server running *)
