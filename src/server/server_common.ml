open Lwt

external limit_open_file : unit -> int = "rlimit_no_file_c"

type lwt_server = {
  shutdown : unit Lwt.t Lazy.t;
}

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

let read_body ~read body =
  let w, n = Lwt.wait () in
  let body_str = ref "" in
  let on_eof () = Lwt.wakeup n !body_str in
  let rec on_read bs ~off ~len =
    body_str := !body_str ^ Bigstringaf.substring bs ~off ~len;
    read body ~on_eof ~on_read in
  read body ~on_eof ~on_read;
  w

let meth_from_ext = function
  | `Other ("patch" | "PATCH" | "Patch") -> Some `PATCH
  | `GET | `PUT | `OPTIONS | `POST | `DELETE | `HEAD as m -> Some m
  | _ -> None

let debug_http_ext ~meth ~target ~headers =
  EzAPIServerUtils.debug "[%t] REQUEST: %s %S" EzAPIServerUtils.pp_time meth target;
  EzAPIServerUtils.debugf ~v:1 @@ fun () ->
  List.iter (fun (name, value) -> EzDebug.printf "  %s: %s" name value) headers

let register_ip ~header time = function
  | Unix.ADDR_INET (iaddr, _port) ->
    let ip = Unix.string_of_inet_addr iaddr in
    let ip = match header "x-forwarded-for" with
      | None -> ip
      | Some ip -> ip
    in
    EzAPIServerUtils.Ip.register time ip
  | Unix.ADDR_UNIX _ -> ()

let mk_uri ~meth ~target ~header =
  match target with
  | "*" ->
    begin match header "host" with
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
        let uri = match header "host" with
          | None -> Uri.(with_scheme (with_host pqs None) None)
          | Some host ->
            let host_uri = Uri.of_string ("//"^host) in
            let uri = Uri.with_host pqs (Uri.host host_uri) in
            Uri.with_port uri (Uri.port host_uri)
        in
        uri
    end

let create_server ?(name="HTTPUN") ~max_connections server_port server_kind handler =
  let open EzAPIServerUtils in
  let s = { server_port; server_kind } in
  Timings.init (GMTime.time ()) @@ EzAPI.Doc.nservices ();
  ignore @@ EzAPI.Doc.all_services_registered ();
  let listen_address = Unix.(ADDR_INET (inet_addr_any, server_port)) in
  EzDebug.printf "Starting %s server (port=%d, connection=%d)" name server_port max_connections;
  establish_server_with_client_socket
    ~nb_max_connections:max_connections
    listen_address (handler s)(* fun sockaddr fd -> *)
        (* handler ?catch ?allow_origin ?allow_headers ?allow_methods *)
        (*   ?allow_credentials s sockaddr fd) *) >>= fun _server ->
  Lwt.return_unit

let server ?name ?catch ?allow_origin ?allow_headers ?allow_methods ?allow_credentials ?footer handler servers =
  let max_connections =
    let n = List.length servers in
    if n = 0 then 0 else limit_open_file () / 2 / n in
  let waiter = fst @@ Lwt.wait () in
  Lwt.join (List.map (fun (port, kind) ->
      let handler s sockaddr fd =
        handler ?catch ?allow_origin ?allow_headers ?allow_methods ?allow_credentials ?footer s sockaddr fd in
      create_server ?name ~max_connections port kind handler) servers) >>= fun () ->
  waiter
