open Lwt

let () =
  Lwt.async_exception_hook := (fun exn -> EzDebug.printf "Exception %s" (Printexc.to_string exn))

let read_body ~read body =
  let w, n = Lwt.wait () in
  let b = Buffer.create 100 in
  let on_eof () = Lwt.wakeup_later n (Buffer.contents b) in
  let rec on_read bs ~off ~len =
    Buffer.add_string b (Bigstringaf.substring bs ~off ~len);
    read body ~on_eof ~on_read in
  read body ~on_eof ~on_read;
  w

let servers = ref []

let create_server ?(name="HTTPUN") ?addr server_port server_kind handler =
  let open EzAPIServerUtils in
  let s = { server_port; server_kind } in
  Timings.init (GMTime.time ()) @@ EzAPI.Doc.nservices ();
  ignore @@ EzAPI.Doc.all_services_registered ();
  EzDebug.printf "Running %s LWT server on %s:%d" name (Option.value ~default:"localhost" addr) server_port;
  let addr = match addr with
    | None -> Unix.inet_addr_loopback
    | Some s -> Unix.inet_addr_of_string s in
  let listen_address = Unix.(ADDR_INET (addr, server_port)) in
  Lwt_io.establish_server_with_client_socket listen_address (handler s) >>= fun s ->
  servers := !servers @ [ server_port, s ];
  Lwt.return_unit

let shutdown () = Lwt_list.iter_s (fun (_, s) -> Lwt_io.shutdown_server s) !servers

let server ?name ?catch ?allow_origin ?footer ?addr handler servers =
  let waiter = fst @@ Lwt.wait () in
  Lwt.join (List.map (fun (port, kind) ->
      let handler s sockaddr fd =
        handler ?catch ?allow_origin ?footer s sockaddr fd in
      create_server ?name ?addr port kind handler) servers) >>= fun () ->
  waiter
