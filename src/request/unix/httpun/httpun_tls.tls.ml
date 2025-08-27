let request ?(alpn_protocols=["http/1.1"]) ~hostname ~socket ~error_handler ~response_handler req =
  let authenticator ?ip:_ ~host:_ _ = Ok None in
  let peer_name = Domain_name.(host_exn (of_string_exn hostname)) in
  let config = Result.get_ok @@ Tls.Config.client ~alpn_protocols ~authenticator ~peer_name () in
  Lwt.bind (Tls_lwt.Unix.client_of_fd config socket) @@ fun tls_client ->
  Lwt.bind (Httpun_lwt_unix.Client.TLS.create_connection tls_client) @@ fun connection ->
  let shutdown () = Httpun_lwt_unix.Client.TLS.shutdown connection in
  Lwt.return_ok (Httpun_lwt_unix.Client.TLS.request connection req ~error_handler ~response_handler, shutdown)
