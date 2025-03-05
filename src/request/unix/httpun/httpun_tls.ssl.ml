let request ?(alpn_protocols=["http/1.1"]) ~hostname ~socket ~error_handler ~response_handler req =
  let client_ctx = Ssl.create_context Ssl.TLSv1_3 Ssl.Client_context in
  Ssl.honor_cipher_order client_ctx;
  Ssl.set_context_alpn_protos client_ctx alpn_protocols;
  let uninitialized_socket = Lwt_ssl.embed_uninitialized_socket socket client_ctx in
  let ssl_socket = Lwt_ssl.ssl_socket_of_uninitialized_socket uninitialized_socket in
  Ssl.set_client_SNI_hostname ssl_socket hostname;
  Ssl.set_host ssl_socket hostname;
  Lwt.bind (Lwt_ssl.ssl_perform_handshake uninitialized_socket) @@ fun ssl_client ->
  Lwt.bind (Httpun_lwt_unix.Client.SSL.create_connection ssl_client) @@ fun connection ->
  Lwt.return_ok @@ Httpun_lwt_unix.Client.SSL.request connection req ~error_handler ~response_handler
