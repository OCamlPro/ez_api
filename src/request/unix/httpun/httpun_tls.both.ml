let encryption : [ `SSL | `TLS ] ref = ref `TLS

let request ?(alpn_protocols=["http/1.1"]) ~hostname ~socket ~error_handler ~response_handler req =
  match !encryption with
  | `TLS ->
    let authenticator ?ip:_ ~host:_ _ = Ok None in
    let peer_name = Domain_name.(host_exn (of_string_exn hostname)) in
    let config = Result.get_ok @@ Tls.Config.client ~alpn_protocols ~authenticator ~peer_name () in
    Lwt.bind (Tls_lwt.Unix.client_of_fd config socket) @@ fun tls_client ->
    Lwt.bind (Httpun_lwt_unix.Client.TLS.create_connection tls_client) @@ fun connection ->
    Lwt.return_ok @@ Httpun_lwt_unix.Client.TLS.request connection req ~error_handler ~response_handler
  | `SSL ->
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
