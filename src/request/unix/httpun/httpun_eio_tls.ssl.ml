let socket ?(alpn_protocols=["http/1.1"]) ~hostname socket =
  let client_ctx = Ssl.create_context Ssl.TLSv1_3 Ssl.Client_context in
  Ssl.honor_cipher_order client_ctx;
  Ssl.set_context_alpn_protos client_ctx alpn_protocols;
  let ssl_ctx = Eio_ssl.Context.create ~ctx:client_ctx socket in
  let ssl_socket = Eio_ssl.Context.ssl_socket ssl_ctx in
  Ssl.set_client_SNI_hostname ssl_socket hostname;
  Ssl.set_hostflags ssl_socket [ Ssl.No_partial_wildcards ];
  Ssl.set_host ssl_socket hostname;
  Ok (Eio_ssl.connect ssl_ctx)
