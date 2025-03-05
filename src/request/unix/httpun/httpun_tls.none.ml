let request ?alpn_protocols:_ ~hostname:_ ~socket:_ ~error_handler:_ ~response_handler:_ _req =
  Lwt.return_error (-1, Some "no tls engine installed: `opam install tls-lwt` or `opam install lwt_ssl`")
