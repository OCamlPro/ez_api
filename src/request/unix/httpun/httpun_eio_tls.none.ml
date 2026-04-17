let socket ?alpn_protocols:_ ~hostname:_ _socket =
  Error (`msg "no tls engine installed: `opam install eio-ssl`")
