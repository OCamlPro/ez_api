let server ?catch:_ _ =
  Format.eprintf
    "Cohttp server implementation not availble\n\
     Try: `opam install cohttp-lwt-unix websocket-lwt-unix calendar geoip`\n\
     or:  `opam install httpaf-lwt-unix geoip`@.";
  Lwt.return_unit

let set_debug () = ()
