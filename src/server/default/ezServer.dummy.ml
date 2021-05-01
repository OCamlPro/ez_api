let server ?catch:_ _ =
  Format.eprintf
    "Cohttp or Httpaf server implementation not availble\n\
     Try: `opam install cohttp-lwt-unix`\n\
     or:  `opam install httpaf-lwt-unix`@.";
  Lwt.return_unit

let set_debug () = ()
