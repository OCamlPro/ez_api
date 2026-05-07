let run ?catch:_ ?allow_origin:_ ?footer:_ ?addr:_ ~env:_ ~sw:_ _l =
  Format.eprintf
    "Cohttp or Httpun server implementation not availble\n\
     Try: `opam install cohttp-eio`\nor `opam install httpun-eio`@."
