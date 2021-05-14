let get ( _req : Req.t ) =
  Format.eprintf "Dummy implementation of cookie server, to install:\n`opam install cohttp`@."
  StringMap.empty

let set ?secure:_ ?http_only:_ (_req : Req.t) ~name ~value =
  Format.eprintf "Dummy implementation of cookie server, to install:\n`opam install cohttp`@."
  (name, value)

let clear _req ~name = (name, "")
