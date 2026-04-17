let first = ref true

let ws _req ?onclose:_ ?step:_ ?body:_ ~react:_ ~bg:_ _id =
  (if !first then (
      first := false;
      Format.eprintf
        "\027[0;31merror: websocket or cohttp-eio not installed\027[0m\n\
         try: `opam install calendar websocket cohttp-eio`@."));
  Error `no_ws_library
