(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let first = ref true

let ws _fd _req ?onclose:_ ?step:_ ~react:_ ~bg:_ _id =
  (if !first then (
      first := false;
      Format.eprintf
        "\027[0;31merror: websocket-httpaf-lwt not installed\027[0m\n\
         try: `opam install calendar`\n\
         `opam pin add websocket-httpaf.~dev https://github.com/anmonteiro/websocket-httpaf.git`\n\
         `opam pin add websocket-httpaf-lwt.~dev https://github.com/anmonteiro/websocket-httpaf.git`@."));
  Lwt.return_error `no_ws_library
