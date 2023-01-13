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

let ws _req ?onclose:_ ?step:_ ~react:_ ~bg:_ _id =
  (if !first then (
      first := false;
      Format.eprintf
        "\027[0;31merror: websocket-lwt-unix not installed\027[0m\n\
         try: `opam install calendar websocket-lwt-unix`@."));
  Lwt.return_error `no_ws_library
