(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let server ?catch:_ ?allow_origin:_ ?allow_headers:_ ?allow_methods:_ ?allow_credentials:_ _ =
  Format.eprintf
    "Cohttp or Httpaf server implementation not availble\n\
     Try: `opam install cohttp-lwt-unix`\n\
     or:  `opam install httpaf-lwt-unix`@.";
  Lwt.return_unit

let set_debug () = ()
