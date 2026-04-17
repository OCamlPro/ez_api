(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let get ( _req : EzAPI.Req.t ) =
  Format.eprintf "Dummy implementation of cookie server, to install:\n`opam install cohttp`@.";
  EzAPI.StringMap.empty

let set ?secure:_ ?http_only:_ ?expiration:_ ~name ~value () =
  Format.eprintf "Dummy implementation of cookie server, to install:\n`opam install cohttp`@.";
  (name, value)

let clear ~name () = (name, "")
