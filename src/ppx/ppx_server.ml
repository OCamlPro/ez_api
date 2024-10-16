(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let () =
  let kind = Some `server in
  Ppxlib.Driver.register_transformation "ez_api_server" ~impl:(Ppx_common.impl ?kind);
  Ppx_common.derivers kind;
  Ppx_common.global_deriver kind
