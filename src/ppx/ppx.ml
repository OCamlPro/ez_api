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
  Ppxlib.Driver.register_transformation "ez_api" ~impl:Ppx_common.impl;
  Ppx_common.derivers None;
  Ppx_common.global_deriver None
