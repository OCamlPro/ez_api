(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Js_of_ocaml

let printf fmt = Printf.kprintf (fun s -> Firebug.console##debug (Js.string s)) fmt
let log s = Firebug.console##log (Js.string s)
