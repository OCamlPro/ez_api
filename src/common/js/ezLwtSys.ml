(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let run = Lwt.async
let sleep d =
  let t, w = Lwt.task () in
  let id = Js_of_ocaml.Dom_html.setTimeout (Lwt.wakeup_later w) (d *. 1000.) in
  Lwt.on_cancel t (fun () -> Js_of_ocaml.Dom_html.clearTimeout id);
  t
