(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open WsCommon
open Lwt.Syntax

let ws req ?onclose ?step ?body ~react ~bg id =
  let rsend = ref (fun _ -> ()) in
  let rclose = ref (false, onclose) in
  let ping_loop, pong_fill = ping_pong ?step id rsend in
  let w, n = Lwt.wait () in
  let* r, send = Websocket_cohttp_lwt.upgrade_connection req
      (ws_react react pong_fill rclose rsend n) in
  rsend := send;
  Option.iter (ws_react_content react send) body;
  Lwt.async (fun () ->
      let* () = Lwt.pick [ ws_loop bg send; ping_loop (); w ] in
      if not (fst !rclose) then close send;
      match snd !rclose with
      | None -> Lwt.return_unit
      | Some f -> f ());
  Lwt.return_ok r
