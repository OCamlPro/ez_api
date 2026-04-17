(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2025 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Httpun_ws
open WsHttpun.Make(struct
    type 'a m = 'a Lwt.t
    let return = Lwt.return
    let bind = Lwt.bind
    let async = Lwt.async
    let sleep = EzLwtSys.sleep
  end)

let ws_loop bg notify_close wsd =
  let send = function
    | Error _ -> WsHttpun.close wsd; Lwt.wakeup_later notify_close ()
    | Ok `none -> ()
    | Ok (`binary content) -> WsHttpun.send ~kind:`Binary ~content wsd
    | Ok (`text content) -> WsHttpun.send ~kind:`Text ~content wsd in
  bg send

let ws reqd upgrade ?onclose ?step ?body:_ ~react:r ~bg id =
  let websocket_handler wsd =
    let ping_loop, pong = ping_pong ?step id wsd in
    let w, n = Lwt.wait () in
    let frame ~opcode ~is_fin:_ ~len:_ payload =
      react r pong (Lwt.wakeup_later n) ~opcode ~payload wsd in
    let eof ?error () =
      Option.iter (function `Exn exn -> Format.eprintf "websocket eof error: %s@." (Printexc.to_string exn)) error;
      Wsd.close wsd in
    Lwt.async (fun () ->
        Lwt.bind (Lwt.pick [ws_loop bg n wsd; ping_loop (); w]) @@ fun () ->
        Option.fold ~none:Lwt.return_unit ~some:(fun f -> f ()) onclose);
    { Websocket_connection.frame; eof } in
  let upgrade_handler upgrade () =
    let ws_conn = Server_connection.create_websocket websocket_handler in
    upgrade (Gluten.make (module Server_connection) ws_conn) in
  match Handshake.respond_with_upgrade ~sha1:WsHttpun.sha1 reqd (upgrade_handler upgrade) with
  | Ok () -> Lwt.return_ok ()
  | Error err_str ->
    let response = Httpun.Response.create ~headers:(Httpun.Headers.of_list [ "Connection", "close" ]) `Bad_request in
    Httpun.Reqd.respond_with_string reqd response err_str;
    Lwt.return_ok ()
