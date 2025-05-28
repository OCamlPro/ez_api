(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2025 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix
open Httpun_ws

let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string

let close ?(code=`Normal_closure) wsd = Wsd.close ~code wsd

let send ~kind ~content wsd =
  let len, off = String.length content, 0 in
  let bs = Bigstringaf.of_string ~off ~len content in
  Wsd.schedule wsd bs ~kind ~off ~len

let ws_react_content react notify_close wsd content =
  Lwt.async @@ fun () -> react content >|= function
  | Ok `none -> ()
  | Ok (`binary content) -> send ~kind:`Binary ~content wsd
  | Ok (`text content) -> send ~kind:`Text ~content wsd
  | Error (`handler_error content) -> send ~kind:`Text ~content wsd
  | Error _ ->
    close ~code:`Internal_server_error wsd;
    Lwt.wakeup notify_close ()

let read_payload ~payload f =
  let b = Buffer.create 100 in
  let on_eof () = f (Buffer.contents b) in
  let rec on_read bs ~off ~len =
    Buffer.add_string b (Bigstringaf.substring bs ~off ~len);
    Payload.schedule_read payload ~on_eof ~on_read in
  Payload.schedule_read payload ~on_eof ~on_read

let make_data content =
  let len, off = String.length content, 0 in
  let buffer = Bigstringaf.of_string ~off ~len content in
  { IOVec.buffer; off; len }

let ws_react react pong notify_close ~opcode ~payload wsd = match opcode with
  | `Ping ->
    read_payload ~payload @@ fun content ->
    let application_data = make_data content in
    Wsd.send_pong ~application_data wsd
  | `Connection_close -> Wsd.close wsd; Lwt.wakeup notify_close ()
  | `Pong -> read_payload ~payload pong
  | `Text | `Binary -> read_payload ~payload (ws_react_content react notify_close wsd)
  | _ -> close ~code:`Protocol_error wsd

let ws_loop bg notify_close wsd =
  let send : (EzAPIServerUtils.Directory.ws_frame, EzAPIServerUtils.Directory.handler_error) result -> unit = function
    | Error _ -> close wsd; Lwt.wakeup notify_close ()
    | Ok `none -> ()
    | Ok (`binary content) -> send ~kind:`Binary ~content wsd
    | Ok (`text content) -> send ~kind:`Text ~content wsd in
  bg send

let ping_table : (string, CalendarLib.Fcalendar.Precise.t) Hashtbl.t = Hashtbl.create 1024

let check_ping ?(step=30.) id key =
  let open CalendarLib.Fcalendar.Precise in
  match Hashtbl.find_opt ping_table (id ^ key) with
  | None -> false
  | Some t ->
    let step = Period.second (Time.Second.from_float step) in
    let now = now () in
    if compare (add t step) now < 0 then (
      Hashtbl.remove ping_table (id ^ key);
      false)
    else true

let ping_pong ?(step=30.) id wsd =
  let id_str = Uuidm.to_string id in
  let content = string_of_int @@ Random.int 1_000_000_000 in
  let len, off = String.length content, 0 in
  let buffer = Bigstringaf.of_string ~off ~len content in
  let application_data = { IOVec.buffer; off; len } in
  let rec loop () =
    Wsd.send_ping ~application_data wsd;
    EzLwtSys.sleep (step /. 2.) >>= fun () ->
    if check_ping ~step id_str content then
      EzLwtSys.sleep (step /. 2.) >>= fun () -> loop ()
    else (
      close ~code:`Going_away wsd;
      Lwt.return_unit) in
  let fill content =
    let now = CalendarLib.Fcalendar.Precise.now () in
    Hashtbl.replace ping_table (id_str ^ content) now in
  loop, fill

let ws reqd upgrade ?onclose ?step ?body:_ ~react ~bg id =
  let websocket_handler wsd =
    let ping_loop, pong = ping_pong ?step id wsd in
    let w, n = Lwt.wait () in
    let frame ~opcode ~is_fin:_ ~len:_ payload = ws_react react pong n ~opcode ~payload wsd in
    let eof ?error () =
      Option.iter (function `Exn exn -> Format.eprintf "websocket eof error: %s@." (Printexc.to_string exn)) error;
      Wsd.close wsd in
    Lwt.async (fun () ->
        (Lwt.pick [ws_loop bg n wsd; ping_loop (); w]) >>= fun () ->
        Option.fold ~none:Lwt.return_unit ~some:(fun f -> f ()) onclose);
    { Websocket_connection.frame; eof } in
  let upgrade_handler upgrade () =
    let ws_conn = Server_connection.create_websocket websocket_handler in
    upgrade (Gluten.make (module Server_connection) ws_conn) in
  match Handshake.respond_with_upgrade ~sha1 reqd (upgrade_handler upgrade) with
  | Ok () -> Lwt.return_ok ()
  | Error err_str ->
    let response = Httpun.Response.create ~headers:(Httpun.Headers.of_list [ "Connection", "close" ]) `Bad_request in
    Httpun.Reqd.respond_with_string reqd response err_str;
    Lwt.return_ok ()
