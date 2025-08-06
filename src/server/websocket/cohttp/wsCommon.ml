(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix
open Websocket.Frame

let ws_react_content react send content =
  Lwt.async @@ fun () ->
  react content >|= function
  | Ok `none -> send None
  | Ok (`binary content) -> send @@ Some (create ~opcode:Opcode.Binary ~content ())
  | Ok (`text content) -> send @@ Some (create ~opcode:Opcode.Text ~content ())
  | Error (`handler_error content) -> send @@ Some (create ~opcode:Opcode.Text ~content ())
  | Error _ -> send @@ Some (close 1011)

let ws_react react pong rclose rsend notify_close fr =
  match fr.opcode with
  | Opcode.Ping -> !rsend @@ Some (create ~opcode:Opcode.Pong ~content:fr.content ())
  | Opcode.Close ->
    if String.length fr.content >= 2 then
      let content = String.sub fr.content 0 2 in
      !rsend @@ Some (create ~opcode:Opcode.Close ~content ())
    else !rsend @@ Some (close 1000);
    Option.iter Lwt.async (snd !rclose);
    rclose := true, None;
    Lwt.wakeup_later notify_close ()
  | Opcode.Pong -> pong fr.content
  | Opcode.Text | Opcode.Binary -> ws_react_content react !rsend fr.content
  | _ -> !rsend @@ Some (close 1002)

let ws_loop bg send =
  let send : (EzAPIServerUtils.Directory.ws_frame, EzAPIServerUtils.Directory.handler_error) result -> unit = function
    | Error _ -> send (Some (close 1000))
    | Ok `none -> send None
    | Ok (`binary content) -> send @@ Some (create ~opcode:Opcode.Binary ~content ())
    | Ok (`text content) -> send @@ Some (create ~opcode:Opcode.Text ~content ()) in
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

let ping_pong ?(step=30.) id rsend =
  let id_str = Uuidm.to_string id in
  let content = string_of_int @@ Random.int 1_000_000_000 in
  let rec loop () =
    !rsend (Some (create ~opcode:Opcode.Ping ~content ()));
    EzLwtSys.sleep (step /. 2.) >>= fun () ->
    if check_ping ~step id_str content then
      EzLwtSys.sleep (step /. 2.) >>= fun () -> loop ()
    else
      Lwt.return_unit in
  let fill content =
    let now = CalendarLib.Fcalendar.Precise.now () in
    Hashtbl.replace ping_table (id_str ^ content) now in
  loop, fill

let close send =
  send (Some (close 1000))
