(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Websocket.Frame

module type S = sig
  type 'a m
  val async : (unit -> unit m) -> unit
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m
  val sleep : float -> unit m
end

let close ?(code=1000) send =
  send (Some (close code))

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

let response req =
  let headers = Cohttp.Request.headers req in
  let key = match Cohttp.Header.get headers "sec-websocket-key" with
    | None -> raise (Invalid_argument "upgrade_connection: missing header `sec-websocket-key`")
    | Some key -> key in
  let hash = Websocket.b64_encoded_sha1sum (key ^ Websocket.websocket_uuid) in
  let response_headers = Cohttp.Header.of_list [
      "upgrade", "websocket"; "connection", "Upgrade"; "sec-websocket-accept", hash ] in
  Cohttp.Response.make ~status:`Switching_protocols
    ~encoding:Cohttp.Transfer.Unknown ~headers:response_headers ()

let ws_loop bg send =
  let send = function
    | Error _ -> close send
    | Ok `none -> send None
    | Ok (`binary content) -> send @@ Some (create ~opcode:Opcode.Binary ~content ())
    | Ok (`text content) -> send @@ Some (create ~opcode:Opcode.Text ~content ()) in
  bg send

module Make(S: S) = struct
  let react_content react send notify_close content =
    S.async @@ fun () ->
    S.bind (react content) @@ fun r ->
    let () = match r with
      | Ok `none -> send None
      | Ok (`binary content) -> send @@ Some (create ~opcode:Opcode.Binary ~content ())
      | Ok (`text content) -> send @@ Some (create ~opcode:Opcode.Text ~content ())
      | Error (`handler_error content) -> send @@ Some (create ~opcode:Opcode.Text ~content ())
      | Error _ -> close ~code:1011 send; notify_close () in
    S.return ()

  let react react pong send notify_close fr =
    match fr.opcode with
    | Opcode.Ping -> send @@ Some (create ~opcode:Opcode.Pong ~content:fr.content ())
    | Opcode.Close ->
      if String.length fr.content >= 2 then
        let content = String.sub fr.content 0 2 in
        send @@ Some (create ~opcode:Opcode.Close ~content ())
      else close send;
      notify_close ()
    | Opcode.Pong -> pong fr.content
    | Opcode.Text | Opcode.Binary -> react_content react send notify_close fr.content
    | _ -> close ~code:1002 send; notify_close ()

  let ping_pong ?(step=30.) id send =
    let id_str = Uuidm.to_string id in
    let content = string_of_int @@ Random.int 1_000_000_000 in
    let rec loop () =
      send (Some (create ~opcode:Opcode.Ping ~content ()));
      S.bind (S.sleep (step /. 2.)) @@ fun () ->
      if check_ping ~step id_str content then
        S.bind (S.sleep (step /. 2.)) @@ fun () -> loop ()
      else S.return () in
    let fill content =
      let now = CalendarLib.Fcalendar.Precise.now () in
      Hashtbl.replace ping_table (id_str ^ content) now in
    loop, fill
end
