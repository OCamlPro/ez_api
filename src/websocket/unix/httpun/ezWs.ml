(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2025 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Syntax
open Httpun_ws
open Httpun_ws_lwt_unix
open EzWsCommon
include Types

let parse url =
  let uri = Uri.of_string url in
  match Uri.host uri, Uri.scheme uri with
  | Some h, Some sch ->
    let p = Option.value ~default:(if sch = "https" then 443 else 80) (Uri.port uri) in
    Ok (h, sch, p, Uri.path_and_query uri)
  | _ -> Error (Format.sprintf "invalid url %s" url)

let error_handler notifier e =
  let e = match e with
    | `Handshake_failure _ -> "handshake failure"
    | `Malformed_response err -> Format.sprintf "malformed response: %s" err
    | `Invalid_response_body_length _ -> "invalid body length"
    | `Exn exn -> Printexc.to_string exn in
  Lwt.wakeup notifier (Error e)

let send ~content wsd =
  let len, off = String.length content, 0 in
  let bs = Bigstringaf.of_string ~off ~len content in
  Wsd.schedule wsd bs ~kind:`Text ~off ~len

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

let websocket_handler ?error ~react notifier action_notifier wsd =
  let b = Buffer.create 1024 in
  let close code =
    let code = Option.map Websocket.Close_code.of_code_exn code in
    Wsd.close ?code wsd; Lwt.return_ok () in
  let send content = send ~content wsd; Lwt.return_ok () in
  let action = { send; close } in
  Lwt.wakeup action_notifier action;
  let frame ~opcode ~is_fin ~len:_ payload = match opcode with
    | `Ping ->
      read_payload ~payload @@ fun content ->
      let application_data = make_data content in
      Wsd.send_pong ~application_data wsd
    | `Pong -> ()
    | `Text | `Binary | `Continuation ->
      read_payload ~payload @@ fun content ->
      Buffer.add_string b content;
      if is_fin then (
        let c = Buffer.contents b in
        Buffer.clear b;
        Lwt.async @@ fun () ->
        let* r = react action c in
        (match r, error with Error e, Some f -> f action e | _ -> ());
        Lwt.return_unit)
    | `Connection_close ->
      Wsd.close wsd;
      Lwt.wakeup notifier (Ok ())
    | _ ->
      Wsd.close ~code:`Protocol_error wsd;
      Lwt.wakeup notifier (Error "protocol error") in
  let eof ?error () =
    Option.iter (function `Exn exn -> Format.eprintf "websocket eof error: %s@." (Printexc.to_string exn)) error;
    Wsd.close wsd in
  { Httpun_ws.Websocket_connection.frame; eof }

let connect ?msg ?protocols:_ ?error ~react url =
  match parse url with
  | Error e -> Lwt.return_error e
  | Ok (host, _scheme, port, resource) ->
    log ~action:"connect" url msg;
    let* addresses = Lwt_unix.getaddrinfo host (Int.to_string port) [Unix.(AI_FAMILY PF_INET)] in
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
    let conn, n = Lwt.wait () in
    let action_w, action_n = Lwt.wait () in
    let nonce = EzAPI.Multipart.random_string () in
    let* _client = Client.connect ~nonce ~host ~port ~resource ~error_handler:(error_handler n)
        ~websocket_handler:(websocket_handler ?error ~react n action_n) socket in
    let* action = action_w in
    Lwt.return_ok { conn; action }

let connect0 ?msg ?protocols ?error ~react base service =
  let EzAPI.URL url = EzAPI.forge0 base service [] in
  let input = EzAPI.Service.input service.EzAPI.s in
  let output = EzAPI.Service.output service.EzAPI.s in
  let errors = EzAPI.Service.errors_encoding service.EzAPI.s in
  let react action s =
    let send i = action.send (EzAPI.IO.to_string input i) in
    let action = {send; close = action.close} in
    match EzAPI.IO.res_from_string output (res_encoding errors) (react action) s with
    | Ok r -> r
    | Error (`destruct_exn exn) -> Lwt.return_error (Printexc.to_string exn) in
  let+ r = connect ?msg ?protocols ?error ~react url in
  match r with
  | Error e -> Error e
  | Ok r ->
    let send i = r.action.send (EzAPI.IO.to_string input i) in
    let action = {send; close = r.action.close} in
    Ok {r with action}
