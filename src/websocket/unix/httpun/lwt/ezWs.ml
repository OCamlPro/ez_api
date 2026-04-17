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
open Httpun_ws_common
include EzWsCommon.Types

open Make(struct
    type 'a m = 'a Lwt.t
    let return = Lwt.return
    let bind = Lwt.bind
    let async = Lwt.async
  end)

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
    let react (send, close) = react { send; close } in
    let error = Option.map (fun f -> (fun (send, close) -> f { send; close })) error in
    let websocket_handler = websocket_handler ?error ~react (Lwt.wakeup_later n) (Lwt.wakeup_later action_n) in
    let* _client = Httpun_ws_lwt_unix.Client.connect ~nonce ~host ~port ~resource
        ~error_handler:(error_handler (Lwt.wakeup_later n)) ~websocket_handler socket in
    let* send, close = action_w in
    Lwt.return_ok { conn; action={send; close} }

let connect0 ?msg ?protocols ?error ~react base service =
  let EzAPI.URL url = EzAPI.forge0 base service [] in
  let input = EzAPI.Service.input service.EzAPI.s in
  let output = EzAPI.Service.output service.EzAPI.s in
  let errors = EzAPI.Service.errors_encoding service.EzAPI.s in
  let react action s =
    let send i = action.send (EzAPI.IO.to_string input i) in
    let action = {send; close = action.close} in
    match EzAPI.IO.res_from_string output (EzWsCommon.res_encoding errors) (react action) s with
    | Ok r -> r
    | Error (`destruct_exn exn) -> Lwt.return_error (Format.asprintf "%a" (Json_encoding.print_error ?print_unknown:None) exn) in
  let+ r = connect ?msg ?protocols ?error ~react url in
  match r with
  | Error e -> Error e
  | Ok r ->
    let send i = r.action.send (EzAPI.IO.to_string input i) in
    let action = {send; close = r.action.close} in
    Ok {r with action}
