(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Js_of_ocaml
open Lwt.Infix
open EzWsCommon
include Types

let ready socket = match socket##.readyState with
  | WebSockets.CONNECTING -> Error "websocket not yet ready"
  | WebSockets.CLOSING -> Error "websocket closing"
  | WebSockets.CLOSED -> Error "websocket closed"
  | _ -> Ok ()

let catch f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let send_frame socket content =
  catch (fun () -> socket##send (Js.string content))

let connect ?msg ?protocols ?error ~react url =
  let url = match String.get url 0 with
    | 'h' -> "ws" ^ String.sub url 4 (String.length url - 4)
    | _ -> url in
  let protocols = match protocols with
    | None -> new%js Js.array_empty
    | Some l -> Js.array @@ Array.of_list @@ List.map Js.string l in
  log ~action:"connect" url msg;
  let w0, n0 = Lwt.wait () in
  Lwt.catch
    (fun () ->
       let socket = new%js WebSockets.webSocket_withProtocols (Js.string url) protocols in
       let send content = match ready socket with
         | Error e -> Lwt.return (Error e)
         | Ok () -> Lwt.return @@ send_frame socket content in
       let close code =
         let code = match code with None -> 1002 | Some code -> code in
         Lwt.return @@ catch (fun () -> socket##close_withCode code) in
       let action = {send; close} in
       socket##.onmessage := Dom.handler @@ (fun e ->
           log url msg;
           let s = Js.to_string e##.data in
           Lwt.async (fun () ->
               react action s >|= function
               | Ok () -> ()
               | Error e -> match error with
                 | Some f -> f action e
                 | None -> ());
           Js._true);
       socket##.onerror := Dom.handler @@ (fun e ->
           match error with
           | Some f -> f action ("websocket error: " ^ Js.to_string e##._type); Js._true
           | None -> (); Js._true);
       let conn, n = Lwt.wait () in
       socket##.onclose := Dom.handler @@ (fun _e ->
           Lwt.wakeup_later n @@ Ok (); Js._true);
       socket##.onopen := Dom.handler @@ (fun _e ->
           Lwt.wakeup_later n0 (Ok {action; conn}); Js._true);
       w0)
    (fun exn -> Lwt.return_error (Printexc.to_string exn))

let connect0 ?msg ?protocols ?error ~react base service =
  let EzAPI.URL url = EzAPI.forge0 base service [] in
  let input = EzAPI.Service.input service.EzAPI.s in
  let output = EzAPI.Service.output service.EzAPI.s in
  let errors = EzAPI.Service.errors_encoding service.EzAPI.s in
  let react a s =
    let send i = a.send (EzAPI.IO.to_string input i) in
    match EzAPI.IO.res_from_string output (res_encoding errors) (react {send; close=a.close}) s with
    | Ok r -> r
    | Error (`destruct_exn exn) -> Lwt.return_error (Format.asprintf "%a" (Json_encoding.print_error ?print_unknown:None) exn) in
  connect ?msg ?protocols ?error ~react url >|= function
  | Error e -> Error e
  | Ok r ->
    let send i = r.action.send (EzAPI.IO.to_string input i) in
    let action = {send; close=r.action.close} in
    Ok {r with action}
