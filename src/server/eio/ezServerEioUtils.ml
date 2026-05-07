(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzAPI
module Timings = Timings
module Directory = Directory
module Answer = Answer
module Req = Req
module GMTime = GMTime
module Ip = Ip
module Cors = Cors

(** Server *)

type server_kind =
  | API of Directory.t
  | Root of string * string option

type server = {
  server_port : int;
  server_kind : server_kind;
}

(** Utils *)

let return ?code ?headers x = Answer.return ?code ?headers x
let return_ok ?code ?headers x = Answer.return ?code ?headers (Ok x)
let return_error ?content ?headers code = Answer.return ~code ?headers (Error content)

(** Register Handler *)

let empty = Directory.empty

let register_res service handler dir =
  let security = Service.security service.s in
  let path = Service.path service.s in
  let handler args input =
    if !Timings.enabled then
      let t0 = (Path.get_root path args).Req.req_time in
      let add_timing_wrap b =
        let t1 = GMTime.time () in
        Timings.add_timing (EzAPI.id service) b t0 (t1-.t0) in
      try
        let res = handler args security input in
        add_timing_wrap true;
        res
      with exn -> add_timing_wrap true; raise exn
    else handler args security input in
  let service = register service in
  Directory.register_http dir service handler

let register_ws_res service ~react ~bg ?onclose ?step dir =
  let security = Service.security service.s in
  let bg r send = bg r security send in
  let react r i = react r security i in
  let service = register service in
  Directory.register_ws dir ?onclose ?step ~react ~bg service

exception Conflict of (Dir_types.Step.t list * Dir_types.conflict)

let register service handler dir =
  match register_res service handler dir with
  | Ok dir -> dir
  | Error (steps, conflict) ->
    Format.eprintf "Conflict for %s: %s@."
      (Dir_types.Step.list_to_string steps)
      (Directory.conflict_to_string conflict);
    raise (Conflict (steps, conflict))

let register_ws service ?onclose ?step ~react ~bg dir =
  match register_ws_res service ?onclose ?step ~react ~bg dir with
  | Ok dir -> dir
  | Error (steps, conflict) ->
    Format.eprintf "Conflict for %s: %s@."
      (Dir_types.Step.list_to_string steps)
      (Directory.conflict_to_string conflict);
    raise (Conflict (steps, conflict))

let handle ?meth ?content_type ?ws ?(allow_origin=`default) ~file s r path body =
  let r, body =
    if content_type = Some Url.content_type then
      Req.add_params r (Url.decode_args body), ""
    else r, body in
  let a = match s with
    | Root (root, default) -> `http (file ?meth ?default root path)
    | API dir ->
      match Directory.lookup ?meth ?content_type dir r path with
      | Error `Not_found -> `http (Answer.not_found ())
      | Error (`Cannot_parse a) -> `http (Answer.cannot_parse a)
      | Error `Method_not_allowed -> `http (Answer.method_not_allowed ())
      | Ok (`options headers) -> `http {Answer.code=200; body=""; headers}
      | Ok `head -> `http {Answer.code=200; body=""; headers =[]}
      | Ok (`http h) ->
        `http (match h body with
          | Error (`destruct_exn exn) -> Answer.destruct_exception exn
          | Error (`unsupported c) -> Answer.unsupported_media_type c
          | Error (`handler_error s) ->
            EzDebug.printf "In %s: error %s" (String.concat "/" path) s;
            Answer.server_error (Failure s)
          | Error (`handler_exn exn) ->
            EzDebug.printf "In %s: exception %s" (String.concat "/" path) @@ Printexc.to_string exn;
            Answer.server_error exn
          | Ok a -> a)
      | Ok (`ws (react, bg, onclose, step)) ->
        begin match ws with
          | None -> assert false
          | Some ws ->
            let body = if body = "" then None else Some body in
            `ws (ws ?onclose ?step ?body ~react ~bg r.Req.req_id)
        end in
  match a with
  | `ws ra -> `ws ra
  | `http a ->
    let origin = match allow_origin with
      | `origin -> StringMap.find_opt "origin" r.Req.req_headers
      | _ -> None in
    let headers =
      Cors.merge_headers_allow_origin ?origin a.Answer.headers allow_origin @
      [ "connection", "close" ] in
    `http { a with Answer.headers }
