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
open EzAPI
open EzAPIServerUtils
open Httpaf

module File = File_lwt

let set_debug () = ()

let mk_uri { Request.meth ; Request.target ; Request.headers ; _ } =
  Httpunaf.mk_uri ~meth ~target  ~header:(Headers.get headers)

let meth_from_httpaf req = Httpunaf.meth req.Request.meth

let headers_from_httpaf req =
  Headers.fold ~f:(fun k v acc ->
      StringMap.add (String.lowercase_ascii k) (String.split_on_char ',' v) acc)
    ~init:StringMap.empty req.Request.headers

let version_from_httpaf req =
  if req.Request.version.Version.minor = 0 then `HTTP_1_0
  else `HTTP_1_1

let read_body body = Lwt_httpunaf.read_body ~read:Body.schedule_read body

let debug_httpaf req =
  let meth = Method.to_string req.Request.meth in
  let headers = Headers.to_list req.Request.headers in
  Log_lwt.request ~meth ~headers req.Request.target

let register_ip req time addr = Httpunaf.register_ip ~header:(Headers.get req.Request.headers) time addr

let file = File_lwt.reply

let connection_handler ?catch ?allow_origin ?footer s sockaddr fd =
  let request_handler sockaddr reqd =
    let req = Reqd.request reqd in
    let time = GMTime.time () in
    register_ip req time sockaddr;
    let headers = headers_from_httpaf req in
    let version = version_from_httpaf req in
    let target, path, content_type, r =
      Req.request ~version ~headers ~time (mk_uri req) in
    let meth = meth_from_httpaf req in
    Lwt.async @@ fun () ->
    debug_httpaf req >>= fun () ->
    read_body (Reqd.request_body reqd) >>= fun body ->
    let ws = WsHttpaf.ws reqd fd in
    Log_lwt.request_content ?content_type body >>= fun () ->
    Lwt.catch
      (fun () -> handle ~ws ?meth ?content_type ?allow_origin ~file s.server_kind r path body)
      (fun exn ->
         EzDebug.printf "In %s: exception %s" target @@ Printexc.to_string exn;
         match catch with
         | None ->  Lwt.return (`http (Answer.server_error exn))
         | Some c -> c target exn >|= fun a -> `http a)
    >>= function
    | `ws (Error `no_ws_library) ->
      let status = Status.unsafe_of_code 501 in
      let response = Response.create status in
      Reqd.respond_with_string reqd response "";
      Lwt.return_unit
    | `ws (Ok (_response, _b)) ->
      Lwt.return_unit
    | `http {Answer.code; body; headers} ->
      let status = Status.unsafe_of_code code in
      Log_lwt.response ~code ~headers ~target body >>= fun () ->
      let headers = Headers.of_list headers in
      let body = Option.fold ~none:body ~some:(fun f -> body ^ f) footer in
      let len = String.length body in
      let headers = Headers.add headers "content-length" (string_of_int len) in
      let response = Response.create ~headers status in
      Reqd.respond_with_string reqd response body;
      Lwt.return_unit
  in

  let error_handler _client_address ?request:_ error start_response =
    let response_body = start_response Headers.empty in
    begin match error with
      | `Exn exn ->
        Body.write_string response_body (Printexc.to_string exn);
        Option.iter (Body.write_string response_body) footer
      | #Status.standard as error ->
        Body.write_string response_body (Status.default_reason_phrase error)
    end;
    Body.flush response_body (fun () -> Body.close_writer response_body)
  in

  Httpaf_lwt_unix.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler
    sockaddr
    fd

let shutdown = Lwt_httpunaf.shutdown

let server ?catch ?allow_origin ?footer ?addr servers =
  Lwt_httpunaf.server ~name:"HTTPAF" ?catch ?allow_origin ?footer ?addr connection_handler servers
