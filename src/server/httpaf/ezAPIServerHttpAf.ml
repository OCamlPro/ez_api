(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2025 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Lwt
open EzAPIServerUtils
open Httpaf

let set_debug () = ()

let mk_uri { Request.meth ; Request.target ; Request.headers ; _ } =
  Server_common.mk_uri ~meth ~target  ~header:(Headers.get headers)

let meth_from_httpaf req = Server_common.meth_from_ext req.Request.meth

let headers_from_httpaf req =
  Headers.fold ~f:(fun k v acc ->
      StringMap.add (String.lowercase_ascii k) (String.split_on_char ',' v) acc)
    ~init:StringMap.empty req.Request.headers

let version_from_httpaf req =
  if req.Request.version.Version.minor = 0 then `HTTP_1_0
  else `HTTP_1_1

let read_body body = Server_common.read_body ~read:Body.schedule_read body

let debug_httpaf req =
  let meth = Method.to_string req.Request.meth in
  let headers = Headers.to_list req.Request.headers in
  Server_common.debug_http_ext ~meth ~target:req.Request.target ~headers

let register_ip req time addr = Server_common.register_ip ~header:(Headers.get req.Request.headers) time addr

let connection_handler ?catch ?allow_origin ?allow_headers ?allow_methods
    ?allow_credentials s sockaddr fd =
  let request_handler sockaddr reqd =
    let req = Reqd.request reqd in
    let time = GMTime.time () in
    register_ip req time sockaddr;
    let headers = headers_from_httpaf req in
    let version = version_from_httpaf req in
    let path_str, path, content_type, r =
      Req.request ~version ~headers ~time (mk_uri req) in
    let meth = meth_from_httpaf req in
    debug_httpaf req;
    Lwt.async @@ fun () ->
    read_body (Reqd.request_body reqd) >>= fun body ->
    let ws = WsHttpaf.ws reqd fd in
    if body <> "" then debug ~v:2 "Request content:\n%s" body;
    Lwt.catch
      (fun () -> handle ~ws ?meth ?content_type s.server_kind r path body)
      (fun exn ->
         EzDebug.printf "In %s: exception %s" path_str @@ Printexc.to_string exn;
         match catch with
         | None -> Answer.server_error exn >|= fun a -> `http a
         | Some c -> c path_str exn >|= fun a -> `http a)
    >>= function
    | `ws (Error _) ->
      let headers = Headers.of_list @@
        merge_headers_with_default ?allow_origin ?allow_headers ?allow_methods
          ?allow_credentials [] in
      let status = Status.unsafe_of_code 501 in
      let response = Response.create ~headers status in
      Reqd.respond_with_string reqd response "";
      Lwt.return_unit
    | `ws (Ok (_response, _b)) ->
      Lwt.return_unit
    | `http {Answer.code; body; headers=resp_headers} ->
      let status = Status.unsafe_of_code code in
      debug ~v:(if code = 200 then 1 else 0) "Reply computed to %S: %d" path_str code;
      if body <> "" then debug ~v:3 "Reply content:\n%s" body;
      let origin = match allow_origin with
        | Some `origin -> StringMap.find_opt "origin" headers
        | _ -> None in
      let headers = merge_headers_with_default ?allow_origin ?allow_headers
          ?allow_methods ?allow_credentials ?origin resp_headers in
      let headers = Headers.of_list headers in
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
        Body.write_string response_body "\n";
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

let server ?catch ?allow_origin ?allow_headers ?allow_methods ?allow_credentials servers =
  Server_common.server ~name:"HTTPAF" ?catch ?allow_origin ?allow_headers
    ?allow_methods ?allow_credentials connection_handler servers
