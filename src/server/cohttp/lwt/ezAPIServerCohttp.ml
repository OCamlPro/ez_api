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
open EzAPI
open EzAPIServerUtils
open Cohttp

module Server = Cohttp_lwt_unix.Server
module File = File_lwt

let set_debug () = Cohttp_lwt_unix.Debug.activate_debug ()

let register_ip req io time =
  let open Conduit_lwt_unix in match io with
  | TCP tcp ->
    begin match Lwt_unix.getpeername tcp.fd with
      | Lwt_unix.ADDR_INET (ip,_port) ->
        let ip = Ipaddr.to_string (Ipaddr_unix.of_inet_addr ip) in
        let ip =
          match Header.get (Request.headers req) "x-forwarded-for" with
          | None -> ip
          | Some ip -> ip in
        Ip.register time ip
      | Lwt_unix.ADDR_UNIX _path -> ()
    end
  | _ -> ()

let file = File.reply

let dispatch ?allow_origin ?catch ?footer s io req body =
  let time = GMTime.time () in
  register_ip req io time ;
  Cohttp_common.debug ~request:Log_lwt.request req >>= fun () ->
  let headers = Cohttp_common.headers req in
  let version = Cohttp_common.version req in
  let target, path, content_type, r =
    Req.request ?version ~headers ~time (Request.uri req) in
  let meth = Cohttp_common.meth req in
  Cohttp_lwt.Body.to_string body >>= fun body ->
  let ws = WsCohttpLwt.ws req in
  Log_lwt.request_content ?content_type body >>= fun () ->
  Lwt.catch (fun () -> handle ~ws ?meth ?content_type ?allow_origin ~file ~printf:Log_lwt.printf
                s.server_kind r path body)
    (fun exn ->
       Log_lwt.printf "In %s: exception %s" target @@ Printexc.to_string exn >>= fun () ->
       match catch with
       | None ->  Lwt.return (`http (Answer.server_error exn))
       | Some c -> c target exn >|= fun a -> `http a)
  >>= function
  | `ws (Ok ra) -> Lwt.return ra
  | `ws (Error _) ->
    let status = Code.status_of_code 501 in
    Server.respond_string ~status ~body:"" () >|= fun (r, b) ->
    `Response (r, b)
  | `http {Answer.code; body; headers} ->
    let status = Code.status_of_code code in
    Log_lwt.response ~code ~target ~headers body >>= fun () ->
    let headers = Header.of_list headers in
    let body = Option.fold ~none:body ~some:(fun f -> body ^ f) footer in
    Server.respond_string ~headers ~status ~body () >|= fun (r, b) ->
    `Response (r, b)

let create_server ?catch ?allow_origin ?footer server_port server_kind =
  let s = { server_port; server_kind } in
  Timings.init (GMTime.time ()) @@ Doc.nservices ();
  ignore @@ Doc.all_services_registered ();
  let callback conn req body = dispatch ?allow_origin ?catch ?footer s (fst conn) req body in
  let on_exn = function
    | Unix.Unix_error (Unix.EPIPE, _, _) -> ()
    | exn -> Format.eprintf "Server Error: %s" (Printexc.to_string exn) in
  Log_lwt.printf "[%t] Running COHTTP LWT server on localhost:%d@." GMTime.pp_now server_port >>= fun () ->
  Server.create
    ~on_exn
    ~mode:(`TCP (`Port server_port))
    (Server.make_response_action ~callback ())

let shutdown () = Lwt.return_unit

let server ?catch ?allow_origin ?footer ?addr:_ servers =
  Lwt.join (List.map (fun (port,kind) ->
      create_server ?catch ?allow_origin ?footer port kind) servers)
