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

let set_debug () = Cohttp_lwt_unix.Debug.activate_debug ()

let register_ip req io time =
  let open Conduit_lwt_unix in match io with
  | Domain_socket _
  | Vchan _ -> ()
  | TCP tcp ->
    match[@warning "-42"] Lwt_unix.getpeername tcp.fd with
    | Lwt_unix.ADDR_INET (ip,_port) ->
      let ip = Ipaddr.to_string (Ipaddr_unix.of_inet_addr ip) in
      let ip =
        match Header.get (Request.headers req) "x-forwarded-for" with
        | None -> ip
        | Some ip -> ip in
      Ip.register time ip
    | Lwt_unix.ADDR_UNIX _path -> ()

let headers_from_cohttp req =
  let headers = ref StringMap.empty in
  Header.iter (fun s v ->
      headers :=
        StringMap.add (String.lowercase_ascii s) (String.split_on_char ',' v) !headers)
    (Request.headers req);
  !headers

let meth_from_cohttp req =
  match Request.meth req with
  | #Meth.all as m -> Some m
  | _ -> None

let version_from_cohttp req =
  match Request.version req with
  | #Req.version as v -> Some v
  | _ -> None

let debug_cohttp req =
  debug "[%s] REQUEST: %s %S" (pp_time ())
    (req |> Request.meth |> Code.string_of_method)
    (req |> Request.uri |> Uri.path_and_query);
  debugf ~v:1 (fun () ->
      Header.iter (fun s v ->
          List.iter (fun v -> EzDebug.printf "  %s: %s" s v)
            (String.split_on_char ',' v))
        (Request.headers req))

let dispatch ?catch s io req body =
  let time = GMTime.time () in
  register_ip req io time ;
  debug_cohttp req;
  let headers = headers_from_cohttp req in
  let version = version_from_cohttp req in
  let path_str, path, content_type, r =
    Req.request ?version ~headers ~time (Request.uri req) in
  let meth = meth_from_cohttp req in
  Cohttp_lwt.Body.to_string body >>= fun body ->
  let ws = WsCohttp.ws req in
  Lwt.catch (fun () -> handle ~ws ?meth ?content_type s.server_kind r path body)
    (fun exn ->
       EzDebug.printf "In %s: exception %s" path_str @@ Printexc.to_string exn;
       match catch with
       | None -> Answer.server_error exn >|= fun a -> `http a
       | Some c -> c path_str exn >|= fun a -> `http a)
  >>= function
  | `ws (Ok ra) -> Lwt.return ra
  | `ws (Error _) ->
    let headers = Header.of_list default_access_control_headers in
    let status = Code.status_of_code 501 in
    Server.respond_string ~headers ~status ~body:"" () >|= fun (r, b) ->
    `Response (r, b)
  | `http {Answer.code; body; headers} ->
    let headers = merge_headers_with_default headers in
    let status = Code.status_of_code code in
    debug ~v:(if code >= 200 && code < 300 then 1 else 0) "Reply computed to %S: %d" path_str code;
    debug ~v:3 "Reply content:\n  %s" body;
    let headers = Header.of_list headers in
    Server.respond_string ~headers ~status ~body () >|= fun (r, b) ->
    `Response (r, b)

let create_server ?catch server_port server_kind =
  let s = { server_port; server_kind } in
  Timings.init (GMTime.time ()) @@ Doc.nservices ();
  ignore @@ Doc.all_services_registered ();
  let callback conn req body = dispatch ?catch s (fst conn) req body in
  let on_exn = function
    | Unix.Unix_error (Unix.EPIPE, _, _) -> ()
    | exn -> EzDebug.printf "Server Error: %s" (Printexc.to_string exn) in
  EzDebug.printf "Starting COHTTP server (port: %d)" server_port;
  Server.create
    ~on_exn
    ~mode:(`TCP (`Port server_port))
    (Server.make_response_action ~callback ())

let server ?catch servers =
  Lwt.join (List.map (fun (port,kind) -> create_server ?catch port kind) servers)
