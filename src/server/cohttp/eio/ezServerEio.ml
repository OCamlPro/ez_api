open EzAPI
open EzServerEioUtils
open Cohttp

let dispatch ?allow_origin ?catch ?footer s req body =
  let time = GMTime.time () in
  Cohttp_common.debug req;
  let headers = Cohttp_common.headers req in
  let version = Cohttp_common.version req in
  let path_str, path, content_type, r =
    Req.request ?version ~headers ~time (Request.uri req) in
  let meth = Cohttp_common.meth req in
  let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
  let ws = WsCohttpEio.ws req in
  Log.debugf ~v:2 (fun () ->
      if body <> "" && (content_type = Some "application/json" || content_type = Some "text/plain") then
        EzDebug.printf "Request content:\n%s" body);
  let a =
    try handle ~ws ?meth ?content_type ?allow_origin s.server_kind r path body
    with exn ->
      EzDebug.printf "In %s: exception %s" path_str @@ Printexc.to_string exn;
      match catch with
      | None ->  `http (Answer.server_error exn)
      | Some c -> `http (c path_str exn) in
  match a with
  | `ws (Ok ra) -> ra
  | `ws (Error _) ->
    let status = Code.status_of_code 501 in
    `Response (Cohttp_eio.Server.respond_string ~status ~body:"" ())
  | `http {Answer.code; body; headers} ->
    let status = Code.status_of_code code in
    Log.debug ~v:(if code >= 200 && code < 300 then 1 else 0) "Reply computed to %S: %d" path_str code;
    Log.debugf ~v:4 (fun () ->
        List.iter (fun (name, value) -> EzDebug.printf "  %s: %s" name value) headers
      );
    Log.debugf ~v:3 (fun () ->
        let content_type = List.assoc_opt "content-type" headers in
        if body <> "" && (content_type = Some "application/json" || content_type = Some "text/plain") then
          EzDebug.printf "Reply content:\n%s" body);
    let headers = Header.of_list headers in
    let body = Option.fold ~none:body ~some:(fun f -> body ^ f) footer in
    `Response (Cohttp_eio.Server.respond_string ~headers ~status ~body ())

let create ?catch ?allow_origin ?footer ~env ~sw server_port server_kind =
  let s = { server_port; server_kind } in
  let net = Eio.Stdenv.net env in
  ignore @@ Doc.all_services_registered ();
  let callback _conn req body =
    dispatch ?allow_origin ?catch ?footer s req body in
  let on_error exn = EzDebug.printf "Server Error: %s" (Printexc.to_string exn) in
  EzDebug.printf "Starting COHTTP server (port: %d)" server_port;
  let additional_domains = Eio.Stdenv.domain_mgr env, Stdlib.Domain.recommended_domain_count () in
  let socket = Eio.Net.listen ~reuse_addr:true ~reuse_port:true ~backlog:128 ~sw net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, server_port)) in
  let server = Cohttp_eio.Server.make_response_action ~callback () in
  Cohttp_eio.Server.run ~on_error ~additional_domains socket server

let run ?catch ?allow_origin ?footer ~env ~sw servers =
  Eio.Fiber.all @@ List.map (fun (port, kind) -> fun () ->
      create ?catch ?allow_origin ?footer ~env ~sw port kind)
    servers
