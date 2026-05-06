open EzAPI
open EzServerEioUtils
open Httpun
open Httpun_eio

let mk_uri { Request.meth ; Request.target ; Request.headers ; _ } =
  Httpunaf.mk_uri ~meth ~target  ~header:(Headers.get headers)

let meth_from_httpun req = Httpunaf.meth req.Request.meth

let headers_from_httpun req =
  Headers.fold ~f:(fun k v acc -> StringMap.add (String.lowercase_ascii k) (String.split_on_char ',' v) acc)
    ~init:StringMap.empty req.Request.headers

let version_from_httpun req =
  if req.Request.version.Version.minor = 0 then `HTTP_1_0
  else `HTTP_1_1

let debug_httpun req =
  let meth = Method.to_string req.Request.meth in
  let headers = Headers.to_list req.Request.headers in
  Httpunaf.debug ~meth ~target:req.Request.target ~headers

let read_body body =
  let w, n = Eio.Promise.create () in
  let b = Buffer.create 100 in
  let rec on_eof () = Eio.Promise.resolve n (Buffer.contents b)
  and on_read bs ~off ~len =
    Buffer.add_string b (Bigstringaf.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_read in
  Body.Reader.schedule_read body ~on_eof ~on_read;
  Eio.Promise.await w

let request_handler ?allow_origin ?catch ?footer ~sw s { Gluten.reqd; upgrade; _ } =
  let req = Reqd.request reqd in
  let time = GMTime.time () in
  let headers = headers_from_httpun req in
  let version = version_from_httpun req in
  let path_str, path, content_type, r =
    Req.request ~version ~headers ~time (mk_uri req) in
  let meth = meth_from_httpun req in
  debug_httpun req;
  Eio.Fiber.fork ~sw @@ fun () ->
  let body = read_body (Reqd.request_body reqd) in
  let ws = WsHttpunEio.ws reqd upgrade in
  Log.debugf ~v:2 (fun () ->
      if body <> "" && (content_type = Some "application/json" || content_type = Some "text/plain") then
        EzDebug.printf "Request content:\n%s" body);
  let r =
    try handle ?meth ?content_type ?allow_origin ~ws s.server_kind r path body
    with exn ->
      EzDebug.printf "In %s: exception %s" path_str @@ Printexc.to_string exn;
      match catch with
      | None -> `http (Answer.server_error exn)
      | Some c -> `http (c path_str exn) in
  match r with
  | `ws (Error `no_ws_library) ->
    let status = Status.unsafe_of_code 501 in
    let response = Response.create status in
    Reqd.respond_with_string reqd response ""
  | `ws (Ok ()) -> ()
  | `http {Answer.code; body; headers} ->
    let status = Status.unsafe_of_code code in
    Log.debug ~v:(if code >= 200 && code < 300 then 1 else 0) "Reply computed to %S: %d" path_str code;
    Log.debugf ~v:4 (fun () ->
        List.iter (fun (name, value) -> EzDebug.printf "  %s: %s" name value) headers
      );
    Log.debugf ~v:3 (fun () ->
        let content_type = List.assoc_opt "content-type" headers in
        if body <> "" && (content_type = Some "application/json" || content_type = Some "text/plain") then
          EzDebug.printf "Reply content:\n%s" body);
    let headers = Headers.of_list headers in
    let body = Option.fold ~none:body ~some:(fun f -> body ^ f) footer in
    let len = String.length body in
    let headers = Headers.add headers "content-length" (string_of_int len) in
    let response = Response.create ~headers status in
    Reqd.respond_with_string reqd response body

let error_handler ?footer _sockaddr ?request:_ error start =
  let response_body = start Headers.empty in
  begin match error with
    | `Exn exn ->
      Body.Writer.write_string response_body (Printexc.to_string exn);
      Option.iter (Body.Writer.write_string response_body) footer
    | #Status.standard as error ->
      Body.Writer.write_string response_body (Status.default_reason_phrase error)
  end;
  Body.Writer.flush response_body (function `Written -> Body.Writer.close response_body | _ -> ())

let create ?catch ?allow_origin ?footer ~env ~sw server_port server_kind =
  let s = { server_port; server_kind } in
  let net = Eio.Stdenv.net env in
  ignore @@ Doc.all_services_registered ();
  let request_handler ~sw _addr reqd = request_handler ?allow_origin ?catch ?footer ~sw s reqd in
  let error_handler = error_handler ?footer in
  let handler ~sw = Server.create_connection_handler ~request_handler:(request_handler ~sw) ~error_handler ~sw in
  let on_error exn = EzDebug.printf "Server Error: %s" (Printexc.to_string exn) in
  EzDebug.printf "Starting HTTPUN server (port: %d)" server_port;
  let additional_domains = Eio.Stdenv.domain_mgr env, Stdlib.Domain.recommended_domain_count () in
  let socket = Eio.Net.listen ~reuse_addr:true ~backlog:128 ~sw net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, server_port)) in
  Eio.Net.run_server socket ~additional_domains ~on_error @@ fun socket peer_addr ->
  Eio.Switch.run @@ fun sw -> handler ~sw peer_addr socket

let run ?catch ?allow_origin ?footer ~env ~sw servers =
  Eio.Fiber.all @@ List.map (fun (port, kind) -> fun () ->
      create ?catch ?allow_origin ?footer ~env ~sw port kind)
    servers
