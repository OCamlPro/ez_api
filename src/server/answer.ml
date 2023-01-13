(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type 'a t = {
  code : int;
  body : 'a;
  headers : (string * string) list;
}

let return ?(code=200) ?(headers=[]) body = Lwt.return {code; body; headers}

let not_found () = return ~code:404 ""

let headers = [ "content-type", "application/json" ]

let cannot_parse (descr, msg, path) =
  let body = EzEncoding.construct Json_encoding.any_ezjson_value @@
    `O [ "error", `String ("Cannot parse path argument " ^ descr.EzAPI.Arg.name);
         "path", `String (String.concat "/" path);
         "msg", `String msg ] in
  return ~code:400 ~headers body

let method_not_allowed () = return ~code:405 ""

let destruct_exception exn =
  let body = EzEncoding.construct Json_encoding.any_ezjson_value @@
    `O [ "error", `String "Destruct exception";
         "exception", `String (Printexc.to_string exn) ] in
  return ~code:400 ~headers body

let unsupported_media_type c =
  let c = match c with None -> "none" | Some c -> c in
  let body = EzEncoding.construct Json_encoding.any_ezjson_value @@
    `O [ "error", `String "Unsupported Media Type";
         "content_type", `String c ] in
  return ~code:415 ~headers body

let server_error exn =
  let body = EzEncoding.construct Json_encoding.any_ezjson_value @@
    `O [ "error", `String "Server Error";
         "msg", `String (Printexc.to_string exn) ] in
  return ~code:500 ~headers body
