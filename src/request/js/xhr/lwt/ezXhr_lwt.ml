(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Js_of_ocaml
open Js_of_ocaml_lwt.XmlHttpRequest
open EzRequest_lwt

let (>|=) = Lwt.(>|=)

let meth_of_str ?(default=`GET) = function
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "PUT" -> `PUT
  | "POST" -> `POST
  | "PATCH" -> `PATCH
  | "DELETE" -> `DELETE
  | "OPTIONS" -> `OPTIONS
  | _ -> default

let make ?msg ?content ?content_type ~meth ~headers url =
  Verbose.request ?msg ~meth ?content url;
  let contents = Option.map (fun s -> `String s) content in
  perform_raw_url ?headers ?content_type ?contents
    ~override_method:(meth_of_str ~default:`POST meth) url >|= fun frame ->
  Verbose.response ?msg ~code:frame.code ~content:frame.content url;
  if frame.code >= 200 && frame.code < 300 then Ok frame.content
  else Result.Error (frame.code, Some frame.content)

module Interface = struct
  let get ?(meth="GET") ?headers ?msg url =
    make ?msg ~meth ~headers url

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?headers ?msg url =
    make ?msg ~content ~content_type ~meth ~headers url
end

include Make(Interface)

let () =
  Js.Unsafe.global##.set_verbose_ := Js.wrap_callback Verbose.set_verbose;
  Format.eprintf "ezXhr Loaded"
