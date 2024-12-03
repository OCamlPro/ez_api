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

let log ?(meth="GET") ?msg url = match msg with
  | None ->
    if !Verbose.v <> 0 then Firebug.console##log (Js.string (Format.sprintf "[ez_api] %s %s@." meth url))
    else ()
  | Some msg -> Firebug.console##log (Js.string ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]"))

let make ?msg ?content ?content_type ~meth ~headers url =
  log ~meth ?msg url;
  if !Verbose.v land 2 <> 0 then Format.printf "[ez_api] sent:\n%s@." (Option.value ~default:"" content);
  let contents = Option.map (fun s -> `String s) content in
  perform_raw_url ?headers ?content_type ?contents
    ~override_method:(meth_of_str ~default:`POST meth) url >|= fun frame ->
  log ~meth:("RECV " ^ string_of_int frame.code) ?msg url;
  if !Verbose.v land 1 <> 0 then Format.printf "[ez_api] received:\n%s@." frame.content;
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
  EzDebug.log "ezXhr Loaded"
