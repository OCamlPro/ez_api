(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzRequest
open Ezjs_fetch

let log ?(meth="GET") url = function
  | None -> ()
  | Some msg -> Ezjs_min.log_str ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]")

let handle_response ?msg url f r =
  match r with
  | Error e ->
    f @@ Error (0, Some (Ezjs_min.to_string e##toString))
  | Ok r ->
    log ~meth:("RECV " ^ string_of_int r.status) url msg;
    if !Verbose.v land 1 <> 0 then Format.printf "[ez_api] received:\n%s@." r.body;
    if r.status >= 200 && r.status < 300 then f @@ Ok r.body
    else f @@ Error (r.status, Some r.body)

let make ?msg ?content ?content_type ~meth ~headers url f =
  log ~meth url msg;
  if !Verbose.v land 2 <> 0 then Format.printf "[ez_api] sent:\n%s@." (Option.value ~default:"" content);
  let headers = Option.fold ~none:headers ~some:(fun ct -> ("Content-Type", ct) :: headers) content_type in
  let body = Option.map (fun s -> RString s) content in
  fetch ~headers ~meth ?body url to_text (handle_response ?msg url f)

module Interface = struct
  let get ?(meth="GET") ?(headers=[]) ?msg url f =
    make ?msg ~meth ~headers url f

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?(headers=[]) ?msg url  f =
    make ?msg ~content ~content_type ~meth ~headers url f
end

include Make(Interface)

let () =
  Js_of_ocaml.Js.Unsafe.global##.set_verbose_ := Js_of_ocaml.Js.wrap_callback Verbose.set_verbose;
  EzDebug.log "ezFetch Loaded"
