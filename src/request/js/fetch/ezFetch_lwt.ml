(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzRequest_lwt
open Ezjs_fetch_lwt

let (>|=) = Lwt.(>|=)

let log ?(meth="GET") ?msg url = match msg with
  | None -> ()
  | Some msg -> Ezjs_min.log_str ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]")

let handle_response ?msg url r =
  match r with
  | Error e -> Error (0, Some (Ezjs_min.to_string e##toString))
  | Ok r ->
    log ~meth:("RECV " ^ string_of_int r.status) ?msg url;
    if !Verbose.v land 1 <> 0 then Format.printf "[ez_api] received:\n%s@." r.body;
    if r.status >= 200 && r.status < 300 then Ok r.body
    else Error (r.status, Some r.body)

let make ?msg ?content ?content_type ~meth ~headers url =
  log ~meth ?msg url;
  if !Verbose.v land 2 <> 0 then Format.printf "[ez_api] sent:\n%s@." (Option.value ~default:"" content);
  let headers = Option.fold ~none:headers ~some:(fun ct -> ("Content-Type", ct) :: headers) content_type in
  let body = Option.map (fun s -> RString s) content in
  fetch ~headers ~meth ?body url to_text >|= (handle_response ?msg url)

module Interface = struct
  let get ?(meth="GET") ?(headers=[]) ?msg url =
    make ~meth ~headers ?msg url

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?(headers=[]) ?msg url =
    make ~meth ~headers ~content_type ~content ?msg url
end

include Make(Interface)

let () =
  Js_of_ocaml.Js.Unsafe.global##.set_verbose_ := Js_of_ocaml.Js.wrap_callback Verbose.set_verbose;
  EzDebug.log "ezFetch Loaded"
