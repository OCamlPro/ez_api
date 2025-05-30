(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  mutable doc_id : int; (* uniq service identifier *)
  doc_name : string option;
  doc_descr : string option;
  doc_path : string;
  doc_args : Arg.descr list;
  doc_params : Param.t list;
  doc_section : section;
  doc_input : Json_schema.schema Lazy.t option;
  doc_output : Json_schema.schema Lazy.t option;
  doc_input_mime : Mime.t list;
  doc_output_mime : Mime.t list;
  doc_errors : (int * Json_schema.schema Lazy.t) list;
  doc_meth : Meth.t;
  doc_security : Security.scheme list;
  doc_input_example : Json_repr.any option;
  doc_output_example : Json_repr.any option;
  doc_hide : bool;
  doc_register : bool;
}

and section = {
  section_name : string;
  mutable section_docs : t list;
}

let default_section = { section_name = "Misc"; section_docs = [] }
let sections = ref [ default_section ]

let definitions_path = "/components/schemas/"

let make :
  type i o. ?name:string -> ?descr:string -> ?register:bool -> ?hide:bool -> ?section:section ->
  ?input_example:i -> ?output_example:o -> (_, i, o, _, _) Service.t -> t =
  fun ?name ?descr ?(register=true) ?(hide=false) ?(section=default_section) ?input_example ?output_example s ->
  let path = Service.path s in
  let input = Service.input s in
  let output = Service.output s in
  let doc_input = match input with
    | Service.IO.Json enc -> Some (lazy (Json_encoding.schema ~definitions_path enc ))
    | _ -> None in
  let doc_output = match output with
    | Service.IO.Json enc -> Some (lazy (Json_encoding.schema ~definitions_path enc ))
    | _ -> None in
  let doc_input_mime = match input with
    | Service.IO.Raw l -> l
    | Service.IO.Empty -> []
    | Service.IO.Json _ -> [ Mime.json ] in
  let doc_output_mime = match output with
    | Service.IO.Raw l -> l
    | Service.IO.Empty -> []
    | Service.IO.Json _ -> [ Mime.json ] in
  let doc_input_example = match input_example, input with
    | Some ex, Service.IO.Json enc -> Some (Json_repr.to_any @@ Json_encoding.construct enc ex)
    | _ -> None in
  let doc_output_example = match output_example, output with
    | Some ex, Service.IO.Json enc -> Some (Json_repr.to_any @@ Json_encoding.construct enc ex)
    | _ -> None in
  {
    doc_path = Path.to_string path;
    doc_args = Path.args path;
    doc_params = Service.params s;
    doc_name = name; doc_descr = descr; doc_id = -1;
    doc_section = section;
    doc_input; doc_input_mime; doc_output_mime;
    doc_output;
    doc_errors = Err.merge_errs_same_code ~definitions_path (Service.errors s);
    doc_meth = Service.meth s;
    doc_security = (Service.security s :> Security.scheme list);
    doc_input_example; doc_output_example;
    doc_hide = hide;
    doc_register = register
  }

let rec update_service_list services doc = match services with
  | [] -> [ doc ]
  | h :: t when h.doc_path = doc.doc_path && h.doc_meth = doc.doc_meth -> doc :: t
  | h :: t -> h :: (update_service_list t doc)

let services = ref []
let nservices = ref 0

let register doc =
  doc.doc_id <- !nservices;
  incr nservices;
  doc.doc_section.section_docs <- update_service_list doc.doc_section.section_docs doc;
  services := update_service_list !services doc

let section section_name =
  let s = { section_name; section_docs = [] } in
  sections := s :: !sections;
  s

let all_services_registered () =
  let s = List.fold_left (fun acc doc ->
      if doc.doc_id = -1 && doc.doc_register then
        Printf.sprintf "%s%s is not registered\n" acc doc.doc_path
      else acc
    ) "" !services in
  if s <> "" then begin
    Printf.eprintf "Warning: unregistered services:\n%s\n%!" s;
    false
  end else true

let section_name s = s.section_name

let nservices () = !nservices

let services () =
  Array.map (fun doc -> doc.doc_path) (Array.of_list (List.rev !services))
