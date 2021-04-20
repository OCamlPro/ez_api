type t = {
  doc_id : int; (* uniq service identifier *)
  doc_name : string option;
  doc_descr : string option;
  doc_path : string;
  doc_args : Arg.descr list;
  doc_params : Param.t list;
  mutable doc_registered : bool;
  doc_section : section;
  doc_input : Json_schema.schema Lazy.t option;
  doc_output : Json_schema.schema Lazy.t option;
  doc_mime : Mime.t list;
  doc_errors : (int * Json_schema.schema Lazy.t) list;
  doc_meth : Meth.t;
  doc_security : Security.scheme list;
  doc_input_example : Json_repr.any option;
  doc_output_example : Json_repr.any option;
}

and section = {
  section_name : string;
  mutable section_docs : t list;
}

let services = ref []
let nservices = ref 0
let default_section = { section_name = "Misc"; section_docs = [] }
let sections = ref [ default_section ]

let definitions_path = "/components/schemas/"

let rec update_service_list services doc = match services with
  | [] -> [ doc ]
  | h :: t when h.doc_path = doc.doc_path -> doc :: t
  | h :: t -> h :: (update_service_list t doc)

let make :
  type i. ?name:string -> ?descr:string -> ?register:bool -> ?section:section ->
  ?input_example:i -> ?output_example:'o -> (_, i, 'o, _, _) Service.t -> t =
  fun ?name ?descr ?(register=true) ?(section=default_section) ?input_example ?output_example s ->
  let path = Service.path s in
  let input = Service.input s in
  let output = Service.output s in
  let doc_input = match input with
    | Service.IO.Json enc -> Some (lazy (Json_encoding.schema ~definitions_path enc ))
    | _ -> None in
  let doc_output = match output with
    | Service.IO.Json enc -> Some (lazy (Json_encoding.schema ~definitions_path enc ))
    | _ -> None in
  let doc_mime = match input with
    | Service.IO.Raw l -> l
    | Service.IO.Empty -> []
    | Service.IO.Json _ -> [ Mime.json ] in
  let doc_input_example = match input_example, input with
    | Some ex, Service.IO.Json enc -> Some (Json_repr.to_any @@ Json_encoding.construct enc ex)
    | _ -> None in
  let doc_output_example = match output_example, output with
    | Some ex, Service.IO.Json enc -> Some (Json_repr.to_any @@ Json_encoding.construct enc ex)
    | _ -> None in
  let doc_id = if register then (
      let id = !nservices in
      incr nservices;
      id)
    else -1 in
  let doc = {
    doc_path = Path.to_string path;
    doc_args = Path.args path;
    doc_params = Service.params s;
    doc_registered = false;
    doc_name = name; doc_descr = descr; doc_id;
    doc_section = section;
    doc_input; doc_mime;
    doc_output;
    doc_errors = Err.merge_errs_same_code ~definitions_path (Service.errors s);
    doc_meth = Service.meth s;
    doc_security = (Service.security s :> Security.scheme list);
    doc_input_example; doc_output_example
  } in
  if register then (
    section.section_docs <- update_service_list section.section_docs doc;
    services := update_service_list !services doc);
  doc

let section section_name =
  let s = { section_name; section_docs = [] } in
  sections := s :: !sections;
  s

let all_services_registered () =
  let s = List.fold_left (fun acc doc ->
      if not doc.doc_registered then
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
