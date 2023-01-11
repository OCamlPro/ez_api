(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Types = struct
  type contact_object = {
    oco_name : string option;
    oco_url : string option;
    oco_email : string option;
  }

  type license_object = {
    oli_name : string;
    oli_url : string option;
  }

  type openapi_info = {
    oin_title : string;
    oin_description : string option;
    oin_terms : string option;
    oin_contact : contact_object option;
    oin_license : license_object option;
    oin_version : string;
  }

  type server_variable_object = {
    osv_enum : string list option;
    osv_default : string;
    osv_description : string option;
  }

  type server_object = {
    osr_url : string;
    osr_description : string option;
    osr_variables : (string * server_variable_object) list option;
  }

  type param_object = {
    opm_name : string;
    opm_in : string;
    opm_description : string option;
    opm_required : bool;
    opm_deprecated : bool option;
    opm_allow_empty : bool option;
    opm_style : string option;
    opm_example : Json_repr.any option;
    opm_schema : Json_repr.ezjsonm option;
  }

  type encoding_object = {
    oenc_content_type : string option;
    oenc_headers : (string * param_object) list option;
    oenc_style : string option;
    oenc_explode : bool option;
    oenc_allow_reserved : bool option;
  }

  type media_type_object = {
    omt_schema : Json_repr.ezjsonm option;
    omt_example : Json_repr.any option;
    omt_encoding : encoding_object option;
  }

  type link_object = {
    olk_op_ref : string option;
    olk_op_id : string option;
    olk_params : (string * Json_repr.any) list option;
    olk_request : Json_repr.any option;
    olk_description : string option;
    olk_server : server_object option;
  }

  type response_object = {
    ors_description : string;
    ors_headers : (string * param_object) list option;
    ors_content : (string * media_type_object) list option;
    ors_links : (string * link_object) list option;
  }

  type external_doc_object = {
    oed_description : string option;
    oed_url : string;
  }

  type request_object = {
    orq_description : string option;
    orq_content : (string * media_type_object) list;
    orq_required : bool option;
  }

  type operation_object = {
    opt_tags : string list option;
    opt_summary : string option;
    opt_description : string option;
    opt_external : external_doc_object option;
    opt_id : string option;
    opt_params : param_object list option;
    opt_request : request_object option;
    opt_responses : (string * response_object) list;
    opt_deprecated : bool option;
    opt_security : EzAPI.Security.scheme list option;
    (* opt_security : (string * string list) list option; *)
    opt_servers : server_object list option;
  }

  type path_item = {
    opa_ref : string option;
    opa_summary : string option;
    opa_description : string option;
    opa_method : string;
    opa_operation : operation_object;
    opa_servers : server_object list option;
    opa_params : param_object list option;
  }

  type example_object = {
    oex_summary : string option;
    oex_description : string option;
    oex_value : Json_repr.any option;
    oex_external : string option;
  }

  type security_scheme_object = {
    osc_type : string;
    osc_description : string option;
    osc_name : string option;
    osc_in : string option;
    osc_scheme : string option;
    osc_format : string option;
    osc_connect_url : string option;
    (* osc_flows *)
  }

  type components_object = {
    ocm_schemas : (string * Json_repr.any) list option;
    ocm_responses : (string * response_object) list option;
    ocm_parameters : (string * param_object) list option;
    ocm_examples : (string * example_object) list option;
    ocm_requests : (string * request_object) list option;
    ocm_headers : (string * param_object) list option;
    ocm_security : EzAPI.Security.scheme list option;
    ocm_links : (string * link_object) list option;
  }

  type openapi_object = {
    oa_version : string;
    oa_info : openapi_info;
    oa_servers : server_object list option;
    oa_paths : (string * path_item) list;
    oa_components : components_object option;
    oa_security : EzAPI.Security.scheme list option;
    oa_tags : string list option;
    oa_external : external_doc_object option;
  }
end

module Makers = struct
  open Types

  let mk_contact ?name ?url ?email () =
    {oco_name = name; oco_url = url; oco_email = email}

  let mk_licence ?url name = {oli_name = name; oli_url = url}

  let mk_info ?descr ?terms ?contact ?license ~version oin_title = {
    oin_title; oin_description = descr; oin_terms = terms;
    oin_contact = contact; oin_license = license; oin_version = version }

  let mk_server_variable ?enum ?descr osv_default =
    { osv_enum = enum; osv_default; osv_description = descr }

  let mk_server ?descr ?variables osr_url =
    { osr_url; osr_description = descr; osr_variables = variables }

  let mk_schema schema =
    let json = Json_schema.to_json schema in
    match json with
    | `O l -> `O (List.filter_map (fun (k, v) ->
        if k = "components" || k = "$schema" then None else Some (k, v)) l)
    | json -> json

  let mk_param ?descr ?(required=true) ?deprecated ?allow_empty ?style ?example
      ?schema ?(loc="path") opm_name =
    let opm_schema = Option.map mk_schema schema in {
    opm_name; opm_in = loc; opm_description = descr; opm_required = required;
    opm_deprecated = deprecated; opm_allow_empty = allow_empty; opm_style = style;
    opm_example = example; opm_schema }

  let mk_media ?schema ?example ?encoding () =
    let omt_schema = Option.map mk_schema schema in
    { omt_schema; omt_example = example; omt_encoding = encoding }

  let mk_response ?headers ?content ?links ors_description =
    { ors_description; ors_headers = headers; ors_content = content; ors_links = links }

  let mk_request ?descr ?required orq_content =
    { orq_description = descr; orq_content; orq_required = required }

  let mk_operation ?tags ?summary ?descr ?extern ?id ?params ?request ?deprecated
      ?security ?servers opt_responses = {
    opt_tags = tags; opt_summary = summary; opt_description = descr;
    opt_external = extern; opt_id = id; opt_params = params; opt_request = request;
    opt_responses; opt_deprecated = deprecated; opt_security = security;
    opt_servers = servers }

  let mk_path ?oref ?summary ?descr ?servers ?params ~meth opa_operation = {
    opa_ref = oref; opa_summary = summary; opa_description = descr;
    opa_method = meth; opa_operation; opa_servers = servers; opa_params = params }

  let mk_example ?summary ?descr ?value ?ext () = {
    oex_summary = summary; oex_description = descr; oex_value = value;
    oex_external = ext }

  let mk_security_scheme ?descr ?name ?loc ?scheme ?format ?connect_url osc_type = {
    osc_type; osc_description = descr; osc_name = name; osc_in = loc;
    osc_scheme = scheme; osc_format = format; osc_connect_url = connect_url }

  let mk_components ?schemas ?responses ?parameters ?examples ?requests
      ?headers ?security ?links () = {
    ocm_schemas = schemas; ocm_responses = responses; ocm_parameters = parameters;
    ocm_examples = examples; ocm_requests = requests; ocm_headers = headers;
    ocm_security = security; ocm_links = links }

  let mk_openapi ?(version="3.0.3") ?servers ?components ?security ?tags ?extern ~info oa_paths = {
    oa_version = version; oa_info = info; oa_servers = servers; oa_paths;
    oa_components = components; oa_security = security; oa_tags = tags; oa_external = extern }

end

module Encoding = struct
  open Types
  open Json_encoding

  let contact_object = conv
      (fun {oco_name; oco_url; oco_email} -> (oco_name, oco_url, oco_email))
      (fun (oco_name, oco_url, oco_email) -> {oco_name; oco_url; oco_email}) @@
    obj3
      (opt "name" string)
      (opt "url" string)
      (opt "email" string)

  let license_object = conv
      (fun {oli_name; oli_url} -> (oli_name, oli_url))
      (fun (oli_name, oli_url) -> {oli_name; oli_url}) @@
    obj2 (req "name" string) (opt "url" string)

  let openapi_info = conv
      (fun {oin_title; oin_description; oin_terms; oin_contact; oin_license; oin_version}
        -> (oin_title, oin_description, oin_terms, oin_contact, oin_license, oin_version))
      (fun (oin_title, oin_description, oin_terms, oin_contact, oin_license, oin_version)
        -> {oin_title; oin_description; oin_terms; oin_contact; oin_license; oin_version}) @@
    obj6
      (req "title" string)
      (opt "description" string)
      (opt "termsOfService" string)
      (opt "contact" contact_object)
      (opt "license" license_object)
      (req "version" string)

  let server_variable_object = conv
      (fun {osv_enum; osv_default; osv_description} -> (osv_enum, osv_default, osv_description))
      (fun (osv_enum, osv_default, osv_description) -> {osv_enum; osv_default; osv_description}) @@
    obj3
      (opt "enum" (list string))
      (req "default" string)
      (opt "description" string)

  let server_object = conv
      (fun {osr_url; osr_description; osr_variables} -> (osr_url, osr_description, osr_variables))
      (fun (osr_url, osr_description, osr_variables) -> {osr_url; osr_description; osr_variables}) @@
    obj3
      (req "url" string)
      (opt "description" string)
      (opt "variables" (assoc server_variable_object))

  let param_type = string_enum [
      "string", EzAPI.Param.PARAM_STRING;
      "integer", EzAPI.Param.PARAM_INT;
      "boolean", EzAPI.Param.PARAM_BOOL ]

  let param_object = conv
      (fun {opm_name; opm_in; opm_description; opm_required; opm_deprecated;
            opm_allow_empty; opm_style; opm_example; opm_schema}
        -> (opm_name, opm_in, opm_description, opm_required, opm_deprecated,
            opm_allow_empty, opm_style, opm_example, opm_schema))
      (fun (opm_name, opm_in, opm_description, opm_required, opm_deprecated,
            opm_allow_empty, opm_style, opm_example, opm_schema)
        -> {opm_name; opm_in; opm_description; opm_required; opm_deprecated;
            opm_allow_empty; opm_style; opm_example; opm_schema}) @@
    obj9
      (req "name" string)
      (req "in" string)
      (opt "description" string)
      (req "required" bool)
      (opt "deprecated" bool)
      (opt "allowEmptyValue" bool)
      (opt "style" string)
      (opt "example" any_value)
      (opt "schema" any_ezjson_value)

  let encoding_object = conv
      (fun {oenc_content_type; oenc_headers; oenc_style; oenc_explode; oenc_allow_reserved}
        -> (oenc_content_type, oenc_headers, oenc_style, oenc_explode, oenc_allow_reserved))
      (fun (oenc_content_type, oenc_headers, oenc_style, oenc_explode, oenc_allow_reserved)
        -> {oenc_content_type; oenc_headers; oenc_style; oenc_explode; oenc_allow_reserved}) @@
    obj5
      (opt "contentType" string)
      (opt "headers" (assoc param_object))
      (opt "style" string)
      (opt "explode" bool)
      (opt "allowReserved" bool)

  let media_type_object = conv
      (fun {omt_schema; omt_example; omt_encoding} -> (omt_schema, omt_example, omt_encoding))
      (fun (omt_schema, omt_example, omt_encoding) -> {omt_schema; omt_example; omt_encoding}) @@
    obj3
      (opt "schema" any_ezjson_value)
      (opt "example" any_value)
      (opt "encoding" encoding_object)

  let link_object = conv
      (fun {olk_op_ref; olk_op_id; olk_params; olk_request; olk_description; olk_server}
        -> (olk_op_ref, olk_op_id, olk_params, olk_request, olk_description, olk_server))
      (fun (olk_op_ref, olk_op_id, olk_params, olk_request, olk_description, olk_server)
        -> {olk_op_ref; olk_op_id; olk_params; olk_request; olk_description; olk_server}) @@
    obj6
      (opt "operationRef" string)
      (opt "operationId" string)
      (opt "parameters" (assoc any_value))
      (opt "requestBody" any_value)
      (opt "description" string)
      (opt "server" server_object)

  let response_object = conv
      (fun {ors_description; ors_headers; ors_content; ors_links}
        -> (ors_description, ors_headers, ors_content, ors_links))
      (fun (ors_description, ors_headers, ors_content, ors_links)
        -> {ors_description; ors_headers; ors_content; ors_links}) @@
    obj4
      (req "description" string)
      (opt "headers" (assoc param_object))
      (opt "content" (assoc media_type_object))
      (opt "links" (assoc link_object))

  let external_doc_object = conv
      (fun {oed_description; oed_url} -> (oed_description, oed_url))
      (fun (oed_description, oed_url) -> {oed_description; oed_url}) @@
    obj2
      (opt "description" string)
      (req "url" string)

  let request_object = conv
      (fun {orq_description; orq_content; orq_required}
        -> (orq_description, orq_content, orq_required))
      (fun (orq_description, orq_content, orq_required)
        -> {orq_description; orq_content; orq_required}) @@
    obj3
      (opt "description" string)
      (req "content" (assoc media_type_object))
      (opt "required" bool)

  let callback_object encoding = assoc encoding

  let operation_object = conv
      (fun {opt_tags; opt_summary; opt_description; opt_external; opt_id;
            opt_params; opt_request; opt_responses; opt_deprecated;
            opt_security; opt_servers}
        -> (opt_tags, opt_summary, opt_description, opt_external, opt_id,
            opt_params, opt_request, opt_responses, opt_deprecated,
            Option.map (List.map (fun s -> [EzAPI.Security.ref_name s, []])) opt_security,
            opt_servers))
      (fun (opt_tags, opt_summary, opt_description, opt_external, opt_id,
            opt_params, opt_request, opt_responses, opt_deprecated,
            _opt_security, opt_servers)
        -> {opt_tags; opt_summary; opt_description; opt_external; opt_id;
            opt_params; opt_request; opt_responses; opt_deprecated;
            opt_security = None; opt_servers}) @@
    EzEncoding.obj11
      (opt "tags" (list string))
      (opt "summary" string)
      (opt "description" string)
      (opt "externalDocs" external_doc_object)
      (opt "operationId" string)
      (opt "parameters" (list param_object))
      (opt "requestBody" request_object)
      (req "responses" (assoc response_object))
      (opt "deprecated" bool)
      (opt "security" (list @@ assoc (list string)))
      (opt "servers" (list server_object))

  let method_enc encoding = conv
      (fun (m, x) -> match m with
         | "get" -> Some x, None, None, None, None, None, None, None
         | "put"-> None, Some x, None, None, None, None, None, None
         | "post" -> None, None, Some x, None, None, None, None, None
         | "delete" -> None, None, None, Some x, None, None, None, None
         | "options" -> None, None, None, None, Some x, None, None, None
         | "head" -> None, None, None, None, None, Some x, None, None
         | "patch" -> None, None, None, None, None, None, Some x, None
         | "trace" -> None, None, None, None, None, None, None, Some x
         | _ -> None, None, None, None, None, None, None, None)
      (function
        | Some x, _, _, _, _, _, _, _ -> "get", x
        | _, Some x, _, _, _, _, _, _ -> "put", x
        | _, _, Some x, _, _, _, _, _ -> "post", x
        | _, _, _, Some x, _, _, _, _ -> "delete", x
        | _, _, _, _, Some x, _, _, _ -> "options", x
        | _, _, _, _, _, Some x, _, _ -> "head", x
        | _, _, _, _, _, _, Some x, _ -> "patch", x
        | _, _, _, _, _, _, _, Some x -> "trace", x
        | _ -> assert false) @@
    obj8
      (opt "get" encoding)
      (opt "put" encoding)
      (opt "post" encoding)
      (opt "delete" encoding)
      (opt "options" encoding)
      (opt "head" encoding)
      (opt "patch" encoding)
      (opt "trace" encoding)

  let path_item = conv
      (fun {opa_ref; opa_summary; opa_description; opa_method; opa_operation;
            opa_servers; opa_params} ->
        (opa_ref, opa_summary, opa_description, opa_servers, opa_params),
        (opa_method, opa_operation))
      (fun ((opa_ref, opa_summary, opa_description, opa_servers, opa_params),
            (opa_method, opa_operation))
        -> {opa_ref; opa_summary; opa_description; opa_method; opa_operation;
            opa_servers; opa_params}) @@
    merge_objs
      (obj5
         (opt "$ref" string)
         (opt "summary" string)
         (opt "description" string)
         (opt "servers" (list server_object))
         (opt "parameters" (list param_object)))
      (method_enc operation_object)

  let example_object = conv
      (fun {oex_summary; oex_description; oex_value; oex_external}
        -> (oex_summary, oex_description, oex_value, oex_external))
      (fun (oex_summary, oex_description, oex_value, oex_external)
        -> {oex_summary; oex_description; oex_value; oex_external}) @@
    obj4
      (opt "summary" string)
      (opt "description" string)
      (opt "value" any_value)
      (opt "externalValue" string)

  let security_scheme_object = conv
      (fun {osc_type; osc_description; osc_name; osc_in; osc_scheme; osc_format;
            osc_connect_url}
        -> (osc_type, osc_description, osc_name, osc_in, osc_scheme, osc_format,
            osc_connect_url))
      (fun (osc_type, osc_description, osc_name, osc_in, osc_scheme, osc_format,
            osc_connect_url)
        -> {osc_type; osc_description; osc_name; osc_in; osc_scheme; osc_format;
            osc_connect_url}) @@
    obj7
      (req "type" string)
      (opt "description" string)
      (opt "name" string)
      (opt "in" string)
      (opt "scheme" string)
      (opt "bearerFormat" string)
      (opt "openIdConnectUrl" string)

  let make_security_scheme = function
    | `Nosecurity _ -> None
    | `Basic { EzAPI.Security.basic_name } ->
      Some (basic_name, Makers.mk_security_scheme ~scheme:"basic" "http")
    | `Bearer { EzAPI.Security.bearer_name; format } ->
      Some (bearer_name, Makers.mk_security_scheme ?format ~scheme:"bearer" "http")
    | `Header { EzAPI.Security.ref_name; name } ->
      Some (ref_name, Makers.mk_security_scheme ~loc:"header" ~name "apiKey")
    | `Cookie { EzAPI.Security.ref_name; name } ->
      Some (ref_name, Makers.mk_security_scheme ~loc:"cookie" ~name "apiKey")
    | `Query { EzAPI.Security.ref_name; name } ->
      Some (ref_name, Makers.mk_security_scheme ~loc:"query" ~name:name.EzAPI.Param.param_id "apiKey")

  let components_object = conv
      (fun {ocm_schemas; ocm_responses; ocm_parameters; ocm_examples; ocm_requests;
            ocm_headers; ocm_security; ocm_links}
        -> (ocm_schemas, ocm_responses, ocm_parameters, ocm_examples, ocm_requests,
            ocm_headers, Option.map (List.filter_map make_security_scheme) ocm_security,
            ocm_links))
      (fun (ocm_schemas, ocm_responses, ocm_parameters, ocm_examples, ocm_requests,
            ocm_headers, _ocm_security, ocm_links)
        -> {ocm_schemas; ocm_responses; ocm_parameters; ocm_examples; ocm_requests;
            ocm_headers; ocm_security = None; ocm_links}) @@
    obj8
      (opt "schemas" (assoc any_value))
      (opt "responses" (assoc response_object))
      (opt "parameters" (assoc param_object))
      (opt "examples" (assoc example_object))
      (opt "requests" (assoc request_object))
      (opt "headers" (assoc param_object))
      (opt "securitySchemes" (assoc security_scheme_object))
      (opt "links" (assoc link_object))

  let openapi_object = conv
      (fun {oa_version; oa_info; oa_servers; oa_paths; oa_components;
            oa_security; oa_tags; oa_external}
        -> (oa_version, oa_info, oa_servers, oa_paths, oa_components,
            Option.map (List.map (fun s -> [EzAPI.Security.ref_name s, []])) oa_security,
            oa_tags, oa_external))
      (fun (oa_version, oa_info, oa_servers, oa_paths, oa_components,
            _oa_security, oa_tags, oa_external)
        -> {oa_version; oa_info; oa_servers; oa_paths; oa_components;
            oa_security = None; oa_tags; oa_external}) @@
    obj8
      (req "openapi" string)
      (req "info" openapi_info)
      (opt "servers" (list server_object))
      (req "paths" (assoc path_item))
      (opt "components" components_object)
      (opt "security" (list @@ assoc (list string)))
      (opt "tags" (list string))
      (opt "externalDocs" external_doc_object)

end


open EzAPI

let make_query_param ?(definitions=Json_schema.any) p =
  let schema = match p.Param.param_schema with
    | Some schema -> schema
    | None -> match p.Param.param_type with
      | Param.PARAM_INT -> Json_schema.(create @@ element @@ Number numeric_specs)
      | Param.PARAM_STRING -> Json_schema.(create @@ element @@ String string_specs)
      | Param.PARAM_BOOL -> Json_schema.(create @@ element Boolean) in
  let schema, definitions = Json_schema.merge_definitions (schema, definitions) in
  Makers.mk_param ?descr:p.Param.param_descr ~required:p.Param.param_required ~loc:"query"
    ~schema (Option.value ~default:p.Param.param_id p.Param.param_name), definitions

let make_path_params args =
  let schema = Json_schema.(create @@ element @@ String string_specs) in
  List.map (fun arg ->
      Makers.mk_param
        ?example:(Option.map (fun s -> Json_repr.to_any (`String s)) arg.Arg.example)
        ?descr:arg.Arg.descr ~schema arg.Arg.name) args

let empty_schema ~none schema f = match Json_schema.root schema with
  | {Json_schema.kind = Json_schema.Object {Json_schema.additional_properties = None; properties = []; _}; _}
    -> none
  | _ -> f schema

let make_request ?example mime schema = match schema, mime with
  | None, [] -> None
  | None, l ->
    (* binary requests *)
    let schema = Json_schema.(
        create @@ element @@ String {string_specs with str_format = Some "binary"} ) in
    Some Makers.(mk_request @@ List.map (fun m -> m, mk_media ~schema ()) l)
  | Some schema, _ ->
    empty_schema ~none:None schema (fun schema ->
        Some Makers.(mk_request ["application/json", mk_media ?example ~schema () ]))

let merge_definitions ?(definitions=Json_schema.any) sd =
  let input_schema, definitions = match sd.Doc.doc_input with
    | None -> None, definitions
    | Some sc ->
      let sc, def =
        Json_schema.merge_definitions (Lazy.force sc, definitions) in
      Some (Json_schema.simplify sc), def in
  let output_schema, definitions = match sd.Doc.doc_output with
    | None -> [], definitions
    | Some sc ->
      let sc, def = Json_schema.merge_definitions (Lazy.force sc, definitions) in
      [200, Json_schema.simplify sc], def in
  let output_schemas, definitions = List.fold_left (fun (acc, definitions) (code, sch) ->
      let sch, definitions = Json_schema.merge_definitions (Lazy.force sch, definitions) in
      (code, Json_schema.simplify sch) :: acc, definitions)
      (output_schema, definitions) sd.Doc.doc_errors in
  input_schema, output_schemas, definitions

let make_path ?(docs=[]) ?definitions sd =
  let open Doc in
  let path = sd.doc_path in
  let id, summary, descr, input_ex, output_ex = match sd.doc_name with
    | None ->
      string_of_int sd.doc_id, sd.doc_name,
      sd.doc_descr, sd.doc_input_example, sd.doc_output_example
    | Some name -> match List.assoc_opt name docs with
      | None ->
        name, sd.doc_name, sd.doc_descr,
        sd.doc_input_example, sd.doc_output_example
      | Some (summary, descr, input, output) ->
        summary, None, Some descr,
        (match input with None -> sd.doc_input_example | Some x -> Some x),
        (match output with None -> sd.doc_output_example | Some x -> Some x) in
  let input_schema, output_schemas, definitions = merge_definitions ?definitions sd in
  let params, definitions = List.fold_left (fun (acc, definitions) p ->
      let p, definitions = make_query_param ~definitions p in
      acc @ [ p ], definitions) ([], definitions) sd.doc_params in
  (path,
   Makers.mk_path ?summary ?descr ~meth:(Meth.to_string sd.doc_meth) (
     Makers.mk_operation ?summary ?descr
       ~tags:[sd.doc_section.section_name] ~id
       ~params:(params @ make_path_params sd.doc_args)
       ~security:sd.doc_security
       ?request:(make_request ?example:input_ex (List.map Mime.to_string sd.doc_mime) input_schema) @@
     List.map (fun (code, schema) ->
         let example = if code = 200 then output_ex else None in
         let code_str = string_of_int code in
         let content =
           empty_schema ~none:[ "application/json", Makers.mk_media ?example ()] schema (fun schema ->
               [ "application/json", Makers.mk_media ?example ~schema () ]) in
         code_str, Makers.mk_response ~content
           (Option.value ~default:code_str @@ Error_codes.error code)) output_schemas)),
  definitions

let definitions_schemas definitions =
  match Json_schema.to_json definitions with
  | `O l -> begin match List.assoc_opt "components" l with
      | Some (`O [ "schemas", `O l ]) ->
        Some (List.map (fun (s, j) -> s, Json_repr.to_any j) l)
      | _ -> None end
  | _ -> None

let json_map f (j : Json_repr.ezjsonm) : Json_repr.ezjsonm =
  let rec map j =
    let j' = match j with
      | `Null | `Bool _ | `Float _ | `String _ -> j
      | `A l ->
        let l' = List.rev_map map l |> List.rev in
        if List.for_all2 (==) l l' then j
        else `A l'
      | `O l ->
        let l' = List.rev_map (fun (s, x) ->
            s, map x
          ) l |> List.rev in
        if List.for_all2 (fun (_, x) (_, x') -> x == x') l l' then j
        else `O l'
    in
    let j' = f j' in
    if j == j' then j else j'
  in
  map j

(* ReDoc ignores item description if it is for a ref (and uses the
   description of the ref instead which makes the feature
   useless). This hack wraps the ref in an allOf combinator to
   workaround the latent ReDoc bug while retaining the same
   semantic. *)
let fix_descr_ref json =
  json_map (fun j ->
      try
        let _description = Ezjsonm.find j ["description"] in
        let ref_ = Ezjsonm.find j ["$ref"] in
        let j = Ezjsonm.update j ["$ref"] None in
        Ezjsonm.update j ["allOf"] @@ Some (
          `A [ `O ["$ref", ref_] ]
        )
      with Not_found -> j
    ) json

let make ?descr ?terms ?contact ?license ?(version="0.1") ?servers ?(docs=[])
    ?(yaml=false) ?(pretty=false) ~sections ~title filename =
  let info = Makers.mk_info ?descr ?terms ?contact ?license ~version title in
  let sds = List.concat @@ List.map (fun s -> s.Doc.section_docs) sections in
  let security = List.rev @@ List.fold_left (fun acc sd ->
      List.fold_left (fun acc s -> if List.mem s acc then acc else s :: acc) acc sd.Doc.doc_security)
      [] sds in
  let paths, definitions = List.fold_left (fun (paths, definitions) sd ->
      let path, definitions = make_path ~definitions ~docs sd in
      path :: paths, definitions) ([], Json_schema.any) sds in
  let schemas = definitions_schemas definitions in
  let oa = Makers.mk_openapi ?servers ~info
      ~components:(Makers.mk_components ~security ?schemas ())
      (List.rev paths) in
  let openapi_json =
    fix_descr_ref @@ Json_encoding.construct Encoding.openapi_object oa in
  if yaml then
    match EzYaml.to_string openapi_json with
    | Error (`Msg msg) ->
      Format.eprintf "%s@." msg;
      filename ^ ".json", EzEncoding.Ezjsonm.to_string ~minify:false openapi_json
    | Ok s -> filename ^ ".yaml", s
  else
    filename ^ ".json", EzEncoding.Ezjsonm.to_string ~minify:(not pretty) openapi_json


let write ?descr ?terms ?contact ?license ?version ?servers ?docs ?(yaml=false)
    ?pretty ~sections ~title filename =
  let filename, s = make ?descr ?terms ?contact ?license ?version ?servers ?docs ~yaml ?pretty ~sections ~title filename in
  let oc = open_out filename in
  output_string oc s;
  close_out oc

let executable ~sections ~docs =
  let open Stdlib in
  let str_opt s = Arg.String (fun x -> s := Some x) in
  let output_file, title, descr, version, terms, contact, license, servers, yaml, pretty =
    ref "openapi", ref "API Documentation", ref None, ref None, ref None,
    ref None, ref None, ref None, ref false, ref false in
  let speclist =
    [ "-o", Arg.Set_string output_file, "Optional name (path) of output file";
      "--descr", str_opt descr, "Optional API description";
      "--version", str_opt version, "Optional API version";
      "--title", Arg.Set_string title, "Optional API title";
      "--terms", str_opt terms, "Optional API terms";
      "--contact", Arg.String (fun s ->
          match String.split_on_char ',' s with
          | [ email ] -> contact := Some (Makers.mk_contact ~email ())
          | [ email; name ] -> contact := Some (Makers.mk_contact ~email ~name ())
          | _ -> ()), "Optional API contact";
      "--license", Arg.String (fun s ->
          match String.split_on_char ',' s with
          | [ name ] -> license := Some (Makers.mk_licence name)
          | [ name; url ] -> license := Some (Makers.mk_licence ~url name)
          | _ -> ()), "Optional API license";
      "--servers", Arg.String (fun s ->
          match String.split_on_char ',' s with
          | [ url ] -> servers := Some [Makers.mk_server url]
          | [ url; descr ] -> servers := Some [Makers.mk_server ~descr url]
          | _ -> ()), "Optional API servers";
      "--pretty", Arg.Set pretty, "Output pretty json";
      "--yaml", Arg.Set yaml, "Output in yaml format";
    ] in
  let usage_msg = "Create a OpenAPI json file with the services of the API" in
  Stdlib.Arg.parse speclist (fun _ -> ()) usage_msg;
  write ?descr:!descr ?version:!version ~title:!title ?terms:!terms
    ?license:!license ?servers:!servers ?contact:!contact ~yaml:!yaml ~pretty:!pretty
    ~docs ~sections !output_file
