
module Resto = Resto1

open StringCompat
module IntMap = Map.Make(struct type t = int let compare = compare end)

exception ResultNotfound

module TYPES = struct

  type ip_info = {
    ip_ip : string;
    mutable ip_last : float;
    mutable ip_nb : int;
    ip_country : string * string;
  }

  type http_version = HTTP_1_0 | HTTP_1_1

  type request_body =
    | BodyString of (* content-type *) string option * (* content *) string

  type request = {
    req_version : http_version;
    req_headers : string list StringMap.t;
    mutable req_params : string list StringMap.t;
    mutable req_body : request_body;
    (* Modify this field if you want to send back specific headers. *)
    mutable rep_headers : (string * string) list;
  }

  type arg_kind = REQUIRED of string | OPTIONAL of string

  type param_type = PARAM_INT | PARAM_STRING | PARAM_BOOL

  type base_url = BASE of string (* with a ending slash *)
  type url = URL of string

  type arg_value =
    | I of int
    | S of string
    | LS of string list

  type security_scheme =
    | Basic of { ref_name : string }
    | Bearer of { ref_name : string ; format : string option }
    | ApiKey of { ref_name : string; in_: [`Header|`Cookie|`Query]; name : string }

end

open TYPES

type request = TYPES.request
type params = TYPES.request
type ip_info = TYPES.ip_info
type base_url = TYPES.base_url
type arg_value = TYPES.arg_value
type url = TYPES.url
type security_scheme = TYPES.security_scheme


type param = {
    param_value : string;
    param_name : string option;
    param_descr : string option;
    param_type : param_type;
    param_required : bool;
    param_examples : string list;
  }

type path =
  | ROOT
  | CONCAT of path * string
  | ENDARG of path * Resto.Arg.descr

type ('a, 'b) p =
  path *
  (request, 'a) Resto.Path.path *
  (unit, 'b) Resto.Path.path *
  'b

type service_doc = {
    doc_id : int; (* uniq service identifier *)
    doc_name : string option;
    doc_descr : string option;
    doc_path : path;
    doc_params : param list;
    mutable doc_registered : bool;
    mutable doc_sample : (base_url -> url);
    doc_section : section;
    doc_input : Json_schema.schema Lazy.t;
    doc_output : Json_schema.schema Lazy.t;
    doc_error_outputs : (int * Json_schema.schema Lazy.t) list;
    doc_meth : string;
    doc_security : security_scheme list;
  }

and section = {
    section_name : string;
    mutable section_docs : service_doc list;
  }

type _ err_case =
    ErrCase : {
      code : int;
      name : string;
      encoding : 'a Json_encoding.encoding;
      select : 'b -> 'a option;
      deselect: 'a -> 'b;
    } -> 'b err_case

(* All our services use 'params' as 'prefix of the service *)
type ('params, 'params2, 'input, 'output, 'error) service
  = {
    s : (request, 'params, 'input, ('output, 'error) result) Resto.service;
    s_OPTIONS : (request, 'params, unit, ('output, 'error) result) Resto.service;
    s_internal : (unit, 'params2, unit, ('output, 'error) result) Resto.service;
    params : param list;
    doc : service_doc;
    enc_input : 'input Json_encoding.encoding;
    enc_output : 'output Json_encoding.encoding;
    enc_error : 'error err_case list;
  }

type ('output, 'error) service0 = (request, unit, unit, 'output, 'error) service
type ('arg, 'output, 'error) service1 =
  (request * 'arg, unit * 'arg, unit, 'output, 'error) service

type ('input, 'output, 'error) post_service0 =
  (request, unit, 'input, 'output, 'error) service
type ('arg,'input,'output, 'error) post_service1 =
  (request * 'arg, unit * 'arg, 'input, 'output, 'error) service

let arg_string ?descr name example: string Resto.Arg.arg * string =
  Resto.Arg.make
    ~name
    ~destruct:( fun a -> Ok a : string -> (string, string) Result.result )
    ~construct:( fun s -> s : string -> string )
    ?descr
    ~example
    (),
  example

let arg_int ?descr name example: int Resto.Arg.arg * int =
  Resto.Arg.make
    ~name
    ~destruct:( fun a -> Ok (int_of_string a))
    ~construct:( fun s -> string_of_int s )
    ?descr
    ~example
    (),
  example

let add_params req params =
  req.req_params <-
    List.fold_left (fun map (arg, list) ->
        try
          let old_list = StringMap.find arg map in
          StringMap.add arg (old_list @ list) map
        with Not_found ->
          StringMap.add arg list map
      ) req.req_params params

let request ?(version=HTTP_1_0)
            ?(headers=StringMap.empty)
            ?(body=BodyString(None,""))
            (params : (string * string list) list) =
  let req =
    {
      req_version = version;
      req_params = StringMap.empty;
      req_headers = headers;
      req_body = body;
      rep_headers = [];
    }
  in
  add_params req params;
  req

let services = ref []
let nservices = ref 0
let default_section = { section_name = "Misc"; section_docs = [] }
let sections = ref [default_section]
let warnings = ref []
let warning s = warnings := s :: !warnings
let warnings f =
  List.iter f (List.rev !warnings);
  warnings := []

let add_end (BASE url) args =
  let urllen = String.length url in
  let url =
    Printf.sprintf "%s%s%s" url
                   (if urllen = 0 || url.[urllen - 1] = '/' then "" else "/")
                   args
  in
  URL url

let encode_args s (URL parts) args =
  let args =
    List.map (fun (arg, v) ->
        if not (List.exists (fun p ->
                    p.param_value = arg.param_value) s.params)
        then
          Printf.kprintf warning "%s: unknown argument %S"
                         parts arg.param_value;
        match v with
        | I n -> (arg.param_value, [string_of_int n])
        | S s ->  (arg.param_value, [s])
        | LS s ->  (arg.param_value, s)
      ) args in
  EzUrl.encode_args args

let forge url s params args =
  let parts, _json = Resto.forge_request s.s_internal params () in
  let parts = String.concat "/" parts in
  let args =
    match args with
    | [] -> ""
    | args ->
       Printf.sprintf "?%s" (encode_args s (URL parts) args)
  in
  add_end url (parts ^ args)

let forge1 url s params args =  forge url s ((), params) args
let forge0 url s args = forge url s () args

module Param = struct
  let param param_type ?name ?descr ?(required=false) ?(examples=[]) param_value =
    { param_value; param_name = name; param_descr = descr;
      param_type; param_required = required; param_examples = examples}
  let string = param PARAM_STRING
  let int = param  PARAM_INT
  let bool = param PARAM_BOOL
end

let find_params p req =
  try
    Some (StringMap.find p.param_value req.req_params)
  with Not_found -> None

let find_param p req =
  match find_params p req with
  | None -> None
  | Some values -> Some (String.concat "," values)

module Path = struct

  let root = (ROOT, Resto.Path.root, Resto.Path.root,())
  let (//) (s1,p1,p2,arg) s =
    (CONCAT (s1,s)),
    (Resto.Path.(/) p1 s),
    (Resto.Path.(/) p2 s),
    arg
  let (/:) (s1,p1,p2,sample1) (arg, sample2) =
    (ENDARG (s1, Resto.Arg.descr arg)),
    (Resto.Path.(/:) p1 arg),
    (Resto.Path.(/:) p2 arg),
    (sample1, sample2)
end

let rec string_of_path p =
  match p with
  | ROOT -> ""
  | CONCAT (p, s) ->
     Printf.sprintf "%s/%s" (string_of_path p) s
  | ENDARG (p, arg) ->
     Printf.sprintf "%s/<%s>" (string_of_path p) arg.Resto.Arg.name

let rec update_service_list services doc = match services with
  | [] -> [ doc ]
  | h :: t when h.doc_path = doc.doc_path -> doc :: t
  | h :: t -> h :: (update_service_list t doc)

let definitions_path = "/components/schemas/"
let responses_path = "/components/responses/"
(* let security_path = "/components/securitySchemes/" *)

let merge_errs_same_code errors =
  let code_map =
    List.fold_left (fun acc (ErrCase { code; _ } as c) ->
        let encs = match IntMap.find_opt code acc with
          | Some l -> l
          | None -> [] in
        IntMap.add code (c :: encs) acc
      ) IntMap.empty errors in
  IntMap.map (fun l ->
      let encoding = match l with
        | [ErrCase { encoding; select; deselect; _ }] ->
          Json_encoding.conv
            (fun x -> match select x with
               | None -> assert false
               | Some x -> x)
            deselect
            encoding
        | _ ->
          let err_cases =
            List.map (function ErrCase { encoding;  select;  deselect; _} ->
                Json_encoding.case encoding select deselect
              ) l in
          Json_encoding.union err_cases in
      lazy (Json_encoding.schema ~definitions_path:responses_path encoding)
    ) code_map
  |> IntMap.bindings

(* This (ploymorphic) case is used when there is no
   declared error for the service but one is produced *)
let catch_all_error_case () = ErrCase {
    code = 500;
    name = "AnyError";
    encoding = (
      let open Json_encoding in
      conv
        (fun x ->
           let s =
             Marshal.to_string x [Marshal.No_sharing]
             |> Digest.string |> Digest.to_hex in
           Format.eprintf "No corresponding error case (MD5 %s)@." s;
           ((), s)
        )
        (fun ((), _) ->
           failwith "Cannot parse from undeclared error")
        (obj2
           (req "error" (constant "Server Error"))
           (req "digest" string))
    );
    select = (fun x -> Some x);
    deselect = (fun x -> x);
  }

let post_service ?(section=default_section)
    ?name
    ?descr
    ?meth
    ~input
    ~output
    ?(error_outputs=[])
    ?(params = [])
    ?(security=[])
    (doc_path,path1,path2,sample) =
  let doc_id = !nservices in
  incr nservices;
  let doc = {
      doc_path;
      doc_params = params;
      doc_registered = false;
      doc_name = name;
      doc_descr = descr;
      doc_sample = (fun _ -> assert false);
      doc_id;
      doc_section = section;
      doc_input = lazy (Json_encoding.schema ~definitions_path input);
      doc_output = lazy (Json_encoding.schema ~definitions_path output);
      doc_error_outputs = merge_errs_same_code error_outputs;
      doc_meth = (match meth with None -> "post" | Some meth -> meth);
      doc_security = security;
    } in
  section.section_docs <- update_service_list section.section_docs doc;
  services := update_service_list !services doc;
  let resto_output =
    let err_cases =
      List.map (function ErrCase { encoding;  select;  deselect; _} ->
          Json_encoding.case encoding select deselect
        ) (error_outputs @ [catch_all_error_case ()]) in
    Json_encoding.(union [
        case output
          (function Ok r -> Some r | Error _ -> None)
          (fun r -> Ok r);
        case (Json_encoding.union err_cases)
          (function Error e -> Some e | Ok _ -> None)
          (fun e -> Error e)
      ]) in
  let service = {
      s = Resto.service ~input ~output:resto_output path1;
      s_OPTIONS = Resto.service ~input:Json_encoding.empty ~output:resto_output path1;
      s_internal = Resto.service ~input:Json_encoding.empty ~output:resto_output path2;
      params;
      doc;
      enc_input = input;
      enc_output = output;
      enc_error = error_outputs;
    } in
  begin
    let make_sample url = forge url service sample [] in
    doc.doc_sample <- make_sample
  end;
  service

let service ?section ?name ?descr ?meth ~output ?error_outputs ?params ?security arg =
  let meth = match meth with None -> "get" | Some s -> s in
  post_service ?section ?name ?descr
    ~input:Json_encoding.empty
    ~output ?error_outputs ~meth ?params ?security arg

let section section_name =
  let s = { section_name; section_docs = [] } in
  sections := s :: !sections;
  s

let printf url fmt =
  Printf.kprintf (add_end url) fmt

let rec buf_path b p =
  match p with
  | ROOT -> ()
  | CONCAT (p, s) ->
     buf_path b p;
     Buffer.add_char b '/';
     Buffer.add_string b s
  | ENDARG (p, arg) ->
     buf_path b p;
     Buffer.add_char b '/';
     Printf.bprintf b "<%s>" arg.Resto.Arg.name

let buf_service ~map ?base_url b doc =
  let has_arguments = doc.doc_params <> [] in
  Buffer.add_string b "## `";
  buf_path b doc.doc_path;
  if has_arguments then
    Printf.bprintf b "?<arguments>";
  Buffer.add_string b "`\n";
  Buffer.add_string b "\n";

  if has_arguments then begin
      Printf.bprintf b "### Arguments\n";
      List.iter (fun p ->
          Printf.bprintf b "* `%s` = `%s` %s :" p.param_value
                         (match p.param_type with
                          | PARAM_INT -> "<int>"
                          | PARAM_STRING -> "<string>"
                          | PARAM_BOOL -> "<bool>"
                         )
                         (match p.param_name with
                          | None -> p.param_value
                          | Some name -> name);
          begin
            match p.param_descr with
            | None -> ()
            | Some descr -> Buffer.add_string b descr
          end;
          Printf.bprintf b "\n";
        ) doc.doc_params;
    end;

  begin
    let content = try
      match doc.doc_name with
        None -> raise Not_found
      | Some info -> StringMap.find info map
    with Not_found ->
      "This service is undocumented\n"
    in
    Buffer.add_string b "\n";
    Buffer.add_string b content;
    Buffer.add_string b "\n";
  end;

  begin
    match base_url with
    | None -> ()
    | Some base_url ->
       let URL url = doc.doc_sample base_url in
       Printf.bprintf b
                      "\n### Sample request:\n\
                       ```bash\n\
                       curl %s\n\
                       ```\n"
                      url
  end;
  begin match doc.doc_name with
    None -> ()
  | Some name ->
     try
       let content = StringMap.find (Printf.sprintf "info-%s.md" name) map in
       Buffer.add_string b "\n";
       Buffer.add_string b content;
       Buffer.add_string b "\n";
     with Not_found -> ()
  end;
  Printf.bprintf b "\n\n<hr/>\n\n";
  ()

let md_of_services ?section ?base_url list =
  let title, services = match section with
    | None -> "All", !services
    | Some s -> s.section_name, List.rev s.section_docs
  in
  let map = List.fold_left (fun map (s1, s2) ->
                StringMap.add s1 s2 map) StringMap.empty list in
  let b = Buffer.create 10000 in
  Printf.bprintf b "# %s\n" title;
  List.iter (buf_service ~map ?base_url b) (List.rev services);
  Buffer.contents b

let register service =
  service.doc.doc_registered <- true;
  service.s, service.s_OPTIONS

let all_services_registered () =
  let b = Buffer.create 1000 in
  List.iter (fun doc ->
      if not doc.doc_registered then begin
          buf_path b doc.doc_path;
          Printf.bprintf b " is not registered\n";
        end
    ) !services;
  let s = Buffer.contents b in
  if s <> "" then begin
      Printf.eprintf "Warning: unregistered services:\n%s\n%!" s;
      false
    end else true

let section_name s = s.section_name

let nservices () = !nservices
let id s = s.doc.doc_id

let services_doc_map f = List.rev_map f !services

let string_of_param_type = function
  | PARAM_INT -> "int"
  | PARAM_STRING -> "string"
  | PARAM_BOOL -> "bool"

let service_to_readable s =
  let unopt = function
    | None -> "N/A"
    | Some x -> x in
  ( s.doc_id, unopt s.doc_name, string_of_path s.doc_path, s.doc_registered,
    List.map (fun p ->
        (p.param_value, unopt p.param_name, unopt p.param_descr,
         string_of_param_type p.param_type))
      s.doc_params)

(* let path s = string_of_path s.doc.doc_path *)
let services () =
  Array.map (fun doc -> string_of_path doc.doc_path)
            (Array.of_list (List.rev !services))

let service_input s = s.enc_input
let service_output s = s.enc_output
let service_errors s ~code =
  match
    List.find_all (function ErrCase { code = c; _ } -> c = code) s.enc_error
  with
  | [] -> None
  | [ ErrCase { encoding = enc; select; deselect; _ } ] ->
    Some (Json_encoding.conv
            (fun x -> match select x with
               | None -> assert false
               | Some x -> x)
            deselect
            enc)
  | l ->
    let cases =
      List.map (function ErrCase { encoding = enc; select; deselect; _ } ->
          Json_encoding.case enc select deselect
        ) l in
    Some (Json_encoding.union cases)

let get_path sd =
  let rec buf_path b p =
    match p with
    | ROOT -> ()
    | CONCAT (p, s) ->
       buf_path b p;
       Buffer.add_char b '/';
       Buffer.add_string b s
    | ENDARG (p, arg) ->
       buf_path b p;
       Buffer.add_char b '/';
       Printf.bprintf b "{%s}" arg.Resto.Arg.name
  in
  let b = Buffer.create 100 in
  buf_path b sd.doc_path;
  Buffer.contents b

module JsonSchema = Json_schema.Make(Json_repr.Ezjsonm)

let schema_security_scheme = function
  | Basic { ref_name } ->
    ref_name, `O [
      "type", `String "http";
      "scheme", `String "basic";
    ]
  | Bearer { ref_name; format } ->
    ref_name, `O ([
        "type", `String "http";
        "scheme", `String "bearer"
      ] @ match format with
      | None -> []
      | Some format -> ["bearerFormat", `String format]
      )
  | ApiKey { ref_name; in_; name } ->
    let in_ = match in_ with
      | `Header -> "header"
      | `Cookie -> "cookie"
      | `Query -> "query" in
    ref_name , `O [
      "type", `String "apiKey";
      "in", `String in_;
      "name", `String name;
    ]

let security_ref_name = function
  | Basic { ref_name; _ } | Bearer { ref_name; _ } | ApiKey { ref_name; _ } ->
    ref_name

let paths_of_sections ?(docs=[]) sections =
  let docs = List.map (fun (name, summary, descr) -> name, (summary, descr)) docs in
  let list = ref [] in
  let map = ref StringMap.empty in
  List.iter (fun s ->
      List.iter (fun sv ->
          let id = string_of_int sv.doc_id in
          if not (StringMap.mem id !map) then begin
              list := sv :: !list;
              map := StringMap.add id sv !map
            end
        ) (List.rev s.section_docs)
    ) sections;
  let services = !list in

  let definitions_schema =
    ref (Json_schema.create (Json_schema.element Json_schema.Any))
  in
  let io_schemas =
    List.map (fun sd ->
        let output_schema = Lazy.force sd.doc_output in
        let input_schema = Lazy.force sd.doc_input in
        let output_schema, updated =
          Json_schema.merge_definitions
            (output_schema, !definitions_schema) in
        let input_schema, updated =
          Json_schema.merge_definitions
            (input_schema, updated) in
        let error_codes_schemas, updated =
          List.fold_left (fun (acc, updated) (code, error_schema) ->
              let error_schema, updated =
                Json_schema.merge_definitions
                  (Lazy.force error_schema, updated) in
              (code, Json_schema.root error_schema) :: acc, updated
            ) ([], updated) sd.doc_error_outputs in
        definitions_schema := updated;
        Json_schema.root input_schema,
        Json_schema.root output_schema,
        List.rev error_codes_schemas
      ) services
  in
  (* let input_schemas, output_schemas = List.split input_output_schemas in *)
  let io_schemas, definitions =
    let sch = Json_schema.update
        (Json_schema.element
           (Json_schema.Combine
              (Json_schema.All_of,
               List.map (fun (input_schema, output_schema, error_codes_schemas) ->
                   Json_schema.element @@
                   Json_schema.Object {
                     Json_schema.object_specs with
                     properties = [
                       "input", input_schema, true, None;
                       "output", output_schema, true, None;
                       "errors",
                       Json_schema.element @@
                       Json_schema.Object {
                         Json_schema.object_specs with
                         properties =  List.map (fun (code, err_sch) ->
                             string_of_int code, err_sch, false, None
                           ) error_codes_schemas
                       }, false, None;
                     ]
                   }
                 ) io_schemas
              )))
        !definitions_schema
    in
    match JsonSchema.to_json sch with
    | `O (
        ("$schema", _) ::
        ("allOf", `A services_sch) ::
        defs
      ) ->
      let definitions = match defs with
        | [] -> []
        | ("components", `O definitions) :: _ -> definitions
        | _ -> assert false in
      let io_json_schemas = List.map (function
          | `O ( ("type", `String "object") ::
                 ("properties", `O [ "input", input;
                                     "output", output;
                                     "errors", `O ( ("type", `String "object") ::
                                                    ("properties", `O errs) :: _)
                                   ]) ::
                 _ ) ->
            input, output, errs
          | j ->
            Printf.eprintf "%s\n%!" (Ezjsonm.value_to_string ~minify:false j);
            assert false
        ) services_sch in
      io_json_schemas, definitions
    | j ->
      Printf.eprintf "%s\n%!" (Ezjsonm.value_to_string ~minify:false j);
      assert false
  in

  let paths = List.mapi (fun i sd ->
      let doc_infos =
        match sd.doc_name with
        | None -> None
        | Some name -> List.assoc_opt name docs in
      let summary, description = match doc_infos with
        | None ->
          let path = get_path sd in
          Format.eprintf
            "Warning: no description provided for service %s@." path;
          path, ""
        | Some (summary, description) -> summary, description in
      let parameters =
        List.map (fun p ->
            let param_descr = match p.param_descr with
              | None -> ""
              | Some descr -> descr in
            let param_type = match p.param_type with
              | PARAM_STRING -> "string"
              | PARAM_INT -> "integer"
              | PARAM_BOOL -> "boolean"
            in
            `O ([
                "name", `String p.param_value;
                "in", `String "query";
                "description", `String param_descr;
                "required", `Bool p.param_required;
                "schema", `O [
                  "type", `String param_type
                ]
              ] @ match p.param_examples with
              | [] -> []
              | e :: _ -> ["example", `String e]
              )
          ) sd.doc_params
      in
      let parameters =
        let rec iter path parameters =
          match path with
          | ROOT -> parameters
          | CONCAT (p,_s) -> iter p parameters
          | ENDARG (p,arg) ->
            let param_descr = match arg.Resto.Arg.descr with
              | None -> ""
              | Some descr -> descr in
            let example = match arg.Resto.Arg.example with
              | None -> []
              | Some example -> [ "example", `String example ] in
            let param =
              `O ([
                "name", `String arg.Resto.Arg.name;
                "in", `String "path";
                "description", `String param_descr;
                "required", `Bool true;
                "schema", `O [
                  "type", `String "string"];
              ] @ example)
            in
            iter p (param :: parameters)
        in
        iter sd.doc_path parameters
      in
      let in_schema, out_schema, err_schemas = List.nth io_schemas i in
      let request_schema = match in_schema with
        | `O ["type", `String "object";
              "properties", `O [];
              "additionalProperties", `Bool false ] -> []
        | _ -> [
            "requestBody", `O [
              "content", `O [
                "application/json", `O [
                  "schema", in_schema
                ]
              ]
            ]]
      in
      let success_response =
        "200",
        `O [
          "description", `String "Success";
          "content", `O [
            "application/json", `O [
              "schema", out_schema
            ]
          ]
        ] in
      let error_responses =
        err_schemas
        |> List.filter (fun (code, _) -> int_of_string code < 500)
        |> List.map (fun (code, (err_schema : Ezjsonm.value)) ->
            let descr = (* Hackish *)
              try match err_schema with
                | `O ["$ref", `String refname] ->
                  (match String.split_on_char '/' refname with
                   | [_; _; _; s] -> s
                   | _ -> raise Not_found
                  )
                | `O (("type", `String "object") :: ("properties", `O l) :: _) -> (
                    List.find (function
                        | ("kind" | "error" | "description"),
                          `O ( ("type", `String "string") :: ("enum", `A [_]) :: _) -> true
                        | _ -> false
                      ) l
                    |> function
                    | (_, `O (_ :: (_, `A [`String s]) :: _)) -> s
                    | _ -> assert false
                  )
                | _ -> raise Not_found
              with Not_found -> "Error " ^ code in
            code,
            `O [
              "description", `String descr;
              "content", `O [
                "application/json", `O [
                  "schema", err_schema
                ]
              ]
            ]
          ) in
      let security = `A (List.map (fun secscheme ->
          `O [security_ref_name secscheme, `A []]
        ) sd.doc_security)
      in
      get_path sd,
      `O [
        sd.doc_meth, `O ([
          "tags", `A [ `String sd.doc_section.section_name ];
          "summary", `String summary;
          "description", `String description;
          "operationId", `String (string_of_int sd.doc_id);
          "parameters", `A parameters;
          "responses", `O (success_response :: error_responses);
          "security", security;
        ] @ request_schema)
      ]
    ) services
  in
  let security_schemes = List.fold_left (fun acc sd ->
      let security = List.map schema_security_scheme sd.doc_security in
      List.rev_append security acc
    ) [] (List.rev services)
  in
  let definitions = definitions @ [ "securitySchemes", `O security_schemes ] in
  paths, definitions


module Legacy = struct

  type uninhabited = |

  let unreachable = function (_ : uninhabited) -> .

  type nonrec ('params, 'params2, 'input, 'output) service =
    ('params, 'params2, 'input, 'output, uninhabited) service

  type 'output service0 =
    (request, unit, unit, 'output) service
  type ('arg, 'output) service1 =
    (request * 'arg, unit * 'arg, unit, 'output) service

  type ('input, 'output) post_service0 =
    (request, unit, 'input, 'output) service
  type ('arg,'input,'output) post_service1 =
    (request * 'arg, unit * 'arg, 'input, 'output) service

  let post_service ?section ?name ?descr ?meth ~input ~output ?params arg =
    post_service ?section ?name ?descr ?meth ~input ~output ?params arg

  let service ?section ?name ?descr ?meth ~output ?params arg =
    service ?section ?name ?descr ?meth ~output ?params arg

end
