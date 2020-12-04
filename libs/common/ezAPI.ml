module Resto = Resto1
open Resto

open StringCompat
module IntMap = Map.Make(struct type t = int let compare = compare end)

exception ResultNotfound

type uninhabited = |

let unreachable = function (_ : uninhabited) -> .

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

end

open TYPES

type request = TYPES.request
type ip_info = TYPES.ip_info
type base_url = TYPES.base_url
type arg_value = TYPES.arg_value
type url = TYPES.url

type param = {
    param_value : string;
    param_name : string option;
    param_descr : string option;
    param_type : param_type;
    param_required : bool;
    param_examples : string list;
  }

type no_security = [ `Nosecurity of uninhabited ]
type 'a apikey_security = {
  ref_name : string;
  name : 'a
}
type bearer_security_desc = { bearer_name : string ; format : string option }
type basic_security_desc = { basic_name : string }
type bearer_security = [ `Bearer of bearer_security_desc ]
type basic_security = [ `Basic of basic_security_desc ]
type header_security = [ `Header of string apikey_security ]
type cookie_security = [ `Cookie of string apikey_security ]
type query_security = [ `Query of param apikey_security ]
type security_scheme =
  [ no_security
  | basic_security
  | bearer_security
  | header_security
  | cookie_security
  | query_security
  ]

type path =
  | ROOT
  | CONCAT of path * string
  | ENDARG of path * Arg.descr

type ('a, 'b) p =
  path *
  (request, 'a) Path.path *
  (unit, 'b) Path.path *
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
    doc_input_example : Json_repr.any option;
    doc_output_example : Json_repr.any option;
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
type nonrec ('params, 'params2, 'input, 'output, 'error, 'security) service
  = {
    s : (request, 'params, 'input, ('output, 'error) result) service;
    s_OPTIONS : (request, 'params, unit, ('output, 'error) result) service;
    s_internal : (unit, 'params2, unit, ('output, 'error) result) service;
    params : param list;
    doc : service_doc;
    enc_input : 'input Json_encoding.encoding;
    enc_output : 'output Json_encoding.encoding;
    enc_error : 'error err_case list;
    s_security : ([< security_scheme ] as 'security) list;
    s_meth : method_type;
  }

type ('output, 'error, 'security) service0 = (request, unit, unit, 'output, 'error, 'security) service
type ('arg, 'output, 'error, 'security) service1 =
  (request * 'arg, unit * 'arg, unit, 'output, 'error, 'security) service
type ('arg1, 'arg2, 'output, 'error, 'security) service2 =
  ((request * 'arg1) * 'arg2, (unit * 'arg1) * 'arg2, unit, 'output, 'error, 'security) service

type ('input, 'output, 'error, 'security) post_service0 =
  (request, unit, 'input, 'output, 'error, 'security) service
type ('arg,'input,'output, 'error, 'security) post_service1 =
  (request * 'arg, unit * 'arg, 'input, 'output, 'error, 'security) service
type ('arg1, 'arg2,'input,'output, 'error, 'security) post_service2 =
  ((request * 'arg1) * 'arg2, (unit * 'arg1) * 'arg2, 'input, 'output, 'error, 'security) service

let arg_string ?descr name example: string Arg.arg * string =
  Arg.make
    ~name
    ~destruct:( fun a -> Ok a : string -> (string, string) Result.result )
    ~construct:( fun s -> s : string -> string )
    ?descr
    ~example
    (),
  example

let arg_int ?descr name example: int Arg.arg * int =
  Arg.make
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
  let parts, _json = forge_request s.s_internal params () in
  let parts = String.concat "/" parts in
  let args =
    match args with
    | [] -> ""
    | args ->
       Printf.sprintf "?%s" (encode_args s (URL parts) args)
  in
  add_end url (parts ^ args)

let forge0 url s args = forge url s () args
let forge1 url s params args =  forge url s ((), params) args
let forge2 url s params1 params2 args =  forge url s (((), params1), params2) args

module Param = struct
  let param param_type ?name ?descr ?(required=false) ?(examples=[]) param_value =
    { param_value; param_name = name; param_descr = descr;
      param_type; param_required = required; param_examples = examples}
  let string = param PARAM_STRING
  let int = param  PARAM_INT
  let bool = param PARAM_BOOL
end

let find_params p req = StringMap.find_opt p.param_value req.req_params

let find_param p req = match find_params p req with
  | None -> None
  | Some values -> Some (String.concat "," values)

module Path = struct

  let root = (ROOT, Path.root, Path.root,())
  let (//) (s1,p1,p2,arg) s =
    (CONCAT (s1,s)),
    (Path.(/) p1 s),
    (Path.(/) p2 s),
    arg
  let (/:) (s1,p1,p2,sample1) (arg, sample2) =
    (ENDARG (s1, Arg.descr arg)),
    (Path.(/:) p1 arg),
    (Path.(/:) p2 arg),
    (sample1, sample2)
end

let rec string_of_path = function
  | ROOT -> ""
  | CONCAT (p, s) -> Printf.sprintf "%s/%s" (string_of_path p) s
  | ENDARG (p, arg) -> Printf.sprintf "%s/{%s}" (string_of_path p) arg.Arg.name

let rec update_service_list services doc = match services with
  | [] -> [ doc ]
  | h :: t when h.doc_path = doc.doc_path -> doc :: t
  | h :: t -> h :: (update_service_list t doc)

let definitions_path = "/components/schemas/"

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
      lazy (Json_encoding.schema ~definitions_path encoding)
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

let params_of_query_security (l : [< security_scheme ] list) =
  List.fold_left (fun acc -> function
      | `Query { name = param ; _ } -> param :: acc
      | `Nosecurity _ | `Basic _ | `Bearer _
      | `Header _ | `Cookie _  -> acc
    ) [] l

let str_of_method = function
  | GET -> "get"
  | HEAD -> "head"
  | POST -> "post"
  | PUT -> "put"
  | DELETE -> "delete"
  | CONNECT -> "connect"
  | OPTIONS -> "options"
  | TRACE -> "trace"
  | PATCH -> "patch"
  | OTHER s -> s

let post_service ?(section=default_section)
    ?name
    ?descr
    ?(meth=POST)
    ~input
    ~output
    ?(error_outputs=[])
    ?(params = [])
    ?(security=[])
    ?(register=true)
    ?input_example
    ?output_example
    (doc_path,path1,path2,sample) =
  let doc_id = !nservices in
  if register then incr nservices;
  let params = List.rev_append (params_of_query_security security) params in
  let doc_input_example = match input_example with
    | None -> None
    | Some ex -> Some (Json_repr.to_any @@ Json_encoding.construct input ex) in
  let doc_output_example = match output_example with
    | None -> None
    | Some ex -> Some (Json_repr.to_any @@ Json_encoding.construct output ex) in
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
      doc_meth = str_of_method meth;
      doc_security = (security :> security_scheme list);
      doc_input_example; doc_output_example
    } in
  if register then (
    section.section_docs <- update_service_list section.section_docs doc;
    services := update_service_list !services doc);
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
      s = service ~meth ~input ~output:resto_output path1;
      s_OPTIONS = service ~meth:OPTIONS ~input:Json_encoding.empty ~output:resto_output path1;
      s_internal = service ~input:Json_encoding.empty ~output:resto_output path2;
      params;
      doc;
      enc_input = input;
      enc_output = output;
      enc_error = error_outputs;
      s_security = security;
      s_meth = meth;
    } in
  let make_sample url = forge url service sample [] in
  doc.doc_sample <- make_sample;
  service

let service ?section ?name ?descr ?(meth=GET) ~output ?error_outputs ?params
    ?security ?register ?output_example arg =
  post_service ?section ?name ?descr
    ~input:Json_encoding.empty
    ~output ?error_outputs ~meth ?params ?security ?register ?output_example arg

let section section_name =
  let s = { section_name; section_docs = [] } in
  sections := s :: !sections;
  s

let register service =
  service.doc.doc_registered <- true;
  service.s, service.s_OPTIONS

let all_services_registered () =
  let s = List.fold_left (fun acc doc ->
      if not doc.doc_registered then
        Printf.sprintf "%s%s is not registered\n" acc (string_of_path doc.doc_path)
      else acc
    ) "" !services in
  if s <> "" then begin
    Printf.eprintf "Warning: unregistered services:\n%s\n%!" s;
    false
  end else true

let section_name s = s.section_name

let nservices () = !nservices
let id s = s.doc.doc_id

let services_doc_map f = List.rev_map f !services

let string_of_param_type = function
  | PARAM_INT -> "integer"
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

let service_security s = s.s_security
let service_meth s = s.s_meth

let security_ref_name = function
  | `Nosecurity u -> unreachable u
  | `Basic { basic_name = ref_name }
  | `Bearer { bearer_name = ref_name; format=_  }
  | `Cookie { ref_name; name=_ }
  | `Header { ref_name; name=_ }
  | `Query { ref_name; name=_ } -> ref_name

module Legacy = struct

  type nonrec ('params, 'params2, 'input, 'output) service =
    ('params, 'params2, 'input, 'output, uninhabited, no_security) service

  type 'output service0 =
    (request, unit, unit, 'output) service
  type ('arg, 'output) service1 =
    (request * 'arg, unit * 'arg, unit, 'output) service
  type ('arg1, 'arg2, 'output) service2 =
    ((request * 'arg1) * 'arg2, (unit * 'arg1) * 'arg2, unit, 'output) service

  type ('input, 'output) post_service0 =
    (request, unit, 'input, 'output) service
  type ('arg, 'input,'output) post_service1 =
    (request * 'arg, unit * 'arg, 'input, 'output) service
  type ('arg1, 'arg2, 'input, 'output) post_service2 =
    ((request * 'arg1) * 'arg2, (unit * 'arg1) * 'arg2, 'input, 'output) service

  let post_service ?section ?name ?descr ?meth ~input ~output ?params arg =
    post_service ?section ?name ?descr ?meth ~input ~output ?params arg

  let service ?section ?name ?descr ?meth ~output ?params arg =
    service ?section ?name ?descr ?meth ~output ?params arg

end
