
module Resto = Resto1

open StringCompat

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

end

open TYPES

type request = TYPES.request
type params = TYPES.request
type ip_info = TYPES.ip_info
type base_url = TYPES.base_url
type arg_value = TYPES.arg_value
type url = TYPES.url


type param = {
    param_value : string;
    param_name : string option;
    param_descr : string option;
    param_type : param_type;
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
    doc_path : path;
    doc_params : param list;
    mutable doc_registered : bool;
    mutable doc_sample : (base_url -> url);
  }

type section = {
    section_name : string;
    mutable section_docs : service_doc list;
  }

(* All our services use 'params' as 'prefix of the service *)
type ('params, 'params2, 'input, 'output) service
  = {
    s : (request, 'params, 'input, 'output) Resto.service;
    s_internal : (unit, 'params2, unit, 'output) Resto.service;
    params : param list;
    doc : service_doc;
    enc_input : 'input Json_encoding.encoding;
    enc_output : 'output Json_encoding.encoding;
  }

type 'output service0 = (request, unit, unit, 'output) service
type ('arg,'output) service1 =
  (request * 'arg, unit * 'arg, unit, 'output) service

type ('input,'output) post_service0 =
  (request, unit, 'input, 'output) service
type ('arg,'input,'output) post_service1 =
  (request * 'arg, unit * 'arg, 'input, 'output) service

let arg_string ?descr name : string Resto.Arg.arg =
  Resto.Arg.make
    ~name
    ~destruct:( fun a -> Ok a : string -> (string, string) Result.result )
    ~construct:( fun s -> s : string -> string )
    ?descr
    ()

let arg_int ?descr name : int Resto.Arg.arg =
  Resto.Arg.make
    ~name
    ~destruct:( fun a -> Ok (int_of_string a))
    ~construct:( fun s -> string_of_int s )
    ?descr
    ()

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
            params =
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

let forge1 url s params args =  forge url s ( (), params ) args
let forge0 url s args = forge url s () args

module Param = struct
  let string ?name ?descr param_value =
    { param_value; param_name = name; param_descr = descr;
      param_type = PARAM_STRING }
  let int ?name ?descr param_value =
    { param_value; param_name = name; param_descr = descr;
      param_type = PARAM_INT }
  let bool ?name ?descr param_value =
    { param_value; param_name = name; param_descr = descr;
      param_type = PARAM_BOOL }
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
    (sample1,sample2)
end

let rec string_of_path p =
  match p with
  | ROOT -> ""
  | CONCAT (p, s) ->
     Printf.sprintf "%s/%s" (string_of_path p) s
  | ENDARG (p, arg) ->
     Printf.sprintf "%s/<%s>" (string_of_path p) arg.Resto.Arg.name

let post_service ?(section=default_section)
                 ?name
                 ~input
                 ~output
                 ?(params = []) (doc_path,path1,path2,sample) =
  let doc_id = !nservices in
  incr nservices;
  let doc = {
      doc_path;
      doc_params = params;
      doc_registered = false;
      doc_name = name;
      doc_sample = (fun _ -> assert false);
      doc_id;
    } in
  section.section_docs <- doc :: section.section_docs;
  services := doc :: !services;
  let service = {
      s = Resto.service ~input ~output path1;
      s_internal = Resto.service ~input:Json_encoding.empty ~output path2;
      params;
      doc;
      enc_input = input;
      enc_output = output;
    } in
  begin
    let make_sample url = forge url service sample [] in
    doc.doc_sample <- make_sample
  end;
  service

let service ?section ?name ~output ?params descr =
  post_service ?section ?name
               ~input:Json_encoding.empty
               ~output ?params descr

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
  let map = List.fold_left (fun map (s1,s2) ->
                StringMap.add s1 s2 map) StringMap.empty list in
  let b = Buffer.create 10000 in
  Printf.bprintf b "# %s\n" title;
  List.iter (buf_service ~map ?base_url b) (List.rev services);
  Buffer.contents b

let register service =
  service.doc.doc_registered <- true;
  service.s

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

let services_map f = List.map f @@ List.rev !services

let service_to_readable s =
  let unopt = function
    | None -> "N/A"
    | Some x -> x in
  ( s.doc_id, unopt s.doc_name, string_of_path s.doc_path,
    List.map (fun p -> (p.param_value, unopt p.param_name, unopt p.param_descr,
                        match p.param_type with
                        | PARAM_INT -> "int"
                        | PARAM_STRING -> "string"
                        | PARAM_BOOL -> "bool")) s.doc_params)

(* let path s = string_of_path s.doc.doc_path *)
let services () =
  Array.map (fun doc -> string_of_path doc.doc_path)
            (Array.of_list (List.rev !services))

let service_input s = s.enc_input
let service_output s = s.enc_output
