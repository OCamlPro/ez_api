(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Ppxlib
open Ast_builder.Default

(** service *)

type options = {
  path : expression;
  input : expression;
  output : expression;
  errors : expression;
  params : expression;
  section : expression;
  name : expression;
  descr : expression;
  security : expression;
  register : expression;
  hide : expression;
  input_example : expression;
  output_example : expression;
  error_type : core_type;
  security_type : core_type;
  debug : bool;
  directory : string option;
  service : expression option;
  nargs : int;
}

let loc = !Ast_helper.default_loc
let global_errors = ref [%expr None]
let global_error_type = ref [%type: exn]
let global_security = ref [%expr None]
let global_security_type = ref [%type: EzAPI.no_security]
let global_headers = ref None
let global_base = ref false
let global_req_error = ref [%expr fun e -> Error (Failure e)]
let loc = ()

let remove_poly c = match c.ptyp_desc with
  | Ptyp_poly ([], c) -> c
  | _ -> c

let remove_expr_constraint e = match e.pexp_desc with
  | Pexp_constraint (e, _) -> e
  | _ -> e

let remove_pat_constraint p = match p.ppat_desc with
  | Ppat_constraint (p, _) -> p
  | _ -> p

let rec get_list_expr ?(acc=[]) e = match e.pexp_desc with
  | Pexp_construct ({txt=Lident "None"; _}, None) -> None
  | Pexp_construct ({txt=Lident "Some"; _}, Some e) -> get_list_expr ~acc e
  | Pexp_construct ({txt=Lident "[]"; _}, None) -> Some (List.rev acc)
  | Pexp_construct ({txt=Lident "::"; _}, Some {pexp_desc=Pexp_tuple [e1; e2]; _}) ->
    get_list_expr ~acc:(e1 :: acc) e2
  | _ -> None

module SMap = Map.Make(String)

let security_scheme_table = ref SMap.empty
let security_schemes_table = ref SMap.empty

let security_list name l =
  let l = List.filter_map (fun e -> match e.pexp_desc with
      | Pexp_ident {txt; _} ->
        let n = Longident.name txt in
        begin match SMap.find_opt n !security_scheme_table with
          | None -> None
          | Some name -> Some (n, name)
        end
      | _ -> None) l in
  security_schemes_table := SMap.add name l !security_schemes_table

let extract_list_type ~loc = function
  | None -> [%type: _]
  | Some t ->
    let t = remove_poly t in
    match t.ptyp_desc with
    | Ptyp_constr ({txt=(Lident "list" | Ldot (Lident "List", "t")); _}, [
        { ptyp_desc=Ptyp_constr ({txt=Ldot (Ldot (Lident "EzAPI", "Err"), "case"); _}, [ c ]); _} ]) -> c
    | Ptyp_constr ({txt=(Lident "list" | Ldot (Lident "List", "t")); _}, [ c ]) -> c
    | _ -> t

let set_global_errors ?typ e =
  let loc = e.pexp_loc in
  global_errors := [%expr Some [%e remove_expr_constraint e]];
  global_error_type := extract_list_type ~loc typ

let set_global_security ?typ e =
  let loc = e.pexp_loc in
  global_security := [%expr Some [%e remove_expr_constraint e]];
  global_security_type := extract_list_type ~loc typ

let set_global_base e =
  let loc = e.pexp_loc in
  global_base := true;
  let e = match e.pexp_desc with
    | Pexp_constant Pconst_string (s, _, _) -> [%expr ref (EzAPI.BASE [%e estring ~loc s])]
    | Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident "!"; _}; _}, [ Nolabel, e ]) -> e
    | _ -> [%expr ![%e e]] in
  [%stri let ezreq_base = [%e e]]

let set_global_headers e =
  global_headers := Some e

let set_global_req_error e =
  global_req_error := e

let set_globals l =
  List.fold_left (fun acc ({txt; _}, e) ->
      let name = Longident.name txt in
      match name with
      | "errors" -> set_global_errors e; acc
      | "security" -> set_global_security e; acc
      | "base" -> Some (set_global_base e)
      | "headers" -> set_global_headers e; acc
      | "req_error" -> set_global_req_error e; acc
      | _ -> acc) None l

let raw e =
  let loc = e.pexp_loc in
  [%expr EzAPI.Raw (List.filter_map EzAPI.Mime.parse [%e e])]

let default_options loc = {
  path = [%expr EzAPI.Path.root];
  input = [%expr EzAPI.Empty];
  output = [%expr EzAPI.Empty];
  errors = !global_errors; params = [%expr None];
  section = [%expr None]; name=[%expr None]; descr = [%expr None];
  security = !global_security; register=[%expr true]; input_example = [%expr None];
  hide = [%expr None]; output_example = [%expr None]; error_type = !global_error_type;
  security_type = !global_security_type;
  debug = false; directory = None; service = None; nargs=0;
}

let methods = [ "get"; "post"; "put"; "patch"; "delete" ]

let parse_arg ~loc s = match String.index_opt s ':' with
  | None -> evar ~loc (String.trim s)
  | Some i ->
    let name = String.(trim @@ sub s 0 (i-1)) in
    let typ = String.(trim @@ sub s (i+1) (length s - i - 1)) in
    match typ with
    | "int" | "float" | "int32" | "int64" | "string" ->
      eapply ~loc (evar ~loc ("EzAPI.Arg." ^ typ)) [ estring ~loc name ]
    | _ ->
      Location.raise_errorf ~loc "argument type not understood: %S" typ

let parse_path ~loc s =
  let l = String.split_on_char '/' s in
  let npath = List.length l in
  let acc, n, _ = List.fold_left (fun (acc, n, i) s ->
      if s = "" && i <> npath-1 then (acc, n, i+1)
      else if s = "" then [%expr EzAPI.Path.add_trailing [%e acc]], n, i+1
      else match String.get s 0 with
        | '{' ->
          let e = parse_arg ~loc String.(sub s 1 (length s - 2)) in
          [%expr EzAPI.Path.add_arg [%e acc] [%e e]], n+1, i+1
        | _ ->
          [%expr EzAPI.Path.add_suffix [%e acc] [%e estring ~loc s]], n, i+1
    ) ([%expr EzAPI.Path.root], 0, 0) l in
  acc, n

let string_literal = function
  | Ppxlib.Pconst_string (s, _, _) -> Some s
  | _ -> None

let get_name = function
  | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_record (l, _); _}, _); _} ] ->
    List.find_map (function
        | ({txt=Lident "name"; _}, {pexp_desc=Pexp_constant Pconst_string (s, _, _); _}) -> Some s
        | _ -> None) l
  | _ -> None

let get_options ~loc ?(options=default_options loc) ?name p =
  match p with
  | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_record (l, _); _}, _); _} ] ->
    let l = List.filter_map (function ({txt=Lident s; loc}, e) -> Some (s, loc, e) | _ -> None) l in
    List.fold_left (fun (name, acc) (s, loc, e) -> match s with
        | "path" | "p" -> begin match e.pexp_desc with
            | Pexp_constant cst ->
              begin match string_literal cst with
                | Some s ->
                  let path, nargs = parse_path ~loc:e.pexp_loc s in
                  name, { acc with path; nargs }
                | _ -> Format.eprintf "path should be a string literal"; name, acc
              end
            | _ -> Format.eprintf "path should be a literal"; name, acc
          end
        | "input" ->
          begin match e.pexp_desc with
            | Pexp_construct ({txt=Lident "::"; _}, _) -> name, { acc with input = raw e }
            | _ -> name, { acc with input = [%expr EzAPI.Json [%e e]] }
          end
        | "raw_input" -> name, { acc with input = raw e }
        | "output" ->
          begin match e.pexp_desc with
            | Pexp_construct ({txt=Lident "::"; _}, _) -> name, { acc with output = raw e }
            | _ -> name, { acc with output = [%expr EzAPI.Json [%e e]] }
          end
        | "raw_output" -> name, { acc with output = raw e }
        | "params" -> name, { acc with params = [%expr Some [%e e]] }
        | "errors" -> name, { acc with errors = [%expr Some [%e e]]; error_type = [%type: _] }
        | "section" -> name, { acc with section = [%expr Some [%e e]] }
        | "name" ->
          begin match e.pexp_desc with
            | Pexp_constant cst ->
              begin match name, string_literal cst with
                | None, Some s -> Some s,  { acc with name = [%expr Some [%e e]] }
                | Some n, _ -> Some n, { acc with name = [%expr Some [%e e]] }
                | _ -> Format.eprintf "name should be a string literal"; name, acc
              end
            | _ ->
              match name with
              | Some n -> Some n, { acc with name = [%expr Some [%e estring ~loc n]] }
              | _ -> name, acc
          end
        | "descr" -> name, { acc with descr = [%expr Some [%e e]] }
        | "security" ->
          let security_type = match acc.security_type.ptyp_desc with
            | Ptyp_constr ({txt; _}, []) when Longident.name txt = "EzAPI.no_security" -> [%type: _]
            | _ -> acc.security_type in
          name, { acc with security = [%expr Some [%e e]]; security_type }
        | "register" -> name, { acc with register = e }
        | "hide" -> name, { acc with hide = e }
        | "input_example" -> name, { acc with input_example = [%expr Some [%e e]] }
        | "output_example" -> name, { acc with output_example = [%expr Some [%e e]] }
        | "debug" -> name, { acc with debug = true }
        | "dir" -> begin match e.pexp_desc with
            | Pexp_constant cst ->
              begin match string_literal cst with
                | Some s -> name, { acc with directory = Some s }
                | _ -> Format.eprintf "directory should be a string literal"; name, acc
              end
            | _ -> Format.eprintf "directory should be a literal"; name, acc
          end
        | "service" ->
          name, { acc with service = Some e; error_type = [%type: _]; security_type = [%type: _] }
        | _ -> name, acc) (name, options) l
  | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_ident _; _} as e, _); _} ] ->
    name, { options with service = Some e; error_type = [%type: _]; security_type = [%type: _] }
  | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_constant Pconst_string (s, loc, _); _}, _); _} ] ->
    let path, nargs = parse_path ~loc s in
    name, { options with path; nargs }
  | PStr s ->
    Format.eprintf "attribute not understood %a@." Pprintast.structure s;
    name, options
  | _ ->
    Format.eprintf "attribute not understood@.";
    name, options

let service_expr ?name ?options ?(parse_options=true) ~meth ~loc p =
  let meth = pexp_variant ~loc (String.uppercase_ascii meth) None in
  let name, options =
    if not parse_options then name, Option.value ~default:(default_options loc) options
    else get_options ?name ?options ~loc p in
  match name with
  | None -> Location.raise_errorf ~loc "service doesn't have a name"
  | Some name ->
    let options_name = match options.name.pexp_desc with
      | Pexp_construct ({txt=Lident "None"; _}, _) -> [%expr Some [%e estring ~loc name]]
      | _ -> options.name in
    let expr = pexp_apply ~loc (evar ~loc "EzAPI.raw_service") [
        Optional "section", options.section;
        Optional "name", options_name;
        Optional "descr", options.descr;
        Optional "params", options.params;
        Labelled "meth", meth;
        Labelled "input", options.input;
        Labelled "output", options.output;
        Optional "errors", options.errors;
        Optional "security", options.security;
        Labelled "register", options.register;
        Optional "input_example", options.input_example;
        Optional "output_example", options.output_example;
        Nolabel, options.path ] in
    let pat = ppat_constraint ~loc (pvar ~loc name) @@
      [%type: (_, _, _, [%t options.error_type], [%t options.security_type]) EzAPI.service] in
    name, options, pat, expr

let service_value ?name ?options ?parse_options ~meth ~loc p =
  let name, options, pat, expr = service_expr ?name ?options ?parse_options ~meth ~loc p in
  let str = pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr ] in
  if options.debug then Format.printf "%a@." Pprintast.structure_item str;
  str, name, options

(** param *)

type param_options = {
  debug: bool;
  id: string;
  name: expression option;
  descr: expression option;
  required: bool;
  examples: expression;
  schema: expression option;
  kind: expression;
  destruct: expression option;
  construct: expression option;
}

let default_param ?(id="") ?kind ?schema ?destruct ?construct loc =
  let kind = Option.value ~default:[%expr EzAPI.Param.PARAM_STRING] kind in
  let name = if id = "" then None else Some (estring ~loc id) in {
    id; debug=false; name; descr=None; required=false; examples=[%expr []];
    schema; kind; destruct; construct;
  }

module SSet = Set.Make(String)

let param_set = ref SSet.empty

let param_options ~typ ~id ?kind ?schema ?destruct ?construct e = match e.pexp_desc with
  | Pexp_construct ({txt=Lident "()"; _}, None) -> default_param ~id ?kind ?schema ?destruct ?construct e.pexp_loc
  | Pexp_constant Pconst_string (id, _, _) -> default_param ~id ?kind ?schema ?destruct ?construct e.pexp_loc
  | Pexp_record (l, None) ->
    let param = default_param ~id ?kind ?destruct ?construct ?schema e.pexp_loc in
    List.fold_left (fun acc ({txt; _}, e) ->
        let loc = e.pexp_loc in
        match Longident.name txt, e.pexp_desc with
        | "id", Pexp_constant Pconst_string (id, _, _) -> { acc with id }
        | "debug", _ -> { acc with debug = true }
        | "name", _ -> { acc with name = Some e }
        | "descr", _ -> { acc with descr = Some e }
        | "examples", _ -> { acc with examples = e }
        | "schema", _ -> { acc with schema = Some e }
        | ("required" | "req"), _ -> { acc with required = true }
        | "kind", _ -> { acc with kind = e }
        | ("des" | "destruct"), _ -> { acc with destruct = Some e }
        | ("cons" | "construct"), _ -> { acc with construct = Some e }
        | "int", _ -> { acc with kind = [%expr EzAPI.Param.PARAM_INT] }
        | "bool", _ -> { acc with kind = [%expr EzAPI.Param.PARAM_BOOL] }
        | "enc", _ ->
          let enc = Ppx_deriving_encoding_lib.Encoding.core typ in
          let construct = Some [%expr fun x -> match Json_encoding.construct [%e enc] x with
              | `String s -> s
              | _ -> failwith [%e estring ~loc ("parameter " ^ acc.id ^ " should be constructed with a json string")]] in
          let destruct = Some [%expr fun s -> try Some (Json_encoding.destruct [%e enc] (`String s)) with _ -> None] in
          { acc with construct; destruct }
        | "assoc", _ ->
          begin match typ.ptyp_desc with
            | Ptyp_constr ({txt; _}, []) ->
              let txt = Longident.name txt in
              let construct = Some [%expr fun x -> List.assoc x @@ List.map (fun (a, b) -> b, a) [%e evar ~loc (txt ^ "_assoc")]] in
              let destruct = Some [%expr fun s -> try Some (List.assoc s [%e evar ~loc (txt ^ "_assoc")]) with _ -> None] in
              { acc with construct; destruct }
            | _ -> acc
          end
        | s, _ -> Format.eprintf "param option %S not understood" s; acc
      ) param l
  | _ -> Location.raise_errorf ~loc:e.pexp_loc "param options not understood"

let param_value p e =
  let loc = p.ppat_loc in
  let t = match p.ppat_desc, e.pexp_desc with
    | _, Pexp_constraint (_, t) | Ppat_constraint (_, t), _ -> remove_poly t
    | _ -> [%type: string] in
  let p, e = remove_pat_constraint p, remove_expr_constraint e in
  let name = match p.ppat_desc with
    | Ppat_var {txt; _} -> txt
    | _ -> Location.raise_errorf ~loc:p.ppat_loc "wrong param pattern" in
  param_set := SSet.add name !param_set;
  let enc_schema t =
    [%expr Json_encoding.schema [%e Ppx_deriving_encoding_lib.Encoding.core t]] in
  let schema, construct, destruct, k = match t.ptyp_desc with
    | Ptyp_constr ({txt; loc}, []) ->
      let n = Longident.name txt in
      begin match n with
        | "int" | "Int.t" ->
          None, None, Some [%expr int_of_string_opt], `int
        | "int32" | "Int32.t" ->
          None, Some [%expr Int32.to_int], Some [%expr Int32.of_string_opt], `int
        | "int64" | "Int64.t" ->
          None, Some [%expr Int64.to_int], Some [%expr Int64.of_string_opt], `int
        | "nativeint" | "Nativeint.t" ->
          None, Some [%expr Nativeint.to_int], Some [%expr Nativeint.of_string_opt], `int
        | "bool" | "Bool.t" ->
          None, None, Some [%expr bool_of_string_opt], `bool
        | "string" | "String.t" ->
          None, None, None, `string
        | _ -> (try Some (enc_schema t) with _ -> None), None, None, `string
      end
    | _ -> (try Some (enc_schema t) with _ -> None), None, None, `string in
  let kind = match k with
    | `int -> [%expr EzAPI.Param.PARAM_INT]
    | `bool -> [%expr EzAPI.Param.PARAM_BOOL]
    | `string -> [%expr EzAPI.Param.PARAM_STRING] in
  let options = param_options ~id:name ~typ:t ~kind ?schema ?construct ?destruct e in
  let loc = e.pexp_loc in
  let aux = function None -> [%expr None] | Some e -> [%expr Some [%e e]] in
  let param_expr = [%expr {
    EzAPI.Param.param_id = [%e estring ~loc options.id]; param_name = [%e aux options.name];
    param_descr = [%e aux options.descr]; param_type = [%e options.kind];
    param_required = [%e ebool ~loc options.required]; param_examples = [%e options.examples];
    param_schema = [%e aux options.schema] }] in
  let param_value = value_binding ~loc ~pat:(pvar ~loc name) ~expr:param_expr in
  let cons_ident = Longident.parse (match k with `int -> "EzAPI.I" | `bool -> "EzAPI.B" | `string -> "EzAPI.S") in
  let cons_expr =
    let v = match options.construct with
      | None -> [%expr p]
      | Some f -> eapply ~loc f [ [%expr p] ] in
    if options.required then
      [%expr fun p -> [ [%e evar ~loc name], [%e pexp_construct ~loc {txt=cons_ident; loc} (Some v)] ]]
    else
      [%expr function None -> [] | Some p -> [ [%e evar ~loc name], [%e pexp_construct ~loc {txt=cons_ident; loc} (Some v)] ]] in
  let cons_value = value_binding ~loc ~pat:(pvar ~loc (name ^ "_cons")) ~expr:cons_expr in
  let des_expr =
    let v some = match options.destruct with
      | None -> if some then [%expr Ok (Some s)] else [%expr Ok s]
      | Some f -> [%expr match [%e f] s with
        | None -> Error [%e estring ~loc ("cannot destruct " ^ options.id ^ " parameter")]
        | Some x -> Ok [%e if some then [%expr Some x] else [%expr x]]] in
    if options.required then
      [%expr fun req -> match EzAPI.Req.find_param [%e evar ~loc name] req with
        | None -> Error [%e estring ~loc (options.id ^ " is required")]
        | Some s -> [%e v false]]
    else
      [%expr fun req -> match EzAPI.Req.find_param [%e evar ~loc name] req with
        | None -> Ok None
        | Some s -> [%e v true]] in
  let des_value = value_binding ~loc ~pat:(pvar ~loc (name ^ "_des")) ~expr:des_expr in
  if options.debug then  Format.printf "%a@." Pprintast.structure [
      pstr_value ~loc Nonrecursive [ param_value ];
      pstr_value ~loc Nonrecursive [ cons_value ];
      pstr_value ~loc Nonrecursive [ des_value ];
    ];
  param_value, (cons_value, des_value)

(** security *)

let get_ref_name e =
  let search_record k l = List.find_map (fun ({txt; _}, e) ->
      let n = Longident.last_exn txt in
      match e.pexp_desc with
      | Pexp_constant Pconst_string (s, _, _) when n = k -> Some s
      | _ -> None) l in
  match e.pexp_desc with
  | Pexp_variant ("Nosecurity", _) -> None
  | Pexp_variant ("Basic", Some {pexp_desc=Pexp_record (l, None); _}) ->
    Some (Option.value ~default:"basic" @@ search_record "basic_name" l)
  | Pexp_variant ("Bearer", Some {pexp_desc=Pexp_record (l, None); _}) ->
    Some (Option.value ~default:"bearer" @@ search_record "bearer_name" l)
  | Pexp_variant ("Header", Some {pexp_desc=Pexp_record (l, None); _}) ->
    Some (Option.value ~default:"header_security" @@ search_record "ref_name" l)
  | Pexp_variant ("Query", Some {pexp_desc=Pexp_record (l, None); _}) ->
    Some (Option.value ~default:"query_security" @@ search_record "ref_name" l)
  | Pexp_variant ("Cookie", Some {pexp_desc=Pexp_tuple [ {pexp_desc=Pexp_record (l, None); _}; _ ]; _}) ->
    Some (Option.value ~default:"cookie_security" @@ search_record "ref_name" l)
  | Pexp_variant ("Basic", _) -> Some "basic"
  | Pexp_variant ("Bearer", _) -> Some "bearer"
  | Pexp_variant ("Header", _) -> Some "header_security"
  | Pexp_variant ("Query", _) -> Some "query_security"
  | Pexp_variant ("Cookie", _) -> Some "cookie_security"
  | _ -> None

let security_scheme p e =
  match get_ref_name e with
  | Some name -> security_scheme_table := SMap.add p name !security_scheme_table
  | _ -> ()

let security_value ~loc pat expr =
  let p = remove_pat_constraint pat in
  let e = remove_expr_constraint expr in
  let () = match p.ppat_desc with
    | Ppat_var {txt=name; _} ->
      begin match get_list_expr e with
        | Some l -> security_list name l
        | None -> security_scheme name e
      end
    | _ -> () in
  value_binding ~loc ~pat ~expr

(** req *)

let format_ref_name s =
  String.map (function '-' | ' ' | '.' -> '_' | c -> c) @@ String.lowercase_ascii s

let rec get_ident_list_expr ?(acc=[]) e = match e.pexp_desc with
  | Pexp_construct ({txt=Lident "None"; _}, None) -> None
  | Pexp_construct ({txt=Lident "Some"; _}, Some e) -> get_ident_list_expr ~acc e
  | Pexp_construct ({txt=Lident "[]"; _}, None) -> Some (List.rev acc)
  | Pexp_construct ({txt=Lident "::"; _}, Some {pexp_desc=Pexp_tuple [{pexp_desc=Pexp_ident {txt; _}; _}; e2]; _}) ->
    get_ident_list_expr ~acc:((Longident.last_exn txt) :: acc) e2
  | _ -> None

let service_params ~loc ~params ~security =
  let lparams = Option.value ~default:[] (get_ident_list_expr params) in
  let lsecurities = get_ident_list_expr security in
  let lsecurities = match lsecurities with
    | Some l ->
      List.filter_map (fun id ->
          match SMap.find_opt id !security_scheme_table with
          | None -> None
          | Some ref_name -> Some (id, ref_name)) l
    | None -> match security.pexp_desc with
      | Pexp_construct ({txt=Lident "Some"; _}, Some {pexp_desc=Pexp_ident {txt; _}; _}) ->
        begin match SMap.find_opt (Longident.name txt) !security_schemes_table with
          | None -> []
          | Some l -> l
        end
      | _ -> [] in
  match lparams, lsecurities with
  | [], [] -> None
  | _ ->
    let req_param_fields =
      let loc = params.pexp_loc in
      let rec aux l e = match l with
        | [] -> e
        | p :: tl ->
          let des = eapply ~loc (evar ~loc (p ^ "_des")) [ evar ~loc "req" ] in
          [%expr Result.bind [%e des] (fun [%p pvar ~loc (p ^ "_r")] -> [%e aux tl e])] in
      aux lparams in
    let req_security_fields =
      let loc = security.pexp_loc in
      let rec aux l e = match l with
        | [] -> e
        | (id, ref_name) :: tl ->
          let ref_name = format_ref_name ref_name in
          [%expr let [%p pvar ~loc (ref_name ^ "_r")] = EzAPI.Req.find_security [%e evar ~loc id] req in
            [%e aux tl e]] in
      aux lsecurities in
    let fields =
      List.map (fun (_id, ref_name) ->
          let ref_name = format_ref_name ref_name in
          pcf_method ~loc ({txt=ref_name; loc}, Public, Cfk_concrete (Fresh, evar ~loc (ref_name ^ "_r")))) lsecurities @
      List.map (fun p ->
          pcf_method ~loc ({txt=p; loc}, Public, Cfk_concrete (Fresh, evar ~loc (p ^ "_r")))) lparams in
    let obj_e = pexp_object ~loc @@ class_structure ~self:(ppat_any ~loc) ~fields in
    let e = req_security_fields @@ req_param_fields @@ [%expr Ok [%e obj_e]] in
    Some [%expr fun req -> [%e e]]

let service_params_item ~loc ~name options =
  match service_params ~loc ~params:options.params ~security:options.security with
  | None -> None
  | Some expr ->
    let it = pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:(pvar ~loc (name ^ "_req")) ~expr ]in
    if options.debug then Format.printf "%a@." Pprintast.structure_item it;
    Some it

(** register service/handler *)

let first = ref true

let ppx_dir ~loc dir =
  if !first && dir = None then (
    first := false;
    [%str let ppx_dir = EzAPIServerUtils.empty])
  else []

let register name a =
  let loc = a.attr_loc in
  let _, options = get_options ~loc a.attr_payload in
  let ppx_dir = ppx_dir ~loc options.directory in
  let ppx_dir_name = match options.directory with None -> "ppx_dir" | Some s -> s in
  match options.service with
  | None -> Location.raise_errorf ~loc "service not defined"
  | Some e ->
    let service_name = match e.pexp_desc with
      | Pexp_ident {txt; _} -> Some (Longident.name txt)
      | _ -> None in
    let register =
      value_binding ~loc ~pat:(pvar ~loc ppx_dir_name)
        ~expr:(eapply ~loc (evar ~loc "EzAPIServerUtils.register") [
            e; evar ~loc name; evar ~loc ppx_dir_name ]) in
    let str = ppx_dir @ [ pstr_value ~loc Nonrecursive [ register ] ] in
    if options.debug then Format.printf "%a@." Pprintast.structure str;
    str, service_name

let register_ws ~onclose react_name bg_name a =
  let loc = a.attr_loc in
  let _, options = get_options ~loc a.attr_payload in
  let ppx_dir = ppx_dir ~loc options.directory in
  let ppx_dir_name = match options.directory with None -> "ppx_dir" | Some s -> s in
  let onclose = match onclose with
    | [] -> [%expr None]
    | [ {pvb_pat = {ppat_desc = Ppat_var {txt; loc}; _}; _} ] -> [%expr Some [%e evar ~loc txt]]
    | _ -> Location.raise_errorf ~loc "too many value bindings" in
  match options.service with
  | None -> Location.raise_errorf ~loc "service not defined"
  | Some  e ->
    let service_name = match e.pexp_desc with
      | Pexp_ident {txt; _} -> Some (Longident.name txt)
      | _ -> None in
    let register =
      value_binding ~loc ~pat:(pvar ~loc ppx_dir_name)
        ~expr:(pexp_apply ~loc (evar ~loc "EzAPIServerUtils.register_ws") [
            Nolabel, e;
            Optional "onclose", onclose;
            Labelled "react", evar ~loc react_name;
            Labelled "bg", evar ~loc bg_name;
            Nolabel, evar ~loc ppx_dir_name ]) in
    let str = ppx_dir @ [ pstr_value ~loc Nonrecursive [ register ] ] in
    if options.debug then Format.printf "%a@." Pprintast.structure str;
    str, service_name

let process ~it name a =
  let loc = a.attr_loc in
  let service_name = if name = "handler" then "service" else name ^ "_s" in
  let service, service_name, options = service_value ~name:service_name ~meth:a.attr_name.txt ~loc a.attr_payload in
  let req = match service_params_item ~loc ~name options with
    | None -> []
    | Some it -> [ it ] in
  let ppx_dir = ppx_dir ~loc options.directory in
  let ppx_dir_name = match options.directory with None -> "ppx_dir" | Some s -> s in
  let register =
    pstr_value ~loc Nonrecursive [
      value_binding ~loc ~pat:(pvar ~loc ppx_dir_name)
        ~expr:(eapply ~loc (evar ~loc "EzAPIServerUtils.register") [
            evar ~loc service_name; evar ~loc name; evar ~loc ppx_dir_name ]) ] in
  let str = ppx_dir @ [ service ] @ req @ [ it; register ] in
  if options.debug then Format.printf "%a@." Pprintast.structure str;
  str

let process_ws ~it ~onclose react_name bg_name a =
  let loc = a.attr_loc in
  let service_name =  react_name ^ "_s" in
  let service, service_name, options =
    service_value ~name:service_name ~meth:"get" ~loc a.attr_payload in
  let req = match service_params_item ~loc ~name:react_name options with
    | None -> []
    | Some it -> [ it ] in
  let ppx_dir = ppx_dir ~loc options.directory in
  let ppx_dir_name = match options.directory with None -> "ppx_dir" | Some s -> s in
  let onclose = match onclose with
    | [] -> [%expr None]
    | [ {pvb_pat = {ppat_desc = Ppat_var {txt; loc}; _}; _} ] -> [%expr Some [%e evar ~loc txt]]
    | _ -> Location.raise_errorf ~loc "too many value bindings" in
  let register =
    pstr_value ~loc Nonrecursive [
      value_binding ~loc ~pat:(pvar ~loc ppx_dir_name)
        ~expr:(pexp_apply ~loc (evar ~loc "EzAPIServerUtils.register_ws") [
            Nolabel, evar ~loc service_name;
            Optional "onclose", onclose;
            Labelled "react", evar ~loc react_name;
            Labelled "bg", evar ~loc bg_name;
            Nolabel, evar ~loc ppx_dir_name ]) ] in
  if options.debug then Format.printf "%a@." Pprintast.structure_item register;
  ppx_dir @ [ service ] @ req @ [ it; register ]

let rec find_req_pattern_ext p =
  match p.ppat_desc with
  | Ppat_extension ({txt="req"; loc}, PStr []) -> Some (pvar ~loc "_req", None)
  | Ppat_extension ({txt="req"; loc}, PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_ident {txt=Lident p; _}; _}, _); _} ]) -> Some (pvar ~loc "_req", Some p)
  | Ppat_tuple [ p1; p2 ] ->
    begin match find_req_pattern_ext p1 with
      | None -> None
      | Some (p, id) -> Some (ppat_tuple ~loc:p.ppat_loc [p; p2], id)
    end
  | _ -> None

let handler_args ~name e =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_fun (_, _, _, {pexp_desc=Pexp_fun (_, _, _, {pexp_desc=Pexp_fun (_, _, _, _); _}); _}) -> e
  | Pexp_fun (_, _, p1, {pexp_desc = Pexp_fun (_, _, p2, f); _}) ->
    let p1, f = match find_req_pattern_ext p1 with
      | None -> p1, f
      | Some (p1, None) ->
        p1, [%expr match [%e evar ~loc (name ^ "_req")] _req with
          | Error e -> EzAPIServerUtils.return ([%e !global_req_error] e)
          | Ok req -> [%e f]]
      | Some (p1, Some id) ->
        p1, [%expr match [%e evar ~loc (name ^ "_req")] _req with
            | Error e -> EzAPIServerUtils.return ([%e !global_req_error] e)
            | Ok [%p pvar ~loc id] -> [%e f]] in
    [%expr fun [%p p1] _ [%p p2] -> [%e f]]
  | Pexp_fun (_, _, p, f) ->
    [%expr fun _ _ [%p p] -> [%e f]]
  | _ -> e

(** server *)

type server_options = {
  port: expression;
  dir: expression;
  catch: expression;
  allow_headers: expression;
  allow_origin: expression;
  allow_methods: expression;
  allow_credentials: expression;
}

let server_options e =
  let loc = e.pexp_loc in
  let dft port = {
    port; dir = evar ~loc "ppx_dir"; catch = [%expr None];
    allow_origin = [%expr None]; allow_methods = [%expr None]; allow_headers = [%expr None];
    allow_credentials = [%expr None] } in
  match e.pexp_desc with
  | Pexp_constant c -> dft (pexp_constant ~loc c)
  | Pexp_record (l, _) ->
    let l = List.filter_map (function ({txt=Lident s; _}, e) -> Some (s, e) | _ -> None) l in
    List.fold_left (fun acc (s, e) -> match s with
        | "port" -> { acc with port = e }
        | "dir" -> { acc with dir = e }
        | "catch" -> { acc with catch = [%expr Some [%e e]] }
        | "headers" -> { acc with allow_headers = [%expr Some [%e e]] }
        | "methods" -> { acc with allow_methods = [%expr Some [%e e]] }
        | "origin" -> { acc with allow_origin = [%expr Some [%e e]] }
        | "credentials" -> { acc with allow_credentials = [%expr Some [%e e]] }
        | _ -> acc) (dft (eint ~loc 8080)) l
  | _ -> Location.raise_errorf ~loc "server options not understood"

let server_aux e =
  let loc = e.pexp_loc in
  let options = server_options e in
  [%expr
    EzAPIServer.server ?catch:[%e options.catch] ?allow_headers:[%e options.allow_headers]
      ?allow_methods:[%e options.allow_methods] ?allow_origin:[%e options.allow_origin]
      ?allow_credentials:[%e options.allow_credentials] [
      ([%e options.port], EzAPIServerUtils.API [%e options.dir])
    ]
  ]

let server ~loc p =
  match p with
  | PStr [ {pstr_desc=Pstr_eval (e, _); _} ] ->
    [%expr EzLwtSys.run (fun () -> [%e server_aux e])]
  | _ -> Location.raise_errorf ~loc "server options not understood"

(** request *)

let request_expr ~meth ~name ?sname ~loc options =
  let pat = pvar ~loc (meth ^ "_" ^ name) in
  let f, headers_expr = match !global_headers with
    | None -> (fun e -> [%expr fun ?headers -> [%e e]]), [%expr headers]
    | Some h -> (fun e -> [%expr fun ?(headers=[%e h]) -> [%e e]]), [%expr Some headers] in
  let f e = f [%expr fun ?params ?msg -> [%e e]] in
  let f, input_expr, url_encode_expr, post_expr =
    if not (meth="get" || meth="put") then
      (fun e -> f [%expr fun ?url_encode ~input -> [%e e]]), [%expr input], [%expr url_encode], [%expr None]
    else (fun e -> f [%expr fun ?post -> [%e e]]), [%expr ()], [%expr None], [%expr post] in
  let service = evar ~loc (Option.value ~default:(name ^ "_s") sname) in
  let f e =
    if !global_base && options.nargs = 0 then f [%expr fun ?(base= !ezreq_base) () -> [%e e]]
    else if !global_base then f [%expr fun ?(base= !ezreq_base) -> [%e e]]
    else f [%expr fun base -> [%e e]] in
  let rec args_pat i e =
    if i = 0 then e
    else [%expr fun [%p pvar ~loc ("arg" ^ string_of_int i)] -> [%e args_pat (i-1) e]] in
  let rec args_expr i =
    if i = 0 then [%expr EzAPI.Req.dummy]
    else [%expr [%e args_expr (i-1)], [%e evar ~loc ("arg" ^ string_of_int i)]] in
  let expr = f @@ args_pat options.nargs @@ [%expr
      EzReq_lwt.wrap @@ EzReq_lwt.request ?headers:[%e headers_expr] ?params ?msg ?post:[%e post_expr]
        ?url_encode:[%e url_encode_expr] ~input:[%e input_expr] base
        [%e service] [%e args_expr options.nargs]
    ] in
  pat, expr

let request_value ~meth ~name ?sname ~loc options =
  let pat, expr = request_expr ~meth ~name ?sname ~loc options in
  let it = pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr ] in
  if options.debug then Format.printf "%a@." Pprintast.structure_item it;
  it

(** main mapper *)

let deprecate =
  let t : (string, unit) Hashtbl.t = Hashtbl.create 10 in
  fun s ->
    match Hashtbl.find_opt t s with
    | None ->
      Hashtbl.add t s ();
      Format.eprintf "deprecated: [@@@@@@%s ...] -> [%%%%%s ...]@." s s
    | Some () -> ()

let transform ?kind () =
  object(self)
    inherit Ast_traverse.map as super
    method! structure str =
      List.rev @@
      List.fold_left (fun acc it ->
          match it.pstr_desc with
          | Pstr_value (rflag, [ v ]) when (kind <> Some `client && kind <> Some `request)  ->
            begin match List.partition (fun a -> List.mem a.attr_name.txt methods) v.pvb_attributes with
              (* service for handler *)
              | [ a ], pvb_attributes ->
                begin match v.pvb_pat.ppat_desc with
                  | Ppat_var {txt=name;_} ->
                    let pvb_expr = handler_args ~name v.pvb_expr in
                    let it = {it with pstr_desc = Pstr_value (rflag, [ {v with pvb_expr; pvb_attributes }])} in
                    let str = process ~it name a in
                    (List.rev str) @ acc
                  | _ ->
                    (self#structure_item it) :: acc
                end
              (* link service *)
              | [], attributes ->
                begin match List.partition (fun a -> a.attr_name.txt = "service") attributes with
                  | [ a ], pvb_attributes ->
                    begin match v.pvb_pat.ppat_desc with
                      | Ppat_var {txt=name;_} ->
                        let register_str, service_name = register name a in
                        let pvb_expr = handler_args ~name:(Option.value ~default:name service_name)  v.pvb_expr in
                        let it = {it with pstr_desc = Pstr_value (rflag, [ {v with pvb_expr; pvb_attributes }])} in
                        (List.rev register_str) @ it :: acc
                      | _ -> (self#structure_item it) :: acc
                    end
                  | _ -> (self#structure_item it) :: acc
                end
              | _ -> (self#structure_item it) :: acc
            end
          | Pstr_value (rflag, (v_react :: v_bg :: onclose)) when (kind <> Some `client && kind <> Some `request) ->
            let attributes = match onclose with
              | [] -> v_bg.pvb_attributes
              | v :: _  -> v.pvb_attributes in
            begin match List.partition (fun a -> a.attr_name.txt = "ws" || a.attr_name.txt = "websocket") attributes with
              (* service for websocket handlers *)
              | [ a ], pvb_attributes ->
                begin match v_react.pvb_pat.ppat_desc, v_bg.pvb_pat.ppat_desc with
                  | Ppat_var {txt=name_react;_}, Ppat_var {txt=name_bg;_} ->
                    let pvb_expr_react = handler_args ~name:name_react v_react.pvb_expr in
                    let pvb_expr_bg = handler_args ~name:name_react v_bg.pvb_expr in
                    let pvb_attributes, vs = match onclose with
                      | [] -> pvb_attributes, []
                      | v :: t -> v_bg.pvb_attributes, {v with pvb_attributes} :: t in
                    let it = {it with pstr_desc = Pstr_value (rflag, (
                        {v_react with pvb_expr = pvb_expr_react } ::
                        {v_bg with pvb_expr = pvb_expr_bg; pvb_attributes } ::
                        vs )) } in
                    let str = process_ws ~it ~onclose name_react name_bg a in
                    (List.rev str) @ acc
                  | _ -> (self#structure_item it) :: acc
                end
              (* link websocket service *)
              | [], attributes ->
                begin match List.partition (fun a -> a.attr_name.txt = "service") attributes with
                  | [ a ], pvb_attributes ->
                    begin match v_react.pvb_pat.ppat_desc, v_bg.pvb_pat.ppat_desc with
                      | Ppat_var {txt=name_react;_}, Ppat_var {txt=name_bg;_} ->
                        let str, service_name = register_ws ~onclose name_react name_bg a in
                        let service_name = Option.value ~default:name_react service_name in
                        let pvb_expr_react = handler_args ~name:service_name v_react.pvb_expr in
                        let pvb_expr_bg = handler_args ~name:service_name v_bg.pvb_expr in
                        let pvb_attributes, vs = match onclose with
                          | [] -> pvb_attributes, []
                          | v :: t -> v_bg.pvb_attributes, {v with pvb_attributes} :: t in
                        let it = {it with pstr_desc = Pstr_value (rflag, (
                            {v_react with pvb_expr = pvb_expr_react } ::
                            {v_bg with pvb_expr = pvb_expr_bg; pvb_attributes } ::
                            vs )) } in
                        (List.rev str) @ it :: acc
                      | _ -> (self#structure_item it) :: acc
                    end
                  | _ -> (self#structure_item it) :: acc
                end
              | _ -> (self#structure_item it) :: acc
            end
          (* server main *)
          | Pstr_attribute a when a.attr_name.txt = "server" && kind = Some `server ->
            deprecate "server";
            let loc = a.attr_loc in
            let expr = server ~loc a.attr_payload in
            pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:(punit ~loc) ~expr ] :: acc
          | Pstr_extension (({txt="server"; loc}, p), _) when kind = Some `server ->
            let expr = server ~loc p in
            pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:(punit ~loc) ~expr ] :: acc
          (* client service *)
          | Pstr_attribute a when List.mem a.attr_name.txt methods ->
            deprecate a.attr_name.txt;
            let loc, meth = a.attr_loc, a.attr_name.txt in
            let options = { (default_options loc) with register = [%expr false] } in
            let service, name, options = service_value ~options ~meth:a.attr_name.txt ~loc:a.attr_loc a.attr_payload in
            let acc = service :: acc in
            let acc = match kind with
              | Some `request -> request_value ~loc ~meth ~name ~sname:name options :: acc
              | _ -> acc in
            let acc = match service_params_item ~loc ~name options with
              | None -> acc
              | Some it -> it :: acc in
            acc
          | Pstr_extension (({txt=meth; loc}, PStr [ { pstr_desc = Pstr_value (_, [ { pvb_expr; pvb_pat= {ppat_desc=Ppat_var {txt=name; _}; _}; _} ]); _} ]), _) when List.mem meth methods ->
            let options = { (default_options loc) with register = [%expr false] } in
            let service, name, options = service_value ~name ~options ~meth ~loc @@ PStr [ pstr_eval ~loc pvb_expr [] ] in
            let acc = service :: acc in
            let acc = match kind with
              | Some `request -> request_value ~loc ~meth ~name ~sname:name options :: acc
              | _ -> acc in
            let acc = match service_params_item ~loc ~name options with
              | None -> acc
              | Some it -> it :: acc in
            acc
          | Pstr_extension (({txt=meth; loc}, p), _) when List.mem meth methods ->
            let options = { (default_options loc) with register = [%expr false] } in
            let service, name, options = service_value ~options ~meth ~loc p in
            let acc = service :: acc in
            let acc = match kind with
              | Some `request -> request_value ~loc ~meth ~name ~sname:name options :: acc
              | _ -> acc in
            let acc = match service_params_item ~loc ~name options with
              | None -> acc
              | Some it -> it :: acc in
            acc
          (* global errors and security *)
          | Pstr_extension (({txt="service"; _}, PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_record (l, _); _}, _); _} ]), _) ->
            let base = set_globals l in
            begin match base, kind with Some it, Some `request -> it :: acc | _ -> acc end
          | Pstr_extension (({txt="service"; loc}, PStr [ {pstr_desc=Pstr_value (_, l); _} ]), _) ->
            let acc_str, base = List.fold_left (fun (acc, base) vb ->
                match vb.pvb_pat.ppat_desc with
                | Ppat_var {txt="errors"; _} ->
                  set_global_errors [%expr errors] ; acc @ [ vb ], base
                | Ppat_var {txt="security"; _} ->
                  Option.iter (security_list "security") @@ get_list_expr (remove_expr_constraint vb.pvb_expr);
                  set_global_security [%expr security]; acc @ [ vb ], base
                | Ppat_var {txt="base"; _} ->
                  acc, Some (set_global_base vb.pvb_expr)
                | Ppat_var {txt="headers"; _} ->
                  set_global_headers vb.pvb_expr; acc, base
                | Ppat_constraint ({ppat_desc = Ppat_var {txt="errors"; _}; _}, typ) ->
                  set_global_errors ~typ [%expr errors]; acc @ [ { vb with pvb_pat = [%pat? errors]; pvb_expr=remove_expr_constraint vb.pvb_expr } ], base
                | Ppat_constraint ({ppat_desc = Ppat_var {txt="security"; _}; _}, typ) ->
                  Option.iter (security_list "security") @@ get_list_expr (remove_expr_constraint vb.pvb_expr);
                  set_global_security ~typ [%expr security]; acc @ [ { vb with pvb_pat = [%pat? security]; pvb_expr=remove_expr_constraint vb.pvb_expr } ], base
                | Ppat_var {txt="req_error"; _} ->
                  set_global_req_error [%expr req_error]; acc @ [ { vb with pvb_pat = [%pat? req_error] } ], base
                | _ -> acc, base) ([], None) l in
            let acc = pstr_value ~loc Nonrecursive acc_str :: acc in
            begin match base, kind with Some it, Some `request -> it :: acc | _ -> acc end
          (* service deriver *)
          | Pstr_type (_rec_flag, [ t ]) ->
            let loc = t.ptype_loc in
            begin match List.find_opt (fun a -> List.mem a.attr_name.txt methods) t.ptype_attributes with
              | None -> (super#structure_item it) :: acc
              | Some a ->
                let meth = a.attr_name.txt in
                let open Ppx_deriving_encoding_lib.Encoding in
                let {enc; _} = expressions t in
                let enc_name = t.ptype_name.txt ^ "_enc" in
                let input, output = match meth with
                  | "get" | "put" -> [%expr EzAPI.Empty], [%expr EzAPI.Json [%e evar ~loc enc_name]]
                  | _ -> [%expr EzAPI.Json [%e evar ~loc enc_name]], [%expr EzAPI.Empty] in
                let options = { (default_options loc) with register = [%expr false]; input; output } in
                let name = Option.value ~default:t.ptype_name.txt @@ get_name a.attr_payload in
                let sname = name ^ "_s" in
                let service, _, options = service_value ~options ~name:sname ~meth ~loc a.attr_payload in
                let enc_value = [%stri let [%p pvar ~loc enc_name] = [%e enc]] in
                let acc = service :: enc_value :: it :: acc in
                let acc = match kind with
                  | Some `request -> request_value ~loc ~meth ~name options :: acc
                  | _ -> acc in
                let acc = match service_params_item ~loc ~name options with
                  | None -> acc
                  | Some it -> it :: acc in
                acc
            end
          | Pstr_type (_rec_flag, [ t_input; t_output ]) ->
            let loc = t_input.ptype_loc in
            begin match List.find_opt (fun a -> List.mem a.attr_name.txt methods) t_output.ptype_attributes with
              | None -> (super#structure_item it) :: acc
              | Some a ->
                let meth = a.attr_name.txt in
                let open Ppx_deriving_encoding_lib.Encoding in
                let input_enc_name = t_input.ptype_name.txt ^ "_enc" in
                let output_enc_name = t_output.ptype_name.txt ^ "_enc" in
                let {enc=input_enc; _} = expressions t_input in
                let {enc=output_enc; _} = expressions t_output in
                let input = [%expr EzAPI.Json [%e evar ~loc input_enc_name]] in
                let output = [%expr EzAPI.Json [%e evar ~loc output_enc_name]] in
                let options = { (default_options loc) with register = [%expr false]; input; output } in
                let name = Option.value ~default:t_input.ptype_name.txt @@ get_name a.attr_payload in
                let sname = name ^ "_s" in
                let service, _, options = service_value ~name:sname ~options ~meth ~loc a.attr_payload in
                let input_enc_value = [%stri let [%p pvar ~loc input_enc_name] = [%e input_enc]] in
                let output_enc_value = [%stri let [%p pvar ~loc output_enc_name] = [%e output_enc]] in
                if options.debug then Format.printf "%a@." Pprintast.structure [ input_enc_value; output_enc_value ];
                let acc = service :: output_enc_value :: input_enc_value :: it :: acc in
                let acc = match kind with
                  | Some `request -> request_value ~loc ~meth ~name options :: acc
                  | _ -> acc in
                let acc = match service_params_item ~loc ~name options with
                  | None -> acc
                  | Some it -> it :: acc in
                acc
            end
          | Pstr_extension (({txt=("param"|"parameter"); loc}, PStr [ { pstr_desc = Pstr_value (_, l); _} ]), _)  ->
            let lp, ltr = List.split @@ List.map (fun vb -> param_value vb.pvb_pat vb.pvb_expr) l in
            let lcons, ldes = List.split ltr in
            let it_p = pstr_value ~loc Nonrecursive lp in
            let it_cons = pstr_value ~loc Nonrecursive lcons in
            let it_des = pstr_value ~loc Nonrecursive ldes in
            it_des :: it_cons :: it_p :: acc

          | Pstr_extension (({txt=("security"|"sec"|"secu"); loc}, PStr [ { pstr_desc = Pstr_value (_, l); _} ]), _)  ->
            let l = List.map (fun vb -> security_value ~loc vb.pvb_pat vb.pvb_expr) l in
            let it = pstr_value ~loc Nonrecursive l in
            it :: acc

          | _ -> (self#structure_item it) :: acc
        ) [] str

    method! expression e = match e.pexp_desc with
      | Pexp_extension ({txt="server"; _}, PStr [ { pstr_desc = Pstr_eval (e, _); _} ]) when kind = Some `server ->
        server_aux e
      | Pexp_extension (({txt=meth; loc}, PStr [ {
          pstr_desc = Pstr_eval ({
              pexp_desc=Pexp_let (_, [ { pvb_expr; pvb_pat= {ppat_desc=Ppat_var {txt=name; _}; _}; _} ], e); _}, _); _} ])) when List.mem meth methods ->
        let options = { (default_options loc) with register = [%expr false] } in
        let name, options, pat, expr = service_expr ~name ~options ~meth ~loc @@ PStr [ pstr_eval ~loc pvb_expr [] ] in
        let e_after = self#expression e in
        let e_params, e_debug = match service_params ~loc ~params:options.params ~security:options.security with
          | None -> e_after, (if options.debug then Some [%expr ()] else None)
          | Some f ->
            let pat = pvar ~loc (name ^ "_params") in
            [%expr let [%p pat] = [%e f] in [%e e_after]],
            (if options.debug then Some [%expr let [%p pat] = [%e f] in ()] else None) in
        let e_request, e_debug = match kind with
          | Some `request ->
            let pat, expr = request_expr ~loc ~meth ~name ~sname:name options in
            [%expr let [%p pat] = [%e expr] in [%e e_params]],
            (Option.map (fun e_debug -> [%expr let [%p pat] = [%e expr] in [%e e_debug]]) e_debug)
          | _ -> e_params, e_debug in
        Option.iter (fun e_debug ->
            let e_debug = [%expr let [%p pat] = [%e expr] in [%e e_debug]] in
            Format.printf "%a@." Pprintast.expression e_debug) e_debug;
        [%expr let [%p pat] = [%e expr] in [%e e_request]]
      | _ -> super#expression e
  end

let impl ?kind str = (transform ?kind ())#structure str

let deriver_str_gen kind meth ~loc ~path:_ (rec_flag, l) path input output errors params section ename
    descr security register hide input_example output_example debug =
  let options = default_options loc in
  let name = match ename with
    | Some {pexp_desc=Pexp_constant Pconst_string (s, _, _); _} -> Some s
    | _ -> None in
  let aux e = match e.pexp_desc with
    | Pexp_construct ({txt=Lident "::"; _}, _) -> raw e
    | _ -> [%expr EzAPI.Json [%e e]] in
  let input, output, tname = match meth, l with
    | _, [ t_input; t_output ] when rec_flag = Recursive ->
      [%expr EzAPI.Json ([%e evar ~loc (t_input.ptype_name.txt ^ "_enc")] ())],
      [%expr EzAPI.Json ([%e evar ~loc (t_output.ptype_name.txt ^ "_enc")] ())],
      (Option.value ~default:t_input.ptype_name.txt name)
    | _, [ t_input; t_output ] ->
      [%expr EzAPI.Json [%e evar ~loc (t_input.ptype_name.txt ^ "_enc")]],
      [%expr EzAPI.Json [%e evar ~loc (t_output.ptype_name.txt ^ "_enc")]],
      (Option.value ~default:t_input.ptype_name.txt name)
    | ("get" | "put"), t :: _ ->
      Option.fold ~none:options.input ~some:aux input,
      [%expr EzAPI.Json [%e evar ~loc (t.ptype_name.txt ^ "_enc")]],
      (Option.value ~default:t.ptype_name.txt name)
    |  _, t :: _ ->
      [%expr EzAPI.Json [%e evar ~loc (t.ptype_name.txt ^ "_enc")]],
      Option.fold ~none:options.output ~some:aux output,
      (Option.value ~default:t.ptype_name.txt name)
    | _ ->
      Option.fold ~none:options.input ~some:aux input,
      Option.fold ~none:options.output ~some:aux output,
      (Option.value ~default:"default" name) in
  let sname = tname ^ "_s" in
  let path, nargs = match path with
    | Some { pexp_desc = Pexp_constant cst; pexp_loc=loc; _ } ->
      begin match string_literal cst with
        | Some s -> parse_path ~loc s
        | _ -> Format.eprintf "path should be a string literal"; options.path, 0
      end
    | _ -> options.path, 0 in
  let some e = [%expr Some [%e e]] in
  let security_type, security = match security with
    | None -> options.security_type, options.security
    | Some e -> [%type: _], some e in
  let error_type, errors = match errors  with
    | None -> options.error_type, options.errors
    | Some e -> [%type: _], some e in
  let options = {
    options with
    path; input; output;
    errors; error_type;
    params = Option.fold ~none:options.params ~some params;
    section = Option.fold ~none:options.section ~some section;
    name = Option.fold ~none:options.name ~some ename;
    descr = Option.fold ~none:options.descr ~some descr;
    security; security_type;
    register = Option.value ~default:[%expr false] register;
    hide = Option.value ~default:options.hide hide;
    input_example = Option.fold ~none:options.input_example ~some input_example;
    output_example = Option.fold ~none:options.output_example ~some output_example;
    debug; nargs;
  } in
  let s, _, options = service_value ~meth ~loc ~options ~name:sname ~parse_options:false (PStr []) in
  let s = match kind with
    | Some `request -> [ s; request_value ~loc ~meth ~name:tname options ]
    | _ -> [ s ] in
  match service_params_item ~loc ~name:tname options with
  | None -> s
  | Some it -> s @ [ it ]


let derivers kind =
  let open Deriving in
  List.iter (fun meth ->
      let args_str = Args.(
          empty
          +> arg "path" __
          +> arg "input" __
          +> arg "output" __
          +> arg "errors" __
          +> arg "params" __
          +> arg "section" __
          +> arg "name" __
          +> arg "descr" __
          +> arg "security" __
          +> arg "register" __
          +> arg "hide" __
          +> arg "input_example" __
          +> arg "output_example" __
          +> flag "debug"
        ) in
      let str_type_decl = Generator.make args_str (deriver_str_gen kind meth) in
      ignore @@ add meth ~str_type_decl) methods

let global_deriver_str_gen kind ~loc:_ ~path:_ (_rec_flag, l) errors security base headers =
  let error_type = List.find_map (fun t ->
      match t.ptype_name.txt, t.ptype_manifest with
      | "errors", Some c -> Some c
      | _ -> None) l in
  let security_type = List.find_map (fun t ->
      match t.ptype_name.txt, t.ptype_manifest with
      | "security", Some c -> Some c
      | _ -> None) l in
  Option.iter (set_global_errors ?typ:error_type) errors;
  Option.iter (set_global_security ?typ:security_type) security;
  Option.iter (set_global_headers) headers;
  match kind, base with Some `request, Some e -> [ set_global_base e ] | _ -> []

let global_deriver kind =
  let open Deriving in
  let arg_str = Args.( empty +> arg "errors" __ +> arg "security" __ +> arg "base" __ +> arg "headers" __) in
  let str_type_decl = Generator.make arg_str (global_deriver_str_gen kind) in
  ignore @@ add "service" ~str_type_decl
