open Ppxlib
open Ast_builder.Default

let str_of_expr e = Pprintast.string_of_expression e
let str_of_pat p =
  Pprintast.pattern Format.str_formatter p;
  Format.flush_str_formatter ()
let str_of_structure e = Pprintast.string_of_structure e

let llid ~loc s = {txt=Longident.parse s; loc}
let esome e =
  let loc = e.pexp_loc in
  pexp_construct ~loc (llid ~loc "Some") (Some e)
let enone ~loc =
  pexp_construct ~loc (llid ~loc "None") None

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
  input_example : expression;
  output_example : expression;
  error_type : core_type;
  security_type : core_type;
  debug : bool;
}

let empty ~loc  = pexp_construct ~loc (llid ~loc "EzAPI.Empty") None
let raw e =
  let loc = e.pexp_loc in
  let e =
    eapply ~loc (evar ~loc "List.filter_map") [ evar ~loc "EzAPI.Mime.parse"; e ] in
  pexp_construct ~loc (llid ~loc "EzAPI.Raw") @@ Some e
let json e =
  let loc = e.pexp_loc in
  pexp_construct ~loc (llid ~loc "EzAPI.Json") @@ Some e

let options ~loc = {
  path = pexp_ident ~loc (llid ~loc "EzAPI.Path.root");
  input = empty ~loc; output = empty ~loc; errors = enone ~loc; params = enone ~loc;
  section = enone ~loc; name = enone ~loc; descr = enone ~loc;
  security = enone ~loc; register = enone ~loc; input_example = enone ~loc;
  output_example = enone ~loc; error_type = ptyp_constr ~loc (llid ~loc "exn") [];
  security_type = ptyp_constr ~loc (llid ~loc "EzAPI.no_security") [];
  debug = false
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
  let path ~loc s = pexp_ident ~loc (llid ~loc ("EzAPI.Path." ^ s)) in
  let l = String.split_on_char '/' s in
  let l = List.filter (fun s -> s <> "") l in
  List.fold_left (fun acc s ->
      match String.get s 0 with
      | '{' ->
        let e = parse_arg ~loc String.(sub s 1 (length s - 2)) in
        eapply ~loc (path ~loc "add_arg") [ acc; e ]
      | _ -> eapply ~loc (path ~loc "add_suffix") [ acc; estring ~loc s ]
    ) (path ~loc "root") l

let get_options ~loc ?name a =
  match a.attr_payload with
  | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_record (l, _); _}, _); _} ] ->
    let l = List.filter_map (function ({txt=Lident s; loc}, e) -> Some (s, loc, e) | _ -> None) l in
    List.fold_left (fun (name, acc) (s, loc, e) -> match s with
        | "path" -> begin match e.pexp_desc with
            | Pexp_constant (Pconst_string (s, _, _)) ->
              name, { acc with path = parse_path ~loc:e.pexp_loc s }
            | _ -> name, acc
          end
        | "input" -> name, { acc with input = json e }
        | "raw_input" -> name, { acc with input = raw e }
        | "output" -> name, { acc with output = json e }
        | "raw_output" -> name, { acc with output = raw e }
        | "params" -> name, { acc with params = esome e }
        | "errors" -> name, { acc with errors = esome e; error_type = ptyp_any ~loc }
        | "section" -> name, { acc with section = esome e }
        | "name" ->
          begin match e.pexp_desc with
            | Pexp_constant (Pconst_string (s, _, _)) -> Some s,  { acc with name = esome e }
            | _ ->
              Format.eprintf "name should be a literal";
              name, acc
          end
        | "descr" -> name, { acc with descr = esome e }
        | "security" -> name, { acc with security = esome e; security_type = ptyp_any ~loc }
        | "register" -> name, { acc with register = esome e }
        | "input_example" -> name, { acc with input_example = esome e }
        | "output_example" -> name, { acc with input_example = esome e }
        | "debug" -> name, { acc with debug = true }
        | _ -> name, acc) (name, options ~loc) l
  | _ -> name, options ~loc

let service_value ?name a =
  let loc = a.attr_loc in
  let meth = pexp_variant ~loc (String.uppercase_ascii a.attr_name.txt) None in
  let name, options = get_options ~loc ?name a in
  match name with
  | None -> Location.raise_errorf ~loc "service doesn't have a name"
  | Some name ->
    let expr = pexp_apply ~loc (evar ~loc "EzAPI.raw_service") [
        Optional "section", options.section;
        Optional "name", options.name;
        Optional "descr", options.descr;
        Labelled "meth", meth;
        Labelled "input", options.input;
        Labelled "output", options.output;
        Optional "errors", options.errors;
        Optional "security", options.security;
        Optional "register", options.register;
        Optional "input_example", options.input_example;
        Optional "output_example", options.output_example;
        Nolabel, options.path ] in
    let pat = ppat_constraint ~loc (pvar ~loc name) @@
      ptyp_constr ~loc (llid ~loc "EzAPI.service") [
        ptyp_any ~loc; ptyp_any ~loc; ptyp_any ~loc; options.error_type;
        options.security_type ] in
    let str = pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr ] in
    if options.debug then Format.printf "%s@." @@ str_of_structure [ str ];
    str, name, options.debug
