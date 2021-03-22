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

let options ?register loc =
  let register = match register with
    | None -> pexp_construct ~loc (llid ~loc "true") None
    | Some register -> register in {
  path = pexp_ident ~loc (llid ~loc "EzAPI.Path.root");
  input = empty ~loc; output = empty ~loc; errors = enone ~loc; params = enone ~loc;
  section = enone ~loc; name = enone ~loc; descr = enone ~loc;
  security = enone ~loc; register; input_example = enone ~loc;
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

let get_options ~loc ?name ?(client=false) a =
  let register = if not client then None else Some (pexp_construct ~loc (llid ~loc "false") None) in
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
        | "register" -> name, { acc with register = e }
        | "input_example" -> name, { acc with input_example = esome e }
        | "output_example" -> name, { acc with input_example = esome e }
        | "debug" -> name, { acc with debug = true }
        | _ -> name, acc) (name, options ?register loc) l
  | _ -> name, options ?register loc

let service_value ?name ?client a =
  let loc = a.attr_loc in
  let meth = pexp_variant ~loc (String.uppercase_ascii a.attr_name.txt) None in
  let name, options = get_options ~loc ?name ?client a in
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
        Labelled "register", options.register;
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

(** register service/handler *)

let first = ref true

let ppx_dir ~loc =
  if !first then (
    first := false;
    [ pstr_value ~loc Nonrecursive [
          value_binding ~loc ~pat:(pvar ~loc "ppx_dir")
            ~expr:(evar ~loc "EzAPIServerUtils.empty") ] ])
  else []

let register name a =
  let loc = a.attr_loc in
  let ppx_dir = ppx_dir ~loc in
  match a.attr_payload with
  | PStr [ {pstr_desc=Pstr_eval (e, _); _} ] ->
    let register =
      value_binding ~loc ~pat:(pvar ~loc "ppx_dir")
        ~expr:(eapply ~loc (evar ~loc "EzAPIServerUtils.register") [
            e; evar ~loc name; evar ~loc "ppx_dir" ]) in
    ppx_dir @ [ pstr_value ~loc Nonrecursive [ register ] ]
  | _ -> Location.raise_errorf ~loc "service name not understood"

let register_ws ~onclose react_name bg_name a =
  let loc = a.attr_loc in
  let ppx_dir = ppx_dir ~loc in
  let onclose = match onclose with
    | [] -> enone ~loc
    | [ {pvb_pat = {ppat_desc = Ppat_var {txt; loc}; _}; _} ] -> esome (evar ~loc txt)
    | _ -> Location.raise_errorf ~loc "too many value bindings" in
  match a.attr_payload with
  | PStr [ {pstr_desc=Pstr_eval (e, _); _} ] ->
    let register =
      value_binding ~loc ~pat:(pvar ~loc "ppx_dir")
        ~expr:(pexp_apply ~loc (evar ~loc "EzAPIServerUtils.register_ws") [
            Nolabel, e;
            Optional "onclose", onclose;
            Labelled "react", evar ~loc react_name;
            Labelled "bg", evar ~loc bg_name;
            Nolabel, evar ~loc "ppx_dir" ]) in
    ppx_dir @ [ pstr_value ~loc Nonrecursive [ register ] ]
  | _ -> Location.raise_errorf ~loc "service name not understood"

let process name a =
  let loc = a.attr_loc in
  let service_name = if name = "handler" then "service" else name ^ "_s" in
  let service, service_name, debug = service_value ~name:service_name a in
  let ppx_dir = ppx_dir ~loc in
  let register =
    pstr_value ~loc Nonrecursive [
      value_binding ~loc ~pat:(pvar ~loc "ppx_dir")
        ~expr:(eapply ~loc (evar ~loc "EzAPIServerUtils.register") [
            evar ~loc service_name; evar ~loc name; evar ~loc "ppx_dir" ]) ] in
  if debug then Format.printf "%s@." @@ str_of_structure [ register ];
  ppx_dir @ [ service; register ]

let process_ws ~onclose react_name bg_name a =
  let loc = a.attr_loc in
  let service_name =  react_name ^ "_s" in
  let service, service_name, debug =
    service_value ~name:service_name { a with attr_name = { a.attr_name with txt = "get" } } in
  let ppx_dir = ppx_dir ~loc in
  let onclose = match onclose with
    | [] -> enone ~loc
    | [ {pvb_pat = {ppat_desc = Ppat_var {txt; loc}; _}; _} ] -> esome (evar ~loc txt)
    | _ -> Location.raise_errorf ~loc "too many value bindings" in
  let register =
    pstr_value ~loc Nonrecursive [
      value_binding ~loc ~pat:(pvar ~loc "ppx_dir")
        ~expr:(pexp_apply ~loc (evar ~loc "EzAPIServerUtils.register_ws") [
            Nolabel, evar ~loc service_name;
            Optional "onclose", onclose;
            Labelled "react", evar ~loc react_name;
            Labelled "bg", evar ~loc bg_name;
            Nolabel, evar ~loc "ppx_dir" ]) ] in
  if debug then Format.printf "%s@." @@ str_of_structure [ register ];
  ppx_dir @ [ service; register ]

let handler_args e =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_fun (_, _, _, {pexp_desc=Pexp_fun (_, _, _, {pexp_desc=Pexp_fun (_, _, _, _); _}); _}) -> e
  | Pexp_fun (_, _, p1, {pexp_desc = Pexp_fun (_, _, p2, f); pexp_loc=loc2; _}) ->
    pexp_fun ~loc Nolabel None p1 (
      pexp_fun ~loc Nolabel None (ppat_any ~loc) (
        pexp_fun ~loc:loc2 Nolabel None p2 f))
  | Pexp_fun (_, _, p, f) ->
    pexp_fun ~loc Nolabel None (ppat_any ~loc) (
      pexp_fun ~loc Nolabel None (ppat_any ~loc) (
        pexp_fun ~loc Nolabel None p f))
  | _ -> e

(** server *)

type server_options = {
  port : expression;
  dir : expression;
  catch : expression;
}

let server_options a =
  let loc = a.attr_loc in
  match a.attr_payload with
  | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_constant c; pexp_loc; _}, _); _} ] ->
    { port = pexp_constant ~loc:pexp_loc c; dir = evar ~loc "ppx_dir";
      catch = enone ~loc }
  | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_record (l, _); pexp_loc=loc; _}, _); _} ] ->
    let l = List.filter_map (function ({txt=Lident s; _}, e) -> Some (s, e) | _ -> None) l in
    List.fold_left (fun acc (s, e) -> match s with
        | "port" -> { acc with port = e }
        | "dir" -> { acc with dir = e }
        | "catch" -> { acc with catch = esome e }
        | _ -> acc) {port = eint ~loc 8080; dir = evar ~loc "ppx_dir"; catch = enone ~loc } l
  | _ -> Location.raise_errorf ~loc "payload not understood"

let server a =
  let options = server_options a in
  let loc = a.attr_loc in
  eapply ~loc (evar ~loc "EzLwtSys.run") [
    pexp_fun ~loc Nolabel None (punit ~loc)
      (pexp_apply ~loc (evar ~loc "EzAPIServer.server") [
          Optional "catch", options.catch;
          Nolabel, elist ~loc [
            pexp_tuple ~loc [
              options.port;
              pexp_construct ~loc (llid ~loc "EzAPIServerUtils.API") (Some options.dir)
            ]
          ]
        ])
  ]

let impl ?kind str =
  List.rev @@ List.fold_left (fun acc str ->
      match str.pstr_desc with
      | Pstr_value (rflag, [ v ]) when kind <> Some `client ->
        begin match List.partition (fun a -> List.mem a.attr_name.txt methods) v.pvb_attributes with
          (* service for handler *)
          | [ a ], pvb_attributes ->
            begin match v.pvb_pat.ppat_desc with
              | Ppat_var {txt=name;_} ->
                let pvb_expr = handler_args v.pvb_expr in
                let str = {str with pstr_desc = Pstr_value (rflag, [ {v with pvb_expr; pvb_attributes }])} in
                (List.rev @@ process name a) @ str :: acc
              | _ ->
                str :: acc
            end
          (* link service *)
          | [], attributes ->
            begin match List.partition (fun a -> a.attr_name.txt = "service") attributes with
              | [ a ], pvb_attributes ->
                begin match v.pvb_pat.ppat_desc with
                  | Ppat_var {txt=name;_} ->
                    let pvb_expr = handler_args v.pvb_expr in
                    let str = {str with pstr_desc = Pstr_value (rflag, [ {v with pvb_expr; pvb_attributes }])} in
                    (List.rev @@ register name a) @ str :: acc
                  | _ -> str :: acc
                end
              | _ -> str :: acc
            end
          | _ -> str :: acc
        end
      | Pstr_value (rflag, (v_react :: v_bg :: onclose)) when kind <> Some `client ->
        let attributes = match onclose with
          | [] -> v_bg.pvb_attributes
          | v :: _  -> v.pvb_attributes in
        begin match List.partition (fun a -> a.attr_name.txt = "ws" || a.attr_name.txt = "websocket") attributes with
          (* service for websocket handlers *)
          | [ a ], pvb_attributes ->
            begin match v_react.pvb_pat.ppat_desc, v_bg.pvb_pat.ppat_desc with
              | Ppat_var {txt=name_react;_}, Ppat_var {txt=name_bg;_} ->
                let pvb_expr_react = handler_args v_react.pvb_expr in
                let pvb_expr_bg = handler_args v_bg.pvb_expr in
                let pvb_attributes, vs = match onclose with
                  | [] -> pvb_attributes, []
                  | v :: t -> v_bg.pvb_attributes, {v with pvb_attributes} :: t in
                let str = {str with pstr_desc = Pstr_value (rflag, (
                    {v_react with pvb_expr = pvb_expr_react } ::
                    {v_bg with pvb_expr = pvb_expr_bg; pvb_attributes } ::
                    vs )) } in
                (List.rev @@ process_ws ~onclose name_react name_bg a) @ str :: acc
              | _ -> str :: acc
            end
          (* link websocket service *)
          | [], attributes ->
            begin match List.partition (fun a -> a.attr_name.txt = "service") attributes with
              | [ a ], pvb_attributes ->
                begin match v_react.pvb_pat.ppat_desc, v_bg.pvb_pat.ppat_desc with
                  | Ppat_var {txt=name_react;_}, Ppat_var {txt=name_bg;_} ->
                    let pvb_expr_react = handler_args v_react.pvb_expr in
                    let pvb_expr_bg = handler_args v_bg.pvb_expr in
                    let pvb_attributes, vs = match onclose with
                      | [] -> pvb_attributes, []
                      | v :: t -> v_bg.pvb_attributes, {v with pvb_attributes} :: t in
                    let str = {str with pstr_desc = Pstr_value (rflag, (
                        {v_react with pvb_expr = pvb_expr_react } ::
                        {v_bg with pvb_expr = pvb_expr_bg; pvb_attributes } ::
                        vs )) } in
                    (List.rev @@ register_ws ~onclose name_react name_bg a) @ str :: acc
                  | _ -> str :: acc
                end
              | _ -> str :: acc
            end
          | _ -> str :: acc
        end
      (* server main *)
      | Pstr_attribute a when a.attr_name.txt = "server" && kind = Some `server ->
        let expr = server a in
        let loc = a.attr_loc in
        pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:(punit ~loc) ~expr ] :: acc
      (* client service *)
      | Pstr_attribute a when List.mem a.attr_name.txt methods ->
        let service, _, _ = service_value ~client:true a in
        service :: acc
      | _ -> str :: acc
    ) [] str
