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
let global_base = ref false

let remove_poly c = match c.ptyp_desc with
  | Ptyp_poly ([], c) -> c
  | _ -> c

let remove_constraint e = match e.pexp_desc with
  | Pexp_constraint (e, _) -> e
  | _ -> e

let extract_list_type = function
  | None -> [%type: _]
  | Some t ->
    let t = remove_poly t in
    match t.ptyp_desc with
    | Ptyp_constr ({txt=(Lident "list" | Ldot (Lident "List", "t")); _}, [ c ]) -> c
    | _ -> t

let set_global_errors ?typ e =
  let loc = e.pexp_loc in
  global_errors := [%expr Some [%e remove_constraint e]];
  global_error_type := extract_list_type typ

let set_global_security ?typ e =
  let loc = e.pexp_loc in
  global_security := [%expr Some [%e remove_constraint e]];
  global_security_type := extract_list_type typ

let set_global_base e =
  let loc = e.pexp_loc in
  global_base := true;
  let e = match e.pexp_desc with
    | Pexp_constant Pconst_string (s, _, _) -> [%expr EzAPI.BASE [%e estring ~loc s]]
    | _ -> e in
  [%stri let ezreq_base = ref [%e e]]

let set_globals l =
  List.fold_left (fun acc ({txt; _}, e) ->
      let name = Longident.name txt in
      match name with
      | "errors" -> set_global_errors e; acc
      | "security" -> set_global_security e; acc
      | "base" -> Some (set_global_base e)
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
  let path ~loc s = pexp_ident ~loc {txt=Longident.parse ("EzAPI.Path." ^ s); loc} in
  let l = String.split_on_char '/' s in
  let l = List.filter (fun s -> s <> "") l in
  List.fold_left (fun (acc, n) s ->
      match String.get s 0 with
      | '{' ->
        let e = parse_arg ~loc String.(sub s 1 (length s - 2)) in
        eapply ~loc (path ~loc "add_arg") [ acc; e ], n+1
      | _ -> eapply ~loc (path ~loc "add_suffix") [ acc; estring ~loc s ], n
    ) (path ~loc "root", 0) l

let string_literal = function
  | Ppxlib.Pconst_string (s, _, _) -> Some s
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
        | "security" -> name, { acc with security = [%expr Some [%e e]]; security_type = [%type: _] }
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
    let expr = pexp_apply ~loc (evar ~loc "EzAPI.raw_service") [
        Optional "section", options.section;
        Optional "name", options.name;
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

(** register service/handler *)

let first = ref true

let ppx_dir ~loc dir =
  if !first && dir = None then (
    first := false;
    [ pstr_value ~loc Nonrecursive [
          value_binding ~loc ~pat:(pvar ~loc "ppx_dir")
            ~expr:(evar ~loc "EzAPIServerUtils.empty") ] ])
  else []

let register name a =
  let loc = a.attr_loc in
  let _, options = get_options ~loc a.attr_payload in
  let ppx_dir = ppx_dir ~loc options.directory in
  let ppx_dir_name = match options.directory with None -> "ppx_dir" | Some s -> s in
  match options.service with
  | None -> Location.raise_errorf ~loc "service not defined"
  | Some e ->
    let register =
      value_binding ~loc ~pat:(pvar ~loc ppx_dir_name)
        ~expr:(eapply ~loc (evar ~loc "EzAPIServerUtils.register") [
            e; evar ~loc name; evar ~loc ppx_dir_name ]) in
    let str = ppx_dir @ [ pstr_value ~loc Nonrecursive [ register ] ] in
    if options.debug then Format.printf "%a@." Pprintast.structure str;
    str

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
    str

let process name a =
  let loc = a.attr_loc in
  let service_name = if name = "handler" then "service" else name ^ "_s" in
  let service, service_name, options = service_value ~name:service_name ~meth:a.attr_name.txt ~loc a.attr_payload in
  let ppx_dir = ppx_dir ~loc options.directory in
  let ppx_dir_name = match options.directory with None -> "ppx_dir" | Some s -> s in
  let register =
    pstr_value ~loc Nonrecursive [
      value_binding ~loc ~pat:(pvar ~loc ppx_dir_name)
        ~expr:(eapply ~loc (evar ~loc "EzAPIServerUtils.register") [
            evar ~loc service_name; evar ~loc name; evar ~loc ppx_dir_name ]) ] in
  if options.debug then Format.printf "%a@." Pprintast.structure_item register;
  ppx_dir @ [ service; register ]

let process_ws ~onclose react_name bg_name a =
  let loc = a.attr_loc in
  let service_name =  react_name ^ "_s" in
  let service, service_name, options =
    service_value ~name:service_name ~meth:"get" ~loc a.attr_payload in
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

let request_value ~meth ~name ~loc ?(input=true) options =
  let pat = pvar ~loc (meth ^ "_" ^ name) in
  let f e = [%expr fun ?headers ?params ?msg -> [%e e]] in
  let f, input_expr, url_encode_expr, post_expr =
    if input then
      (fun e -> f [%expr fun ?url_encode ~input -> [%e e]]), [%expr input], [%expr url_encode], [%expr None]
    else (fun e -> f [%expr fun ?post -> [%e e]]), [%expr ()], [%expr None], [%expr post] in
  let service = evar ~loc (name ^ "_s") in
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
      EzReq_lwt.wrap @@ EzReq_lwt.request ?headers ?params ?msg ?post:[%e post_expr]
        ?url_encode:[%e url_encode_expr] ~input:[%e input_expr] base
        [%e service] [%e args_expr options.nargs]
    ] in
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
                    let pvb_expr = handler_args v.pvb_expr in
                    let it = {it with pstr_desc = Pstr_value (rflag, [ {v with pvb_expr; pvb_attributes }])} in
                    (List.rev @@ process name a) @ it :: acc
                  | _ ->
                    (self#structure_item it) :: acc
                end
              (* link service *)
              | [], attributes ->
                begin match List.partition (fun a -> a.attr_name.txt = "service") attributes with
                  | [ a ], pvb_attributes ->
                    begin match v.pvb_pat.ppat_desc with
                      | Ppat_var {txt=name;_} ->
                        let pvb_expr = handler_args v.pvb_expr in
                        let it = {it with pstr_desc = Pstr_value (rflag, [ {v with pvb_expr; pvb_attributes }])} in
                        (List.rev @@ register name a) @ it :: acc
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
                    let pvb_expr_react = handler_args v_react.pvb_expr in
                    let pvb_expr_bg = handler_args v_bg.pvb_expr in
                    let pvb_attributes, vs = match onclose with
                      | [] -> pvb_attributes, []
                      | v :: t -> v_bg.pvb_attributes, {v with pvb_attributes} :: t in
                    let it = {it with pstr_desc = Pstr_value (rflag, (
                        {v_react with pvb_expr = pvb_expr_react } ::
                        {v_bg with pvb_expr = pvb_expr_bg; pvb_attributes } ::
                        vs )) } in
                    (List.rev @@ process_ws ~onclose name_react name_bg a) @ it :: acc
                  | _ -> (self#structure_item it) :: acc
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
                        let it = {it with pstr_desc = Pstr_value (rflag, (
                            {v_react with pvb_expr = pvb_expr_react } ::
                            {v_bg with pvb_expr = pvb_expr_bg; pvb_attributes } ::
                            vs )) } in
                        (List.rev @@ register_ws ~onclose name_react name_bg a) @ it :: acc
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
            let loc = a.attr_loc in
            let options = { (default_options loc) with register = [%expr false] } in
            let service, _, _ = service_value ~options ~meth:a.attr_name.txt ~loc:a.attr_loc a.attr_payload in
            service :: acc
          | Pstr_extension (({txt; loc}, PStr [ { pstr_desc = Pstr_value (_, [ { pvb_expr; pvb_pat= {ppat_desc=Ppat_var {txt=name; _}; _}; _} ]); _} ]), _) when List.mem txt methods ->
            let options = { (default_options loc) with register = [%expr false] } in
            let service, _, _ = service_value ~name ~options ~meth:txt ~loc @@ PStr [ pstr_eval ~loc pvb_expr [] ] in
            service :: acc
          | Pstr_extension (({txt; loc}, p), _) when List.mem txt methods ->
            let options = { (default_options loc) with register = [%expr false] } in
            let service, _, _ = service_value ~options ~meth:txt ~loc p in
            service :: acc
          (* global errors and security *)
          | Pstr_extension (({txt="service"; _}, PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_record (l, _); _}, _); _} ]), _) ->
            let base = set_globals l in
            begin match base, kind with Some it, Some `request -> it :: acc | _ -> acc end
          | Pstr_extension (({txt="service"; _}, PStr [ {pstr_desc=Pstr_value (_, l); _} ]), _) ->
            let base = List.fold_left (fun acc vb ->
                match vb.pvb_pat.ppat_desc with
                | Ppat_var {txt="errors"; _} -> set_global_errors vb.pvb_expr; acc
                | Ppat_var {txt="security"; _} -> set_global_security vb.pvb_expr; acc
                | Ppat_var {txt="base"; _} -> Some (set_global_base vb.pvb_expr)
                | Ppat_constraint ({ppat_desc = Ppat_var {txt="errors"; _}; _}, typ) ->
                  set_global_errors ~typ vb.pvb_expr; acc
                | Ppat_constraint ({ppat_desc = Ppat_var {txt="security"; _}; _}, typ) ->
                  set_global_security ~typ vb.pvb_expr; acc
                | _ -> acc) None l in
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
                let input, output, with_input = match meth with
                  | "get" | "put" -> [%expr EzAPI.Empty], [%expr EzAPI.Json [%e evar ~loc enc_name]], false
                  | _ -> [%expr EzAPI.Json [%e evar ~loc enc_name]], [%expr EzAPI.Empty], true in
                let options = { (default_options loc) with register = [%expr false]; input; output } in
                let name = t.ptype_name.txt ^ "_s" in
                let service, _, options = service_value ~name ~options ~meth ~loc a.attr_payload in
                let enc_value = [%stri let [%p pvar ~loc enc_name] = [%e enc]] in
                let acc = service :: enc_value :: it :: acc in
                match kind with
                | Some `request -> request_value ~loc ~meth ~name:t.ptype_name.txt ~input:with_input options :: acc
                | _ -> acc
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
                let name = t_input.ptype_name.txt ^ "_s" in
                let service, _, options = service_value ~name ~options ~meth ~loc a.attr_payload in
                let input_enc_value = [%stri let [%p pvar ~loc input_enc_name] = [%e input_enc]] in
                let output_enc_value = [%stri let [%p pvar ~loc output_enc_name] = [%e output_enc]] in
                if options.debug then Format.printf "%a@." Pprintast.structure [ input_enc_value; output_enc_value ];
                let acc = service :: output_enc_value :: input_enc_value :: it :: acc in
                match kind with
                | Some `request -> request_value ~loc ~meth ~name:t_input.ptype_name.txt options :: acc
                | _ -> acc
            end
          | _ -> (self#structure_item it) :: acc
        ) [] str

    method! expression e = match e.pexp_desc with
      | Pexp_extension ({txt="server"; _}, PStr [ { pstr_desc = Pstr_eval (e, _); _} ]) when kind = Some `server ->
        server_aux e
      | Pexp_extension (({txt; loc}, PStr [ {
          pstr_desc = Pstr_eval ({
              pexp_desc=Pexp_let (_, [ { pvb_expr; pvb_pat= {ppat_desc=Ppat_var {txt=name; _}; _}; _} ], e); _}, _); _} ])) when List.mem txt methods ->
        let options = { (default_options loc) with register = [%expr false] } in
        let _, _, pat, expr = service_expr ~name ~options ~meth:txt ~loc @@ PStr [ pstr_eval ~loc pvb_expr [] ] in
        let e = self#expression e in
        if options.debug then Format.printf "%a@." Pprintast.expression expr;
        [%expr let [%p pat] = [%e expr] in [%e e]]
      | _ -> super#expression e
  end

let impl ?kind str = (transform ?kind ())#structure str

let deriver_str_gen kind meth ~loc ~path:_ (rec_flag, l) path input output errors params section name
    descr security register hide input_example output_example debug =
  let options = default_options loc in
  let sname = match l with t :: _ -> Some (t.ptype_name.txt ^ "_s") | [] -> None in
  let aux e = match e.pexp_desc with
    | Pexp_construct ({txt=Lident "::"; _}, _) -> raw e
    | _ -> [%expr EzAPI.Json [%e e]] in
  let input, output, tname = match meth, l with
    | _, [ t_input; t_output ] when rec_flag = Recursive ->
      [%expr EzAPI.Json ([%e evar ~loc (t_input.ptype_name.txt ^ "_enc")] ())],
      [%expr EzAPI.Json ([%e evar ~loc (t_output.ptype_name.txt ^ "_enc")] ())],
      t_input.ptype_name.txt
    | _, [ t_input; t_output ] ->
      [%expr EzAPI.Json [%e evar ~loc (t_input.ptype_name.txt ^ "_enc")]],
      [%expr EzAPI.Json [%e evar ~loc (t_output.ptype_name.txt ^ "_enc")]],
      t_input.ptype_name.txt
    | ("get" | "put"), t :: _ ->
      Option.fold ~none:options.input ~some:aux input,
      [%expr EzAPI.Json [%e evar ~loc (t.ptype_name.txt ^ "_enc")]],
      t.ptype_name.txt
    |  _, t :: _ ->
      [%expr EzAPI.Json [%e evar ~loc (t.ptype_name.txt ^ "_enc")]],
      Option.fold ~none:options.output ~some:aux output,
      t.ptype_name.txt
    | _ ->
      Option.fold ~none:options.input ~some:aux input,
      Option.fold ~none:options.output ~some:aux output,
      "default" in
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
    name = Option.fold ~none:options.name ~some name;
    descr = Option.fold ~none:options.descr ~some descr;
    security; security_type;
    register = Option.value ~default:[%expr false] register;
    hide = Option.value ~default:options.hide hide;
    input_example = Option.fold ~none:options.input_example ~some input_example;
    output_example = Option.fold ~none:options.output_example ~some output_example;
    debug; nargs;
  } in
  let s, _, options = service_value ~meth ~loc ~options ?name:sname ~parse_options:false (PStr []) in
  match kind with
  | Some `request -> [ s; request_value ~loc ~meth ~name:tname options ]
  | _ -> [ s ]

let derivers kind =
  let open Ppxlib.Deriving in
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
