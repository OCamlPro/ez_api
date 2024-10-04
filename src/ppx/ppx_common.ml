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

let str_of_expr e = Pprintast.string_of_expression e
let str_of_pat p =
  Pprintast.pattern Format.str_formatter p;
  Format.flush_str_formatter ()
let str_of_structure e = Pprintast.string_of_structure e

let llid ~loc s = {txt=Longident.parse s; loc}
let esome e =
  let loc = e.pexp_loc in
  pexp_construct ~loc (llid ~loc "Some") (Some e)

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

let options ?register ?name loc =
  let register = match register with
    | None -> pexp_construct ~loc (llid ~loc "true") None
    | Some register -> register in
  let name = match name with
    | None -> [%expr None]
    | Some name -> esome (estring ~loc name) in {
  path = pexp_ident ~loc (llid ~loc "EzAPI.Path.root");
  input = empty ~loc; output = empty ~loc; errors = [%expr None]; params = [%expr None];
  section = [%expr None]; name; descr = [%expr None];
  security = [%expr None]; register; input_example = [%expr None]; hide = [%expr None];
  output_example = [%expr None]; error_type = ptyp_constr ~loc (llid ~loc "exn") [];
  security_type = ptyp_constr ~loc (llid ~loc "EzAPI.no_security") [];
  debug = false; directory = None; service = None
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

let string_literal = function
  | Ppxlib.Pconst_string (s, _, _) -> Some s
  | _ -> None

let get_options ~loc ?name ?(client=false) p =
  let register = if not client then None else Some (pexp_construct ~loc (llid ~loc "false") None) in
  match p with
  | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_record (l, _); _}, _); _} ] ->
    let l = List.filter_map (function ({txt=Lident s; loc}, e) -> Some (s, loc, e) | _ -> None) l in
    List.fold_left (fun (name, acc) (s, loc, e) -> match s with
        | "path" -> begin match e.pexp_desc with
            | Pexp_constant cst ->
              begin match string_literal cst with
                | Some s -> name, { acc with path = parse_path ~loc:e.pexp_loc s }
                | _ -> Format.eprintf "path should be a string literal"; name, acc
              end
            | _ -> Format.eprintf "path should be a literal"; name, acc
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
            | Pexp_constant cst ->
              begin match name, string_literal cst with
                | None, Some s -> Some s,  { acc with name = esome e }
                | Some n, _ -> Some n, { acc with name = esome e }
                | _ -> Format.eprintf "name should be a string literal"; name, acc
              end
            | _ ->
              Format.eprintf "name should be a literal";
              name, acc
          end
        | "descr" -> name, { acc with descr = esome e }
        | "security" -> name, { acc with security = esome e; security_type = ptyp_any ~loc }
        | "register" -> name, { acc with register = e }
        | "hide" -> name, { acc with hide = e }
        | "input_example" -> name, { acc with input_example = esome e }
        | "output_example" -> name, { acc with output_example = esome e }
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
          name, { acc with service = Some e; error_type = ptyp_any ~loc; security_type = ptyp_any ~loc }
        | _ -> name, acc) (name, options ?register ?name loc) l
  | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_ident _; _} as e, _); _} ] ->
    let o = options ?register ?name loc in
    name, { o with service = Some e; error_type = ptyp_any ~loc; security_type = ptyp_any ~loc }
  | PStr s ->
    Format.eprintf "attribute not understood %a@." Pprintast.structure s;
    name, options ?register ?name loc
  | _ ->
    Format.eprintf "attribute not understood@.";
    name, options ?register ?name loc

let service_value ?name ?client ~meth ~loc p =
  let meth = pexp_variant ~loc (String.uppercase_ascii meth) None in
  let name, options = get_options ~loc ?name ?client p in
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
      ptyp_constr ~loc (llid ~loc "EzAPI.service") [
        ptyp_any ~loc; ptyp_any ~loc; ptyp_any ~loc; options.error_type;
        options.security_type ] in
    let str = pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr ] in
    if options.debug then Format.printf "%s@." @@ str_of_structure [ str ];
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
    if options.debug then Format.printf "%s@." @@ str_of_structure str;
    str

let register_ws ~onclose react_name bg_name a =
  let loc = a.attr_loc in
  let _, options = get_options ~loc a.attr_payload in
  let ppx_dir = ppx_dir ~loc options.directory in
  let ppx_dir_name = match options.directory with None -> "ppx_dir" | Some s -> s in
  let onclose = match onclose with
    | [] -> [%expr None]
    | [ {pvb_pat = {ppat_desc = Ppat_var {txt; loc}; _}; _} ] -> esome (evar ~loc txt)
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
    if options.debug then Format.printf "%s@." @@ str_of_structure str;
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
  if options.debug then Format.printf "%s@." @@ str_of_structure [ register ];
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
    | [ {pvb_pat = {ppat_desc = Ppat_var {txt; loc}; _}; _} ] -> esome (evar ~loc txt)
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
  if options.debug then Format.printf "%s@." @@ str_of_structure [ register ];
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
        | "catch" -> { acc with catch = esome e }
        | "headers" -> { acc with allow_headers = esome e }
        | "methods" -> { acc with allow_methods = esome e }
        | "origin" -> { acc with allow_origin = esome e }
        | "credentials" -> { acc with allow_credentials = esome e }
        | _ -> acc) (dft (eint ~loc 8080)) l
  | _ -> Location.raise_errorf ~loc "server options not understood"

let server_aux e =
  let loc = e.pexp_loc in
  let options = server_options e in
  [%expr
    EzAPIServer.server ?catch:[%e options.catch] ?allow_headers:[%e options.allow_headers]
      ?allow_methods:[%e options.allow_methods] ?allow_origin:[%e options.allow_origin]
      ?allow_credentials:[%e options.allow_credentials]
      [%e elist ~loc [
          pexp_tuple ~loc [
            options.port;
            pexp_construct ~loc (llid ~loc "EzAPIServerUtils.API") (Some options.dir)
          ] ] ] ]

let server ~loc p =
  match p with
  | PStr [ {pstr_desc=Pstr_eval (e, _); _} ] ->
    [%expr EzLwtSys.run (fun () -> [%e server_aux e])]
  | _ -> Location.raise_errorf ~loc "server options not understood"

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
          | Pstr_value (rflag, [ v ]) when kind <> Some `client ->
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
            let service, _, _ = service_value ~client:true ~meth:a.attr_name.txt ~loc:a.attr_loc a.attr_payload in
            service :: acc
          | Pstr_extension (({txt; loc}, PStr [ { pstr_desc = Pstr_value (_, [ { pvb_expr; pvb_pat= {ppat_desc=Ppat_var {txt=name; _}; _}; _} ]); _} ]), _) when List.mem txt methods ->
            let service, _, _ = service_value ~name ~client:true ~meth:txt ~loc @@ PStr [ pstr_eval ~loc pvb_expr [] ] in
            service :: acc
          | Pstr_extension (({txt; loc}, p), _) when List.mem txt methods ->
            let service, _, _ = service_value ~client:true ~meth:txt ~loc p in
            service :: acc
          | _ -> (self#structure_item it) :: acc
        ) [] str

    method! expression e = match e.pexp_desc with
      | Pexp_extension ({txt="server"; _}, PStr [ { pstr_desc = Pstr_eval (e, _); _} ]) when kind = Some `server ->
        server_aux e
      | _ -> super#expression e
  end

let impl ?kind str = (transform ?kind ())#structure str
