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
open Ppx_deriving_encoding_lib

let mk ~loc ?enc ?(kind_label="kind") ~title name code =
  let kind_enc = Utils.(enc_apply ~loc "obj1" [
      enc_apply ~loc "req" [
        estring ~loc kind_label;
        enc_apply ~loc "constant" [ estring ~loc name ] ] ]) in
  let encoding = match enc with
    | None -> kind_enc
    | Some enc -> Utils.enc_apply ~loc "merge_objs" [kind_enc; enc] in
  let encoding =
    if title then Utils.enc_apply ~loc "def" [ estring ~loc name; encoding ]
    else encoding in
  let select = pexp_function ~loc [
      case ~guard:None
        ~lhs:(ppat_variant ~loc name (Option.map (fun _ -> [%pat? x]) enc))
        ~rhs:(Option.fold ~none:[%expr Some ()] ~some:(fun _ -> [%expr Some ((), x)]) enc);
      case ~guard:None ~lhs:[%pat? _] ~rhs:[%expr None] ] in
  let deselect = Utils.pexp_fun
      (Option.fold ~none:[%pat? ()] ~some:(fun _ -> [%pat? ((), x)]) enc)
      (pexp_variant ~loc name (Option.map (fun _ -> [%expr x]) enc)) in
  [%expr
    EzAPI.Err.make ~code:[%e eint ~loc code] ~name:[%e estring ~loc name]
      ~encoding:[%e encoding] ~select:[%e select] ~deselect:[%e deselect] ]

let get_int_attr = function
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_integer (s, None)); _}, _); _}] ->
    Some (int_of_string s)
  | _ -> None

let get_code attrs =
  List.fold_left (fun acc a -> match a.attr_name.txt with
      | "code" -> begin match get_int_attr a.attr_payload with
          | Some i -> i
          | None -> acc
        end
      | _ -> acc) 500 attrs

let row ?kind_label ~title prf =
  let loc = prf.prf_loc in
  let code = get_code prf.prf_attributes in
  match prf.prf_desc with
  | Rtag ({txt; loc}, _, []) -> txt, mk ~loc ?kind_label ~title txt code
  | Rtag ({txt; loc}, _, (h :: _)) ->
    let enc = Encoding.core h in
    txt, mk ~loc ~enc ?kind_label ~title txt code
  | _ ->
    Location.raise_errorf ~loc "inherit not handled"

let expressions ?kind_label ~title t =
  let loc = t.ptype_loc in
  match t.ptype_kind, t.ptype_manifest with
  | Ptype_abstract, Some {ptyp_desc=Ptyp_variant (l, _, _); _} ->
    `variant (List.map (row ?kind_label ~title) l)
  | Ptype_open, None -> `type_ext t.ptype_name.txt
  | _ -> Location.raise_errorf ~loc "error cases only from variants and type extension"

let str_gen ~loc ~path:_ (_rec_flag, l) debug title kind_label =
  let l = List.map (fun t ->
      let loc = t.ptype_loc in
      let r = expressions ?kind_label ~title t in
      match r with
      | `variant cases ->
        List.map (fun (name, expr) ->
            let pat = ppat_constraint ~loc (pvar ~loc (String.lowercase_ascii name ^ "_case"))
                [%type: [%t ptyp_constr ~loc (Utils.llid ~loc t.ptype_name.txt) []] EzAPI.Err.case] in
            value_binding ~loc ~pat ~expr) cases
      | `type_ext name ->
        let t = ptyp_constr ~loc (Utils.llid ~loc name) [] in
        let pat = [%pat? ([%p pvar ~loc ("_error_selects_" ^ name)] : (int * ([%t t] -> [%t t] option)) list ref)] in
        let selects = value_binding ~loc ~pat ~expr:[%expr ref []] in
        let pat = [%pat? ([%p pvar ~loc ("_error_cases_" ^ name)] : (int * [%t t] Json_encoding.case) list ref)] in
        let cases = value_binding ~loc ~pat ~expr:[%expr ref []] in
        [ selects; cases ]
    ) l in
  let l = List.flatten l in
  let s = [ pstr_value ~loc Nonrecursive l ] in
  if debug then Format.printf "%a@." Pprintast.structure s;
  s

let attribute_code ~code attrs =
  let c = List.find_map (fun a -> match a.attr_name.txt, a.attr_payload with
      | "code", PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant Pconst_integer (s, _); _ }, _); _ } ] ->
        Some (int_of_string s)
      | _ -> None) attrs in
  match c, code with Some c, _ | _, Some c -> c | _ -> 500

let attribute_nowrap attrs =
  List.find_map (fun a -> match a.attr_name.txt with
      | "nowrap" -> Some false
      | _ -> None) attrs

let str_type_ext ~loc:_ ~path:_ t debug code =
  let loc = t.ptyext_loc in
  let name = Longident.name t.ptyext_path.txt in
  let l = List.filter_map (fun pext ->
      let loc = pext.pext_loc in
      match pext.pext_kind with
      | Pext_decl ([], args, None) ->
        let code = attribute_code ~code pext.pext_attributes in
        let wrap = attribute_nowrap pext.pext_attributes in
        let case = Encoding.resolve_case ~loc @@ Encoding.constructor_label ?wrap ~case:`snake
            ~loc ~name:pext.pext_name.txt ~attrs:pext.pext_attributes args in
        let select = pext.pext_name.txt, (match args with Pcstr_tuple [] -> false | _ -> true) in
        Some (code, case, select)
      | _ -> None
    ) t.ptyext_constructors in
  let cases = elist ~loc @@ List.map (fun (code, case, _) ->
      [%expr [%e eint ~loc code], [%e case]]) l in
  let select_grouped = List.fold_left (fun acc (code, _, select) ->
      match List.assoc_opt code acc with
      | None -> acc @ [code, [ select ]]
      | Some l -> (List.remove_assoc code acc) @ [ code, l @ [ select ] ]
    ) [] l in
  let select_merged cons = pexp_function ~loc (
      (List.map (fun (name, has_arg) ->
           case ~guard:None
             ~lhs:(ppat_alias ~loc (ppat_construct ~loc (Utils.llid ~loc name) (if has_arg then Some [%pat? _] else None)) {txt="x"; loc})
             ~rhs:[%expr Some x]) cons) @ [
        case ~guard:None ~lhs:[%pat? _] ~rhs:[%expr None]
      ]) in
  let selects = elist ~loc @@ List.map (fun (code, cons) ->
      [%expr [%e eint ~loc code], [%e select_merged cons] ]) select_grouped in
  let cases_name = "_error_cases_" ^ name in
  let selects_name = "_error_selects_" ^ name in
  let expr = [%expr
    [%e evar ~loc cases_name] := ![%e evar ~loc cases_name] @ [%e cases];
    [%e evar ~loc selects_name] := ![%e evar ~loc selects_name] @ [%e selects];
  ] in
  let s = [
    pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:[%pat? ()] ~expr ]
  ] in
  if debug then Format.printf "%a@." Pprintast.structure s;
  s

let remove_spaces s =
  let b = Bytes.create (String.length s) in
  let n = String.fold_left (fun i -> function ' ' -> i | c -> Bytes.set b i c; i+1) 0 s in
  Bytes.(to_string @@ sub b 0 n)

let type_ext_err_case ~loc ~typ ?(def=true)code =
  match EzAPI.Error_codes.error code with
  | None -> Location.raise_errorf ~loc "code is not standard"
  | Some name ->
    let enc = [%expr
      Json_encoding.union @@ List.filter_map (fun (code, case) ->
          if code = [%e eint ~loc code] then Some case else None) ![%e evar ~loc ("_error_cases_" ^ typ)]
    ] in
    let enc =
      if not def then enc
      else [%expr Json_encoding.def [%e estring ~loc (remove_spaces name)] [%e enc]] in
    [%expr
      let select = EzAPI.Err.merge_selects @@ List.filter_map (fun (code, case) ->
          if code = [%e eint ~loc code] then Some case else None) ![%e evar ~loc ("_error_selects_" ^ typ)] in
      EzAPI.Err.make ~code:[%e eint ~loc code] ~name:[%e estring ~loc name]
        ~encoding:[%e enc] ~select ~deselect:Fun.id ]

let remove_poly c = match c.ptyp_desc with Ptyp_poly (_, c) -> c | _ -> c

let get_err_case_options ~loc l =
  let code, debug, def = List.fold_left (fun (code, debug, def) (lid, e) -> match Longident.name lid.txt, e.pexp_desc with
      | "code", Pexp_constant Pconst_integer (s, _) -> Some (int_of_string s), debug, def
      | "debug", _ -> code, true, def
      | "nodef", _ -> code, debug, false
      | "def", Pexp_construct ({txt=Lident "false"; _}, None) -> code, debug, false
      | s, _ -> Format.eprintf "%s option not understood@." s; code, debug, def
    ) (None, false, true) l in
  match code with
  | None -> Location.raise_errorf ~loc "code not found"
  | Some code -> code, debug, def

let transform =
  object
    inherit Ast_traverse.map
    method! structure_item it = match it.pstr_desc with
      | Pstr_extension (({txt="err_case"; _}, PStr [{pstr_desc=Pstr_value (_, l); pstr_loc=loc; _}]), _) ->
        let l, debug = List.fold_left (fun (acc, acc_debug) vb ->
            let typ, e, pat = match vb.pvb_expr.pexp_desc, vb.pvb_pat.ppat_desc with
              | Pexp_constraint (e, typ), (Ppat_constraint ({ppat_desc=p; _}, _) | p) ->
                remove_poly typ, e, { vb.pvb_pat with ppat_desc=p }
              | _, Ppat_constraint (p, typ) ->
                remove_poly typ, vb.pvb_expr, p
              | _ -> Location.raise_errorf ~loc "no error type given to derive the error case" in
            let code, debug, def = match e.pexp_desc with
              | Pexp_constant Pconst_integer (s, _) -> int_of_string s, false, true
              | Pexp_record (l, None) -> get_err_case_options ~loc:e.pexp_loc l
              | _ -> Location.raise_errorf ~loc:e.pexp_loc "code not found" in
            let typ = match typ.ptyp_desc with
              | Ptyp_constr ({txt; _}, [])
              | Ptyp_constr ({txt=(Ldot (Ldot (Lident "EzAPI", "Err"), "case") | Ldot (Lident "Err", "case")) ; _}, [
                    { ptyp_desc = Ptyp_constr ({txt; _}, []); _ }
                  ]) -> Longident.name txt
              | _ -> Location.raise_errorf ~loc:typ.ptyp_loc "couldn't find type to derive error case" in
            let expr = type_ext_err_case ~loc ~typ ~def code in
            acc @ [ value_binding ~loc ~pat ~expr ], acc_debug || debug
          ) ([], false) l in
        let it = pstr_value ~loc Nonrecursive l in
        if debug then Format.printf "%a@." Pprintast.structure_item it;
        it
      | _ -> it
  end

let () =
  let open Deriving in
  let args_str = Args.(
      empty
      +> flag "debug"
      +> flag "title"
      +> arg "kind_label" (estring __)
    ) in
  let str_type_decl = Generator.make args_str str_gen in
  let args_type_ext = Args.(empty +> flag "debug" +> arg "code" (eint __)) in
  let str_type_ext = Generator.make args_type_ext str_type_ext in
  ignore @@ add "err_case" ~str_type_decl ~str_type_ext;
  Driver.register_transformation "err_case" ~impl:transform#structure
