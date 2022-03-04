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
        ~lhs:(ppat_variant ~loc name (Option.map (fun _ -> pvar ~loc "x") enc))
        ~rhs:(Option.fold ~none:(Utils.esome (eunit ~loc))
                ~some:(fun _ -> Utils.esome (pexp_tuple ~loc [eunit ~loc; evar ~loc "x"])) enc) ;
      case ~guard:None
        ~lhs:(ppat_any ~loc)
        ~rhs:(Utils.enone ~loc) ] in
  let deselect = Utils.pexp_fun
      (Option.fold ~none:(punit ~loc) ~some:(fun _ -> ppat_tuple ~loc [punit ~loc; pvar ~loc "x"]) enc)
      (pexp_variant ~loc name (Option.map (fun _ -> evar ~loc "x") enc)) in
  pexp_apply ~loc (evar ~loc "EzAPI.Err.make") [
    Labelled "code", eint ~loc code;
    Labelled "name", estring ~loc name;
    Labelled "encoding", encoding;
    Labelled "select", select;
    Labelled "deselect", deselect ]

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
    let enc = Encoding.core ~wrap:false h in
    txt, mk ~loc ~enc ?kind_label ~title txt code
  | _ ->
    Location.raise_errorf ~loc "inherit not handled"

let expressions ?kind_label ~title t =
  let loc = t.ptype_loc in
  match t.ptype_kind, t.ptype_manifest with
  | Ptype_abstract, Some {ptyp_desc=Ptyp_variant (l, _, _); _} ->
    List.map (row ?kind_label ~title) l
  | _ -> Location.raise_errorf ~loc "error cases only from variants"

let str_gen ~loc ~path:_ (rec_flag, l) debug title kind_label =
  let l = List.map (fun t ->
      let loc = t.ptype_loc in
      let cases = expressions ?kind_label ~title t in
      List.map (fun (name, expr) ->
          let pat = ppat_constraint ~loc (pvar ~loc (String.lowercase_ascii name ^ "_case"))
              (ptyp_constr ~loc (Utils.llid ~loc "EzAPI.Err.case") [
                  ptyp_constr ~loc (Utils.llid ~loc t.ptype_name.txt) [] ]) in
          value_binding ~loc ~pat ~expr) cases) l in
  let l = List.flatten l in
  let rec_flag = if List.length l < 2 then Nonrecursive else rec_flag in
  let s = [ pstr_value ~loc rec_flag l ] in
  if debug then Format.printf "%s@." (Pprintast.string_of_structure s);
  s

let () =
  let args_str = Deriving.Args.(
      empty
      +> flag "debug"
      +> flag "title"
      +> arg "kind_label" (estring __)
    ) in
  let str_type_decl = Deriving.Generator.make args_str str_gen in
  Deriving.ignore @@ Deriving.add "err_case" ~str_type_decl
