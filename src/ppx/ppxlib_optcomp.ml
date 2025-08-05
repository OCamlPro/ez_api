open Ppxlib

let keep attrs =
  match List.find_map (fun a -> if a.attr_name.txt = "if" then Some a.attr_payload else None) attrs with
  | Some PStr [ {pstr_desc=Pstr_eval ({
    pexp_desc=Pexp_apply ({
      pexp_desc=Pexp_ident {txt=Lident cmp; _}; _}, [
        Nolabel, {pexp_desc=Pexp_ident {txt=Lident "ast_version"; _}; _};
        Nolabel, {pexp_desc=Pexp_constant Pconst_integer (v, None); _}
      ]); _}, _); _} ] ->
    let f = match cmp with
      | "=" -> (=) | ">" -> (>) | ">=" -> (>=) | "<" -> (<) | "<=" -> (<=)
      | "<>" -> (<>) | _ -> (fun _ _ -> true) in
    f Selected_ast.version (int_of_string v)
  | _ -> true

let rec filter_pattern = function
  | { ppat_desc = Ppat_or (p1, p2); _ } as p ->
    (match filter_pattern p1, filter_pattern p2 with
     | None, None -> None
     | Some p1, None -> Some p1
     | None, Some p2 -> Some p2
     | Some p1, Some p2 -> Some { p with ppat_desc = Ppat_or (p1, p2) })
  | { ppat_attributes; _ } as p ->
    if keep ppat_attributes then Some p else None


let transform = object inherit Ast_traverse.map as super
  method! structure s =
    let s = List.filter_map (fun it -> match it.pstr_desc with
      | Pstr_value (flag, l) ->
        let l = List.filter (fun vb -> keep vb.pvb_attributes) l in
        (match l with [] -> None | _ -> Some { it with pstr_desc = Pstr_value (flag, l) })
      | _ -> Some it
    ) s in
    super#structure s

  method! cases l =
    let l = List.filter_map (fun c ->
      let p = filter_pattern c.pc_lhs in
      Option.map (fun pc_lhs -> { c with pc_lhs }) p
    ) l in
    super#cases l

  method! expression e =
    match e.pexp_desc with
    | Pexp_let (flag, l, end_) ->
      let l = List.filter (fun vb -> keep vb.pvb_attributes) l in
      let e = match l with [] -> end_ | _ -> { e with pexp_desc = Pexp_let (flag, l, end_) } in
      super#expression e
    | _ -> super#expression e
end

let () = Driver.register_transformation ~impl:transform#structure "ppxlib_optcomp"
