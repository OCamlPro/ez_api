open Ppxlib
open Ast_builder.Default
open Ppx_common

(** register service/handler *)

let first = ref true

let ppx_dir ~loc =
  if !first then (
    first := false;
    [ pstr_value ~loc Nonrecursive [
          value_binding ~loc ~pat:(pvar ~loc "ppx_dir")
            ~expr:(evar ~loc "EzAPIServer.empty") ] ])
  else []

let register name a =
  let loc = a.attr_loc in
  let ppx_dir = ppx_dir ~loc in
  match a.attr_payload with
  | PStr [ {pstr_desc=Pstr_eval (e, _); _} ] ->
    let register =
      value_binding ~loc ~pat:(pvar ~loc "ppx_dir")
        ~expr:(eapply ~loc (evar ~loc "EzAPIServer.register") [
            e; evar ~loc name; evar ~loc "ppx_dir" ]) in
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
        ~expr:(eapply ~loc (evar ~loc "EzAPIServer.register") [
            evar ~loc service_name; evar ~loc name; evar ~loc "ppx_dir" ]) ] in
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
              pexp_construct ~loc (llid ~loc "EzAPIServer.API") (Some options.dir)
            ]
          ]
        ])
  ]

let impl str =
  List.rev @@ List.fold_left (fun acc str ->
      match str.pstr_desc with
      | Pstr_value (rflag, [ v ]) ->
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
      (* server main *)
      | Pstr_attribute a when a.attr_name.txt = "server" ->
        let expr = server a in
        let loc = a.attr_loc in
        pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:(punit ~loc) ~expr ] :: acc
      | _ -> str :: acc
    ) [] str

let () =
  Driver.register_transformation "ez-api" ~impl
