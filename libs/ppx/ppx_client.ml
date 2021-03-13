open Ppxlib
open Ppx_common

let impl str =
  List.rev @@ List.fold_left (fun acc str ->
      match str.pstr_desc with
      (* client service *)
      | Pstr_attribute a when List.mem a.attr_name.txt methods ->
        let service, _, _ = service_value a in
        service :: acc
      | _ -> str :: acc
    ) [] str

let () =
  Driver.register_transformation "ez-api-client" ~impl
