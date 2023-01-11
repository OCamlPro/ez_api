(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module IntMap = Map.Make(struct type t = int let compare = compare end)

type _ case =
    Case : {
      code : int;
      name : string;
      encoding : 'a Json_encoding.encoding;
      select : 'b -> 'a option;
      deselect: 'a -> 'b;
    } -> 'b case

let make ~code ~name ~encoding ~select ~deselect =
  Case {code; name; encoding; select; deselect}

let merge_errs_same_code ?definitions_path errors =
  let code_map =
    List.fold_left (fun acc (Case { code; _ } as c) ->
        let encs = match IntMap.find_opt code acc with
          | Some l -> l
          | None -> [] in
        IntMap.add code (c :: encs) acc
      ) IntMap.empty errors in
  IntMap.map (fun l ->
      let encoding = match l with
        | [Case { encoding; select; deselect; _ }] ->
          Json_encoding.conv
            (fun x -> match select x with
               | None -> assert false
               | Some x -> x)
            deselect
            encoding
        | _ ->
          let err_cases =
            List.map (function Case { encoding;  select;  deselect; _} ->
                Json_encoding.case encoding select deselect
              ) l in
          Json_encoding.union err_cases in
      lazy (Json_encoding.schema ?definitions_path encoding)
    ) code_map
  |> IntMap.bindings

let catch_all_error_case () = Case {
    code = 500;
    name = "AnyError";
    encoding = (
      let open Json_encoding in
      conv
        (fun x ->
           let s =
             Marshal.to_string x [Marshal.No_sharing]
             |> Digest.string |> Digest.to_hex in
           Format.eprintf "No corresponding error case (MD5 %s)@." s;
           ((), s)
        )
        (fun ((), _) ->
           failwith "Cannot parse from undeclared error")
        (obj2
           (req "error" (constant "Server Error"))
           (req "digest" string))
    );
    select = (fun x -> Some x);
    deselect = (fun x -> x);
  }

let get ~code l =
  match List.find_all (function Case { code = c; _ } -> c = code) l with
  | [] -> None
  | [ Case { encoding = enc; select; deselect; _ } ] ->
    Some (Json_encoding.conv
            (fun x -> match select x with
               | None -> assert false
               | Some x -> x)
            deselect
            enc)
  | l ->
    let cases =
      List.map (function Case { encoding = enc; select; deselect; _ } ->
          Json_encoding.case enc select deselect
        ) l in
    Some (Json_encoding.union cases)
