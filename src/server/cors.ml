open EzAPI

let allow_origin_name = "access-control-allow-origin"
let allow_headers_name = "access-control-allow-headers"
let allow_methods_name = "access-control-allow-methods"
let allow_credentials_name = "access-control-allow-credentials"

let allow_methods_header l =
  let meths = StringSet.of_list @@ List.map Meth.to_string l in
  StringMap.singleton allow_methods_name meths

let allow_credentials_header = function
  | true -> StringMap.singleton allow_credentials_name (StringSet.singleton "true")
  | _ -> StringMap.empty

let insert acc (k, v) = match StringMap.find_opt k acc with
  | None -> StringMap.add k v acc
  | Some v2 -> StringMap.add k (StringSet.union v2 v) acc

let union s1 s2 =
  StringMap.union (fun k v1 v2 ->
      if k = allow_credentials_name && (StringSet.mem "true" v1 || StringSet.mem "true" v2) then
        Some (StringSet.singleton "true")
      else if k = allow_credentials_name then None
      else if k = allow_origin_name && (StringSet.mem "*" v1 || StringSet.mem "*" v2) then
        Some (StringSet.singleton "*")
      else Some (StringSet.union v1 v2)) s1 s2

let to_list s =
  List.filter_map (fun (k, v) ->
      match StringSet.elements v with
      | [] -> None
      | l -> Some (k, String.concat ", " l)) @@ StringMap.bindings s

let of_list l =
  List.fold_left (fun acc (k, v) ->
      let v = StringSet.of_list @@ List.map String.trim @@ String.split_on_char ',' v in
      StringMap.add (String.lowercase_ascii k) v acc
    ) StringMap.empty l

type allow_kind = [ `all | `origin | `default | `custom of string list ]

let merge_headers_allow_origin ?origin headers kind =
  match kind with
  | `none -> headers
  | `origin -> (match origin with None -> headers | Some o -> headers @ [ allow_origin_name, String.concat "," o ])
  | `all | `default -> List.remove_assoc allow_origin_name headers @ [ allow_origin_name, "*" ]
  | `custom l -> match List.assoc_opt allow_origin_name headers with
    | None -> headers @ [ allow_origin_name, String.concat "," l ]
    | Some "*" -> (List.remove_assoc allow_origin_name headers) @ [ allow_origin_name, String.concat "," l ]
    | Some v -> (List.remove_assoc allow_origin_name headers) @ [ allow_origin_name, String.concat "," (v :: l) ]
