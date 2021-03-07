module StringMap = Map.Make(String)

type version = [ `HTTP_1_0 | `HTTP_1_1 ]

type t = {
  req_version : version;
  req_headers : string list StringMap.t;
  req_params : string list StringMap.t;
}

let dummy = {
  req_version = `HTTP_1_1;
  req_headers = StringMap.empty;
  req_params = StringMap.empty }

let add_params req params =
  let req_params =
    List.fold_left (fun map (arg, l1) ->
        match StringMap.find_opt arg map with
        | Some l0 -> StringMap.add arg (l0 @ l1) map
        | None -> StringMap.add arg l1 map
      ) req.req_params params in
  { req with req_params }

let request ?(version=`HTTP_1_1) ?(headers=StringMap.empty) uri =
  let path_str = Uri.path uri in
  let path = List.filter (fun s -> s <> "") @@ String.split_on_char '/' path_str in
  let req = { req_params = StringMap.empty; req_headers = headers; req_version = version } in
  let content_type = match StringMap.find_opt "content-type" headers with
    | Some (c :: _) -> Some c
    | _ -> None in
  path_str, path, content_type, add_params req (Uri.query uri)

let find_params p req = StringMap.find_opt p.Param.param_id req.req_params

let find_param p req = match find_params p req with
  | None -> None
  | Some values -> Some (String.concat "," values)
