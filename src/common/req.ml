(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module StringMap = Map.Make(String)

type version = [ `HTTP_1_0 | `HTTP_1_1 ]

type t = {
  req_version : version;
  req_time : float;
  req_headers : string list StringMap.t;
  req_params : string list StringMap.t;
  req_id : Uuidm.t
}

let dummy = {
  req_version = `HTTP_1_1;
  req_time = 0.;
  req_headers = StringMap.empty;
  req_params = StringMap.empty;
  req_id = Uuidm.nil
}

let add_params req params =
  let req_params =
    List.fold_left (fun map (arg, l1) ->
        match StringMap.find_opt arg map with
        | Some l0 -> StringMap.add arg (l0 @ l1) map
        | None -> StringMap.add arg l1 map
      ) req.req_params params in
  { req with req_params }

let request ?(version=`HTTP_1_1) ?(headers=StringMap.empty) ?(time=0.) uri =
  let path_str = Uri.path uri in
  let path = List.filter (fun s -> s <> "") @@ String.split_on_char '/' path_str in
  let req_id = Uuidm.v4_gen (Random.get_state ()) () in
  let req = { req_params = StringMap.empty; req_headers = headers;
              req_version = version; req_time = time; req_id } in
  let content_type = match StringMap.find_opt "content-type" headers with
    | Some (c :: _) -> Some c
    | _ -> None in
  path_str, path, content_type, add_params req (Uri.query uri)

let find_params p req = StringMap.find_opt p.Param.param_id req.req_params

let find_param p req = match find_params p req with
  | None | Some [] -> None
  | Some values -> Some (String.concat "," values)

let find_header_security key headers =
  let key = String.uncapitalize_ascii key in
  match StringMap.fold (fun k v acc -> match acc with
      | Some _ -> acc
      | None -> if String.uncapitalize_ascii k = key then Some v else acc) headers None with
  | None | Some [] -> None
  | Some l -> Some (String.concat "," l)

let find_authorization_security kind headers =
  match find_header_security "authorization" headers with
  | None -> None
  | Some v -> match String.split_on_char ' ' v with
    | [ k; token] when String.uncapitalize_ascii k = kind -> Some token
    | _ -> None

let find_security (sec: [< Security.scheme]) req = match sec with
  | `Nosecurity _ -> None
  | `Bearer _ -> find_authorization_security "bearer" req.req_headers
  | `Basic _ -> find_authorization_security "basic" req.req_headers
  | `Header {Security.name; _} | `Cookie ({Security.name; _}, _) -> find_header_security name req.req_headers
  | `Query {Security.name; _} -> find_param name req

let find_securities secs req = List.find_map (fun sec -> find_security sec req) secs
