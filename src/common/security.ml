(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type uninhabited = |
type none = [ `Nosecurity of uninhabited ]
type 'a apikey = { ref_name : string; name : 'a }
type bearer_desc = { bearer_name : string ; format : string option }
type basic_desc = { basic_name : string }
type bearer = [ `Bearer of bearer_desc ]
type basic = [ `Basic of basic_desc ]
type header = [ `Header of string apikey ]
(* cookie name * max age atribute (defaults to 1 day) *)
type cookie = [ `Cookie of string apikey * int64 option ]
type query = [ `Query of Param.t apikey ]
type scheme = [
  | none
  | basic
  | bearer
  | header
  | cookie
  | query ]

let unreachable = function (_ : uninhabited) -> .

let ref_name = function
  | `Nosecurity u -> unreachable u
  | `Basic { basic_name = ref_name }
  | `Bearer { bearer_name = ref_name; format=_  }
  | `Cookie ({ ref_name; name=_ }, _ )
  | `Header { ref_name; name=_ }
  | `Query { ref_name; name=_ } -> ref_name

let params (l : [< scheme ] list) =
  List.fold_left (fun acc -> function
      | `Query { name = param ; _ } -> param :: acc
      | `Nosecurity _ | `Basic _ | `Bearer _
      | `Header _ | `Cookie _  -> acc
    ) [] l

module StringSet = Set.Make(String)

let headers (sec : [< scheme ] list) =
  List.fold_left (fun headers -> function
      | `Nosecurity _ -> headers
      | `Basic _ | `Bearer _ -> StringSet.add "Authorization" headers
      | `Query _ -> headers
      | `Header { name; _ } -> StringSet.add name headers
      | `Cookie _ -> StringSet.add "Cookie" headers
    ) StringSet.empty sec

let header s =
  match StringSet.elements s with
  | [] -> []
  | l -> ["access-control-allow-headers", String.concat ", " l]
