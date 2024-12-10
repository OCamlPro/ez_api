(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type empty = [ `OPTIONS | `HEAD ]
type t = [ `GET | `POST | `PUT | `DELETE | `PATCH ]
type all = [ t | empty ]

let to_string : [< all ] -> string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"
  | `OPTIONS -> "OPTIONS"
  | `HEAD -> "HEAD"

let of_string s : [> all | `Other of string ] = match s with
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "PUT" -> `PUT
  | "POST" -> `POST
  | "PATCH" -> `PATCH
  | "DELETE" -> `DELETE
  | "OPTIONS" -> `OPTIONS
  | s -> `Other s

let headers l =
  let meths = String.concat "," @@ List.map to_string l in
  [ "access-control-allow-methods", meths ]
