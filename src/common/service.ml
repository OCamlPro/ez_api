(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module IO = struct
  type _ io =
    | Empty : unit io
    | Json : 'a Json_encoding.encoding -> 'a io
    | Raw : Mime.t list -> string io

  let to_string :
    type a. a io -> a -> string =
    fun io a -> match io with
      | Empty -> ""
      | Raw _ -> a
      | Json enc -> EzEncoding.construct enc a

  let from_string :
    type a. a io -> (a -> 'b) -> string -> ('b, [> EzEncoding.destruct_error ]) result =
    fun io f s -> match io with
      | Empty -> Ok (f ())
      | Raw _ -> Ok (f s)
      | Json enc -> match EzEncoding.destruct_res enc s with
        | Error e -> Error e
        | Ok a -> Ok (f a)

  let res_from_string :
    type a. a io -> (a Json_encoding.encoding -> (a, 'e) result Json_encoding.encoding) ->
    ((a, 'e) result -> 'b) -> string ->
    ('b, [> EzEncoding.destruct_error ]) result =
    fun io fenc f s -> match io with
      | Empty -> Ok (f (Ok ()))
      | Raw _ -> Ok (f (Ok s))
      | Json enc -> match EzEncoding.destruct_res (fenc enc) s with
        | Error e -> Error e
        | Ok a -> Ok (f a)

end

type ('args, 'input, 'output, 'error, 'security) t = {
  path : (Req.t, 'args) Path.t;
  input : 'input IO.io;
  output : 'output IO.io;
  errors : 'error Err.case list;
  meth : Meth.t;
  params : Param.t list;
  security: ([< Security.scheme ] as 'security) list;
}

let make ?(meth : Meth.t =`GET) ?(params=[]) ?(security=[]) ?(errors=[]) ~input ~output path =
  { path ; input ; output; errors; meth; params; security }

let input s = s.input
let output s = s.output
let errors s = s.errors
let errors_encoding s =
  Json_encoding.union @@
  List.map (function Err.Case { encoding;  select;  deselect; _} ->
      Json_encoding.case encoding select deselect
    ) (s.errors @ [Err.catch_all_error_case ()])

let meth s = s.meth
let path s = s.path
let security s = s.security
let params s = s.params

let error s ~code = Err.get ~code s.errors
