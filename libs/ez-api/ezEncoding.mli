
exception DestructError

val destruct :  'a Json_encoding.encoding -> string -> 'a
val construct : 'a Json_encoding.encoding -> 'a -> string

(* Use this function in Javascript to avoid the non-tailrec Ezjsonm.
   This is done automatically by calling `EzEncodingJS.init ()`
   from library `ezAPI-js`.
 *)
val json_of_string : (string -> Json_repr.ezjsonm) ref


(* The `string` encoding works only for utf8 strings without '"' for
    example. This one works always. *)
val encoded_string : string Json_encoding.encoding

val int64 : Int64.t Json_encoding.encoding

val register :
  ?name:string ->
  ?descr:string -> 'a Json_encoding.encoding -> unit

val merge_objs :
  ?name:string ->
  ?descr:string ->
  'a Json_encoding.encoding ->
  'b Json_encoding.encoding -> ('a * 'b) Json_encoding.encoding
