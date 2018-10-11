
val int64 : int64 Json_encoding.encoding

exception DestructError

val destruct :  'a Json_encoding.encoding -> string -> 'a
val construct : ?compact:bool -> 'a Json_encoding.encoding -> 'a -> string

(* Use this function in Javascript to avoid the non-tailrec Ezjsonm.
   This is done automatically by calling `EzEncodingJS.init ()`
   from library `ezAPI-js`.
 *)
val json_of_string : (string -> Json_repr.ezjsonm) ref


(* The `string` encoding works only for utf8 strings without '"' for
    example. This one works always. *)
val encoded_string : string Json_encoding.encoding

val init : unit -> unit
