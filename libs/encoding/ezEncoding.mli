
exception DestructError

val destruct :  'a Json_encoding.encoding -> string -> 'a
val construct : ?compact:bool -> 'a Json_encoding.encoding -> 'a -> string

(* Use this function in Javascript to avoid the non-tailrec Ezjsonm.
   This is done automatically by calling `EzEncodingJS.init ()`
   from library `ezAPI-js`.
 *)
val json_of_string : (string -> Json_repr.ezjsonm) ref
val string_of_json : (?minify:bool -> Json_repr.ezjsonm -> string) ref

module Ezjsonm : sig
  val from_string : string -> Json_repr.ezjsonm
  val to_string : ?minify:bool -> Json_repr.ezjsonm -> string
end

(* The `string` encoding works only for utf8 strings without '"' for
    example. This one works always. *)
val encoded_string : string Json_encoding.encoding

val init : unit -> unit

val obj11 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k)
    Json_encoding.encoding

val obj12 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l)
    Json_encoding.encoding

val obj13 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm)
    Json_encoding.encoding

val obj14 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n)
    Json_encoding.encoding

val obj15 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o)
    Json_encoding.encoding

val obj16 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  'p Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o * 'p)
    Json_encoding.encoding

val obj17 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  'p Json_encoding.field ->
  'q Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o * 'p * 'q)
    Json_encoding.encoding

val obj18 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  'p Json_encoding.field ->
  'q Json_encoding.field ->
  'r Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o * 'p * 'q * 'r)
    Json_encoding.encoding

val obj19 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  'p Json_encoding.field ->
  'q Json_encoding.field ->
  'r Json_encoding.field ->
  's Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o * 'p * 'q * 'r * 's)
    Json_encoding.encoding

val obj20 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  'p Json_encoding.field ->
  'q Json_encoding.field ->
  'r Json_encoding.field ->
  's Json_encoding.field ->
  't Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o * 'p * 'q * 'r * 's * 't)
    Json_encoding.encoding

val obj21 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  'p Json_encoding.field ->
  'q Json_encoding.field ->
  'r Json_encoding.field ->
  's Json_encoding.field ->
  't Json_encoding.field ->
  'u Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o * 'p * 'q * 'r * 's * 't * 'u)
    Json_encoding.encoding

val obj22 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  'p Json_encoding.field ->
  'q Json_encoding.field ->
  'r Json_encoding.field ->
  's Json_encoding.field ->
  't Json_encoding.field ->
  'u Json_encoding.field ->
  'v Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o * 'p * 'q * 'r * 's * 't * 'u * 'v)
    Json_encoding.encoding

val obj23 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  'p Json_encoding.field ->
  'q Json_encoding.field ->
  'r Json_encoding.field ->
  's Json_encoding.field ->
  't Json_encoding.field ->
  'u Json_encoding.field ->
  'v Json_encoding.field ->
  'w Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o * 'p * 'q * 'r * 's * 't * 'u * 'v * 'w)
    Json_encoding.encoding

val obj24 :
  'a Json_encoding.field ->
  'b Json_encoding.field ->
  'c Json_encoding.field ->
  'd Json_encoding.field ->
  'e Json_encoding.field ->
  'f Json_encoding.field ->
  'g Json_encoding.field ->
  'h Json_encoding.field ->
  'i Json_encoding.field ->
  'j Json_encoding.field ->
  'k Json_encoding.field ->
  'l Json_encoding.field ->
  'm Json_encoding.field ->
  'n Json_encoding.field ->
  'o Json_encoding.field ->
  'p Json_encoding.field ->
  'q Json_encoding.field ->
  'r Json_encoding.field ->
  's Json_encoding.field ->
  't Json_encoding.field ->
  'u Json_encoding.field ->
  'v Json_encoding.field ->
  'w Json_encoding.field ->
  'x Json_encoding.field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm *
   'n * 'o * 'p * 'q * 'r * 's * 't * 'u * 'v * 'w * 'x)
    Json_encoding.encoding

val int64 : Int64.t Json_encoding.encoding
val int : int Json_encoding.encoding
val tup1_int : int Json_encoding.encoding
val tup1_int64 : int64 Json_encoding.encoding
val tup1_string : string Json_encoding.encoding

val register :
  ?name:string ->
  ?descr:string -> 'a Json_encoding.encoding -> unit

val merge_objs :
  ?name:string ->
  ?descr:string ->
  'a Json_encoding.encoding ->
  'b Json_encoding.encoding -> ('a * 'b) Json_encoding.encoding

val result : 'a Json_encoding.encoding -> 'b Json_encoding.encoding ->
  ('a, 'b) result Json_encoding.encoding

val ignore_enc : 'a Json_encoding.encoding -> 'a Json_encoding.encoding
