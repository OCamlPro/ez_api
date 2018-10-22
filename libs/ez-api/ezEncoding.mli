
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
