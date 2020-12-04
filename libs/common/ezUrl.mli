
val encode : string -> string
val encode_args : ?url:bool -> (string * string list) list -> string

val decode : string -> string
val decode_args : ?url:bool -> string -> (string * string list) list

val content_type : string

val encode_obj : ?url:bool -> 'a Json_encoding.encoding -> 'a -> string
