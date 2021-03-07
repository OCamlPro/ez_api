(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

module Ty : sig

  exception Not_equal
  type (_, _) eq = Eq : ('a, 'a) eq

  type 'a id
  val eq : 'a id -> 'b id -> ('a, 'b) eq

end

(** Typed path argument. *)
module Arg : sig

  type descr = {
    name: string ;
    descr: string option ;
    example: string option
  }

  type 'a t = {
    id: 'a Ty.id;
    destruct: string -> ('a, string) result ;
    construct: 'a -> string ;
    description: descr ;
  }

  val make:
    ?example:'a ->
    ?descr:string ->
    name:string ->
    destruct:(string -> ('a, string) result) ->
    construct:('a -> string) ->
    unit -> 'a t

  val descr: 'a t -> descr

  val int: int t
  val int32: int32 t
  val int64: int64 t
  val float: float t

end

(** Parametrized path to services. *)
module Path : sig

  type _ t =
    | Root : unit t
    | Static : 'key t * string -> 'key t
    | Dynamic : 'key t * 'a Arg.t -> ('key * 'a) t

  val root: unit t

  val add_suffix: 'args t -> string -> 'args t
  val (/): 'args t -> string -> 'args t

  val add_arg: 'args t -> 'a Arg.t -> ('args * 'a) t
  val (/:): 'args t -> 'a Arg.t -> ('args * 'a) t

  val to_list : ?root:string list -> ?wrap:(string -> string) -> _ t -> string list
  val to_string : ?root:string -> ?wrap:(string -> string) -> _ t -> string

  val forge: 'args t -> 'args -> string list

  val args : _ t -> Arg.descr list

end

(** Mime *)
module Mime : sig
  type str_or_star = [ `star | `str of string ]

  type t = {
    typ : str_or_star;
    subtyp : str_or_star;
    param : (string * string) option
  }

  val parse : string -> t option

  val to_string : t -> string

  val json : t
end

(** Query param *)
module Param : sig
  type kind = PARAM_INT | PARAM_STRING | PARAM_BOOL

  type param = {
    param_id : string;
    param_name : string option;
    param_descr : string option;
    param_type : kind;
    param_required : bool;
    param_examples : string list;
  }

  val make : kind -> ?name:string -> ?descr:string
    -> ?required:bool -> ?examples:string list -> string -> param

  val string : ?name:string -> ?descr:string -> ?required:bool
    -> ?examples:string list -> string -> param

  val int : ?name:string -> ?descr:string -> ?required:bool
    -> ?examples:string list -> string -> param

  val bool : ?name:string -> ?descr:string -> ?required:bool
    -> ?examples:string list -> string -> param

  type value =
    | I of int
    | S of string
    | B of bool
    | LS of string list
end

(** Security *)
module Security : sig
  type uninhabited = |
  type no_security = [ `Nosecurity of uninhabited ]
  type 'a apikey_security = { ref_name : string; name : 'a }
  type bearer_security_desc = { bearer_name : string ; format : string option }
  type basic_security_desc = { basic_name : string }
  type bearer_security = [ `Bearer of bearer_security_desc ]
  type basic_security = [ `Basic of basic_security_desc ]
  type header_security = [ `Header of string apikey_security ]
  type cookie_security = [ `Cookie of string apikey_security ]
  type query_security = [ `Query of Param.param apikey_security ]
  type scheme = [
    | no_security
    | basic_security
    | bearer_security
    | header_security
    | cookie_security
    | query_security ]

  val unreachable : uninhabited -> _

  val ref_name : [< scheme ] -> string

  val params : [< scheme ] list -> Param.param list

end

(** Errors *)
module Err : sig
  type _ case =
      Case : {
        code : int;
        name : string;
        encoding : 'a Json_encoding.encoding;
        select : 'b -> 'a option;
        deselect: 'a -> 'b;
      } -> 'b case

  val make : code:int -> name:string -> encoding:'a Json_encoding.encoding
    -> select:('b -> 'a option) -> deselect:('a -> 'b) -> 'b case

  val merge_errs_same_code : 'a case list -> (int * Json_schema.schema lazy_t) list

  val catch_all_error_case : unit -> 'a case
end

(** Services. *)
type 'a arg = 'a Arg.t
type 'a path = 'a Path.t
type mime = Mime.t

type empty_meth = [ `OPTIONS | `HEAD ]
type meth = [ `GET | `POST | `PUT | `DELETE | `PATCH ]
type all_meth = [ meth | empty_meth ]

val str_of_meth : [< all_meth ] -> string

type _ input =
  | Empty : unit input
  | Json : 'a Json_encoding.encoding -> 'a input
  | Raw : mime list -> string input

type ('args, 'input, 'output, 'error, 'security) service

val service:
  ?meth:meth ->
  ?params:Param.param list ->
  ?security:[< Security.scheme as 'security] list ->
  ?errors:'error Err.case list ->
  input:'input input ->
  output:'output Json_encoding.encoding ->
  'args path ->
  ('args, 'input, 'output, 'error, 'security) service

val input : (_, 'input, _, _, _) service -> 'input input
val output : (_, _, 'output, _, _) service -> 'output Json_encoding.encoding
val meth : (_, _, _, _, _) service -> meth
val path : ('args, _, _, _, _) service -> 'args path
val security : (_, _, _, _, 'security) service -> 'security list
val errors : (_, _, _, 'error, _) service -> 'error Err.case list
val params : (_, _, _, _, _) service -> Param.param list

type json = Json_repr.Ezjsonm.value

val forge_request:
  ('args, 'input, _, _, _) service ->
  'args -> 'input -> string list * json

val read_answer:
  ('args, _, 'output, _, _) service ->
  json -> ('output, string) result

module Make (Repr : Json_repr.Repr) : sig

  val forge_request:
    ('args, 'input, _, _, _) service ->
    'args -> 'input -> string list * Repr.value

  val read_answer:
    ('args, _, 'output, _, _) service ->
    Repr.value -> ('output, string) result
end
