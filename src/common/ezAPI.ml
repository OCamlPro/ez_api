(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Arg = Arg
module Url = Url
module Security = Security
module Param = Param
module Err = Err
module Meth = Meth
module Service = Service
module Mime = Mime
module Path = Path
module Doc = Doc
module Error_codes = Error_codes
module Req = Req
module IO = Service.IO

module TYPES = struct
  include Url.TYPES

  type param_value =
    | I of int
    | S of string
    | B of bool
    | LS of string list

  type ip_info = {
    ip_ip : string;
    mutable ip_last : float;
    mutable ip_nb : int;
    ip_country : string * string;
  }
end
include TYPES

type no_security = Security.none

type 'b io = 'b IO.io =
  | Empty : unit io
  | Json : 'a Json_encoding.encoding -> 'a io
  | Raw : Mime.t list -> string io

type ('args, 'input, 'output, 'error, 'security) service = {
  s : ('args, 'input, 'output, 'error, 'security) Service.t;
  doc : Doc.t;
}

type ('output, 'error, 'security) service0 =
  (Req.t, unit, 'output, 'error, 'security) service
type ('arg, 'output, 'error, 'security) service1 =
  (Req.t * 'arg, unit, 'output, 'error, 'security) service
type ('arg1, 'arg2, 'output, 'error, 'security) service2 =
  ((Req.t * 'arg1) * 'arg2, unit, 'output, 'error, 'security) service

type ('input, 'output, 'error, 'security) post_service0 =
  (Req.t, 'input, 'output, 'error, 'security) service
type ('arg,'input,'output, 'error, 'security) post_service1 =
  ((Req.t * 'arg), 'input, 'output, 'error, 'security) service
type ('arg1, 'arg2,'input,'output, 'error, 'security) post_service2 =
  ((Req.t * 'arg1) * 'arg2, 'input, 'output, 'error, 'security) service

type ('input, 'output, 'error, 'security) ws_service0 =
  (Req.t, 'input, 'output, 'error, 'security) service
type ('arg, 'input, 'output, 'error, 'security) ws_service1 =
  (Req.t * 'arg, 'input, 'output, 'error, 'security) service
type ('arg1, 'arg2, 'input, 'output, 'error, 'security) ws_service2 =
  ((Req.t * 'arg1) * 'arg2, 'input, 'output, 'error, 'security) service

let warnings = ref []
let warning s = warnings := s :: !warnings
let warnings f =
  List.iter f (List.rev !warnings);
  warnings := []

let encode_params s params =
  let open Param in
  let params =
    List.map (fun (param, v) ->
        if not (List.exists (fun p -> p.param_id = param.param_id) (Service.params s))
        then Printf.kprintf warning "unknown argument %S" param.param_id;
        match v with
        | I n -> param.param_id, [string_of_int n]
        | S s -> param.param_id, [s]
        | B b -> param.param_id, [string_of_bool b]
        | LS s ->  (param.param_id, s)
      ) params in
  Url.encode_args params

let forge url s args params =
  let parts = String.concat "/" @@ Path.forge (Service.path s.s) args in
  let params = match params with
    | [] -> ""
    | params -> Printf.sprintf "?%s" (encode_params s.s params) in
  Url.assemble url parts params

let forge0 url s params = forge url s Req.dummy params
let forge1 url s arg1 params =  forge url s (Req.dummy, arg1) params
let forge2 url s arg1 arg2 params =  forge url s ((Req.dummy, arg1), arg2) params

(* access_control allows to set acces control that will be included in server response headers. *)
let raw_service :
  type i. ?section:Doc.section -> ?name:string -> ?descr:string -> ?meth:Meth.t ->
  input:i io -> output:'o io -> ?errors:'e Err.case list -> ?params:Param.t list ->
  ?security:'s list -> ?access_control:(string * string) list -> ?register:bool -> ?hide:bool ->
  ?input_example:i -> ?output_example:'o -> (Req.t, 'a) Path.t ->
  ('a, i, 'o, 'e, 's) service =
  fun ?section ?name ?descr ?meth ~input ~output ?(errors=[]) ?(params=[])
    ?(security=[]) ?access_control ?register ?hide ?input_example ?output_example path ->
  let meth = match meth, input with
    | None, Empty -> `GET
    | None, _ -> `POST
    | Some m, _ -> m in
  let s = Service.make ~meth ~input ~output
      ~errors ~params ~security ?access_control path in
  let doc = Doc.make ?name ?descr ?section ?input_example ?output_example ?hide ?register s in
  { s; doc }

let post_service ?section ?name ?descr ?(meth=`POST)
    ~input ~output ?errors ?params
    ?security ?register ?access_control ?input_example ?output_example
    path =
  raw_service ?section ?name ?descr ~input:(Json input) ~output:(Json output)
    ?errors ~meth ?params ?security ?access_control ?register ?input_example ?output_example path

let service ?section ?name ?descr ?(meth=`GET) ~output ?errors ?params
    ?security ?access_control ?register ?output_example path =
  raw_service ?section ?name ?descr ~input:Empty ~output:(Json output)
    ?errors ~meth ?params ?security ?access_control ?register ?output_example path

let ws_service ?section ?name ?descr ~input ~output ?errors ?params
    ?security ?access_control ?register ?output_example path =
  raw_service ?section ?name ?descr ~input ~output
    ?errors ~meth:`GET ?params ?security ?access_control ?register ?output_example path

let register service =
  Doc.register service.doc;
  service.s

let id s = s.doc.Doc.doc_id

module Legacy = struct

  type nonrec ('args2, 'args, 'input, 'output) service =
    ('args, 'input, 'output, Security.uninhabited, Security.none) service

  type 'output service0 =
    (unit, Req.t, unit, 'output) service
  type ('arg, 'output) service1 =
    (unit * 'arg, Req.t * 'arg, unit, 'output) service
  type ('arg1, 'arg2, 'output) service2 =
    ((unit * 'arg1) * 'arg2, (Req.t * 'arg1) * 'arg2, unit, 'output) service

  type ('input, 'output) post_service0 =
    (unit, Req.t, 'input, 'output) service
  type ('arg, 'input,'output) post_service1 =
    (unit * 'arg, Req.t * 'arg, 'input, 'output) service
  type ('arg1, 'arg2, 'input, 'output) post_service2 =
    ((unit * 'arg1) * 'arg2, (Req.t * 'arg1) * 'arg2, 'input, 'output) service

  let post_service ?section ?name ?descr ?meth ~input ~output ?params arg =
    post_service ?section ?name ?descr ?meth
      ~input ~output ?params arg

  let service ?section ?name ?descr ?meth ~output ?params arg =
    service ?section ?name ?descr ?meth ~output ?params arg

end
