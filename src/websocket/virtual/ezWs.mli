(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

include EzWsCommon.S

val connect :
  ?msg:string ->
  ?protocols:string list ->
  ?error:(string action -> string -> unit) ->
  react:(string action -> string -> unit rp) ->
  string ->
  string ws rp

val connect0 :
  ?msg:string ->
  ?protocols:string list ->
  ?error:(string action -> string -> unit) ->
  react:('input action -> ('output, 'error) result -> unit rp) ->
  EzAPI.base_url ->
  ('input, 'output, 'error, _) EzAPI.ws_service0 ->
  'input ws rp
