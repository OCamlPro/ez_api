(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzAPIServerUtils

val server : ?catch:(string -> exn -> string Answer.t Lwt.t) ->
  ?allow_origin:[ allow_kind | `origin ] -> ?allow_headers:allow_kind ->
  ?allow_methods:allow_kind -> ?allow_credentials:bool -> ?footer:string ->
  (int * server_kind) list -> unit Lwt.t

val set_debug : unit -> unit
