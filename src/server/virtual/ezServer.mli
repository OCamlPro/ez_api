(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val server : ?catch:(string -> exn -> string EzAPIServerUtils.Answer.t Lwt.t) ->
  (int * EzAPIServerUtils.server_kind) list -> unit Lwt.t

val set_debug : unit -> unit
