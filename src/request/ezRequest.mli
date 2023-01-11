(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* If the code is > 0, it is an HTTP reply code. Otherwise, it is an
  internal error (-1 connection, -2 decoding) *)

val request_reply_hook : (unit -> unit) ref

(* Engine independent implementation. Beware: if you use these calls,
   you must initialize an engine independantly.*)

module type S = EzReq_S.S
module type Interface = EzReq_S.Interface

module ANY : S

module Make(_ : Interface) : S
