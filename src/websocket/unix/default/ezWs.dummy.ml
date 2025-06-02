(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2025 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let err = "ezWs not implemented, either install httpun-ws-lwt-unix or websocket-lwt-unix"

let connect ?msg:_ ?protocols:_ ?error:_ ~react:_ _url = Lwt.return_error err

let connect0 ?msg:_ ?protocols:_ ?error:_ ~react:_ _base _service = Lwt.return_error err
