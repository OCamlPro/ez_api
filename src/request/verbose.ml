(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(*
  verbose binary flag
  1: string received over http
  2: string sent over http
  4: library logs or headers
*)

let v = match Sys.getenv_opt "EZAPICLIENT" with
  | Some "true" -> ref 7
  | Some x ->
    begin match int_of_string_opt x with
      | None -> ref 0
      | Some i -> ref i
    end
  | _ -> ref 0

let callback = ref (fun (_ : int) -> ())

let set_verbose i =
  v := i;
  !callback i
