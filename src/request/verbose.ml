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

let log ?(kind=`meth "GET") url msg =
  let s = match kind with
    | `meth meth -> Format.sprintf "%s %s" meth url
    | `code code ->
      let color = if code >= 200 && code < 300 then 31 else 42 in
      let cstart, cend = EzAPI.apply_ansi_color color in
      Format.sprintf "%s%d %s%s" cstart code url cend in
  match msg with
  | None -> if !v <> 0 then Format.eprintf "[ez_api] %s@." s
  | Some msg -> Format.eprintf "[>%s %s]@." msg s

let request ?msg ?meth ?content ?(headers=[]) url =
  let kind = Option.map (fun m -> `meth m) meth in
  log ?kind url msg;
  (if !v land 4 <> 0 then
     Format.printf "[ez_api] headers\n  %s@." @@
     String.concat "\n  " @@ List.map (fun (k, v) -> k ^ " : " ^ v) headers);
  if !v land 2 <> 0 then match content with
    | Some s when s <> "" -> Format.printf "[ez_api] sent:\n%s@." s
    | _ -> ()

let response ?msg ~code ~content url =
  log ~kind:(`code code) url msg;
  if !v land 1 <> 0 && content <> "" then Format.printf "[ez_api] received:\n%s@." content
