(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Ezjs_min

type nonrec options = {
  meth : string option; [@key "method"]
  headers : ((string * string) list [@assoc]) option;
} [@@deriving jsoo]

class type message = object
  method headers : js_string t Table.t readonly_prop
  method httpVersion : js_string t readonly_prop
  method method_ : js_string t opt readonly_prop
  method statusCode : int readonly_prop
  method statusMessage : js_string t readonly_prop
  method url : js_string t readonly_prop
  method on_data : js_string t -> (Typed_array.arrayBuffer t -> unit) callback -> unit meth
  method on_end : js_string t -> (unit -> unit) callback -> unit meth
end

class type err = object
  inherit error
  method code : int readonly_prop
end

class type request = object
  method end_ : js_string t optdef -> unit meth
  method on_error : js_string t -> (err t -> unit) callback -> unit meth
end

class type http = object
  method request : js_string t -> options_jsoo t optdef ->
    (message t -> unit) callback optdef -> request t meth
  method get : js_string t -> options_jsoo t optdef ->
    (message t -> unit) callback optdef -> request t meth
end

let log ?(meth="GET") url = function
  | None -> ()
  | Some msg -> log_str ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]")

let require s = Unsafe.(fun_call (pure_js_expr "require") [|inject @@ string s|])

let http : http t = require "http"
let https : http t = require "https"

let handle f (m : message t) =
  if m##.statusCode >= 200 && m##.statusCode < 300 then
    let s = ref "" in
    m##on_data (string "data") (wrap_callback (fun chunk ->
        s := !s ^ (Typed_array.String.of_arrayBuffer chunk)));
    m##on_end (string "end") (wrap_callback (fun () ->
        if !Verbose.v land 1 <> 0 then Format.printf "[ez_api] received:\n%s@." !s;
        f (Ok !s)))
  else (
    if !Verbose.v land 1 <> 0 then Format.printf "[ez_api] received:\n%s@." (to_string m##.statusMessage);
    f (Error (m##.statusCode, Some (to_string m##.statusMessage))))

let get ?(protocol=http) ?options url f =
  if !Verbose.v land 2 <> 0 then Format.printf "[ez_api] sent:\n@.";
  let o = optdef options_to_jsoo options in
  let req = protocol##get (string url) o (def @@ wrap_callback (handle f)) in
  req##on_error (string "error") (wrap_callback (fun (e : err t) ->
      f (Error (e##.code, Some (to_string e##.message)))));
  req##end_ undefined

let post ?(protocol=http) ?options url ~content f =
  if !Verbose.v land 2 <> 0 then Format.printf "[ez_api] sent:\n%s@." content;
  let o = optdef options_to_jsoo options in
  let req = protocol##request (string url) o (def @@ wrap_callback (handle f)) in
  req##on_error (string "error") (wrap_callback (fun (e : err t) ->
      f (Error (e##.code, Some (to_string e##.message)))));
  req##end_ (def (string content))

let get_protocol url =
  try if String.sub url 0 5 = "https" then Some https else Some http
  with _ -> None

let () =
  (Js_of_ocaml.Js.Unsafe.pure_js_expr "global")##.set_verbose_ := Js_of_ocaml.Js.wrap_callback Verbose.set_verbose;
  EzDebug.log "ezNodeJs Loaded"
