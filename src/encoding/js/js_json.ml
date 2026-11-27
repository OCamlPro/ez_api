(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2022 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Js_of_ocaml

(** {2 JS helpers} *)

type leaf = [ `Null | `Bool of bool | `Float of float | `String of string ]

let t_string = Js.string "string"
let t_number = Js.string "number"
let t_boolean = Js.string "boolean"
let t_object = Js.string "object"
let t_undefined = Js.string "undefined"

let null : Js.Unsafe.any = Js.Unsafe.inject Js.null

let array_constr : < isArray : Js.Unsafe.any -> bool Js.t Js.meth > Js.t = Js.Unsafe.global##._Array

(* One level of a JS value, as an ezjsonm-like value whose children are
   converted with [f d]. With [f = id], this is a view (used by the zipper);
   with the recursive conversion as [f], the result is directly the converted
   value: a single [typeof] dispatch per node and no intermediate allocation.

   [f] takes an extra argument [d] rather than being a closure: this avoids
   a partial application at each call site, which is slow in js_of_ocaml.
   [==] on JS strings is [===], i.e. a value comparison. *)
let view_js f d (j : Js.Unsafe.any) =
  let ty = Js.typeof j in
  if ty == t_string then `String (Js.to_string (Js.Unsafe.coerce j : Js.js_string Js.t))
  else if ty == t_number then `Float (Js.float_of_number (Js.Unsafe.coerce j : Js.number Js.t))
  else if ty == t_object then
    if j == null then `Null
    else if Js.to_bool (array_constr##isArray j) then
      let a = (Js.Unsafe.coerce j : Js.Unsafe.any Js.js_array Js.t) in
      let acc = ref [] in
      for i = a##.length - 1 downto 0 do
        acc := f d (Js.Unsafe.get a i) :: !acc
      done;
      `A !acc
    else
      let keys = Js.object_keys j in
      let acc = ref [] in
      for i = keys##.length - 1 downto 0 do
        let k : Js.js_string Js.t = Js.Unsafe.get keys i in
        acc := (Js.to_string k, f d (Js.Unsafe.get j k)) :: !acc
      done;
      `O !acc
  else if ty == t_boolean then `Bool (Js.to_bool (Js.Unsafe.coerce j : bool Js.t))
  else if ty == t_undefined then `Null
  else raise (Invalid_argument ("json_of_js: " ^ Js.to_string ty))

let js_of_leaf : [< leaf ] -> Js.Unsafe.any = function
  | `String s -> Js.Unsafe.inject (Js.string s)
  | `Float f -> Js.Unsafe.inject (Js.number_of_float f)
  | `Bool b -> Js.Unsafe.inject (Js.bool b)
  | `Null -> null

let js_array_of_list f d l : Js.Unsafe.any =
  let a : Js.Unsafe.any Js.js_array Js.t = new%js Js.array_empty in
  List.iter (fun x -> ignore (a##push (f d x))) l;
  Js.Unsafe.inject a

let js_object_of_list f d l : Js.Unsafe.any =
  let o = Js.Unsafe.obj [||] in
  List.iter (fun (k, v) -> Js.Unsafe.set o (Js.string k) (f d v)) l;
  o

let id () x = x

(** {2 Stack-safe conversion} *)

type ('a, 'b) kind =
  | Leaf of 'b
  | Arr of 'a list
  | Obj of (string * 'a) list

type ('a, 'b) zip =
  | LeafVal of 'b
  | KeyVal of string
  | LeafZip of 'a
  | ArrZip of 'b list * 'a list
  | ObjZip of (string * 'b) list * (string * 'a) list

module type Converter = sig
  type 'a _from
  type _to
  val kind_of : 'a _from -> ('a _from, _to) kind
  val arr : _to list -> _to
  val obj : (string * _to) list -> _to
end

module Js_to_JSON_Converter : Converter with type _ _from = Js.Unsafe.any
                                         and type _to = Json_repr.ezjsonm
= struct
  type _ _from = Js.Unsafe.any
  type _to = Json_repr.ezjsonm
  let arr l = `A l
  let obj l = `O l
  let kind_of j = match view_js id () j with
    | #leaf as leaf -> Leaf leaf
    | `A l -> Arr l
    | `O l -> Obj l
end

module JSON_to_Js_Converter : Converter with type 'a _from = Json_repr.ezjsonm
                                         and type _to = Js.Unsafe.any
= struct
  type 'a _from = Json_repr.ezjsonm
  type _to = Js.Unsafe.any
  let arr l = js_array_of_list id () l
  let obj l = js_object_of_list id () l
  let kind_of = function
    | #leaf as leaf -> Leaf (js_of_leaf leaf)
    | `A l -> Arr l
    | `O kl -> Obj kl
end

module Make_Conv (C : Converter) : sig

  val convert : ('a C._from, C._to) zip list -> C._to

end = struct

  (* This is tail-recursive with a manual (non-constant) stack. Js_of_ocaml
     compiles simple tail-recursive functions like this one to loops, so the
     manual stack will live in the heap. *)
  let rec convert = function
    | [LeafVal v] -> v
    | LeafZip j :: stack ->
      begin match C.kind_of j with
        | Leaf leaf -> convert (LeafVal leaf :: stack)
        | Arr [] -> convert (LeafVal (C.arr []) :: stack)
        | Obj [] -> convert (LeafVal (C.obj []) :: stack)
        | Arr (j :: l) -> convert (LeafZip j :: ArrZip ([], l) :: stack)
        | Obj ((k, j) :: kl) ->
          convert (LeafZip j :: KeyVal k :: ObjZip ([], kl) :: stack)
      end

    | LeafVal v :: ArrZip (ez_revlist, []) :: stack ->
      convert (LeafVal (C.arr (List.rev (v :: ez_revlist))) :: stack)
    | LeafVal v :: KeyVal k :: ObjZip (ez_revlist, []) :: stack ->
      convert (LeafVal (C.obj (List.rev ((k, v) :: ez_revlist))) :: stack)

    | LeafVal v :: ArrZip (ez_revlist, j :: js_list) :: stack ->
      convert (LeafZip j :: ArrZip (v :: ez_revlist, js_list) :: stack)

    | LeafVal v :: KeyVal k1 ::
      ObjZip (ez_revlist, (k2, j) :: js_list) :: stack ->
      convert (LeafZip j :: KeyVal k2 ::
               ObjZip ((k1, v) :: ez_revlist, js_list) :: stack)
    | []
    | (LeafVal _ | KeyVal _ | ArrZip _ | ObjZip _) :: _ -> assert false
end

module Js_to_JSON = Make_Conv (Js_to_JSON_Converter)

module JSON_to_Js = Make_Conv (JSON_to_Js_Converter)

(** {2 Fast conversion}

    Direct recursion is much faster than the zipper above (no frame
    allocation, no intermediate [kind] values and lists), but uses the call
    stack. So it is only used up to [max_depth] nested levels: deeper
    subtrees are handed to the stack-safe zipper conversion, which keeps the
    total stack usage bounded whatever the depth of the document. *)

let max_depth = 1000

let rec json_of_js_rec depth (j : Js.Unsafe.any) : Json_repr.ezjsonm =
  if depth >= max_depth then Js_to_JSON.convert [LeafZip j]
  else view_js json_of_js_rec (depth + 1) j

let rec js_of_json_rec depth (j : Json_repr.ezjsonm) : Js.Unsafe.any = match j with
  | #leaf as leaf -> js_of_leaf leaf
  | (`A _ | `O _) when depth >= max_depth -> JSON_to_Js.convert [LeafZip j]
  | `A l -> js_array_of_list js_of_json_rec (depth + 1) l
  | `O l -> js_object_of_list js_of_json_rec (depth + 1) l

let json_of_js j = json_of_js_rec 0 j

let js_of_json j = js_of_json_rec 0 j
