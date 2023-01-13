(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Json_encoding
module Ezjsonm  = Ezjsonm_interface

exception DestructError

(* Default behavior to destruct a json value *)
let destruct encoding buf =
  try
    let json = Ezjsonm.from_string buf in
    destruct encoding json
  with
  | Cannot_destruct (path, exn)  ->
    Format.eprintf "Error during destruction path %a : %s\n\n %s\n%!"
      (Json_query.print_path_as_json_path ~wildcards:true) path
      (Printexc.to_string exn)
      buf;
    raise DestructError
  | Unexpected_field field ->
    Format.eprintf "Error during destruction path, unexpected field %S %s\n%!"
      field buf ;
    raise DestructError

type destruct_error = [ `destruct_exn of exn ]

let destruct_res encoding s =
  try Ok (Json_encoding.destruct encoding (Ezjsonm.from_string s))
  with exn -> Error (`destruct_exn exn)

let construct ?compact encoding data =
  Ezjsonm.to_string ?minify:compact (Json_encoding.construct encoding data)

let unexpected_error ~kind ~expected =
  raise @@ Cannot_destruct ([], Unexpected (kind, expected))

exception Not_utf8

let is_valid_utf8 str =
  try
    Uutf.String.fold_utf_8 (fun _ _ -> function
        | `Malformed _ -> raise Not_utf8
        | _ -> ()
      ) () str;
    true
  with Not_utf8 -> false

let encode_string str =
  if is_valid_utf8 str then `String str
  else
    let `Hex h = Hex.of_string str in
    `O [ "hex", `String h ]

let decode_string = function
  | `String str -> str
  | `O [ "hex", `String str ] -> Hex.to_string (`Hex str)
  | _ -> unexpected_error ~kind:"raw string" ~expected:"encoded string"

let encoded_string =
  conv encode_string decode_string any_ezjson_value

let () =
  Printexc.register_printer @@ function
  | Cannot_destruct (path, exn) ->
    let s = Printf.sprintf "Cannot destruct JSON (%s, %s)"
        (Json_query.json_pointer_of_path path)
        (Printexc.to_string exn) in
    Some s
  | _ -> None

let obj11 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),x11)
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),x11)
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
    )
    (merge_objs
       (obj10
          f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj1 f11))

let obj12 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
    )
    (merge_objs
       (obj10
          f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj2 f11 f12)
    )

let obj13 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
    )
    (merge_objs
       (obj10
          f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj3 f11 f12 f13)
    )

let obj14 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14)
    )
    (merge_objs
       (obj10
          f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj4 f11 f12 f13 f14)
    )

let obj15 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15)
    )
    (merge_objs
       (obj10
          f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj5 f11 f12 f13 f14 f15)
    )

let obj16 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15,x16))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15,x16))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16)
    )
    (merge_objs
       (obj10
          f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj6 f11 f12 f13 f14 f15 f16)
    )

let obj17 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15,x16,x17))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15,x16,x17))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17)
    )
    (merge_objs
       (obj10
          f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj7 f11 f12 f13 f14 f15 f16 f17)
    )

let obj18 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15,x16,x17,x18))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15,x16,x17,x18))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18)
    )
    (merge_objs
       (obj10
          f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj8 f11 f12 f13 f14 f15 f16 f17 f18)
    )

let obj19 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15,x16,x17,x18,x19))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),(x11,x12,x13,x14,x15,x16,x17,x18,x19))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19)
    )
    (merge_objs
       (obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj9 f11 f12 f13 f14 f15 f16 f17 f18 f19)
    )

let obj20 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
         (x11,x12,x13,x14,x15,x16,x17,x18,x19,x20))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
       (x11,x12,x13,x14,x15,x16,x17,x18,x19,x20))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
    )
    (merge_objs
       (obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (obj10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20)
    )

let obj21
    f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
    f11 f12 f13 f14 f15 f16 f17 f18 f19 f20
    f21
  =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
       x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,
       x21)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
         (
           (x11,x12,x13,x14,x15,x16,x17,x18,x19,x20),
           (x21)
         ))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
       ((x11,x12,x13,x14,x15,x16,x17,x18,x19,x20),
        (x21)
       ))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
       x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,
       x21)
    )
    (merge_objs
       (obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (merge_objs
          (obj10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20)
          (obj1 f21)
       )
    )

let obj22
    f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
    f11 f12 f13 f14 f15 f16 f17 f18 f19 f20
    f21 f22
  =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
       x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,
       x21,x22)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
         (
           (x11,x12,x13,x14,x15,x16,x17,x18,x19,x20),
           (x21,x22)
         ))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
       ((x11,x12,x13,x14,x15,x16,x17,x18,x19,x20),
        (x21,x22)
       ))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
       x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,
       x21,x22)
    )
    (merge_objs
       (obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (merge_objs
          (obj10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20)
          (obj2 f21 f22)
       )
    )

let obj23
    f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
    f11 f12 f13 f14 f15 f16 f17 f18 f19 f20
    f21 f22 f23
  =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
       x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,
       x21,x22,x23)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
         (
           (x11,x12,x13,x14,x15,x16,x17,x18,x19,x20),
           (x21,x22,x23)
         ))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
       ((x11,x12,x13,x14,x15,x16,x17,x18,x19,x20),
        (x21,x22,x23)
       ))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
       x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,
       x21,x22,x23)
    )
    (merge_objs
       (obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (merge_objs
          (obj10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20)
          (obj3 f21 f22 f23)
       )
    )

let obj24
    f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
    f11 f12 f13 f14 f15 f16 f17 f18 f19 f20
    f21 f22 f23 f24
  =
  conv
    (fun
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
       x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,
       x21,x22,x23,x24)
      ->
        ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
         (
           (x11,x12,x13,x14,x15,x16,x17,x18,x19,x20),
           (x21,x22,x23,x24)
         ))
    )
    (fun
      ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),
       ((x11,x12,x13,x14,x15,x16,x17,x18,x19,x20),
        (x21,x22,x23,x24)
       ))
      ->
      (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
       x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,
       x21,x22,x23,x24)
    )
    (merge_objs
       (obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
       (merge_objs
          (obj10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20)
          (obj4 f21 f22 f23 f24)
       )
    )

(* for swagger *)

let defs = ref []

let int_kind = Json_schema.(Integer numeric_specs)
let int_element = Json_schema.element int_kind
let tup1_int_kind = Json_schema.(Monomorphic_array (int_element, array_specs))
let tup1_int_element = Json_schema.element tup1_int_kind
let int64_element = Json_schema.({int_element with format = Some "int64"})
let tup1_int64_kind = Json_schema.(Monomorphic_array (int64_element, array_specs))
let tup1_int64_element = Json_schema.element tup1_int64_kind
let string_kind = Json_schema.(String string_specs)
let string_element = Json_schema.element string_kind
let tup1_string_kind = Json_schema.(Monomorphic_array (string_element, array_specs))
let tup1_string_element = Json_schema.element tup1_string_kind

let int64 =
  conv
    (fun x -> Int64.to_string x)
    (fun x -> Int64.of_string x)
    ~schema:(Json_schema.create int64_element)
    string

let int =
  conv (fun x -> x) (fun x -> x) ~schema:(Json_schema.create int_element) int
let tup1_int =
  conv (fun x -> x) (fun x -> x) ~schema:(Json_schema.create tup1_int_element) (tup1 int)
let tup1_int64 =
  conv (fun x -> x) (fun x -> x) ~schema:(Json_schema.create tup1_int64_element) (tup1 int64)
let tup1_string =
  conv (fun x -> x) (fun x -> x) ~schema:(Json_schema.create tup1_string_element) (tup1 string)


let register ?name ?(descr="") o =
  match name with
  | None -> ()
  | Some name ->
    let sch = lazy ( Json_encoding.schema o ) in
    defs := (name, (descr, sch)) :: !defs;
    ()

let merge_objs ?name ?descr o1 o2 =
  let o = Json_encoding.merge_objs o1 o2 in
  register ?name ?descr o;
  o

let result ok_enc err_enc =
  union [
    case
      ok_enc
      (function Ok s -> Some s | Error _ -> None)
      (fun s -> Ok s);
    case
      (obj1 (req "error" err_enc))
      (function Error s -> Some s | Ok _ -> None)
      (fun s -> Error s)
  ]

let ignore_enc encoding =
  conv (fun x -> x, ()) (fun (x, _) -> x) @@
  merge_objs encoding unit

let enc_constant enc v =
  conv (fun () -> v) (fun v2 -> if v2 = v then () else assert false) enc
