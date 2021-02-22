open Js_of_ocaml

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

module Js_to_JSON_Converter : Converter with type 'a _from = 'a Js.t Js.opt
                                         and type _to = Json_repr.ezjsonm
= struct

  type 'a _from = 'a Js.t Js.opt
  type _to = Json_repr.ezjsonm

  let arr l = `A l

  let obj l = `O l

  let kind_of j =
    Js.Opt.case j
      (fun () -> (* null *)
         Leaf (`Null))
      (fun j -> (* not null *)
         match Js.to_string (Js.typeof j) with
         | "object" ->
           begin
             try Arr
                   (Array.to_list
                      (Js.to_array (Js.Unsafe.coerce j : 'a Js.js_array Js.t)))
             with _ ->
               let keys = Array.to_list (Js.to_array (Js.object_keys j)) in
               let l =
                 List.map (fun k -> Js.to_string k, Js.Unsafe.get j k) keys in
               Obj l
           end
         | "string" ->
           Leaf (`String
                   (Js.to_string (Js.Unsafe.coerce j : Js.js_string Js.t)))
         | "number" ->
           Leaf (`Float
                   (Js.float_of_number (Js.Unsafe.coerce j : Js.number Js.t)))
         | "boolean" ->
           Leaf (`Bool (Js.to_bool (Js.Unsafe.coerce j : bool Js.t)))
         (* | "undefined" -> Leaf (`Null) *)
         | tof -> raise (Invalid_argument ("json_of_js: "^tof))
      )
end

module JSON_to_Js_Converter : Converter with type 'a _from = Json_repr.ezjsonm
                                         and type _to = Js.Unsafe.any
= struct

  type 'a _from = Json_repr.ezjsonm
  type _to = Js.Unsafe.any

  let arr l = Js.array (Array.of_list l) |> Js.Unsafe.inject

  let obj l = Js.Unsafe.obj (Array.of_list l) |> Js.Unsafe.inject

  let kind_of = function
    | `Null -> Leaf (Js.Unsafe.inject Js.null)
    | `Bool b -> Leaf (Js.Unsafe.inject (Js.bool b))
    | `Float f -> Leaf (Js.Unsafe.inject (Js.number_of_float f))
    | `String s -> Leaf (Js.Unsafe.inject (Js.string s))
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

let json_of_js j = Js_to_JSON.convert [LeafZip j]

let js_of_json j = JSON_to_Js.convert [LeafZip j]
