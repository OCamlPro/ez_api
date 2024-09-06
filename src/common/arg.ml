(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Ty = struct

  type 'a witness = ..

  exception Not_equal

  type (_, _) eq = Eq : ('a, 'a) eq

  module type Ty = sig
    type t val witness : t witness
    val eq: 'a witness -> ('a, t) eq
  end

  type 'a id = (module Ty with type t = 'a)

  let new_id (type a) () =
    let module Ty = struct
      type t = a
      type 'a witness += Ty : t witness
      let witness = Ty
      let eq (type b) : b witness -> (b, t) eq =
        function Ty -> Eq | _ -> raise Not_equal
    end in
    (module Ty : Ty with type t = a)

  let eq : type a b. a id -> b id -> (a, b) eq =
    fun (module TyA) (module TyB) ->  TyB.eq TyA.witness

end

type descr = {
  name: string;
  descr: string option;
  example: string option;
  schema: Json_schema.schema option;
}

type 'a t = {
  id: 'a Ty.id;
  destruct: string -> ('a, string) result;
  construct: 'a -> string;
  description: descr;
}

let make ?example ?descr ?schema ~name ~destruct ~construct () =
  let id = Ty.new_id () in
  let example = match example with
    | None -> None
    | Some example -> Some (construct example) in
  let description = { name; descr; example; schema } in
  { description; id; construct; destruct }

let descr (ty: 'a t) = ty.description

let int ?descr ?example ?schema name =
  let destruct s =
    try Ok (int_of_string s)
    with Failure _ ->
      Error (Printf.sprintf "Cannot parse integer value: %S." s) in
  let schema = match schema with
    | None -> Json_schema.(
        create @@ element @@ Number
          { numeric_specs with minimum=Some (Int.to_float (-(1 lsl 30)), `Inclusive);
                               maximum=Some (Int.to_float ((1 lsl 30) -1), `Inclusive) })
    | Some sch -> sch in
  make ?example ?descr ~schema ~name ~destruct ~construct:string_of_int ()

let float ?descr ?example ?schema name =
  let destruct s = match float_of_string_opt s with
    | Some f -> Ok f
    | None -> Error (Printf.sprintf "Cannot parse float value: %S." s) in
  let schema = match schema with
    | None -> Json_schema.(create @@ element @@ Number numeric_specs)
    | Some sch -> sch in
  make ?example ?descr ~schema ~name ~destruct ~construct:string_of_float ()

let int32 ?descr ?example ?schema name =
  let destruct s = match Int32.of_string_opt s with
    | Some i -> Ok i
    | None -> Error (Printf.sprintf "Cannot parse int32 value: %S." s) in
  let schema = match schema with
    | None -> Json_schema.(
        create @@ element @@ Number
          { numeric_specs with minimum=Some (Int32.(to_float min_int), `Inclusive);
                               maximum=Some (Int32.(to_float max_int), `Inclusive) })
    | Some sch -> sch in
  make ?example ?descr ~schema ~name ~destruct ~construct:Int32.to_string ()

let int64 ?descr ?example ?schema name =
  let destruct s = match Int64.of_string_opt s with
    | Some i -> Ok i
    | None -> Error (Printf.sprintf "Cannot parse int64 value: %S." s) in
  let schema = match schema with
    | None -> Json_schema.(
        create @@ element @@ Number
          { numeric_specs with minimum=Some (Int64.(to_float min_int), `Inclusive);
                              maximum=Some (Int64.(to_float max_int), `Inclusive) })
    | Some sch -> sch in
  make ?descr ?example ~schema ~name ~destruct ~construct:Int64.to_string ()

let string ?descr ?example ?schema name =
  make ?descr ?example ?schema ~name ~destruct:Result.ok ~construct:(fun s -> s) ()
