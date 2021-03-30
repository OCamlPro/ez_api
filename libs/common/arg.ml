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
  name: string ;
  descr: string option ;
  example: string option
}

type 'a t = {
  id: 'a Ty.id;
  destruct: string -> ('a, string) result ;
  construct: 'a -> string ;
  description: descr ;
}

let make ?example ?descr ~name ~destruct ~construct () =
  let id = Ty.new_id () in
  let example = match example with
    | None -> None
    | Some example -> Some (construct example) in
  let description = { name ; descr; example } in
  { description ; id ; construct ; destruct }

let descr (ty: 'a t) = ty.description

let int ?descr ?example name =
  let int_of_string s =
    try Ok (int_of_string s)
    with Failure _ ->
      Error (Printf.sprintf "Cannot parse integer value: %S." s) in
  make ?example ?descr ~name ~destruct:int_of_string ~construct:string_of_int ()

let float ?descr ?example name =
  let float_of_string s = match float_of_string_opt s with
    | Some f -> Ok f
    | None -> Error (Printf.sprintf "Cannot parse float value: %S." s) in
  make ?example ?descr ~name ~destruct:float_of_string ~construct:string_of_float ()

let int32 ?descr ?example name =
  let int32_of_string s = match Int32.of_string_opt s with
    | Some i -> Ok i
    | None -> Error (Printf.sprintf "Cannot parse int32 value: %S." s) in
  make ?example ?descr ~name ~destruct:int32_of_string ~construct:Int32.to_string ()

let int64 ?descr ?example name =
  let int64_of_string s = match Int64.of_string_opt s with
    | Some i -> Ok i
    | None -> Error (Printf.sprintf "Cannot parse int64 value: %S." s) in
  make ?descr ?example ~name ~destruct:int64_of_string ~construct:Int64.to_string ()

let string ?descr ?example name =
  make ?descr ?example ~name ~destruct:Result.ok ~construct:(fun s -> s) ()
