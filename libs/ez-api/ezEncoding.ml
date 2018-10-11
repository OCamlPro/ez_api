module Ezjsonm_direct = Ezjsonm
open Json_encoding

let json_of_string = ref Ezjsonm.from_string

module Ezjsonm : sig

  val from_string : string -> Json_repr.ezjsonm

  end = struct

  let from_string s = !json_of_string s
end

exception DestructError

let int64 =
  union [
    case
      int32
      (fun i ->
         let j = Int64.to_int32 i in
         if Int64.equal (Int64.of_int32 j) i then Some j else None)
      Int64.of_int32 ;
    case
      string
      (fun i -> Some (Int64.to_string i))
      Int64.of_string
  ]

(* Default behavior to destruct a json value *)
let destruct encoding buf =
  try
    let json = Ezjsonm.from_string buf in
    Json_encoding.destruct encoding json
  with Json_encoding.Cannot_destruct (path, exn)  ->
    Format.eprintf "Error during destruction path %a : %s\n\n %s\n%!"
      (Json_query.print_path_as_json_path ~wildcards:true) path
      (Printexc.to_string exn)
      buf ;
    raise DestructError
     | Json_encoding.Unexpected_field field ->
       Format.eprintf "Error during destruction path, unexpected field %S %s\n%!"
         field buf ;
       raise DestructError

let construct ?(compact=true) encoding data =
    let ezjson =
      (module Json_repr.Ezjsonm : Json_repr.Repr with type value = Json_repr.ezjsonm ) in
    Json_repr.pp
      ~compact
      ezjson
      Format.str_formatter
      (Json_encoding.construct encoding data) ;
    Format.flush_str_formatter ()

let unexpected_error ~kind ~expected =
  raise (Json_encoding.Cannot_destruct
           ([],
            Json_encoding.Unexpected (kind, expected)))

let encoded_string =
  conv
    (fun s -> Ezjsonm_direct.encode_string s)
    (fun enc ->
      match Ezjsonm_direct.decode_string enc with
          | None -> unexpected_error
                      ~kind:"raw string"
                      ~expected:"encoded string"
          | Some s -> s)
      any_ezjson_value

let () =
  Printexc.register_printer (fun exn ->
      match exn with
      | Json_encoding.Cannot_destruct (path, exn) ->
        let s = Printf.sprintf "Cannot destruct JSON (%s, %s)"
            (Json_query.json_pointer_of_path path)
            (Printexc.to_string exn)
        in
        Some s
      | _ -> None)

let init () = ()
