let addr = ref (None: string option)
let port = ref 8888
let root = ref "."
let default = ref "index.html"

let specs = [
  "-a", Arg.String (fun s -> addr := Some s), "Address of the server";
  "-p", Arg.Set_int port, "Port of the server";
  "-r", Arg.Set_string root, "Root directory of the server";
  "-d", Arg.Set_string default, "Default file instead of not found"
]

let conf s =
  match String.split_on_char ':' s with
  | [ a; p ] ->
    (try addr := Some a; port := int_of_string p with _ -> raise (Invalid_argument "port is not an integer"))
  | _ -> raise (Invalid_argument "bad format for argument addr:port")
