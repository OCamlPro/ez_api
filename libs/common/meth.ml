type empty = [ `OPTIONS | `HEAD ]
type t = [ `GET | `POST | `PUT | `DELETE | `PATCH ]
type all = [ t | empty ]

let to_string : [< all ] -> string = function
  | `GET -> "get"
  | `POST -> "post"
  | `PUT -> "put"
  | `DELETE -> "delete"
  | `PATCH -> "patch"
  | `OPTIONS -> "options"
  | `HEAD -> "head"

let headers l =
  let meths = String.concat "," @@ List.map to_string l in
  [ "access-control-allow-methods", meths ]
