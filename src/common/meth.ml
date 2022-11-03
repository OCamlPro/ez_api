type empty = [ `OPTIONS | `HEAD ]
type t = [ `GET | `POST | `PUT | `DELETE | `PATCH ]
type all = [ t | empty ]

let to_string : [< all ] -> string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"
  | `OPTIONS -> "OPTIONS"
  | `HEAD -> "HEAD"

let headers l =
  let meths = String.concat "," @@ List.map to_string l in
  [ "access-control-allow-methods", meths ]
