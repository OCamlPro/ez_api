open Js_of_ocaml

exception Parse_error of Json_repr.ezjsonm * string

let from_string s =
  try
    Js._JSON##parse (Js.string s) |> Js_json.json_of_js
  with (Js_error.Exn e) ->
    let e2 = Js_error.to_error e in
    if Js.to_string e2##.name = "SyntaxError" then
      raise @@ Parse_error (`Null, Printf.sprintf "Ezjsonm.from_string %s" (Js.to_string e2##.message))
    else Js_error.raise_ e

let to_string ?(minify=true) j =
  if minify then
    Js._JSON##stringify (Js_json.js_of_json j) |> Js.to_string
  else
    Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr "JSON.stringify")
      [| Js_json.js_of_json j; Js.Unsafe.inject (Js.null); Js.Unsafe.inject 2 |]
    |> Js.to_string
