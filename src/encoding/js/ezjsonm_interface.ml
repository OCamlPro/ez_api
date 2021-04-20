open Js_of_ocaml

exception Parse_error of Json_repr.ezjsonm * string

let from_string s =
  try
    Js._JSON##parse (Js.string s) |> Js_json.json_of_js
  with (Js.Error e) ->
    if Js.to_string e##.name = "SyntaxError" then
      raise @@ Parse_error (`Null, Printf.sprintf "Ezjsonm.from_string %s" (Js.to_string e##.message))
    else Js.raise_js_error e

let to_string ?(minify=true) j =
  if minify then
    Js._JSON##stringify (Js_json.js_of_json j) |> Js.to_string
  else
    Js.Unsafe.fun_call (Js.Unsafe.variable "JSON.stringify")
      [| Js_json.js_of_json j; Js.Unsafe.inject (Js.null); Js.Unsafe.inject 2 |]
    |> Js.to_string
