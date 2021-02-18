open Js_of_ocaml

let printf fmt =
  Format.kfprintf
    (fun _fmt -> Firebug.console##debug (Js.string (Format.flush_str_formatter ())))
    Format.str_formatter
    fmt

let log s = Firebug.console##log (Js.string s)
