open Js_of_ocaml

let printf fmt = Printf.kprintf (fun s -> Firebug.console##debug (Js.string s)) fmt
let log s = Firebug.console##log (Js.string s)
