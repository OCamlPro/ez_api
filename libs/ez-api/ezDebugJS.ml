
let () =
  EzDebug.printer := (fun s -> Js_of_ocaml.(Firebug.console##log (Js.string s)))

let init () = ()
