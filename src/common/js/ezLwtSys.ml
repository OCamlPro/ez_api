let run = Lwt.async
let sleep d =
  let t, w = Lwt.task () in
  let id = Js_of_ocaml.Dom_html.setTimeout (Lwt.wakeup w) (d *. 1000.) in
  Lwt.on_cancel t (fun () -> Js_of_ocaml.Dom_html.clearTimeout id);
  t
