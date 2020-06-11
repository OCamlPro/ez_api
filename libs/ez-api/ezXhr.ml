open Js_of_ocaml
open EzRequest

let log ?(meth="GET") msg url = Firebug.console##log (
    Js.string ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]"))

include Make(struct

    let xhr_get ?(meth="GET") msg url ?(headers=[]) f =
      if msg <> "" then log ~meth msg url;
      let xhr = XmlHttpRequest.create () in
      xhr##_open (Js.string meth) (Js.string url) Js._true ;
      List.iter (fun (name, value) ->
          xhr##setRequestHeader
            (Js.string name) (Js.string value) ;
        ) headers;
      xhr##.onreadystatechange :=
        Js.wrap_callback (fun _ ->
            if xhr##.readyState = XmlHttpRequest.DONE then
              let status = xhr##.status in
              if msg <> "" then log ~meth:("RECV " ^ string_of_int status) msg url;
              if status = 200 then
                f (CodeOk (Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string))
              else
                f (CodeError (
                    status,
                    Js.Opt.case xhr##.responseText
                      (fun () -> None) (fun s -> Some (Js.to_string s))))) ;
      xhr##send Js.null

    let xhr_post ?(meth="POST") ?(content_type="application/json") ?(content="{}") msg url
        ?(headers=[]) f =
      if msg <> "" then log ~meth msg url;
      let xhr = XmlHttpRequest.create () in
      xhr##_open (Js.string meth) (Js.string url) Js._true ;
      xhr##setRequestHeader
        (Js.string "Content-Type") (Js.string content_type) ;
      List.iter (fun (name, value) ->
          xhr##setRequestHeader
            (Js.string name) (Js.string value) ;
        ) headers;
      xhr##.onreadystatechange :=
        Js.wrap_callback (fun _ ->
            if xhr##.readyState = XmlHttpRequest.DONE then
              let status = xhr##.status in
              if msg <> "" then log ~meth:("RECV " ^ string_of_int status) msg url;
              if status = 200 then
                f (CodeOk (Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string))
              else
                f (CodeError (
                    status,
                    Js.Opt.case xhr##.responseText
                      (fun () -> None) (fun s -> Some (Js.to_string s) )))) ;
      xhr##send (Js.some @@ Js.string content)
  end)

(* Use our own version of Ezjsonm.from_string to avoid errors *)
let init () =
  EzEncodingJS.init ();
  EzDebugJS.init ();
  init ();
  EzRequest.log := (fun s -> Firebug.console##log (Js.string s));
  ()
