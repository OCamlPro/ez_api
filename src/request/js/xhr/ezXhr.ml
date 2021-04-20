open Js_of_ocaml

let log ?(meth="GET") url = function
  | None -> ()
  | Some msg ->
    Firebug.console##log (
      Js.string ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]"))

module Interface = struct

  let get ?(meth="GET") ?(headers=[]) ?msg url f =
    log ~meth url msg;
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
            log ~meth:("RECV " ^ string_of_int status) url msg;
            if status >= 200 && status < 300 then
              f (Ok (Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string))
            else
              f (Error (
                  status,
                  Js.Opt.case xhr##.responseText
                    (fun () -> None) (fun s -> Some (Js.to_string s))))) ;
    xhr##send Js.null

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}")
      ?(headers=[]) ?msg url f =
    log ~meth url msg;
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
            log ~meth:("RECV " ^ string_of_int status) url msg;
            if status >= 200 && status < 300 then
              f (Ok (Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string))
            else
              f (Error (
                  status,
                  Js.Opt.case xhr##.responseText
                    (fun () -> None) (fun s -> Some (Js.to_string s) )))) ;
    xhr##send (Js.some @@ Js.string content)
end

include EzRequest.Make(Interface)

let () = EzDebug.log "ezXhr Loaded"
