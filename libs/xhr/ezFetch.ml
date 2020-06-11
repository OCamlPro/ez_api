open Js_of_ocaml
open EzRequest
open Fetch

let (>|=) = Lwt.(>|=)

let log ?(meth="GET") ~msg url = match msg with
  | "" -> ()
  | msg -> Firebug.console##log (
      Js.string ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]"))

let handle_response ~msg url f r =
  match r with
  | Error s -> f @@ CodeError (0, Some s)
  | Ok r ->
    log ~meth:("RECV " ^ string_of_int r.status) ~msg url ;
    if r.status >= 200 && r.status < 300 then f @@ CodeOk r.body
    else f @@ CodeError (r.status, Some r.body)

include Make(struct

    let xhr_get ?(meth="GET") msg url ?headers f =
      Lwt.async @@ fun () ->
      log ~meth ~msg url;
      fetch ?headers ~meth url to_text >|= handle_response ~msg url f

    let xhr_post ?(meth="POST") ?(content_type="application/json") ?(content="{}") msg url ?(headers=[]) f =
      Lwt.async @@ fun () ->
      log ~meth ~msg url;
      let headers = ("Content-Type", content_type) :: headers in
      fetch ~headers ~meth ~body:(RString content) url to_text >|= handle_response ~msg url f

  end)

(* Use our own version of Ezjsonm.from_string to avoid errors *)
let init () =
  EzEncodingJS.init ();
  EzDebugJS.init ();
  init ();
  EzRequest.log := (fun s -> Firebug.console##log (Js.string s));
  !EzRequest.log "ezXhr Loaded"
