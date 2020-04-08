open Js_of_ocaml
module Xhr = Js_of_ocaml_lwt.XmlHttpRequest
open Xhr
open EzRequest_lwt

let (>|=) = Lwt.(>|=)

let log ?(meth="GET") ?msg url = match msg with
  | None -> ()
  | Some msg -> Firebug.console##log (
      Js.string ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]"))

include Make(struct

    let get ?headers ?msg url =
      log ?msg url;
      perform_raw_url ?headers url >|= fun frame ->
      log ~meth:("RECV " ^ string_of_int frame.code) ?msg url;
      if frame.code >= 200 && frame.code < 300 then Ok frame.content
      else Error (frame.code, Some frame.content)

    let post ?(content_type="application/json") ?(content="{}") ?headers ?msg url =
      log ~meth:"POST" ?msg url;
      perform_raw_url ?headers ~content_type ~contents:(`String content) url >|= fun frame ->
      log ~meth:("RECV " ^ string_of_int frame.code) ?msg url;
      if frame.code >= 200 && frame.code < 300 then Ok frame.content
      else Error (frame.code, Some frame.content)

  end)

(* Use our own version of Ezjsonm.from_string to avoid errors *)
let init () =
  EzEncodingJS.init ();
  EzDebugJS.init ();
  init ();
  EzRequest_lwt.log := (fun s -> Firebug.console##log (Js.string s))
