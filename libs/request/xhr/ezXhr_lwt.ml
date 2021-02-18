open Js_of_ocaml
module Xhr = Js_of_ocaml_lwt.XmlHttpRequest
open Xhr
open EzRequest_lwt

let (>|=) = Lwt.(>|=)

let meth_of_str ?(default=`GET) = function
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "PUT" -> `PUT
  | "POST" -> `POST
  | "PATCH" -> `PATCH
  | "DELETE" -> `DELETE
  | "OPTIONS" -> `OPTIONS
  | _ -> default

let log ?(meth="GET") ?msg url = match msg with
  | None -> ()
  | Some msg -> Firebug.console##log (
      Js.string ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]"))

module Interface = struct

  let get ?(meth="GET") ?headers ?msg url =
    log ~meth ?msg url;
    perform_raw_url ?headers ~override_method:(meth_of_str meth) url >|= fun frame ->
    log ~meth:("RECV " ^ string_of_int frame.code) ?msg url;
    if frame.code >= 200 && frame.code < 300 then Ok frame.content
    else Error (frame.code, Some frame.content)

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?headers ?msg url =
    log ~meth ?msg url;
    perform_raw_url ?headers ~content_type ~contents:(`String content)
      ~override_method:(meth_of_str ~default:`POST meth) url >|= fun frame ->
    log ~meth:("RECV " ^ string_of_int frame.code) ?msg url;
    if frame.code >= 200 && frame.code < 300 then Ok frame.content
    else Error (frame.code, Some frame.content)

end

include Make(Interface)

(* Use our own version of Ezjsonm.from_string to avoid errors *)
let init () =
  init ();
  EzRequest_lwt.log := (fun s -> Firebug.console##log (Js.string s));
  !EzRequest_lwt.log "ezXhr Loaded"
