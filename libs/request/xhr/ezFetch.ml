open EzRequest
open Ezjs_fetch

let log ?(meth="GET") ~msg url = match msg with
  | "" -> ()
  | msg -> Ezjs_min.log_str ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]")

let handle_response ~msg url f r =
  match r with
  | Error s -> f @@ Error (0, Some (Ezjs_min.string_of_error s))
  | Ok r ->
    log ~meth:("RECV " ^ string_of_int r.status) ~msg url ;
    if r.status >= 200 && r.status < 300 then f @@ Ok r.body
    else f @@ Error (r.status, Some r.body)

module Interface = struct
  let get ?(meth="GET") msg url ?headers f =
    log ~meth ~msg url;
    fetch ?headers ~meth url to_text (handle_response ~msg url f)

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") msg url ?(headers=[]) f =
    log ~meth ~msg url;
    let headers = ("Content-Type", content_type) :: headers in
    fetch ~headers ~meth ~body:(RString content) url to_text (handle_response ~msg url f)
end

include Make(Interface)

(* Use our own version of Ezjsonm.from_string to avoid errors *)
let init () =
  init ();
  EzRequest.log := Ezjs_min.log_str;
  !EzRequest.log "ezFetch Loaded"
