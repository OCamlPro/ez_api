open EzRequest
open Ezjs_fetch

let log ?(meth="GET") url = function
  | None -> ()
  | Some msg -> Ezjs_min.log_str ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]")

let handle_response ?msg url f r =
  match r with
  | Error e ->
    f @@ Error (0, Some (Ezjs_min.to_string e##toString))
  | Ok r ->
    log ~meth:("RECV " ^ string_of_int r.status) url msg;
    if r.status >= 200 && r.status < 300 then f @@ Ok r.body
    else f @@ Error (r.status, Some r.body)

module Interface = struct
  let get ?(meth="GET") ?headers ?msg url f =
    log ~meth url msg;
    fetch ?headers ~meth url to_text (handle_response ?msg url f)

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?(headers=[]) ?msg url  f =
    log ~meth url msg;
    let headers = ("Content-Type", content_type) :: headers in
    fetch ~headers ~meth ~body:(RString content) url to_text (handle_response ?msg url f)
end

include Make(Interface)

let () = EzDebug.log "ezFetch Loaded"
