open EzRequest_lwt
open Ezjs_fetch_lwt

let (>|=) = Lwt.(>|=)

let log ?(meth="GET") ?msg url = match msg with
  | None -> ()
  | Some msg -> Ezjs_min.log_str ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]")

let handle_response ?msg url r =
  match r with
  | Error e -> Error (0, Some (Ezjs_min.to_string e##toString))
  | Ok r ->
    log ~meth:("RECV " ^ string_of_int r.status) ?msg url ;
    if r.status >= 200 && r.status < 300 then Ok r.body
    else Error (r.status, Some r.body)

module Interface = struct
  let get ?(meth="GET") ?headers ?msg url =
    log ~meth ?msg url;
    fetch ?headers ~meth url to_text >|= (handle_response ?msg url)

  let post ?(meth="POST") ?(content_type="application/json") ?(content="{}") ?(headers=[]) ?msg url =
    log ~meth ?msg url;
    let headers = ("Content-Type", content_type) :: headers in
    fetch ~headers ~meth ~body:(RString content) url to_text >|= (handle_response ?msg url)
end

include Make(Interface)

let () = EzDebug.log "ezFetch Loaded"
