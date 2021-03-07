type 'a t = {
  code : int;
  body : 'a;
  headers : (string * string) list;
}

let return ?(code=200) ?(headers=[]) body = Lwt.return {code; body; headers}

let not_found () = return ~code:404 ""

let headers = [ "content-type", "application/json" ]

let cannot_parse (descr, msg, path) =
  let body =
    Format.sprintf {|{"error": "Cannot parse path argument %s", "path": %S, "msg": %S|}
      descr.EzAPI.Arg.name (String.concat "/" path) msg in
  return ~code:400 ~headers body

let method_not_allowed () = return ~code:405 ""

let cannot_destruct (path, exn) =
  let body =
    Format.sprintf {|{"error": "Cannot destruct JSON", "path": %S, "msg": %S|}
      path exn in
  return ~code:400 ~headers body

let unexpected_field f =
  let body = Format.sprintf {|{"error": "Unexpected field in JSON", "field": %S|} f in
  return ~code:400 ~headers body

let unsupported_media_type c =
  let c = match c with None -> "none" | Some c -> c in
  let body = Format.sprintf {|{"error": "Unsupported Media Type", "content_type": %S|} c in
  return ~code:415 ~headers body

let server_error exn =
  let body = Format.sprintf {|{"error": "Server Error", "msg": %S|} (Printexc.to_string exn) in
  return ~code:500 ~headers body
