type 'a t = {
  code : int;
  body : 'a;
  headers : (string * string) list;
}

let return ?(code=200) ?(headers=[]) body = Lwt.return {code; body; headers}

let not_found () = return ~code:404 ""

let headers = [ "content-type", "application/json" ]

let cannot_parse (descr, msg, path) =
  let body = EzEncoding.construct Json_encoding.any_ezjson_value @@
    `O [ "error", `String ("Cannot parse path argument " ^ descr.EzAPI.Arg.name);
         "path", `String (String.concat "/" path);
         "msg", `String msg ] in
  return ~code:400 ~headers body

let method_not_allowed () = return ~code:405 ""

let cannot_destruct (path, exn) =
  let body = EzEncoding.construct Json_encoding.any_ezjson_value @@
    `O [ "error", `String "Cannot destruct JSON";
         "path", `String path;
         "msg", `String exn ] in
  return ~code:400 ~headers body

let unexpected_field f =
  let body = EzEncoding.construct Json_encoding.any_ezjson_value @@
    `O [ "error", `String "Unexpected field in JSON";
         "field", `String f ] in
  return ~code:400 ~headers body

let unsupported_media_type c =
  let c = match c with None -> "none" | Some c -> c in
  let body = EzEncoding.construct Json_encoding.any_ezjson_value @@
    `O [ "error", `String "Unsupported Media Type";
         "content_type", `String c ] in
  return ~code:415 ~headers body

let server_error exn =
  let body = EzEncoding.construct Json_encoding.any_ezjson_value @@
    `O [ "error", `String "Server Error";
         "msg", `String (Printexc.to_string exn) ] in
  return ~code:500 ~headers body
