open Httpun

type 'content with_headers = { content: 'content; headers: (string * string) list }

type _ response =
  | Simple : string response
  | WithHeaders : string with_headers response

type http_error = [ `http of (int * string) ]
type perform_error = [
  | `msg of string
  | `exn of exn
  | `timeout of float
  | `invalid_url of string
]
type stream_error_base = [ `interval_timeout of float | `exn of exn | http_error ]
type 'e stream_error = [ stream_error_base | `cb of 'e ]
type error = [ http_error | perform_error | stream_error_base ]

let log ?(meth="GET") url = function
  | None -> if !Verbose.v <> 0 then Format.printf "[ez_api] %s %s@." meth url
  | Some msg -> Format.printf "[>%s %s %s ]@." msg meth url

let pp_error fmt (e: [< error]) = match e with
  | `msg s -> Format.fprintf fmt "%s" s
  | `exn exn -> Format.fprintf fmt "exn: %s" (Printexc.to_string exn)
  | `timeout f -> Format.fprintf fmt "timeout %.1fs" f
  | `interval_timeout f -> Format.fprintf fmt "interval timeout %.1fs" f
  | `http (code, content) -> Format.fprintf fmt "http error %d: %s" code content
  | `invalid_url url -> Format.fprintf fmt "invalid url: %s" url

let error_handler cb error =
  cb (Error (match error with
      | `Malformed_response err -> `msg (Format.sprintf "malformed response: %s" err)
      | `Invalid_response_body_length _ -> `msg "invalid body length"
      | `Exn exn -> `exn exn))

let parse url =
  let uri = Uri.of_string url in
  match Uri.host uri, Uri.scheme uri with
  | Some h, Some sch ->
    let p = Option.value ~default:(if sch = "https" then 443 else 80) (Uri.port uri) in
    Ok (h, sch, p, Uri.path_and_query uri)
  | _ -> Error (`invalid_url url)

let response_handler :
  type r. ?msg:string -> res:r response -> url:string -> ((r, [> http_error]) result -> 'a) ->
  Response.t -> Body.Reader.t -> 'a = fun ?msg ~res ~url cb response body ->
  let b = Buffer.create 0x1000 in
  let open Response in
  log ~meth:("RECV " ^ (string_of_int @@ Status.to_code response.status)) url msg;
  let on_eof f () =
    let content = Buffer.contents b in
    if !Verbose.v land 1 <> 0 && content <> "" then Format.printf "[ez_api] received:\n%s@." content;
    cb @@ f res response content in
  let rec on_read f bs ~off ~len =
    Buffer.add_string b (Bigstringaf.substring ~off ~len bs);
    Body.Reader.schedule_read body ~on_read:(on_read f) ~on_eof:(on_eof f) in
  match response.status with
  | #Status.successful ->
    let f : type r. r response -> _ -> string -> (r, _) result = fun res response content -> match res with
      | Simple -> Ok content
      | WithHeaders ->
        let headers = Headers.to_list response.headers in
        (Ok { content; headers }) in
    let on_eof, on_read = on_eof f, on_read f in
    Body.Reader.schedule_read body ~on_read ~on_eof
  | _ ->
    let f _ _ content = Error (`http (Status.to_code response.status, content)) in
    let on_eof, on_read = on_eof f, on_read f in
    Body.Reader.schedule_read body ~on_read ~on_eof
