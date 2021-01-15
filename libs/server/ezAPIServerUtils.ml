open EzAPI.TYPES
open Lwt.Infix
open Resto1

type 'a directory = {
  meth_GET :'a RestoDirectory1.directory;
  meth_OPTIONS :'a RestoDirectory1.directory;
}

type 'output answer = 'output RestoDirectory1.Answer.answer

let empty = {
  meth_GET = RestoDirectory1.empty;
  meth_OPTIONS = RestoDirectory1.empty;
}

module StringSet = Set.Make(String)

let return ?code x = RestoDirectory1.Answer.return ?code x
let return_raw ?code s = RestoDirectory1.Answer.return_raw ?code s

let verbose = match Sys.getenv_opt "EZAPISERVER" with
  | None -> ref 0
  | Some s -> match int_of_string_opt s with
    | None -> ref 1
    | Some i -> ref i

let set_verbose i = verbose := i

let pp_time () =
  GMTime.(date_of_tm @@ Unix.gmtime @@ time ())

let debug ?(v=0) fmt =
  if !verbose > v then EzDebug.printf fmt
  else Printf.ifprintf () fmt

let debugf ?(v=0) f =
  if !verbose > v then f ()

exception EzRawReturn of string
exception EzRawError of int
exception EzContentError of (int * string)

(* Remove empty strings from a list of string *)
let list_trim l = List.filter (fun s -> s <> "") l


let t0 = GMTime.time ()
let req_time = ref t0
let set_req_time () =
  req_time := GMTime.time ()
let req_time () = !req_time

type timings = {
  mutable timings_ok : Timings.t array;
  mutable timings_fail : Timings.t array;
}

let timings = {
  timings_ok = [||];
  timings_fail = [||];
}

let add_timing n ok t dt =
  if ok then
    Timings.add timings.timings_ok.(n) t dt
  else
    Timings.add timings.timings_fail.(n) t dt

let init_timings nservices =
  timings.timings_ok <-Array.init nservices
      (fun _ -> Timings.create t0);
  timings.timings_fail <- Array.init nservices
      (fun _ -> Timings.create t0);
  ()

let req_ips = Hashtbl.create 1111

let register_ip ip =
  try
    let s = Hashtbl.find req_ips ip in
    s.ip_last <- req_time();
    s.ip_nb <- s.ip_nb + 1
  with Not_found ->
    let gi = Geoip.init_exn Geoip.GEOIP_MEMORY_CACHE in
    let country_name =
      match Geoip.country_name_by_name gi ip with
      | None -> ""
      | Some s -> s
    in
    let country_code =
      match Geoip.country_code_by_name gi ip with
      | None -> ""
      | Some s -> s
    in
    Geoip.close gi;
    let s = {
      ip_ip = ip;
      ip_last = req_time();
      ip_nb = 1;
      ip_country = (country_name, country_code);
    } in
    Hashtbl.add req_ips ip s

exception EzReturnOPTIONS of (string * string) list

let options_headers_of_security (sec : [< EzAPI.security_scheme ] list) =
  let values =
    List.fold_left (fun headers -> function
        | `Nosecurity _ -> headers
        | `Basic _ | `Bearer _ -> StringSet.add "Authorization" headers
        | `Query _ -> headers
        | `Header { EzAPI.name; _ } -> StringSet.add name headers
        | `Cookie _ -> StringSet.add "Cookie" headers
      ) StringSet.empty sec
    |> StringSet.elements in
  match values with
  | [] -> []
  | _ -> ["access-control-allow-headers", String.concat ", " values]

let register ?(options_headers=[]) service handler dir =
  let security = EzAPI.service_security service in
  let options_headers =
    options_headers @
    options_headers_of_security security
  in
  let handler_GET a b =
    try
      handler a security b
    with
    | (EzRawReturn _ | EzRawError _ | EzContentError _) as exn -> Lwt.fail exn
    | exn ->
      Printf.eprintf "*** exception %s in handler ***\n%!"
        (Printexc.to_string exn);
      Lwt.fail EzAPI.ResultNotfound
  in

  let handler_GET a b =
    let t0 = req_time () in
    let add_timing_wrap b =
      let t1 = Unix.gettimeofday () in
      add_timing (EzAPI.id service) b t0 (t1-.t0)
    in
    Lwt.catch
      (function () ->
         handler_GET a b >>=
         function res ->
           add_timing_wrap true;
           Lwt.return res)
      (function
        | (EzRawReturn _ | EzRawError _ | EzContentError _) as exn ->
          add_timing_wrap true;
          Lwt.fail exn
        | exn ->
          add_timing_wrap false;
          Lwt.fail exn)
  in
  let handler_OPTIONS _a _b =
    Lwt.fail (EzReturnOPTIONS options_headers)
  in
  let service_GET, service_OPTIONS = EzAPI.register service in
  {
    meth_GET =
      RestoDirectory1.register dir.meth_GET service_GET handler_GET;
    meth_OPTIONS =
      RestoDirectory1.register dir.meth_OPTIONS service_OPTIONS
        handler_OPTIONS;
  }

let json_root = function
  | `O ctns -> `O ctns
  | `A ctns -> `A ctns
  | `Null -> `O []
  | oth -> `A [ oth ]

type reply =
  | ReplyNone
  | ReplyJson of Json_repr.Ezjsonm.value
  | ReplyString of (* content-type *) string * (* content *) string

type server_kind =
  | API of EzAPI.request directory
  | Root of string * string option

type server = {
  server_port : int;
  server_kind : server_kind;
}

let reply_none code = Lwt.return (code, ReplyNone)
let reply_json code json = Lwt.return (code, ReplyJson json)
let reply_raw_json code json = Lwt.return (code, ReplyString ("application/javascript",json))
let reply_timeout () =
  Lwt.return @@ 408, ReplyNone
let reply_answer
    { RestoDirectory1.Answer.code; RestoDirectory1.Answer.body } =
  match body with
  | RestoDirectory1.Answer.Empty ->
    reply_none code
  | RestoDirectory1.Answer.Single json ->
    reply_json code json
  | RestoDirectory1.Answer.Single_raw s ->
    reply_raw_json code s
  | RestoDirectory1.Answer.Stream _ ->
    reply_none 500

let rev_extensions filename =
  List.rev (String.split_on_char '.'
              (String.lowercase_ascii
                 (Filename.basename filename)))

let normalize_path path =
  let rec normalize_path path revpath =
    match path, revpath with
    | [],_ -> List.rev revpath
    | ("" | ".") :: path, _ -> normalize_path path revpath
    | ".." :: path, _ :: revpath -> normalize_path path revpath
    | ".." :: path, [] -> normalize_path path []
    | dir :: path, revpath -> normalize_path path (dir :: revpath)
  in
  normalize_path path []

let content_type_of_file file =
  let exts = rev_extensions file in
  debug ~v:2 "content_type_of_file: [%s]" (String.concat "," exts);
  match exts with
  | "js" :: _ -> "application/javascript"
  | "pdf" :: _ -> "application/pdf"
  | "json" :: _ -> "application/json"
  | "xml" :: _ -> "application/xml"
  | "zip" :: _ -> "application/zip"

  | ("html" | "htm") :: _ -> "text/html"

  | "map" :: "css" :: _
  | "css" :: _ -> "text/css"

  | "png" :: _ -> "image/png"
  | "jpg" :: _ -> "image/jpeg"
  | "gif" :: _ -> "image/gif"
  | "svg" :: _ -> "image/svg+xml"

  | _ -> "application/octet-stream"

module FileString = struct
  let size = 16_000
  let s = Bytes.make size '\000'
  let b = Buffer.create size

  let read_file ic =
    Buffer.clear b;
    let rec iter ic b s =
      let nread = input ic s 0 size in
      if nread > 0 then begin
        Buffer.add_subbytes b s 0 nread;
        iter ic b s
      end
    in
    iter ic b s;
    Buffer.contents b

  let read_file filename =
    let ic = open_in_bin filename in
    try
      let s = read_file ic in
      close_in ic;
      s
    with e ->
      close_in ic;
      raise e
end

let rec reply_file ?(meth=Resto1.GET) ?default root path =
  let path = normalize_path path in
  let file = Filename.concat root (String.concat "/" path) in
  try
    let content_type = content_type_of_file file in
    match meth with
    | OPTIONS ->
      if Sys.file_exists file then
        Lwt.return (200, ReplyNone)
      else raise Not_found
    | _ ->
      let content = FileString.read_file file in
      EzDebug.printf "Returning file %S of length %d" file
        (String.length content);
      Lwt.return (200, ReplyString (content_type, content))
  with _exn ->
  match default with
  | None ->
    raise Not_found
  | Some file ->
    reply_file ~meth root
      (String.split_on_char '/' file)

let return_error ?content code =
  match content with
  | None -> raise (EzRawError code)
  | Some content -> raise (EzContentError (code, content))


module Legacy = struct

  open Lwt.Infix
  open EzAPI.Legacy

  let register ?options_headers
      (service : ('a, 'b, 'c, 'd) service)
      handler dir =
    let open RestoDirectory1.Answer in
    let handler a sec b =
      handler a sec b >>= fun { code ; body } ->
      let body = match body with
        | Empty -> Empty
        | Single res -> Single (Ok res)
        | Single_raw s -> Single_raw s
        | Stream { next; shutdown } ->
          Stream {
            next = (fun () ->
                next () >|= function
                | None -> None
                | Some res -> Some (Ok res)
              );
            shutdown = shutdown;
          } in
      Lwt.return { code ; body }
    in
    register ?options_headers service handler dir

end
