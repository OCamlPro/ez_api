open EzRequestEio
open Ezjs_fetch_eio

module Impl = struct
  type t = unit
  type tag = [`Generic]
  let listen () ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw:_ _listen_addr = assert false
  let connect () ~sw:_ _addr = assert false
  let datagram_socket () ~reuse_addr:_ ~reuse_port:_ ~sw:_ _saddr = assert false
  let getaddrinfo () = assert false
  let getnameinfo () = assert false
end

let net =
  let handler = Eio.Net.Pi.network (module Impl) in
  Eio.Resource.T ((), handler)

let log ?(meth="GET") ?msg url = match msg with
  | None -> if !Verbose.v <> 0 then Ezjs_min.log "[ez_api] %s %s@." meth url
  | Some msg -> Ezjs_min.log_str ("[>" ^ msg ^ " " ^ meth ^ " " ^ url ^ "]")

let handle_response ?msg url r =
  match r with
  | Error e -> 0, Ezjs_min.to_string e##toString, []
  | Ok r ->
    log ~meth:("RECV " ^ string_of_int r.status) ?msg url;
    if !Verbose.v land 1 <> 0 && r.body <> "" then Format.printf "[ez_api] received:\n%s@." r.body;
    if r.status >= 200 && r.status < 300 then r.status, r.body, r.headers
    else r.status, r.body, r.headers

let make ?msg ?content ?content_type ~(meth: EzAPI.Meth.t) ~headers url =
  let meth = EzAPI.Meth.to_string meth in
  log ~meth ?msg url;
  if !Verbose.v land 2 <> 0 then (match content with
    | Some s when s <> "" -> Format.printf "[ez_api] sent:\n%s@." s
    | _ -> ());
  let headers = Option.fold ~none:headers ~some:(fun ct -> ("Content-Type", ct) :: headers) content_type in
  let body = Option.map (fun s -> RString s) content in
  let r = fetch ~headers ~meth ?body url to_text in
  handle_response ?msg url r

module Interface = struct
  type ctx = unit

  let get ?(meth=`GET) ?(headers=[]) ?msg ~net:_ ~sw:_ url =
    make ~meth ~headers ?msg url

  let post ?(meth=`POST) ?(headers=[]) ?msg ?(content_type="application/json") ?(content="{}") ~net:_ ~sw:_ url =
    make ~meth ~headers ~content_type ~content ?msg url
end

include Make(Interface)

let () =
  Js_of_ocaml.Js.Unsafe.global##.set_verbose_ := Js_of_ocaml.Js.wrap_callback Verbose.set_verbose;
  EzDebug.log "ezFetch Loaded"
