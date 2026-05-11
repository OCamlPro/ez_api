open EzRequestEio
open Ezjs_fetch_eio

let handle_response ?msg url r =
  match r with
  | Error e -> 0, Ezjs_min.to_string e##toString, []
  | Ok r ->
    Verbose.response ?msg ~code:r.status ~content:r.body url;
    if r.status >= 200 && r.status < 300 then r.status, r.body, r.headers
    else r.status, r.body, r.headers

let make ?msg ?content ?content_type ~(meth: EzAPI.Meth.t) ~headers url =
  let meth = EzAPI.Meth.to_string meth in
  Verbose.request ?msg ~meth ?content url;
  let headers = Option.fold ~none:headers ~some:(fun ct -> ("Content-Type", ct) :: headers) content_type in
  let body = Option.map (fun s -> RString s) content in
  let r = fetch ~headers ~meth ?body url to_text in
  handle_response ?msg url r

module Interface = struct
  let get ?(meth=`GET) ?(headers=[]) ?msg ~net:_ ~sw:_ url =
    make ~meth ~headers ?msg url

  let post ?(meth=`POST) ?(headers=[]) ?msg ?(content_type="application/json") ?(content="{}") ~net:_ ~sw:_ url =
    make ~meth ~headers ~content_type ~content ?msg url
end

include Make(Interface)

let () =
  Js_of_ocaml.Js.Unsafe.global##.set_verbose_ := Js_of_ocaml.Js.wrap_callback Verbose.set_verbose;
  Format.eprintf "ezFetch Loaded"
