open Httpun
open Httpun_common

let (let$) = Result.bind

let perform ?msg ?meth ?content ?content_type ?(headers=[]) ?timeout:_ ~net ~sw handler url =
  let meth = match meth, content with
    | Some `PATCH, _ -> `Other "PATCH"
    | Some (`GET | `POST | `PUT | `DELETE as m), _ -> m
    | _, None -> `GET
    | _ -> `POST in
  log ~meth:(Method.to_string meth) url msg;
  (if !Verbose.v land 4 <> 0 then
     Format.printf "[ez_api] headers\n  %s@." @@
     String.concat "\n  " @@ List.map (fun (k, v) -> k ^ " : " ^ v) headers);
  (match !Verbose.v land 2 <> 0, content with
   | true, Some content when content <> "" -> Format.printf "[ez_api] sent:\n%s@." content;
   | _ -> ());
  let$ hostname, scheme, port, path = parse url in
  let w, notify = Eio.Promise.create () in
  let addresses = Eio_unix.run_in_systhread @@ fun () ->
    Unix.getaddrinfo hostname (Int.to_string port) [ Unix.(AI_FAMILY PF_INET) ] in
  let addr = Option.get @@ List.find_map (fun (addr : Unix.addr_info) -> match addr.Unix.ai_addr with
      | Unix.ADDR_UNIX _ -> None
      | Unix.ADDR_INET (addr, port) -> Some (`Tcp (Eio_unix.Net.Ipaddr.of_unix addr, port))) addresses in
  let socket = Eio.Net.connect ~sw net addr in
  let$ socket = if scheme <> "https" then Ok socket else Httpun_eio_tls.socket ~hostname socket in
  let host = if (port <> 80 && port <> 443) then Format.sprintf "%s:%d" hostname port else hostname in
  let headers = Headers.of_list @@
    [ "host", host ] @ headers @
    Option.fold ~none:[ "content-length", "0" ] ~some:(fun c -> [ "content-length", string_of_int (String.length c)]) content @
    Option.fold ~none:[] ~some:(fun c -> [ "content-type", c]) content_type in
  let req = Request.create ~headers meth path in
  let response_handler = handler (Eio.Promise.resolve notify) in
  let error_handler = error_handler (Eio.Promise.resolve notify) in
  let connection = Httpun_eio.Client.create_connection ~sw socket in
  let body = Httpun_eio.Client.request ~error_handler ~response_handler connection req in
  Option.iter (fun c -> Body.Writer.write_string body c) content;
  Body.Writer.close body;
  let r = Eio.Promise.await w in
  Eio.Promise.await @@ Httpun_eio.Client.shutdown connection;
  r

let call ?meth ?headers ?msg ?content ?content_type ~net ~sw url =
  let handler n = response_handler ?msg ~url ~res:WithHeaders n in
  let aux e = Format.asprintf "%a" pp_error e in
  match perform ?meth ?content ?content_type ?headers handler ~net ~sw url with
  | Error `http (code, content) -> code, content, []
  | Error (`timeout _ as e) -> 408, aux e, []
  | Error (`invalid_url _ as e) -> 400, aux e, []
  | Error e -> -1, aux e, []
  | Ok { content; headers } -> 200, content, headers

module Interface = struct
  let get ?(meth=`GET) ?headers ?msg:_ ~net ~sw url =
    let meth = (meth :> EzAPI.Meth.all) in
    call ~meth ?headers ~net ~sw url

  let post ?(meth=`POST) ?headers ?msg:_ ?(content_type="application/json") ?(content="{}") ~net ~sw url =
    call ~meth ?headers ~content ~content_type ~net ~sw url
end

include EzRequestEio.Make(Interface)
