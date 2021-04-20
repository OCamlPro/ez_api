open Lwt.Infix
open Websocket
open Websocket_lwt_unix
open Frame
open EzWsCommon
include Types

let catch f =
  Lwt.catch f (fun exn -> Lwt.return_error (Printexc.to_string exn))

let send_frame ?content ?opcode send =
  send (Frame.create ?opcode ?content ())

let aux_react =
  let b = Buffer.create 1024 in
  fun f action send fr ->
    match fr.opcode, fr.final with
    | Opcode.Ping, _ ->
      send_frame ~opcode:Opcode.Pong ~content:fr.content send >|= fun () ->
      Ok `Open
    | Opcode.Pong, _ -> Lwt.return_ok `Open
    | Opcode.Close, _ ->
      (if String.length fr.content >= 2 then
         send_frame ~opcode:Opcode.Close ~content:(String.sub fr.content 0 2) send
       else send (Frame.close 1000)) >|= fun () -> Ok `Closed
    | Opcode.Text, final | Opcode.Binary, final ->
      if final then f action fr.content >>= function
        | Ok () -> Lwt.return_ok `Open
        | Error e -> Lwt.return_error e
      else (Buffer.add_string b fr.content; Lwt.return_ok `Open)
    | Opcode.Continuation, final ->
      if final then (
        Buffer.add_string b fr.content;
        let c = Buffer.contents b in
        Buffer.clear b;
        f action c >>= function
        | Ok () -> Lwt.return_ok `Open
        | Error e -> Lwt.return_error e)
      else (Buffer.add_string b fr.content; Lwt.return_ok `Open)
    | _ ->
      send (Frame.close 1002) >>= fun _ ->
      Lwt.return_ok `Closed

let connect ?msg ?protocols ?error ~react url =
  let url = match String.get url 0 with
    | 'w' -> "http" ^ String.sub url 2 (String.length url - 2)
    | _ -> url in
  log ~action:"connect" url msg;
  let uri = Uri.of_string url in
  let ctx = Conduit_lwt_unix.default_ctx in
  let extra_headers = match protocols with
    | None -> None
    | Some l -> Some (Cohttp.Header.of_list [ "Sec-WebSocket-Protocol", String.concat "," l ]) in
  catch @@ fun () ->
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>=
  Conduit_lwt_unix.endp_to_client ~ctx >>= fun client ->
  with_connection ?extra_headers ~ctx client uri >>= fun (recv, send0) ->
  let close code =
    let code = match code with None -> 1000 | Some code -> code in
    catch (fun () -> send0 (Frame.close code) >>= Lwt.return_ok) in
  let send content = catch (fun () -> send_frame ~content send0 >>= Lwt.return_ok) in
  let action = {send; close} in
  let rec conn () =
    catch (fun () ->
        recv () >>= fun fr ->
        log ~action:(Opcode.to_string fr.opcode) url msg;
        aux_react react action send0 fr) >>= function
    | Ok `Open -> conn ()
    | Ok `Closed -> Lwt.return_ok ()
    | Error e -> match error with
      | Some f -> f action e; conn ()
      | None -> conn () in
  Lwt.return_ok { action; conn = conn () }

let connect0 ?msg ?protocols ?error ~react base service =
  let EzAPI.URL url = EzAPI.forge0 base service [] in
  let input = EzAPI.Service.input service.EzAPI.s in
  let output = EzAPI.Service.output service.EzAPI.s in
  let errors = EzAPI.Service.errors_encoding service.EzAPI.s in
  let react action s =
    let send i = action.send (EzAPI.IO.to_string input i) in
    let action = {send; close = action.close} in
    match EzAPI.IO.res_from_string output (res_encoding errors) (react action) s with
    | Ok r -> r
    | Error e -> Lwt.return_error (EzEncoding.error_to_string e) in
  connect ?msg ?protocols ?error ~react url >|= function
  | Error e -> Error e
  | Ok r ->
    let send i = r.action.send (EzAPI.IO.to_string input i) in
    let action = {send; close = r.action.close} in
    Ok {r with action}
