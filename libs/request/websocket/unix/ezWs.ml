open Lwt.Infix
open Websocket
open Websocket_lwt_unix
open Frame
open EzWsCommon
include Types

let aux_react =
  let b = Buffer.create 1024 in
  fun f_str send fr ->
    catch @@ fun () ->
    match fr.opcode, fr.final with
    | Opcode.Ping, _ ->
      send @@ Frame.create ~opcode:Opcode.Pong () >>= fun () ->
      Lwt.return_ok `Open
    | Opcode.Pong, _ -> Lwt.return_ok `Open
    | Opcode.Close, _ ->
      (if String.length fr.content >= 2 then
         send @@ Frame.create ~opcode:Opcode.Close ~content:(String.sub fr.content 0 2) ()
       else send @@ Frame.close 1000) >>= fun () ->
      Lwt.return_ok `Closed
    | Opcode.Text, final | Opcode.Binary, final ->
      if final then f_str fr.content >>= fun () -> Lwt.return_ok `Open
      else (Buffer.add_string b fr.content; Lwt.return_ok `Open)
    | Opcode.Continuation, final ->
      if final then (
        Buffer.add_string b fr.content;
        let c = Buffer.contents b in
        Buffer.clear b;
        f_str c >>= fun () -> Lwt.return_ok `Open)
      else (Buffer.add_string b fr.content; Lwt.return_ok `Open)
    | _ ->
      send @@ Frame.close 1002 >>= fun () ->
      Lwt.return_error (0, Some "opcode not handled")

let connect ?msg ~react:f url =
  let url = match String.get url 0 with
    | 'w' -> "http" ^ String.sub url 2 (String.length url - 2)
    | _ -> url in
  log ~action:"connect" url msg;
  let uri = Uri.of_string url in
  let ctx = Conduit_lwt_unix.default_ctx in
  catch @@ fun () ->
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>=
  Conduit_lwt_unix.endp_to_client ~ctx >>= fun client ->
  with_connection ~ctx client uri >>= fun (recv, send) ->
  let rec react () =
    recv () >>= fun fr -> log url msg; aux_react f send fr >>= function
    | Ok `Open -> react ()
    | Ok `Closed -> Lwt.return_ok ()
    | Error e -> Lwt.return_error e in
  let close () = catch (fun () -> send (Frame.close 1000) >>= Lwt.return_ok) in
  let send content =
    catch (fun () -> send @@ Frame.create ~content () >>= fun () -> Lwt.return_ok ()) in
  Lwt.return_ok {send; close; react = react ()}
