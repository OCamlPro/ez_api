open Js_of_ocaml
open Lwt.Infix
open EzWsCommon
include Types

let ready socket = match socket##.readyState with
  | WebSockets.CONNECTING -> Error "websocket not yet ready"
  | WebSockets.CLOSING -> Error "websocket closing"
  | WebSockets.CLOSED -> Error "websocket closed"
  | _ -> Ok ()

let catch f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let send_frame socket content =
  catch (fun () -> socket##send (Js.string content))

let connect ?msg ?protocols ~react url =
  let url = match String.get url 0 with
    | 'h' -> "ws" ^ String.sub url 4 (String.length url - 4)
    | _ -> url in
  let protocols = match protocols with
    | None -> new%js Js.array_empty
    | Some l -> Js.array @@ Array.of_list @@ List.map Js.string l in
  log ~action:"connect" url msg;
  Lwt.return @@ catch @@ fun () ->
  let socket = new%js WebSockets.webSocket_withProtocols (Js.string url) protocols in
  let conn, n = Lwt.wait () in
  socket##.onmessage := Dom.handler @@ (fun e ->
      log url msg;
      Lwt.async (fun () ->
          react (fun s -> Lwt.return @@ send_frame socket s)
            (Js.to_string e##.data) >|= function
        | Ok () -> ()
        | Error e -> Lwt.wakeup n (Error e));
      Js._true);
  socket##.onerror := Dom.handler @@ (fun e ->
      Lwt.wakeup n @@
      Error ("websocket error: " ^ Js.to_string e##._type);
      Js._true);
  socket##.onclose := Dom.handler @@ (fun _e ->
      Lwt.wakeup n @@ Ok (); Js._true);
  let send content = match ready socket with
    | Error e -> Lwt.return (Error e)
    | Ok () -> Lwt.return @@ send_frame socket content in
  let close () = Lwt.return @@ catch (fun () -> socket##close) in
  {send; conn; close}

let connect0 ?msg ?protocols ~react base service =
  let EzAPI.TYPES.URL url = EzAPI.forge0 base service [] in
  let input = EzAPI.Service.input service.EzAPI.s in
  let output = EzAPI.Service.output service.EzAPI.s in
  let errors = EzAPI.Service.errors_encoding service.EzAPI.s in
  let react f s =
    let f i = f (EzAPI.IO.to_string input i) in
    match EzAPI.IO.res_from_string output (res_encoding errors) (react f) s with
    | Ok r -> r
    | Error e -> Lwt.return_error (EzEncoding.error_to_string e) in
  connect ?msg ?protocols ~react url >|= function
  | Error e -> Error e
  | Ok r ->
    let send i = r.send (EzAPI.IO.to_string input i) in
    Ok {r with send}
