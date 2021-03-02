open Js_of_ocaml
open Lwt.Infix
open EzWsCommon
include Types

let connect ?msg ~react url =
  let url = match String.get url 0 with
    | 'h' -> "ws" ^ String.sub url 4 (String.length url - 4)
    | _ -> url in
  log ~action:"connect" url msg;
  catch @@ fun () ->
  let socket = new%js WebSockets.webSocket (Js.string url) in
  let w, n = Lwt.wait () in
  socket##.onmessage := Dom.handler @@ (fun e ->
      log url msg;
      Lwt.async (fun () -> react (Js.to_string e##.data) >|= fun _ -> ());
      Js._true);
  socket##.onerror := Dom.handler @@ (fun e ->
      Lwt.wakeup n @@
      Error (0, Some ("websocket error: " ^ Js.to_string e##._type));
      Js._true);
  socket##.onclose := Dom.handler @@ (fun _e ->
      Lwt.wakeup n @@ Ok (); Js._true);
  let send content = match socket##.readyState with
    | WebSockets.CONNECTING -> Lwt.return_error (0, Some "websocket not yet ready")
    | WebSockets.CLOSING -> Lwt.return_error (2, Some "websocket closing")
    | WebSockets.CLOSED -> Lwt.return_error (3, Some "websocket closed")
    | _ ->
      catch (fun () -> socket##send (Js.string content); Lwt.return_ok ()) in
  let close () = catch (fun () -> socket##close; Lwt.return_ok ()) in
  Lwt.return_ok {send; react=w; close}

let connect0 :
  type i. ?msg:string -> react:('output -> unit Lwt.t) -> EzAPI.base_url ->
  (i, 'output, 'error, 'security) EzAPI.ws_service ->
  (i ws_res, int * string option) result Lwt.t = fun ?msg ~react base service ->
  let EzAPI.TYPES.URL url = EzAPI.forge0 base service [] in
  let input_type = EzAPI.service_input service in
  let output = EzAPI.service_output service in
  connect ?msg ~react:(fun s -> react (EzEncoding.destruct output s)) url >|= function
  | Error e -> Error e
  | Ok r ->
    let send (input : i) =
      let content = match input_type with
        | EzAPI.Empty -> ""
        | EzAPI.Binary _ -> input
        | EzAPI.Json enc -> EzEncoding.construct enc input in
      r.send content in
    Ok {r with send}
