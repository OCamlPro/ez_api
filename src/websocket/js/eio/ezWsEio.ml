open Js_of_ocaml
include EzWsEioTypes.Types

let log ?(action="recv") url =
  Option.iter @@ fun msg -> EzDebug.printf "[>%s %s %s]" msg action url

let res_encoding err ok = Json_encoding.(union [
    case ok Result.to_option Result.ok;
    case err (function Error e -> Some e | _ -> None) Result.error
  ])

let ready socket = match socket##.readyState with
  | WebSockets.CONNECTING -> Error "websocket not yet ready"
  | WebSockets.CLOSING -> Error "websocket closing"
  | WebSockets.CLOSED -> Error "websocket closed"
  | _ -> Ok ()

let catch f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let send_frame socket content =
  catch (fun () -> socket##send (Js.string content))

let connect ?msg ?protocols ?error ~net:_ ~sw ~react url =
  let url = match String.get url 0 with
    | 'h' -> "ws" ^ String.sub url 4 (String.length url - 4)
    | _ -> url in
  let protocols = match protocols with
    | None -> new%js Js.array_empty
    | Some l -> Js.array @@ Array.of_list @@ List.map Js.string l in
  log ~action:"connect" url msg;
  let w0, n0 = Eio.Promise.create () in
  try
    let socket = new%js WebSockets.webSocket_withProtocols (Js.string url) protocols in
    let send content = match ready socket with
      | Error e -> Error e
      | Ok () -> send_frame socket content in
    let close code =
      let code = match code with None -> 1002 | Some code -> code in
      catch (fun () -> socket##close_withCode code) in
    let action = {send; close} in
    socket##.onmessage := Dom.handler @@ (fun e ->
        log url msg;
        let s = Js.to_string e##.data in
        Eio.Fiber.fork ~sw (fun () ->
            Result.iter_error (fun e -> match error with
                | Some f -> f action e
                | None -> ())
              (react action s));
        Js._true);
    socket##.onerror := Dom.handler @@ (fun e ->
        match error with
        | Some f -> f action ("websocket error: " ^ Js.to_string e##._type); Js._true
        | None -> (); Js._true);
    let conn, n = Eio.Promise.create () in
    socket##.onclose := Dom.handler @@ (fun _e ->
        Eio.Promise.resolve n @@ Ok (); Js._true);
    socket##.onopen := Dom.handler @@ (fun _e ->
        Eio.Promise.resolve n0 (Ok {action; conn}); Js._true);
    Eio.Promise.await w0
  with exn -> Error (Printexc.to_string exn)

let connect0 ?msg ?protocols ?error ~net ~sw ~react base service =
  let EzAPI.URL url = EzAPI.forge0 base service [] in
  let input = EzAPI.Service.input service.EzAPI.s in
  let output = EzAPI.Service.output service.EzAPI.s in
  let errors = EzAPI.Service.errors_encoding service.EzAPI.s in
  let react a s =
    let send i = a.send (EzAPI.IO.to_string input i) in
    match EzAPI.IO.res_from_string output (res_encoding errors) (react {send; close=a.close}) s with
    | Ok r -> r
    | Error (`destruct_exn exn) -> Error (Format.asprintf "%a" (Json_encoding.print_error ?print_unknown:None) exn) in
  Result.map (fun r ->
      let send i = r.action.send (EzAPI.IO.to_string input i) in
      let action = {send; close=r.action.close} in
      {r with action}
    ) @@ connect ?msg ?protocols ?error ~react ~net ~sw url
