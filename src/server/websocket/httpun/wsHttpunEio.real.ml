open Httpun_ws

let ws_loop bg notify_close wsd =
  let send = function
    | Error _ -> WsHttpun.close wsd; notify_close ()
    | Ok `none -> ()
    | Ok (`binary content) -> WsHttpun.send ~kind:`Binary ~content wsd
    | Ok (`text content) -> WsHttpun.send ~kind:`Text ~content wsd in
  bg send

let ws reqd upgrade ?onclose ?step ?body:_ ~react:r ~bg id =
  Eio.Switch.run @@ fun sw ->
  let open WsHttpun.Make(struct
      type 'a m = 'a
      let return = Fun.id
      let bind p f = f p
      let async f = Eio.Fiber.fork ~sw f
      let sleep t = Eio_unix.sleep t
    end) in
  let websocket_handler wsd =
    let ping_loop, pong = ping_pong ?step id wsd in
    let w, n = Eio.Promise.create () in
    let frame ~opcode ~is_fin:_ ~len:_ payload =
      react r pong (Eio.Promise.resolve n) ~opcode ~payload wsd in
    let eof ?error () =
      Option.iter (function `Exn exn -> Format.eprintf "websocket eof error: %s@." (Printexc.to_string exn)) error;
      Wsd.close wsd in
    Eio.Fiber.fork ~sw (fun () ->
        Eio.Fiber.any [
          (fun () -> ws_loop bg (Eio.Promise.resolve n) wsd);
          ping_loop;
          (fun () -> Eio.Promise.await w)
        ];
        Option.iter (fun f -> f ()) onclose);
    { Websocket_connection.frame; eof } in
  let upgrade_handler upgrade () =
    let ws_conn = Server_connection.create_websocket websocket_handler in
    upgrade (Gluten.make (module Server_connection) ws_conn) in
  match Handshake.respond_with_upgrade ~sha1:WsHttpun.sha1 reqd (upgrade_handler upgrade) with
  | Ok () -> Ok ()
  | Error err_str ->
    let response = Httpun.Response.create ~headers:(Httpun.Headers.of_list [ "connection", "close" ]) `Bad_request in
    Httpun.Reqd.respond_with_string reqd response err_str;
    Ok ()
