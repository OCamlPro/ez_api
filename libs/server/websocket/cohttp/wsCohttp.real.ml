open WsCommon

let ws req ?onclose ?step ~react ~bg id =
  let rsend = ref (fun _ -> ()) in
  let ping_loop, pong_fill = ping_pong ?step id rsend in
  Lwt.bind (Websocket_cohttp_lwt.upgrade_connection req
              (ws_react ?onclose react pong_fill rsend)) @@ fun (r, send) ->
  rsend := send;
  Lwt.async (fun () ->
      Lwt.bind
        (Lwt.pick [ws_loop bg send; ping_loop ()])
        (fun () -> close send;
          match onclose with
          | None -> Lwt.return_unit
          | Some f -> f ()));
  Lwt.return_ok r
