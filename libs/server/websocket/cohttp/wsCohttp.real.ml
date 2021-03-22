open WsCommon

let ws ?step req ?onclose ~react ~bg id =
  let rsend = ref (fun _ -> ()) in
  let ping_loop, pong_fill = ping_pong ?step ?onclose id rsend in
  Lwt.bind (Websocket_cohttp_lwt.upgrade_connection req
              (ws_react ?onclose react pong_fill rsend)) @@ fun (r, send) ->
  rsend := send;
  Lwt.async (fun () -> Lwt.pick [ws_loop bg send; ping_loop ()]);
  Lwt.return_ok r
