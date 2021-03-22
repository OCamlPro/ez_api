open WsCommon

let ws ?step reqd fd ?onclose ~react ~bg () =
  let rsend = ref (fun _ -> ()) in
  let ping_loop, pong_fill = ping_pong ?step ?onclose rsend in
  Lwt.bind (Websocket_httpaf_lwt.upgrade_connection reqd fd
              (ws_react ?onclose react pong_fill rsend)) @@ fun (r, body, send) ->
  rsend := send;
  Lwt.async (fun () -> Lwt.pick [ws_loop bg send; ping_loop ()]);
  Lwt.return_ok (r, body)
