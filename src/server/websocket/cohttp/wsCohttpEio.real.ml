module Eio_IO = Websocket.Make (Cohttp_eio.Private.IO)

let send_frames stream oc =
  let buf = Buffer.create 128 in
  let send_frame fr =
    Buffer.clear buf;
    Eio_IO.write_frame_to_buf ~mode:Eio_IO.Server buf fr;
    Eio.Buf_write.string oc @@ Buffer.contents buf in
  let rec aux stream = Eio.Stream.take stream |> fun fr ->
    send_frame fr; aux stream in
  aux stream

let read_frames ic oc handler_fn =
  let read_frame () = Eio_IO.make_read_frame ~mode:Eio_IO.Server ic oc () in
  let rec inner () = read_frame () |> handler_fn |> inner in
  inner ()

let ws req ?onclose ?step ?body ~react:r ~bg id =
  let resp = WsCohttp.response req in
  let f ic oc =
    Eio.Switch.run @@ fun sw ->
    let open WsCohttp.Make(struct
        type 'a m = 'a
        let return = Fun.id
        let bind p f = f p
        let async f = Eio.Fiber.fork ~sw f
        let sleep t = Eio_unix.sleep t
      end) in
    let w, n = Eio.Promise.create () in
    let frames_out_stream = Eio.Stream.create max_int in
    let send fr = Option.iter (Eio.Stream.add frames_out_stream) fr in
    let ping_loop, pong_fill = ping_pong ?step id send in
    let handler = react r pong_fill send (Eio.Promise.resolve n) in
    Option.iter (react_content r send (Eio.Promise.resolve n)) body;
    Eio.Fiber.any [
      (fun () -> read_frames ic oc handler);
      (fun () -> send_frames frames_out_stream oc);
      (fun () -> WsCohttp.ws_loop bg send);
      (fun () -> ping_loop ());
      (fun () -> Eio.Promise.await w; Option.iter (fun f -> f ()) onclose)
    ] in
  Ok (`Expert (resp, f))
