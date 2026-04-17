open Httpun_ws

let (let$) = Result.bind

module type S = sig
  type 'a m
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val async : (unit -> unit m) -> unit
end

let log ?(action="recv") url =
  Option.iter @@ fun msg -> EzDebug.printf "[>%s %s %s]" msg action url

let parse url =
  let uri = Uri.of_string url in
  match Uri.host uri, Uri.scheme uri with
  | Some h, Some sch ->
    let p = Option.value ~default:(if sch = "https" || sch = "wss" then 443 else 80) (Uri.port uri) in
    Ok (h, sch, p, Uri.path_and_query uri)
  | _ -> Error (Format.sprintf "invalid url %s" url)

let error_handler f e =
  let e = match e with
    | `Handshake_failure _ -> "handshake failure"
    | `Malformed_response err -> Format.sprintf "malformed response: %s" err
    | `Invalid_response_body_length _ -> "invalid body length"
    | `Exn exn -> Printexc.to_string exn in
  f (Error e)

let send ~content wsd =
  let len, off = String.length content, 0 in
  let bs = Bigstringaf.of_string ~off ~len content in
  Wsd.schedule wsd bs ~kind:`Text ~off ~len

let read_payload ~payload f =
  let b = Buffer.create 100 in
  let on_eof () = f (Buffer.contents b) in
  let rec on_read bs ~off ~len =
    Buffer.add_string b (Bigstringaf.substring bs ~off ~len);
    Payload.schedule_read payload ~on_eof ~on_read in
  Payload.schedule_read payload ~on_eof ~on_read

let make_data content =
  let len, off = String.length content, 0 in
  let buffer = Bigstringaf.of_string ~off ~len content in
  { IOVec.buffer; off; len }

module Make(S: S) = struct
  let websocket_handler ?error ~react notifier action_notifier wsd =
    let b = Buffer.create 1024 in
    let close code =
      let code = Option.map Websocket.Close_code.of_code_exn code in
      Wsd.close ?code wsd; S.return (Ok ()) in
    let send content = send ~content wsd; S.return (Ok ()) in
    let a = (send, close) in
    action_notifier a;
    let frame ~opcode ~is_fin ~len:_ payload = match opcode with
      | `Ping ->
        read_payload ~payload @@ fun content ->
        let application_data = make_data content in
        Wsd.send_pong ~application_data wsd
      | `Pong -> ()
      | `Text | `Binary | `Continuation ->
        read_payload ~payload @@ fun content ->
        Buffer.add_string b content;
        if is_fin then (
          let c = Buffer.contents b in
          Buffer.clear b;
          S.async @@ fun () ->
          S.bind (react a c) @@ fun r ->
          (match r, error with Error e, Some f -> S.return (f a e) | _ -> S.return ()))
      | `Connection_close ->
        Wsd.close wsd;
        notifier (Ok ())
      | _ ->
        Wsd.close ~code:`Protocol_error wsd;
        notifier (Error "protocol error") in
    let eof ?error () =
      Option.iter (function `Exn exn ->
          Format.eprintf "websocket eof error: %s@." (Printexc.to_string exn)
        ) error;
      Wsd.close wsd in
    { Httpun_ws.Websocket_connection.frame; eof }
end
