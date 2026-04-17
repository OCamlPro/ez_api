open Httpun_ws_common

include EzWsEioTypes.Types

let res_encoding err ok = Json_encoding.(union [
    case ok Result.to_option Result.ok;
    case err (function Error e -> Some e | _ -> None) Result.error
  ])

let connect ?msg ?protocols:_ ?error ~net ~sw ~react url =
  let$ host, _scheme, port, resource = parse url in
  let addresses = Eio_unix.run_in_systhread @@ fun () ->
    Unix.getaddrinfo host (Int.to_string port) [ Unix.(AI_FAMILY PF_INET) ] in
  let addr = Option.get @@ List.find_map (fun (addr : Unix.addr_info) -> match addr.Unix.ai_addr with
      | Unix.ADDR_UNIX _ -> None
      | Unix.ADDR_INET (addr, port) -> Some (`Tcp (Eio_unix.Net.Ipaddr.of_unix addr, port))) addresses in
  let socket = Eio.Net.connect ~sw net addr in
  log ~action:"connect" url msg;
  let nonce = EzAPI.Multipart.random_string () in
  let conn, n = Eio.Promise.create () in
  let action_w, action_n = Eio.Promise.create () in
  let react (send, close) s = react {send; close} s in
  let error = Option.map (fun f -> fun (send, close) s -> f {send; close} s) error in
  let open Make(struct
      type 'a m = 'a
      let return = Fun.id
      let bind p f = f p
      let async = Eio.Fiber.fork ~sw
    end) in
  let _client = Httpun_ws_eio.Client.connect ~sw ~nonce ~host ~port ~resource
      ~error_handler:(error_handler (Eio.Promise.resolve n))
      ~websocket_handler:(websocket_handler ?error ~react (Eio.Promise.resolve n) (Eio.Promise.resolve action_n)) socket in
  let send, close = Eio.Promise.await action_w in
  Ok { conn; action={ send; close } }

let connect0 ?msg ?protocols ?error ~net ~sw ~react base service =
  let EzAPI.URL url = EzAPI.forge0 base service [] in
  let input = EzAPI.Service.input service.EzAPI.s in
  let output = EzAPI.Service.output service.EzAPI.s in
  let errors = EzAPI.Service.errors_encoding service.EzAPI.s in
  let react action s =
    let send i = action.send (EzAPI.IO.to_string input i) in
    let action = {send; close = action.close} in
    match EzAPI.IO.res_from_string output (res_encoding errors) (react action) s with
    | Ok r -> r
    | Error (`destruct_exn exn) -> Error (Format.asprintf "%a" (Json_encoding.print_error ?print_unknown:None) exn) in
  let$ r = connect ?msg ?protocols ?error ~net ~sw ~react url in
  let send i = r.action.send (EzAPI.IO.to_string input i) in
  let action = {send; close = r.action.close} in
  Ok {r with action}
