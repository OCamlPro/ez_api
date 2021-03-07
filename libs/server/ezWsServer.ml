open Lwt.Infix
open Websocket
open Frame
open Websocket_lwt_unix

let react client =
  let rec inner () =
    Connected_client.recv client >>= fun fr ->
    match fr.Frame.opcode with
    | Frame.Opcode.Ping ->
      Connected_client.send client
        Frame.(create ~opcode:Opcode.Pong ~content:fr.content ()) >>=
      inner
    | Frame.Opcode.Close ->
      (* Immediately echo and pass this last message to the user *)
      if String.length fr.content >= 2 then
        let content = String.sub fr.content 0 2 in
        Connected_client.send client Frame.(create ~opcode:Opcode.Close ~content ())
      else
      Connected_client.send client @@ Frame.close 1000
    | Frame.Opcode.Pong ->
      inner ()
    | Frame.Opcode.Text
    | Frame.Opcode.Binary ->
      Connected_client.send client fr >>=
      inner
    | _ ->
      Connected_client.send client Frame.(close 1002)
  in inner ()

let server port =
  let echo_fun client =
    Lwt.catch (fun () -> react client) Lwt.fail in
  let on_exn exn =
    EzDebug.printf "\027[0;31mServer Error:\027[0m %s" @@ Printexc.to_string exn in
  establish_server ~on_exn ~mode:(`TCP (`Port port)) echo_fun
