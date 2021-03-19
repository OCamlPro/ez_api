open EzWs
open Lwt.Infix

let react _send = function
  | Ok s ->
    EzDebug.printf "client react: %s" s;
    Lwt.return_ok ()
  | Error exn ->
    EzDebug.printf "client react to error: %s" (Printexc.to_string exn);
    Lwt.return_ok ()

let error content =
  Lwt.return @@ EzDebug.printf "client error %s" content

let handle {conn; send; close} =
  let rec loop i =
    EzDebug.printf "client loop step %d" i;
    send @@ "client send " ^ string_of_int i >>= function
    | Error _ -> close ()
    | Ok () -> Lwt.bind (EzLwtSys.sleep 11.) (fun () -> loop (i+1)) in
  Lwt.choose [ conn; loop 0 ]

let main () =
  connect0 ~msg:"ws" ~react (EzAPI.BASE "http://localhost:8080") Test_ws_lib.service >>= function
  | Error e -> error e
  | Ok con -> handle con >>= function
    | Error e -> error e
    | Ok () ->
      EzDebug.printf "client connection ended properly";
      Lwt.return_unit

let () =
  EzLwtSys.run main
