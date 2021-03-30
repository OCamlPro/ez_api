open Lwt.Infix

let () =
  Cohttp_lwt_unix.Debug.activate_debug ();
  EzLwtSys.run @@ fun () ->
  EzWs.connect ~msg:"test" ~react:(fun _send s ->
      EzDebug.printf "message received %s" s; Lwt.return_ok ())
    "ws://localhost:9000" >>= function
  | Ok {EzWs.action = {EzWs.send; _}; conn; _} ->
    EzLwtSys.sleep 10. >>= fun () ->
    send "message" >>= begin function
      | Ok _ ->
        EzDebug.printf "message sent";
        conn >>= fun _ ->
        Lwt.return @@ EzDebug.printf "loop error"
      | Error s ->
        Lwt.return @@ EzDebug.printf "message error %s" s
    end
  | Error s ->
    Lwt.return @@ EzDebug.printf "cannot connect %s" s
