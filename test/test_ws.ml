open Lwt.Infix

let () =
  EzLwtSys.run @@ fun () ->
  EzWs.connect ~msg:"test" ~react:(fun s -> EzDebug.printf "message received %s" s; Lwt.return_unit)
    "ws://localhost:9000" >>= function
  | Ok {EzWs.send; _} ->
    EzLwtSys.sleep 10. >>= fun () ->
    send "message" >>= begin function
      | Ok _ ->
        EzDebug.printf "message sent";
        fst @@ Lwt.wait ()
      | Error (_, s) ->
        let s = Option.value ~default:"none" s in
        Lwt.return @@ EzDebug.printf "message error %s" s
    end
  | Error (_, s) ->
    let s = Option.value ~default:"none" s in
    Lwt.return @@ EzDebug.printf "cannot connect %s" s
