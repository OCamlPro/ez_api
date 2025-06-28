open Test_ppx_lib
open Lwt.Infix

let () =
  EzLwtSys.run @@ fun () ->
  EzReq_lwt.post0 (EzAPI.BASE "http://localhost:8080") echo_input ~input:"bla" >|= function
  | Error (code, `unknown msg) ->
    EzDebug.printf "error %d %s" code (Option.value ~default:"none" msg)
  | Error _ -> EzDebug.printf "error"
  | Ok s -> EzDebug.printf "ok %s" s
