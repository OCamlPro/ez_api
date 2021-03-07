open EzAPIServer
open Lwt.Infix

(* react handler *)
let react _req _sec s =
  EzDebug.printf "server react: %s" s;
  Lwt.return_ok @@ "server echo: " ^ s

(* background loop handler *)
let rec bg =
  let i = ref (-1) in fun _req _sec send ->
  EzLwtSys.sleep 10. >>= fun () ->
  incr i;
  EzDebug.printf "server loop step %d" !i;
  send @@ Ok ("server send " ^ string_of_int !i);
  bg _req _sec send

(* server services *)
let services = register_ws Test_ws_lib.service ~react ~bg empty

let main () = server [ 8080, API services ]

let () =
  EzLwtSys.run main
