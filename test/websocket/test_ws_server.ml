open EzAPIServer
open Lwt.Infix

(* react handler *)
let react _req _sec s =
  EzDebug.printf "server react: %s" s;
  Lwt.return_ok @@ "server echo: " ^ s

(* background loop handler *)
let bg _req _sec send =
  let rec aux i =
    EzDebug.printf "server loop step %d" i;
    send @@ Ok ("server send " ^ string_of_int i);
    EzLwtSys.sleep 10. >>= fun () -> aux (i+1) in
  aux 0

(* server services *)
let services = register_ws Test_ws_lib.service ~react ~bg empty

let main () = server [ 8080, API services ]

let () =
  EzLwtSys.run main
