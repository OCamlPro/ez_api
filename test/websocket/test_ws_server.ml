open EzAPIServer
open Lwt.Infix

module ReqSet = Set.Make(Uuidm)

let on = ref ReqSet.empty

(* react handler *)
let react req _sec s =
  on := ReqSet.add req.EzAPI.Req.req_id !on;
  EzDebug.printf "server react: %s" s;
  Lwt.return_ok @@ "server echo: " ^ s

(* background loop handler *)
let bg req _sec send =
  on := ReqSet.add req.EzAPI.Req.req_id !on;
  let rec aux i =
    EzDebug.printf "server loop step %d" i;
    send @@ Ok ("server send " ^ string_of_int i);
    EzLwtSys.sleep 10. >>= fun () ->
    if ReqSet.mem req.EzAPI.Req.req_id !on then aux (i+1)
    else Lwt.return_unit in
  aux 0

let onclose req =
  on := ReqSet.remove req.EzAPI.Req.req_id !on;
  EzDebug.printf "server close";
  Lwt.return_unit

(* server services *)
let services = register_ws Test_ws_lib.service ~onclose ~react ~bg empty

let main () = server [ 8080, API services ]

let () =
  EzLwtSys.run main
