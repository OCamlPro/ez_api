open Test_session_lib

let ( >>= ) = Lwt.( >>= )

module Service = Services
module Users = EzSessionServer.UserStoreInMemory (SessionArg)
module Session = EzSessionServer.Make (Users)

module Handler = struct
  open Types
  open EzSession.TYPES
  open EzAPIServer

  let test1 params _ () =
    match Req.find_param Service.param_arg params with
    | None -> failwith "test1: missing argument"
    | Some s -> return (Ok {name= "test1"; query= s; version= 1})

  let test2 (params, s) _ () =
    match Req.find_param Service.param_arg params with
    | None -> failwith "test2: missing argument"
    | Some arg -> return (Ok {name= "test2"; query= s ^ arg; version= 1})

  let test3 _params _ r =
    return (Ok {name= "test3"; query= r.user ^ r.hash; version= 1})

  let test4 (req, _arg) _ r =
    Session.get_request_session req
    >>= function
    | Some {session_login; _} ->
        return
          (Ok
             { name= "test4 for " ^ session_login
             ; query= r.user ^ r.hash
             ; version= 1 } )
    | None -> return ~code:401 (Error (failwith "You should login first"))
end

let dir =
  EzAPIServer.empty
  |> EzAPIServer.register Service.test1 Handler.test1
  |> EzAPIServer.register Service.test11 Handler.test1
  |> EzAPIServer.register Service.test2 Handler.test2
  |> EzAPIServer.register Service.test22 Handler.test2
  |> EzAPIServer.register Service.test3 Handler.test3
  |> EzAPIServer.register Service.test4 Handler.test4
  |> Session.register_handlers

let () =
  Users.create_user ~password:user1_password ~login:user1_login user1_info ;
  let servers = [(api_port, EzAPIServerUtils.API dir)] in
  EzLwtSys.run
  @@ fun () ->
  Printf.eprintf "Starting server on port %s\n%!"
    (String.concat ","
       (List.map (fun (port, _) -> string_of_int port) servers) ) ;
  EzAPIServer.server servers
