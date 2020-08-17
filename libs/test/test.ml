
let (>>=) = Lwt.(>>=)

let server_mode = ref true
let api_port = ref 8887
let root_port = ref 8888
let root = ref None
let default = ref None

module Types = struct

  type input = {
      user : string;
      hash : string;
    }

  type output = {
      name : string;
      query : string;
      version : int;
    }

  type user_info = string

end


module Encoding = struct
  open Types
  open Json_encoding

  let input =
    conv
      (fun {user;hash} -> (user,hash))
      (fun (user,hash) -> {user;hash})
      (obj2
         (req "user" string)
         (req "hash" string))

  let output =
    conv
      (fun {name;query;version} -> (name, query, version))
      (fun (name, query, version) -> {name;query;version})
      (obj3
         (req "name" string)
         (req "query" string)
         (req "version" int))

  let user_info = string

end

let user1_login = "user1"
let user1_password = EzSession.Hash.password user1_login "dalfhhdgkfh"
let user1_info = "info-on-user"

module SessionArg = struct
  type user_id = string
  type user_info = Types.user_info
  let user_id_encoding = Json_encoding.string
  let user_info_encoding = Encoding.user_info
  let rpc_path = []
  let token_kind = `CSRF "X-Csrf-Token" (* `Cookie "EZSESSION" *)
end


module MakeService(S : sig end) = struct

  let arg_test =
    EzAPI.arg_string "arg-in-path" "example-of-value"

  let param_arg =
    EzAPI.Param.string ~name:"arg-string" ~descr:"An example argument" "arg"

  let section_test = EzAPI.section "Tests Requests"

  let test1 : Types.output EzAPI.service0  =
    EzAPI.service
      ~section:section_test
      ~name:"test1"
      ~params:[param_arg]
      ~output:Encoding.output
      EzAPI.Path.(root // "test1")

  let test2 : (string, Types.output) EzAPI.service1  =
    EzAPI.service
      ~section:section_test
      ~name:"test2"
      ~params:[param_arg]
      ~output:Encoding.output
      EzAPI.Path.(root // "test2" /: arg_test)

  let test3 : (Types.input,Types.output) EzAPI.post_service0  =
    EzAPI.post_service
      ~section:section_test
      ~name:"test3"
      ~params:[]
      ~input:Encoding.input
      ~output:Encoding.output
      EzAPI.Path.(root // "test3")

  let test4 : (string,Types.input,Types.output) EzAPI.post_service1  =
    EzAPI.post_service
      ~section:section_test
      ~name:"test4"
      ~params:[param_arg]
      ~input:Encoding.input
      ~output:Encoding.output
      EzAPI.Path.(root // "test4" /: arg_test)

end

module MakeServer(S : sig end) = struct
  module Service = MakeService(S)

  module Users = EzSessionServer.UserStoreInMemory(SessionArg)
  module Session = EzSessionServer.Make(Users)

  module Handler = struct

    open Types
    open EzSession.TYPES

    let test1 params () =
      match EzAPI.find_param Service.param_arg params with
      | None -> failwith "test1: missing argument"
      | Some s ->
         EzAPIServerUtils.return { name = "test1"; query = s; version = 1 }

    let test2 (params, s) () =
      match EzAPI.find_param Service.param_arg params with
      | None -> failwith "test2: missing argument"
      | Some arg ->
         EzAPIServerUtils.return { name = "test2";
                                        query = s ^ arg; version = 1 }

    let test3 s r =
      EzAPIServerUtils.return { name = "test3";
                                     query = r.user^r.hash; version = 1 }

    let test4 (req,s) r =
      Session.get_request_session req >>= function
      | Some { session_login } ->
         EzAPIServerUtils.return { name = "test4 for " ^ session_login;
                                     query = r.user^r.hash; version = 1 }
      | None ->
         EzAPIServerUtils.return_error 403
  end

  let dir =
    EzAPIServerUtils.empty
    |> EzAPIServerUtils.register Service.test1 Handler.test1
    |> EzAPIServerUtils.register Service.test2 Handler.test2
    |> EzAPIServerUtils.register Service.test3 Handler.test3
    |> EzAPIServerUtils.register Service.test4 Handler.test4
    |> Session.register_handlers

  let main () =
    Users.create_user ~password:user1_password ~login:user1_login user1_info;
    let servers = [ !api_port, EzAPIServerUtils.API dir ] in
    let servers = match !root with
      | None -> servers
      | Some root ->
         (!root_port, EzAPIServerUtils.Root (root, !default)) :: servers
    in
    Lwt_main.run (
        Printf.eprintf "Starting servers on ports %s\n%!"
                       (String.concat ","
                                      (List.map (fun (port,_) ->
                                           string_of_int port) servers));
        EzAPIServer.server servers
      )

end

module MakeClient(S : sig end) = struct

  module Service = MakeService(S)
  open Types

  module Session = EzSessionClient.Make(SessionArg)
  open Session.TYPES
  open EzAPI.TYPES

  let string_of_test t =
    Printf.sprintf "{ name = %S;\n  query = %S;\n  version = %d;\n}"
                   t.name t.query t.version

  let waiter,finalizer = Lwt.wait ()
  let waiting = ref false
  let nrequests = ref 0
  let begin_request () = incr nrequests
  let end_request () =
    decr nrequests;
    if !waiting && !nrequests = 0 then
      Lwt.wakeup finalizer ()

  let error test n =
    Printf.eprintf "Error: request %s returned code %d\n%!" test n;
    exit 2

  let test1 api =
    begin_request ();
    EzRequest.ANY.get0 api
                       Service.test1 "test1"
                       ~error:(error "test1")
                       ~params:[ Service.param_arg, S "example-of-arg"]
                       (fun r ->
                         Printf.eprintf
                           "Test test1 returned %s\n%!"
                           (string_of_test r);
                         end_request ()
                       )
                       ()

  let test1' api =
    begin_request ();
    EzRequest.ANY.get0 api
                       Service.test1 "test1"
                       ~post:true
                       ~error:(error "test1'")
                       ~params:[ Service.param_arg, S "example-of-arg"]
                       (fun r ->
                         Printf.eprintf
                           "Test test1 returned %s\n%!"
                           (string_of_test r);
                         end_request ()
                       )
                       ()

  let test2 api =
    begin_request ();
    EzRequest.ANY.get1 api
                       Service.test2 "test2"
                       ~error:(error "test2")
                       ~params: [Service.param_arg, S " -- arg" ]
                       (fun r ->
                         Printf.eprintf
                           "Test test2 returned %s\n%!"
                           (string_of_test r);
                         end_request ()
                       )
                       "arg-of-test2"

  let test2' api =
    begin_request ();
    EzRequest.ANY.get1 api
                       Service.test2 "test2"
                       ~post:true
                       ~error:(error "test2'")
                       ~params: [Service.param_arg, S " -- arg" ]
                       (fun r ->
                         Printf.eprintf
                           "Test test2 returned %s\n%!"
                           (string_of_test r);
                         end_request ()
                       )
                       "arg-of-test2"

  let test3 arg api =
    begin_request ();
    EzRequest.ANY.post0 api
                        Service.test3 "test3"
                        ~error:(error "test3")
                        ~input:arg
                        (fun r ->
                          Printf.eprintf
                            "Test test3 returned %s\n%!"
                            (string_of_test r);
                          end_request ()
                        )

  let test4 arg api =
    begin_request ();
    Session.connect
      api
      (function
       | Error _ ->
          Printf.eprintf "Error in connect\n%!";
          exit 2
       | Ok (Some u) -> assert false
       | Ok None ->
          Session.login
            api
            user1_login user1_password
            (function
             | Error _ ->
                Printf.eprintf "Error in login\n%!";
                exit 2
             | Ok u ->
                Printf.eprintf "auth login = %S\n%!" u.auth_login;
                assert (u.auth_login = user1_login);
                assert (u.auth_user = user1_info);
                EzRequest.ANY.post1
                  api
                  Service.test4 "test4"
                  ~error:(error "test4")
                  ~input:arg
                  ~headers:(
                    ("X-Another-Header2:", "x2") ::
                    Session.auth_headers u.auth_token)
                  "arg-of-post1"
                  (fun r ->
                    Printf.eprintf
                      "Test test4 returned %s\n%!"
                      (string_of_test r);
                    Session.logout
                      api
                      ~token:u.auth_token
                      (function
                       | Error _ ->
                          Printf.eprintf "Error in logout\n%!";
                          exit 2
                       | Ok bool ->
                          Printf.eprintf "logout OK %b\n%!" bool;
                          end_request ()
                  ))
      ))


  let main () =
    let api = Printf.sprintf "http://localhost:%d" !api_port in
    let api = BASE api in
    let requests = [
        test1;
        test1';
        test2;
        test2';
        test3 { user = "toto"; hash = "hash-of-toto"};
        test4 { user = "toto"; hash = "hash-of-toto"};
      ]
    in
    List.iter (fun test -> test api) requests;
    if !nrequests > 0 then begin
        waiting := true;
        Lwt_main.run waiter
      end

end

let main () =
  Arg.parse [
      "--client", Arg.Clear server_mode, " Run in client mode";
      "--api-port", Arg.Int ((:=) api_port), "PORT Run API on this port";
      "--root-port", Arg.Int ((:=) root_port), "PORT Run root FS on this port";
      "--root", Arg.String (fun s -> root := Some s),
      "ROOT Serve files from ROOT";
      "--default", Arg.String (fun s -> default := Some s),
      "FILE Serve this FILE when not found in ROOT (`index.html` for example)";
    ]
            (fun s ->
              Printf.eprintf "Error: unexpected argument %S\nAborting.\n%!"
                             s)
            "ez-api-test [--client][--port PORT]";
  if !server_mode then
    let module Server = MakeServer(struct end) in
    Server.main ()
  else
    let module Client = MakeClient(struct end) in
    Client.main ()
