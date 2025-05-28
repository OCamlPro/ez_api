open Test_session_lib
open Types
open EzAPI.TYPES
module Service = Services

(* Request implementation that REQUIRES with_crefentials to be always true
   (to use cookies, that is necessarily)*)
module Client = Cohttp_lwt_jsoo.Make_client_async (struct
  let chunked_response = true

  let chunk_size = 128 * 1024

  let convert_body_string = Js_of_ocaml.Js.to_bytestring

  let with_credentials = true
end)

module Base = EzCohttp_base.Make (Client)

module Interface = struct
  open Lwt.Infix

  let get ?meth ?headers ?msg url f =
    Lwt.async @@ fun () -> Base.get ?meth ?headers ?msg url >|= f

  let post ?meth ?content_type ?content ?headers ?msg url f =
    Lwt.async
    @@ fun () ->
    Base.post ?meth ?content_type ?content ?headers ?msg url >|= f
end

module Request = EzRequest.Make (Interface)
module Session = EzSessionClient.Make (SessionArg) (Request)

let wrap_res ?error f = function
  | Ok x -> f x
  | Error exn -> (
      let s = Printexc.to_string exn in
      match error with
      | None -> Printf.eprintf "%s" s
      | Some e -> e 500 (Some s) )

let get0 ?post ?headers ?params ?error ?msg ?(host = BASE api_host) service f
    =
  Request.get0 host service ?msg ?post ?headers ?error ?params
    (wrap_res ?error f)

let get1 ?post ?headers ?params ?error ?msg ?(host = BASE api_host) service f
    arg1 =
  Request.get1 host service arg1 ?msg ?post ?headers ?error ?params
    (wrap_res ?error f)

let post0 ?headers ?params ?error ?msg ?(host = BASE api_host) service f
    ~input =
  Request.post0 host service ?msg ?headers ?error ?params (wrap_res ?error f)
    ~input

let post1 ?headers ?params ?error ?msg ?(host = BASE api_host) service f
    ~input arg1 =
  Request.post1 host service arg1 ?msg ?headers ?error ?params
    (wrap_res ?error f) ~input

let string_of_test t =
  Printf.sprintf "{ name = %S;\n  query = %S;\n  version = %d;\n}" t.name
    t.query t.version

let waiter, finalizer = Lwt.wait ()

let waiting = ref false

let nrequests = ref 0

let begin_request () = incr nrequests

let end_request () =
  decr nrequests ;
  if !waiting && !nrequests = 0 then Lwt.wakeup finalizer ()

let error test n =
  Printf.eprintf "Error: request %s returned code %d\n%!" test n ;
  exit 2

let test1 api =
  begin_request () ;
  Request.get0 ~msg:"test1" api Service.test1 ~error:(error "test1")
    ~params:[(Service.param_arg, S "example-of-arg")]
    (function
      | Ok r ->
          Printf.eprintf "Test test1 returned %s\n%!" (string_of_test r) ;
          end_request ()
      | Error e ->
          Printf.eprintf "%s\n%!" @@ Printexc.to_string e ;
          end_request () )

let test1' api =
  begin_request () ;
  Request.get0 ~msg:"test1" api Service.test11 ~post:true
    ~error:(error "test1'")
    ~params:[(Service.param_arg, S "example-of-arg")]
    (function
      | Ok r ->
          Printf.eprintf "Test test1 returned %s\n%!" (string_of_test r) ;
          end_request ()
      | Error e ->
          Printf.eprintf "%s\n%!" @@ Printexc.to_string e ;
          end_request () )

let test2 api =
  begin_request () ;
  Request.get1 ~msg:"test2" api Service.test2 ~error:(error "test2")
    ~params:[(Service.param_arg, S " -- arg")]
    "arg-of-test2"
    (function
      | Ok r ->
          Printf.eprintf "Test test2 returned %s\n%!" (string_of_test r) ;
          end_request ()
      | Error e ->
          Printf.eprintf "%s\n%!" @@ Printexc.to_string e ;
          end_request () )

let test2' api =
  begin_request () ;
  Request.get1 ~msg:"test2" api Service.test22 ~post:true
    ~error:(error "test2'")
    ~params:[(Service.param_arg, S " -- arg")]
    "arg-of-test2"
    (function
      | Ok r ->
          Printf.eprintf "Test test2 returned %s\n%!" (string_of_test r) ;
          end_request ()
      | Error e ->
          Printf.eprintf "%s\n%!" @@ Printexc.to_string e ;
          end_request () )

let test3 arg api =
  begin_request () ;
  Request.post0 ~msg:"test3" api Service.test3 ~error:(error "test3")
    ~input:arg (function
    | Ok r ->
        Printf.eprintf "Test test3 returned %s\n%!" (string_of_test r) ;
        end_request ()
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string e ;
        end_request () )

let test4 arg api =
  let open EzSession.TYPES in
  begin_request () ;
  Session.connect api (function
    | Error _ ->
        Printf.eprintf "Error in connect\n%!" ;
        exit 2
    | Ok (Some _u) -> assert false
    | Ok None ->
        Session.login api ~login:user1_login ~password:user1_password
          (function
          | Error _ ->
              Printf.eprintf "Error in login\n%!" ;
              exit 2
          | Ok u ->
              Printf.eprintf "auth login = %S\n%!" u.auth_login ;
              assert (u.auth_login = user1_login) ;
              assert (u.auth_user_info = user1_info) ;
              Request.post1 ~msg:"test4" api Service.test4
                ~error:(error "test4") ~input:arg "arg-of-post1" (function
                | Ok r ->
                    Printf.eprintf "Test test4 returned %s\n%!"
                      (string_of_test r) ;
                    Session.logout api ~token:u.auth_token (function
                      | Error _ ->
                          Printf.eprintf "Error in logout\n%!" ;
                          exit 2
                      | Ok bool ->
                          Printf.eprintf "logout OK %b\n%!" bool ;
                          end_request () )
                | Error e ->
                    Printf.eprintf "%s\n%!" @@ Printexc.to_string e ;
                    end_request () ) ) )

open Js_of_ocaml

let auth_state = ref None

let update_button login =
  Console.console##log (if login then "LOGIN" else "LOGOUT") ;
  let login_button =
    Option.get
    @@ Dom_html.getElementById_coerce "submit-connection"
         Dom_html.CoerceTo.input
  in
  login_button##.value := Js.string (if login then "Login" else "Logout")

let update_connection_status status =
  Console.console##log (Js.string status) ;
  let elt = Dom_html.getElementById "connection-status" in
  elt##.innerHTML := Js.string status

let connect () =
  Console.console##log "1" ;
  Session.connect (EzAPI.BASE api_host) (function
    | Ok (Some auth) ->
        Console.console##log "2" ;
        update_connection_status @@ "Connected using cookie; (login = "
        ^ auth.EzSession.TYPES.auth_login ^ ")" ;
        update_button false ;
        auth_state := Some auth
    | Ok None ->
        Console.console##log "3" ;
        update_connection_status "Not connected (needs authentication)"
    | Error _ ->
        Console.console##log "4" ;
        update_connection_status "Not connected (connect error)" )

let setup_connection () =
  let login_button =
    Option.get
    @@ Dom_html.getElementById_coerce "submit-connection"
         Dom_html.CoerceTo.input
  in
  login_button##.onclick :=
    Dom.handler (fun _ ->
        ( match !auth_state with
        | Some auth ->
            Session.logout (BASE api_host)
              ~token:auth.EzSession.TYPES.auth_token (function
              | Ok _ ->
                  update_connection_status
                    "Not connected (needs authentication)" ;
                  auth_state := None ;
                  update_button true
              | Error _ ->
                  update_connection_status "Not disconnected (logout error)" )
        | None ->
            Session.login (BASE api_host) ~login:user1_login
              ~password:user1_password (function
              | Ok auth ->
                  update_connection_status
                    EzSession.TYPES.(
                      "Connected using login and password; (login = "
                      ^ auth.auth_login ^ ")" ) ;
                  auth_state := Some auth ;
                  update_button false
              | Error _ ->
                  update_connection_status "Not loggined (login error)" ) ) ;
        Js._false )

let setup_tests () =
  let error elt _ _ = elt##.style##.color := Js.string "red" in
  let callback elt _ = elt##.style##.color := Js.string "green" in
  let test1_elt = Dom_html.getElementById "test1" in
  let test1'_elt = Dom_html.getElementById "test1'" in
  let test2_elt = Dom_html.getElementById "test2" in
  let test2'_elt = Dom_html.getElementById "test2'" in
  let test3_elt = Dom_html.getElementById "test3" in
  let test4_elt = Dom_html.getElementById "test4" in
  let param1 = [(Service.param_arg, S " -- arg")] in
  let arg1 = "arg-of-test2" in
  let arg2 = {user= "m"; hash= "1"} in
  test1_elt##.onclick :=
    Dom.handler (fun _ ->
        get0 Services.test1 ~params:param1 ~error:(error test1_elt)
          (callback test1_elt) ;
        Js._false ) ;
  test1'_elt##.onclick :=
    Dom.handler (fun _ ->
        get0 Services.test11 ~params:param1 ~error:(error test1'_elt)
          (callback test1'_elt) ;
        Js._false ) ;
  test2_elt##.onclick :=
    Dom.handler (fun _ ->
        get1 Services.test2 ~error:(error test2_elt) (callback test2_elt)
          ~params:param1 arg1 ;
        Js._false ) ;
  test2'_elt##.onclick :=
    Dom.handler (fun _ ->
        get1 Services.test22 ~error:(error test2'_elt) (callback test2'_elt)
          ~params:param1 arg1 ;
        Js._false ) ;
  test3_elt##.onclick :=
    Dom.handler (fun _ ->
        post0 Service.test3 ~error:(error test3_elt) (callback test3_elt)
          ~input:arg2 ;
        Js._false ) ;
  test4_elt##.onclick :=
    Dom.handler (fun _ ->
        post1 Service.test4 ~error:(error test4_elt) (callback test4_elt)
          ~params:param1 ~input:arg2 arg1 ;
        Js._false )

let () = Request.init () ; connect () ; setup_tests () ; setup_connection ()
