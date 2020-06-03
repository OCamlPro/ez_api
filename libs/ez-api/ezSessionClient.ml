
open EzSession.TYPES

module Make(S: SessionArg) : sig

  module TYPES : sig
    type auth = {
      auth_login : string;
      auth_user_id : S.user_id;
      auth_user : S.user_info;
      auth_token : string;
    }
  end
  open TYPES

(* If cookies are in use on server-side, `connect` might return
  an already authenticated user. Otherwise (CSRF protection),
  a token can be provided (saved in local storage).
*)
  val connect :
    EzAPI.base_url ->
    ?token:string ->
    ((auth option, exn) result -> unit) -> unit

  val login :
    ?format:(string -> string) ->
    EzAPI.base_url ->
    login:string -> (* login *)
    password:string -> (* password *)
    ((auth, exn) result -> unit) -> unit

  val logout :
    EzAPI.base_url ->
    token:string -> ((bool, exn) result -> unit) -> unit

  (* Tell the network layer that we think that the session has ended
     (a request probably returned an error status for missing auth).
     Since the state is kept internally, we have to do this first to
     trigger a full reconnection by `login`.
  *)
  val disconnected : unit -> unit

  val auth_headers : token:string -> (string * string) list
  val get : unit -> TYPES.auth option


  end = struct

  module TYPES = struct
    type auth = {
      auth_login : string;
      auth_user_id : S.user_id;
      auth_user : S.user_info;
      auth_token : string;
    }
  end
  open TYPES

  module M = EzSession.Make(S)
  include M

  let set_cookie _token = (* TODO *)
    ()

  let remove_cookie _token = (* TODO *)
    ()

  type state =
    | Disconnected
    | Connected of (* challenge_id *) string * (* challenge *) string
    | User of auth

  let state = ref Disconnected

  let disconnected () = state := Disconnected

  let auth_headers ~token =
    match S.token_kind with
    | `Cookie _name -> [] (* Cookies are automatically added by browsers *)
    | `CSRF name -> [name, token ]

  let connect api ?token f =
    begin
      match token with
      | None -> ()
      | Some _ -> disconnected ()
    end;
    match !state with
    | Disconnected ->
       let headers = match token with
         | Some token -> Some (auth_headers ~token)
         | None -> None
       in
       EzRequest.ANY.get0
         api
         Service.connect "connect"
         ~post:true
         ?headers
         ~error:(fun _n _ ->
             state := Disconnected;
             f (Error (Failure "Error connect")))
         ~params:[]
         (function
          | Ok AuthNeeded (challenge_id, challenge) -> (* TODO *)
             state := Connected (challenge_id, challenge);
             f (Ok None)
          | Ok AuthOK (auth_login, auth_user_id, auth_token, auth_user) ->
             let u = { auth_login; auth_user_id; auth_token; auth_user } in
             state := User u;
             f (Ok (Some u))
          | Error () -> assert false
         )
         ()
    | Connected _ ->
       (try f (Ok None) with _ -> ())
    | User u ->
       (try f (Ok (Some u)) with _ -> ())

  let rec login_rec ?format ntries api (u_login : string) u_password f =
    if ntries = 0 then
      (try f (Error (Failure "too many login attempts")) with _ -> ())
    else
      match !state with
      | Disconnected ->
        connect api
          (function
            | Error _ as err -> f err
            | Ok None -> login_rec ?format (ntries-1) api u_login u_password f
            | Ok (Some u) ->
              if u.auth_login <> u_login then
                logout
                  api
                  ~token:u.auth_token
                  (function
                      Ok _ ->
                      login_rec ?format (ntries-1) api u_login u_password f
                    | Error _ as err ->
                      f err )
              else
                f (Ok u))
      | User u ->
        if u.auth_login <> u_login then
          logout api
            ~token:u.auth_token
            (function
              | Ok _ ->
                login_rec ?format (ntries-1) api u_login u_password f
              | Error _ as err ->
                f err
            )
        else
          f (Ok u)
      | Connected (challenge_id, challenge) ->
        let pwhash = EzSession.Hash.password ~login:u_login ~password:u_password in
        let pwhash = match format with
          | None -> pwhash
          | Some f -> f pwhash in
        let challenge_reply =
          EzSession.Hash.challenge ~challenge ~pwhash
        in
        EzRequest.ANY.post0
          api
          Service.login "login"
          ~error:(fun code _ ->
              if code = 401 then
                login_rec ?format (ntries-1) api u_login u_password f
              else
                f (Error (Failure "Error login")))
          ~params:[]
          ~input: {
            login_user = u_login;
            login_challenge_id = challenge_id;
            login_challenge_reply = challenge_reply;
          }
          (function
            | Ok (auth_login, auth_user_id, auth_token, auth_user) ->
              let u = { auth_login; auth_user_id; auth_token; auth_user } in
              set_cookie u.auth_token;
              state := User u;
              f (Ok u)
            | Error () -> assert false (* TODO *)
          )

  and login ?format api ~login:u_login ~password:u_password f =
    login_rec ?format 4 api u_login u_password f

  and logout api ~token f =
    remove_cookie ();
    match !state with
    | Disconnected
    | Connected _
      -> (try f (Ok false) with _ -> ())
    | User u ->
      EzRequest.ANY.get0
        api
        Service.logout "logout"
        ~error:(fun _n _data -> f (Error (Failure "Error")))
        ~params:[]
        ~headers:(auth_headers ~token)
        (function
          | Ok (challenge_id, challenge) ->
           remove_cookie u.auth_token;
           state := Connected (challenge_id, challenge);
           f (Ok true)
          | Error () -> assert false (* TODO *)
        )
        ()

  let get () =
    match !state with
    | User u -> Some u
    | Disconnected
      | Connected _ -> None

end
