
open EzSession.TYPES

module Make(S: SessionArg) : sig

(* If cookies are in use on server-side, `connect` might return
  an already authenticated user. Otherwise (CSRF protection),
  a token can be provided (saved in local storage).
*)
  val connect :
    EzAPI.base_url ->
    ?token:string ->
    ((S.auth option, [`Session_expired]) result -> unit) -> unit

  val login :
    ?format:(string -> string) ->
    EzAPI.base_url ->
    login:string -> (* login *)
    password:string -> (* password *)
    ((S.auth, [ `Bad_user_or_password | `Too_many_login_attempts | `Invalid_session | `Session_expired ]) result -> unit) -> unit

  val logout :
    EzAPI.base_url ->
    token:string -> ((bool, logout_error) result -> unit) -> unit

  (* Tell the network layer that we think that the session has ended
     (a request probably returned an error status for missing auth).
     Since the state is kept internally, we have to do this first to
     trigger a full reconnection by `login`.
  *)
  val disconnected : unit -> unit

  val auth_headers : token:string -> (string * string) list
  val get : unit -> S.auth option


  end = struct

  module M = EzSession.Make(S)
  include M

  let set_cookie _token = (* TODO *)
    ()

  let remove_cookie _token = (* TODO *)
    ()

  type state =
    | Disconnected
    | Connected of EzSession.TYPES.auth_needed
    | User of S.auth

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
         ~params:[]
         (function
          | Ok (AuthOk auth) ->
             state := User auth;
             f (Ok (Some auth))
          | Ok (AuthNeeded auth_needed) ->
             state := Connected auth_needed;
             f (Ok None)
          | Error `Session_expired ->
             state := Disconnected;
             f (Error `Session_expired)
         )
         ()
    | Connected _ ->
       (try f (Ok None) with _ -> ())
    | User u ->
       (try f (Ok (Some u)) with _ -> ())

  let logout api ~token f =
    remove_cookie ();
    match !state with
    | Disconnected
    | Connected _
      -> (try f (Ok false) with _ -> ())
    | User u ->
      EzRequest.ANY.get0
        api
        Service.logout "logout"
        ~params:[]
        ~headers:(auth_headers ~token)
        (function
          | Ok auth_needed ->
            remove_cookie u.auth_token;
            state := Connected auth_needed;
            f (Ok true)
          | Error e ->
            f (Error e)
        )
        ()

  type login_error =
      [ `Bad_user_or_password | `Invalid_session | `Too_many_login_attempts | `Session_expired ]

  let rec login_rec ?format ntries api (u_login : string) u_password
      (f : (('a, login_error) result -> unit)) =
    if ntries = 0 then
      (try f (Error `Too_many_login_attempts) with _ -> ())
    else
      match !state with
      | Disconnected ->
        connect api
          (function
            | Error e -> f (Error (e :> login_error))
            | Ok None -> login_rec ?format (ntries-1) api u_login u_password f
            | Ok (Some u) ->
              if u.auth_login <> u_login then
                logout
                  api
                  ~token:u.auth_token
                  (function
                      Ok _ ->
                      login_rec ?format (ntries-1) api u_login u_password f
                    | Error e ->
                      f (Error (e :> login_error)) )
              else
                f (Ok u))
      | User u ->

        if u.auth_login <> u_login then
          logout api
            ~token:u.auth_token
            (function
              | Ok _ ->
                login_rec ?format (ntries-1) api u_login u_password f
              | Error e ->
                f (Error (e :> login_error))
            )
        else
          f (Ok u)
      | Connected { challenge_id; challenge } ->
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
          ~params:[]
          ~input: {
            login_user = u_login;
            login_challenge_id = challenge_id;
            login_challenge_reply = challenge_reply;
          }
          (function
            | Ok u ->
              set_cookie u.auth_token;
              state := User u;
              f (Ok u)
            | Error `Bad_user_or_password ->
              f (Error `Bad_user_or_password)
            | Error `Challenge_not_found_or_expired _ ->
              login_rec ?format (ntries-1) api u_login u_password f
          )

  let login ?format api ~login:u_login ~password:u_password f =
    login_rec ?format 4 api u_login u_password f

  let get () =
    match !state with
    | User u -> Some u
    | Disconnected
      | Connected _ -> None

end
