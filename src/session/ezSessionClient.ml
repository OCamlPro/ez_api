
open EzSession.TYPES

module type Make_S = sig
  type auth

  type nonrec login_error = [
    | login_error
    | connect_error
    | logout_error
    | `Too_many_login_attempts
    | `Session_expired ]

  val connect :
    EzAPI.base_url ->
    ?token:string ->
    ((auth option, connect_error) result -> unit) -> unit

  val login :
    ?format:(string -> string) ->
    EzAPI.base_url ->
    ?login:string -> (* login *)
    ?password:string -> (* password *)
    ?foreign:(string * string) -> (* foreign authentication : origin * token *)
    ((auth, login_error) result -> unit) -> unit

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
  val get : unit -> auth option
end

module Make(S: SessionArg) : Make_S with
  type auth = (S.user_id, S.user_info) auth = struct

  type nonrec login_error = [
    | login_error
    | connect_error
    | logout_error
    | `Too_many_login_attempts
    | `Session_expired ]

(* If cookies are in use on server-side, `connect` might return
  an already authenticated user. Otherwise (CSRF protection),
  a token can be provided (saved in local storage).
*)

  module M = EzSession.Make(S)
  include M

  let set_cookie _token = (* TODO *)
    ()

  let remove_cookie _token = (* TODO *)
    ()

  type state =
    | Disconnected
    | Connected of EzSession.TYPES.auth_needed
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
       EzReq.get0 ~msg:"connect"
         api
         Service.connect
         ?headers
         ~params:[]
         (function
          | Ok (AuthOk auth) ->
             state := User auth;
             f (Ok (Some auth))
          | Ok (AuthNeeded auth_needed) ->
             state := Connected auth_needed;
             f (Ok None)
          | Error e ->
             state := Disconnected;
             f (Error e)
         )
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
      EzReq.get0 ~msg:"logout"
        api
        Service.logout
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

  let rec login_rec ?format ntries api ?login ?password ?foreign
      (f : (('a, login_error) result -> unit)) =
    if ntries = 0 then
      (try f (Error `Too_many_login_attempts) with _ -> ())
    else
      match !state with
      | Disconnected ->
        connect api
          (function
            | Error e -> f (Error (e :> login_error))
            | Ok None -> login_rec ?format (ntries-1) api ?login ?password ?foreign f
            | Ok (Some u) ->
              if Some u.auth_login <> login then
                logout
                  api
                  ~token:u.auth_token
                  (function
                      Ok _ ->
                      login_rec ?format (ntries-1) api ?login ?password ?foreign f
                    | Error e ->
                      f (Error (e :> login_error)) )
              else
                f (Ok u))
      | User u ->
        if Some u.auth_login <> login then
          logout api
            ~token:u.auth_token
            (function
              | Ok _ ->
                login_rec ?format (ntries-1) api ?login ?password f
              | Error e ->
                f (Error (e :> login_error))
            )
        else
          f (Ok u)
      | Connected { challenge_id; challenge } ->
        match login, password, foreign with
        | Some login, Some password, _ ->
          let pwhash = EzSession.Hash.password ~login ~password in
          let pwhash = match format with
            | None -> pwhash
            | Some f -> f pwhash in
          let login_challenge_reply = EzSession.Hash.challenge ~challenge ~pwhash in
          EzReq.post0 ~msg:"login" api Service.login
            ~input:(Local {
                login_user = login;
                login_challenge_id = challenge_id;
                login_challenge_reply;
              })
            (function
              | Ok (LoginOk u) ->
                set_cookie u.auth_token;
                state := User u;
                f (Ok u)
              | Ok _ ->
                f (Error `Unverified_user)
              | Error `Challenge_not_found_or_expired _ ->
                login_rec ?format (ntries-1) api ~login ~password f
              | Error e -> f (Error (e :> login_error))
            )
        | _, _, Some (foreign_origin, foreign_token) ->
          EzReq.post0 ~msg:"login" api Service.login
            ~input:(Foreign { foreign_origin; foreign_token })
            (function
              | Ok (LoginOk u) ->
                set_cookie u.auth_token;
                state := User u;
                f (Ok u)
              | Ok _ ->
                f (Error `Unverified_user)
              | Error `Challenge_not_found_or_expired _ ->
                login_rec ?format (ntries-1) api ?foreign f
              | Error e -> f (Error (e :> login_error))
            )
        | _ -> assert false

  let login ?format api ?login ?password ?foreign f =
    login_rec ?format 4 api ?login ?password ?foreign f

  let get () =
    match !state with
    | User u -> Some u
    | Disconnected
      | Connected _ -> None

end
