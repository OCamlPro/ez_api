
module Make(S: EzSession.TYPES.SessionArg) : sig

(* If cookies are in use on server-side, `connect` might return
  an already authenticated user. Otherwise (CSRF protection),
  a token can be provided (saved in local storage).
*)
  type user_id = S.user_id
  type user_info = S.user_info
  type nonrec auth = (S.user_id, S.user_info) EzSession.TYPES.auth

  module Service : EzSession.M with type user_id = user_id
                                and type user_info = user_info

  type login_error = [
    | EzSession.TYPES.login_error
    | EzSession.TYPES.connect_error
    | EzSession.TYPES.logout_error
    | `Too_many_login_attempts
    | `Session_expired ]

  val connect :
    EzAPI.base_url ->
    ?token:string ->
    (((S.user_id, S.user_info) EzSession.TYPES.auth option, EzSession.TYPES.connect_error) result -> unit) -> unit

  val login :
    ?format:(string -> string) ->
    EzAPI.base_url ->
    ?login:string -> (* login *)
    ?password:string -> (* password *)
    ?foreign:(string * string) -> (* foreing auth : origin, token *)
    (((S.user_id, S.user_info) EzSession.TYPES.auth, login_error) result -> unit) -> unit

  val logout :
    EzAPI.base_url ->
    token:string -> ((bool, EzSession.TYPES.logout_error) result -> unit) -> unit

  (* Tell the network layer that we think that the session has ended
     (a request probably returned an error status for missing auth).
     Since the state is kept internally, we have to do this first to
     trigger a full reconnection by `login`.
  *)
  val disconnected : unit -> unit

  (* In `CSRF mode, these headers should be added to all queries that
    need authentication *)
  val auth_headers : token:string -> (string * string) list

  val get : unit -> (S.user_id, S.user_info) EzSession.TYPES.auth option

  end
