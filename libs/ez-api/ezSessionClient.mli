
module Make(S: EzSession.TYPES.SessionArg) : sig

  module TYPES : sig
    type auth = {
        auth_login : string;
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
    EzAPI.base_url ->
    login:string -> (* login *)
    password:string -> (* password *)
    ((auth, exn) result -> unit) -> unit

  val logout : EzAPI.base_url -> ((unit, exn) result -> unit) -> unit

  (* Tell the network layer that we think that the session has ended
     (a request probably returned an error status for missing auth).
     Since the state is kept internally, we have to do this first to
     trigger a full reconnection by `login`.
  *)
  val disconnected : unit -> unit

  (* In `CSRF mode, these headers should be added to all queries that
    need authentication *)
  val auth_headers : token:string -> (string * string) list

  val get : unit -> TYPES.auth option

  end
