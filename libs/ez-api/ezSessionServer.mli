open EzSession.TYPES

module type SessionStore = sig
  type user_id
  val create_session : login:string -> user_id -> user_id session Lwt.t
  val get_session : cookie:string -> user_id session option Lwt.t
  val remove_session : user_id -> cookie:string -> unit Lwt.t
end

module type Arg = sig
  module SessionArg : EzSession.TYPES.SessionArg
  module SessionStore : SessionStore with type user_id = SessionArg.user_id
  val find_user : login:string ->
    (string * SessionArg.user_id *
     SessionArg.user_info) option Lwt.t
  (* val error_wrapper : (string -> string) option *)
end

module Make(S: Arg) : sig

  (* User `register_handlers` to declare the handlers for the authentification
     services *)
  val register_handlers :
    EzAPI.request EzAPIServerUtils.directory ->
    EzAPI.request EzAPIServerUtils.directory

 (* handlers that need authentification should use `get_request_session`
   to collect the user identity and session. *)
  val get_request_session :
    EzAPI.request -> S.SessionArg.user_id session option Lwt.t

  (* Use this one to be sure that OPTIONS requests are correctly replied to *)
  val register :
    ('arg, 'b, 'input, 'd, 'e) EzAPI.service ->
    ('arg -> 'input -> ('d, 'e) result EzAPIServerUtils.answer Lwt.t) ->
    EzAPI.request EzAPIServerUtils.directory ->
    EzAPI.request EzAPIServerUtils.directory

end





exception UserAlreadyDefined
exception NoPasswordProvided
module UserStoreInMemory(S : SessionArg with type user_id = string) : sig

  val create_user :
    ?pwhash:Digest.t ->
    ?password:string -> login:string -> S.user_info -> unit
  val remove_user : login:string -> unit
  val find_user : login:string -> (string * S.user_id * S.user_info) option Lwt.t

  module SessionArg : EzSession.TYPES.SessionArg
    with type user_info = S.user_info
     and type user_id = S.user_id
  module SessionStore : SessionStore with type user_id = S.user_id

end

module SessionStoreInMemory : SessionStore
