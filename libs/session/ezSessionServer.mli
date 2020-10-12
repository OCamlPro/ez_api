open EzSession.TYPES

val random_challenge : unit -> string

module type SessionStore = sig
  type user_id
  val create_session : ?foreign:foreign_info -> login:string -> user_id -> user_id session Lwt.t
  val get_session : cookie:string -> user_id session option Lwt.t
  val remove_session : user_id -> cookie:string -> unit Lwt.t
end

module type Arg = sig
  module SessionArg : EzSession.TYPES.SessionArg
  module SessionStore : SessionStore with type user_id = SessionArg.user_id
  val find_user : login:string ->
    (string option * SessionArg.user_id * SessionArg.user_info) option Lwt.t
  val check_foreign : origin:string -> token:string ->
    (string, int * string option) result Lwt.t
  val register_foreign : origin:string -> token:string ->
    (SessionArg.user_id * SessionArg.user_info option, int * string option) result Lwt.t
end

val default_check_foreign : origin:string -> token:string ->
  (_, int * string option) result Lwt.t
val default_register_foreign : origin:string -> token:string ->
  (_, int * string option) result Lwt.t

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

end


exception UserAlreadyDefined
exception NoPasswordProvided
module UserStoreInMemory(S : SessionArg with type user_id = string) : sig

  val create_user :
    ?pwhash:Digest.t ->
    ?password:string -> ?kind:string -> login:string -> S.user_info -> unit
  val remove_user : login:string -> unit
  val find_user : login:string -> (string option * S.user_id * S.user_info) option Lwt.t
  val check_foreign : origin:string -> token:string ->
    (string, int * string option) result Lwt.t
  val register_foreign : origin:string -> token:string ->
    (S.user_id * S.user_info option, int * string option) result Lwt.t

  module SessionArg : EzSession.TYPES.SessionArg
    with type user_info = S.user_info
     and type user_id = S.user_id
  module SessionStore : SessionStore with type user_id = S.user_id

end

module SessionStoreInMemory : SessionStore
