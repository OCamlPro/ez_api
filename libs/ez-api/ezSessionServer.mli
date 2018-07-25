open EzSession.TYPES

module type SessionStore = sig
  val create_session : login:string -> session
  val get_session : cookie:string -> session option Lwt.t
  val remove_session : login:string -> cookie:string -> unit Lwt.t
end

module Make(S: sig
                module SessionArg : EzSession.TYPES.SessionArg
                module SessionStore : SessionStore
                val find_user :
                  login:string ->
                  (string * (* pwhash *)
                     SessionArg.user_info) option Lwt.t
              end) : sig

  (* User `register_handlers` to declare the handlers for the authentification
     services *)
  val register_handlers :
    EzAPI.request EzAPIServer.directory ->
    EzAPI.request EzAPIServer.directory

 (* handlers that need authentification should use `get_request_session`
   to collect the user identity and session. *)
  val get_request_session : EzAPI.request -> session option Lwt.t

  (* Use this one to be sure that OPTIONS requests are correctly replied to *)
  val register :
    ('arg, 'b, 'input, 'd) EzAPI.service ->
    ('arg -> 'input -> 'd EzAPIServer.answer Lwt.t) ->
    EzAPI.request EzAPIServer.directory ->
    EzAPI.request EzAPIServer.directory

end





exception UserAlreadyDefined
exception NoPasswordProvided
module UserStoreInMemory(S : SessionArg) : sig

  val create_user :
    ?pwhash:Digest.t ->
    ?password:string -> login:string -> S.user_info -> unit
  val remove_user : login:string -> unit
  val find_user : login:string -> (string * S.user_info) option Lwt.t

  module SessionArg : EzSession.TYPES.SessionArg
         with type user_info = S.user_info
  module SessionStore : SessionStore

end

module SessionStoreInMemory : SessionStore
