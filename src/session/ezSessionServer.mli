open EzSession.TYPES
open EzAPIServerUtils

(** Returns random challenge 30 characters long *)
val random_challenge : unit -> string

(** Module that defines a way to store user's sessions. *)
module type SessionStore = sig

  (** User id. See {!EzSession.TYPES.SessionArg.user_id}. *)
  type user_id

  (** Creates session from the specified information, generates random token, and associates
  session to the given token in storage. *)
  val create_session : ?foreign:foreign_info -> login:string -> req:Req.t -> user_id -> user_id session Lwt.t
  
  (** Looks up for the session associated with the given token. If [req] is also specified, 
  then the returned session synchronizes its time with request. *)
  val get_session : ?req:Req.t -> string -> user_id session option Lwt.t
  
  (** Removes session from [session_by_token] if user_id corresponds to session's one. *)
  val remove_session : user_id -> token:string -> unit Lwt.t
end

(** Main argument for Make functor. Specifies session's and user's storage and session main 
    configurations. *)
module type Arg = sig
  
  (** See {!EzSession.TYPES.SessionArg} *)
  module SessionArg : EzSession.TYPES.SessionArg
  
  (** See {!SessionStore} *)
  module SessionStore : SessionStore with type user_id = SessionArg.user_id

  (** Searches for user with given login and returns its password, if defined, its id and 
  its information. *)
  val find_user : login:string ->
    (string option * SessionArg.user_id * SessionArg.user_info) option Lwt.t
  
  (** Checks for foreign user. If exists, returns its login.
  Otherwise returns an error number with an error message. *)
  val check_foreign : origin:string -> token:string ->
    (string, int * string option) result Lwt.t

  (** Registers foreign user. *)
  val register_foreign : origin:string -> token:string ->
    (SessionArg.user_id * SessionArg.user_info option, int * string option) result Lwt.t
end

(** Not implemented search for foreign user. *)
val default_check_foreign : origin:string -> token:string ->
  (_, int * string option) result Lwt.t

(** Not implemented registration for foreign user. *)
val default_register_foreign : origin:string -> token:string ->
  (_, int * string option) result Lwt.t

(** Main functor that defines handlers for authentication services. *)
module Make(S: Arg) : sig

  (** See {!EzSession.Make} *)
  module Service : EzSession.M with type user_id = S.SessionArg.user_id
                                and type user_info = S.SessionArg.user_info

  (* User `register_handlers` to declare the handlers for the authentification
     services *)

  (** Registers authentication (connect, login and logout) services for current API. 
  For more information about every one of them, see .ml file. *)
  val register_handlers :
    Directory.t -> Directory.t

 (* handlers that need authentification should use `get_request_session`
   to collect the user identity and session. *)

  (** Gets user's session from user's request. *)
  val get_request_session :
    Req.t -> S.SessionArg.user_id session option Lwt.t

end

(** Exception when trying to create user that already exists. *)
exception UserAlreadyDefined

(** Exception when trying to create non-foreign user without password. *)
exception NoPasswordProvided

(** User storage located in the memory of the server. *)
module UserStoreInMemory(S : SessionArg with type user_id = string) : sig

  (** Creates user and adds it to [users]. If [kind] is specified, than creates user without
  password (foreign user). User id corresponds to login. Hash [password] if [pwhash] is not
  specified. Raises {!UserAlreadyDefined} if a user with the same login already exists. 
  Raises {!NoPasswordProvided} if no [pwhash] or [password] was specified for a local (non
  foreign) user. *)
  val create_user :
    ?pwhash:Digest.t ->
    ?password:string -> ?kind:string -> login:string -> S.user_info -> unit
  
  (** Removes user with the given login from the storage *)
  val remove_user : login:string -> unit

  (** Searches for user with given login and returns its password, if defined, its id and 
  its information. *)
  val find_user : login:string -> (string option * S.user_id * S.user_info) option Lwt.t
  
  (** Checks for foreign user with the login [origin-token]. If exists, returns its login.
  Otherwise returns an error number with an error message. *)
  val check_foreign : origin:string -> token:string ->
    (string, int * string option) result Lwt.t
  
  (** See [default_register_foreign] (not implemented yet). *)
  val register_foreign : origin:string -> token:string ->
    (S.user_id * S.user_info option, int * string option) result Lwt.t

  (** See {EzSession.TYPES.SessionArg} *)
  module SessionArg : EzSession.TYPES.SessionArg
    with type user_info = S.user_info
     and type user_id = S.user_id

  (** See {!SessionStore} *)
  module SessionStore : SessionStore with type user_id = S.user_id

end

(** Implementation of session store in the server's memory. *)
module SessionStoreInMemory : SessionStore
