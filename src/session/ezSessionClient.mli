(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Make(S: EzSession.TYPES.SessionArg)(_ : EzReq_S.S) : sig

(* If cookies are in use on server-side, `connect` might return
  an already authenticated user. Otherwise (CSRF protection),
  a token can be provided (saved in local storage).
*)

  (** See {EzSession.TYPES.SessionArg.user_id}. *)
  type user_id = S.user_id

  (** See {EzSession.TYPES.SessionArg.user_info}. *)
  type user_info = S.user_info

  (** See {EzSession.TYPES.auth}. *)
  type nonrec auth = (S.user_id, S.user_info) EzSession.TYPES.auth

  (** See {!EzSession.Make} *)
  module Service : EzSession.M with type user_id = user_id
                                and type user_info = user_info

  (** Error that could be raised by [login] request. *)
  type login_error = [
    | EzSession.TYPES.login_error
    | EzSession.TYPES.connect_error
    | EzSession.TYPES.logout_error
    | `Too_many_login_attempts
    | `Session_expired ]

  (** Performs request to the server's authentication service if actual state is set to 
  disconnected and calls callback on the response result. If state is set to connected or
  authenticated, then callback is called on actual connection/authentication status without 
  performing request, unless [token] is specified. *)
  val connect :
    EzAPI.base_url ->
    ?token:string ->
    (((S.user_id, S.user_info) EzSession.TYPES.auth option, EzSession.TYPES.connect_error) result -> unit) -> unit

  (** Performs request to the server's login service. If actual state is set to disconnected,
  then firstly, it performs [connect] request. If connect request returns authentication 
  information for different user (if token or cookies wasn't updated) or if other user is
  already autenticated, it performs logout for this user, and retry logining. If user is
  connected but not authenticated, it constructs challenge reply by using user's [login] and 
  [password] or uses foreign user information to authenticate. Calls callback on 
  authentification status. *)
  val login :
    ?format:(string -> string) ->
    EzAPI.base_url ->
    ?login:string -> (* login *)
    ?password:string -> (* password *)
    ?foreign:(string * string) -> (* foreing auth : origin, token *)
    (((S.user_id, S.user_info) EzSession.TYPES.auth, login_error) result -> unit) -> unit

  (** Performs request to the server's logout service if actual state is set to authenticated.
  After this step, state is set to connected. Callback is called with [Ok true] if logout was 
  successfully performed. If previous state wasn't set to authenticated, then callback is 
  called with [Ok false]. *)
  val logout :
    EzAPI.base_url ->
    token:string -> ((bool, EzSession.TYPES.logout_error) result -> unit) -> unit

  (** Tell the network layer that we think that the session has ended
     (a request probably returned an error status for missing auth).
     Since the state is kept internally, we have to do this first to
     trigger a full reconnection by `login`. *)
  val disconnected : unit -> unit

  (* In `CSRF mode, these headers should be added to all queries that
    need authentication *)

  (** Returns authentication headers depending on [S.token_kind] for client requests. 
  It doesn't include cookies because cookies are automatically added by browser. *)
  val auth_headers : token:string -> (string * string) list

  (** Gets current authentication status, if exists. *)
  val get : unit -> (S.user_id, S.user_info) EzSession.TYPES.auth option

  end
