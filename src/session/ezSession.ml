(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* TODO:
  * Use a better hash fuction than md5 !!!
 *)

let debug = false

module TYPES = struct

  (** Foreign user without password, whose login is equal to [foreign_origin ^ foreign_token] and
  whose password is not req. *)
  type foreign_info = {
    foreign_origin : string;
    foreign_token : string;
  }

  (** A session that helps to keep connection for the given user and stores useful information
  about his communication with the sever. *)
  type 'user_id session = {
    session_token : string;
    session_login : string;
    session_user_id : 'user_id;
    session_last : float;
    session_foreign : foreign_info option;
  }

  (** Main module that specifies the session parameters for given implementation of API server. *)
  module type SessionArg = sig

    (** User identifier, which generally is the same as login *)
    type user_id

    (** Associated to user information *)
    type user_info

    (** Web host, that should be used in access control headers, if specified. If web_host isn't specified,
        then acces-control header in response will be set to '*' and authentication wwith cookies wouldn't 
        work.
        Note : Cookies would be set by browser only if request's flag 'with_credentials' is set to true. 
        Last one in turn, requires that "Access-control_allow_origin" header by reponse returns something 
        different from "*". *)
    val web_host : string option 

    (** Json encoding for user's id *)
    val user_id_encoding : user_id Json_encoding.encoding

    (** Json encoding for user's information *)
    val user_info_encoding : user_info Json_encoding.encoding

    (** RPC path where authentication services like {b connect}, {b login} and {b logout}
    would be placed. *)
    val rpc_path : string list (* ["v1"] *)

    (*
  Using a cookie (e.g. `Cookie "EZSESSION" `) allows CSRF (Client-Side
  Request Forgery), it is better to use a specific header for security
  (`CSRF "X-Csrf-Token" `).
     *)

    (** Describes two ways to store a token within a client request : 
    - Stores as a cookie associated with the given cookie name and its max-age, if provided. 
    - Stores as a CSRF header with the given name. *)
    val token_kind : [`Cookie of string * int64 option | `CSRF of string ]

  end

  (** Authentification information returned by server after successful connection *)
  type ('user_id, 'user_info) auth = {
    auth_login : string;
    auth_user_id : 'user_id;
    auth_token : string;
    auth_user_info : 'user_info;
  }

  (** Challenge that should be resolved to be able to connect *)
  type auth_needed = {
    challenge_id : string;
    challenge : string;
  }

  (** Connection response, that either describes user information if connection successes either
  describes challenge to resolve to be able to connect *)
  type 'auth connect_response =
    | AuthOk of 'auth
    | AuthNeeded of auth_needed

  (** Logining request, that contains the login and the challenge resolution obtained by hashing
  challenge and password provided by the user. *)
  type local_login_message = {
    login_user : string;
    login_challenge_id : string;
    login_challenge_reply : string;
  }

  (** Logining request, that could be asked either by user with the password provided either by a
  foreign user without a password. *)
  type login_message =
    | Local of local_login_message
    | Foreign of foreign_info

  (** Possible logining outcomes. *)
  type ('user_id, 'user_info) login_response =
    | LoginOk of ('user_id, 'user_info) auth
    | LoginWait of 'user_id

  (** Errors that could be raised while logining. *)
  type login_error =
    [ `Bad_user_or_password
    | `User_not_registered
    | `Unverified_user
    | `Challenge_not_found_or_expired of string
    | `Invalid_session_login of string ]

  (** Errors that could be raised while disconnecting. *)
  type logout_error =
    [ `Invalid_session_logout of string]

  (** Errors that could be raised while connecting. *)
  type connect_error =
    [ `Session_expired | `Invalid_session_connect of string ]

end

open TYPES

(** Hash module, that hashing algorithms. *)
module Hash = struct

  include EzHash

  (** Hashed version of the password that is computed by the hash function applied on
  [login ^ password] *)
  let password ~login ~password =
    let s = hash (login ^ password) in
    if debug then
      EzDebug.printf "EzSession.Hash.password:\n  %S %S => %S"
        login password s;
    s

  (** Hashed version of the challenge that is computed by the hash function applied on
  [challenge ^ pwhash] *)
  let challenge ~challenge ~pwhash =
    let s = hash (challenge ^ pwhash) in
    if debug then
      EzDebug.printf "EzSession.Hash.challenge:\n  %S %S => %S"
        challenge pwhash s;
    s

end

(** Output signature for Make functor *)
module type M = sig
  type user_id
  type user_info
  type nonrec auth = (user_id, user_info) auth
  type token_security =
    [ EzAPI.Security.cookie | EzAPI.Security.header | EzAPI.Security.query ]
  val section_session : EzAPI.Doc.section
  val param_token : EzAPI.Param.t
  val security : token_security list
  val connect : (auth connect_response, connect_error, token_security) EzAPI.service0
  val login : (login_message, (user_id, user_info) login_response, login_error, EzAPI.Security.none) EzAPI.post_service0
  val logout : (auth_needed, logout_error, token_security) EzAPI.service0
end

(** Main functor that produces definition for authentication services and encodings for types used
by service's input, output and errors. *)
module Make(S : SessionArg) = struct

  type nonrec auth = (S.user_id, S.user_info) auth

  (** Encodings for data types used in server's requests/responses and for error cases that
  could be raised by one of them. *)
  module Encoding = struct
    open Json_encoding

    let auth_needed =
      def ~title:"needed" "needed_authentication" @@
      conv
        (fun { challenge_id; challenge } ->
           (challenge_id, challenge))
        (fun (challenge_id, challenge) ->
           { challenge_id; challenge }) @@
      obj2
        (req "challenge_id" string)
        (req "challenge" string)

    let auth_ok =
      def ~title:"success" "success_authentication" @@
      conv
        (fun { auth_login; auth_user_id; auth_token; auth_user_info } ->
           (auth_login, auth_user_id, auth_token, auth_user_info))
        (fun (auth_login, auth_user_id, auth_token, auth_user_info) ->
           { auth_login; auth_user_id; auth_token; auth_user_info }) @@
      obj4
        (req "login" EzEncoding.encoded_string)
        (req "user_id" S.user_id_encoding)
        (req "token" string)
        (req "user_info" S.user_info_encoding)

    let connect_response = union [
        case auth_ok
          (function AuthOk x -> Some x | _ -> None)
          (fun x -> AuthOk x);
        case auth_needed
          (function AuthNeeded x -> Some x | _ -> None)
          (fun x -> AuthNeeded x) ]

    let foreign_message =
      def ~title:"foreign login" "foreign_login_message" @@ conv
        (fun {foreign_origin; foreign_token} -> (foreign_origin, foreign_token))
        (fun (foreign_origin, foreign_token) -> {foreign_origin; foreign_token}) @@
      obj2 (req "auth_origin" string) (req "token" string)

    let local_message =
      def ~title:"local login" "local_login_message" @@
      conv
        (fun
           { login_user; login_challenge_id; login_challenge_reply }
           ->
          ( login_user, login_challenge_id, login_challenge_reply )
        )
        (fun
           ( login_user, login_challenge_id, login_challenge_reply )
           ->
           { login_user; login_challenge_id; login_challenge_reply }
        )
        (obj3
           (req "user" EzEncoding.encoded_string)
           (req "challenge_id" string)
           (req "challenge_reply" EzEncoding.encoded_string))

    let login_message = union [
        case local_message
          (function Local l -> Some l | _ -> None)
          (fun l -> Local l);
        case foreign_message
          (function Foreign f -> Some f | _ -> None)
          (fun f -> Foreign f) ]

    let login_response = union [
        case auth_ok
          (function LoginOk x -> Some x | _ -> None)
          (fun x -> LoginOk x);
        case (def ~title:"pending" "login_validation_pending" @@ obj1 (req "user_id" S.user_id_encoding))
          (function LoginWait x -> Some x | _ -> None)
          (fun x -> LoginWait x) ]

    let session_expired_case =
      EzAPI.Err.Case {
        code = 440;
        name = "SessionExpired";
        encoding = (obj1 (req "error" (constant "SessionExpired")));
        select = (function `Session_expired -> Some () | _ -> None);
        deselect = (fun () -> `Session_expired);
      }

    let bad_user_case =
      EzAPI.Err.Case {
        code = 401;
        name = "BadUserOrPassword";
        encoding = (obj1 (req "error" (constant "BadUserOrPassword")));
        select = (function `Bad_user_or_password -> Some () | _ -> None);
        deselect = (fun () -> `Bad_user_or_password);
      }

    let user_not_registered_case =
      EzAPI.Err.Case {
        code = 400;
        name = "UserNotRegistered";
        encoding = (obj1 (req "error" (constant "UserNotRegistered")));
        select = (function `User_not_registered -> Some () | _ -> None);
        deselect = (fun () -> `User_not_registered);
      }

    let unverified_user_case =
      EzAPI.Err.Case {
        code = 400;
        name = "UnverifiedUser";
        encoding = (obj1 (req "error" (constant "unverified")));
        select = (function `Unverified_user -> Some () | _ -> None);
        deselect = (fun () -> `Unverified_user);
      }

    let challenge_not_found_case =
      EzAPI.Err.Case {
        code = 401;
        name = "ChallengeNotFoundOrExpired";
        encoding = (obj2
                      (req "error" (constant "ChallengeNotFoundOrExpired"))
                      (req "challenge_id" string));
        select = (function `Challenge_not_found_or_expired s -> Some ((), s) | _ -> None);
        deselect = (fun ((), s) -> `Challenge_not_found_or_expired s);
      }

    let invalid_session_login_case =
      EzAPI.Err.Case {
        code = 400;
        name = "InvalidSession";
        encoding = (obj2
                      (req "error" (constant "InvalidSession"))
                      (req "reason" string));
        select = (function `Invalid_session_login s -> Some ((), s) | _ -> None);
        deselect = (fun ((), s) -> `Invalid_session_login s);
      }

    let invalid_session_logout_case =
      EzAPI.Err.Case {
        code = 400;
        name = "InvalidSession";
        encoding = (obj2
                      (req "error" (constant "InvalidSession"))
                      (req "reason" string));
        select = (function `Invalid_session_logout s -> Some ((), s));
        deselect = (fun ((), s) -> `Invalid_session_logout s);
      }

    let invalid_session_connect_case =
      EzAPI.Err.Case {
        code = 400;
        name = "InvalidSession";
        encoding = (obj2
                      (req "error" (constant "InvalidSession"))
                      (req "reason" string));
        select = (function `Invalid_session_connect s -> Some ((), s) | _ -> None);
        deselect = (fun ((), s) -> `Invalid_session_connect s);
      }

  end

  (** Definition for services and their security's configuration. *)
  module Service = struct
    type user_id = S.user_id
    type user_info = S.user_info
    type nonrec auth = auth

    (** Documentation section for openapi. *)
    let section_session = EzAPI.Doc.section "Session Requests"

    (** Parameter with name {i token} that stores an authentication token string *)
    let param_token =
      EzAPI.Param.string ~name:"token" ~descr:"An authentication token" "token"

    (** Type that represents security by authentication token and the way that request uses
    to store it. *)
    type token_security =
      [ EzAPI.Security.cookie | EzAPI.Security.header | EzAPI.Security.query ]

    (** Security that requires [param_token] parameter in query. *)
    let param_security =
      EzAPI.(`Query {
          Security.ref_name = "Token parameter";
          name = param_token
        })

    (** Security that checks [S.token_kind]:
        If it is a CSRF token, then requires a CSRF header.
        Otherwise requires token to be found in the cookies.
    *)
    let header_cookie_security =
      match S.token_kind with
      | `CSRF name ->
        EzAPI.(`Header { Security.ref_name = name ^ " Header"; name })
      | `Cookie (name, max_age) ->
        EzAPI.(`Cookie ({ Security.ref_name = name ^ " Cookie"; name }, max_age))

    (** Security that combines [param_security] and [header_cookie_security]
        in the corresponding order. Represents the security configuration for
        [connect] and [logout] requests.
    *)
    let security : token_security list = [
      param_security; (* Parameter fisrt *)
      header_cookie_security; (* Header CSRF or Cookie *)
    ]

    (** Defines path to authentication services *)
    let rpc_root =
      List.fold_left (fun path s ->
           EzAPI.Path.( path // s )
        ) EzAPI.Path.root S.rpc_path

    let access_control = 
      [ "access-control-allow-credentials", "true"; 
        "access-control-allow-origin", 
          match S.web_host with None -> "*" | Some origin -> origin ]


    (** Connection service that requires authentication token. For more details, see corresponding 
    [EzSessionServer.Make.connect] handler and default client request implementation 

    [EzSessionClient.Make.connect]. *)
    let connect : (auth connect_response, connect_error, token_security) EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"connect"
        ~output:Encoding.connect_response
        ~errors:[Encoding.session_expired_case; Encoding.invalid_session_connect_case]
        ~security
        ~access_control
        EzAPI.Path.(rpc_root // "connect")

    (** Logining service. For more details, see corresponding [EzSessionServer.Make.login] handler
    and default client request implementation [EzSessionClient.Make.login]. *)
    let login : (login_message, (S.user_id, S.user_info) login_response, login_error, EzAPI.Security.none) EzAPI.post_service0  =
      EzAPI.post_service
        ~section:section_session
        ~name:"login"
        ~input:Encoding.login_message
        ~output:Encoding.login_response
        ~errors:[Encoding.bad_user_case;
                 Encoding.user_not_registered_case;
                 Encoding.unverified_user_case;
                 Encoding.challenge_not_found_case;
                 Encoding.invalid_session_login_case]
        ~access_control
        EzAPI.Path.(rpc_root // "login")

    (** Disconnection service that requires authentication token. For more details, see corresponding
    [EzSessionServer.Make.logout] handler and default client request implementation
    [EzSessionClient.Make.logout]. *)
    let logout : (auth_needed, logout_error, token_security) EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"logout"
        ~meth:`PUT
        ~output:Encoding.auth_needed
        ~errors:[Encoding.invalid_session_logout_case]
        ~security
        ~access_control
        EzAPI.Path.(rpc_root // "logout")
  end

end
