open StringCompat

(* TODO:
  * Use a better hash fuction than md5 !!!
 *)

let debug = false

module TYPES = struct

  type 'user_id session = {
    session_cookie : string;
    session_login : string;
    session_user_id : 'user_id;
    mutable session_variables : string StringMap.t;
    mutable session_last : float;
  }

  module type SessionArg = sig

    type user_id
    type user_info

    val user_id_encoding : user_id Json_encoding.encoding
    val user_info_encoding : user_info Json_encoding.encoding

    val rpc_path : string list (* ["v1"] *)

    (*
  Using a cookie (e.g. `Cookie "EZSESSION" `) allows CSRF (Client-Side
  Request Forgery), it is better to use a specific header for security
  (`CSRF "X-Csrf-Token" `).
     *)

    val token_kind : [`Cookie of string | `CSRF of string ]
  end

  type ('user_id, 'user_info) auth = {
    auth_login : string;
    auth_user_id : 'user_id;
    auth_token : string;
    auth_user_info : 'user_info;
    auth_kind : string option;
  }

  type auth_needed = {
    challenge_id : string;
    challenge : string;
  }

  type 'auth connect_response = AuthOk of 'auth | AuthNeeded of auth_needed

  type local_login_message = {
    login_user : string;
    login_challenge_id : string;
    login_challenge_reply : string;
  }

  type foreign_login_message = {
    foreign_origin : string;
    foreign_token : string;
  }

  type login_message =
    | Local of local_login_message
    | Foreign of foreign_login_message

  type login_error =
    [ `Bad_user_or_password
    | `Challenge_not_found_or_expired of string
    | `Invalid_session ]

  type logout_error =
    [ `Invalid_session ]

  type connect_error =
    [ `Session_expired ]

end

open TYPES

module Hash = struct

  let hash_fun = ref Digest.string
  let hash s = !hash_fun s

  let password ~login ~password =
    let s = hash (login ^ password) in
    if debug then
      EzDebug.printf "EzSession.Hash.password:\n  %S %S => %S"
                     login password s;
    s

  let challenge ~challenge ~pwhash =
    let s = hash (challenge ^ pwhash) in
    if debug then
      EzDebug.printf "EzSession.Hash.challenge:\n  %S %S => %S"
                     challenge pwhash s;
    s


end

module Make(S : SessionArg) = struct

  type nonrec auth = (S.user_id, S.user_info) auth

  module Encoding = struct
    open Json_encoding

    let auth_needed =
      conv
        (fun { challenge_id; challenge } ->
           (challenge_id, challenge))
        (fun (challenge_id, challenge) ->
           { challenge_id; challenge }) @@
      obj2
        (req "challenge_id" string)
        (req "challenge" string)

    let session_expired_case =
      EzAPI.ErrCase {
        code = 440;
        name = "SessionExpired";
        encoding = (obj1 (req "error" (constant "SessionExpired")));
        select = (function `Session_expired -> Some () (* | _ -> None *));
        deselect = (fun () -> `Session_expired);
      }

    let auth_ok =
      conv
        (fun { auth_login; auth_user_id; auth_token; auth_user_info; auth_kind } ->
           (auth_login, auth_user_id, auth_token, auth_user_info, auth_kind))
        (fun (auth_login, auth_user_id, auth_token, auth_user_info, auth_kind) ->
           { auth_login; auth_user_id; auth_token; auth_user_info; auth_kind }) @@
      obj5
        (req "login" EzEncoding.encoded_string)
        (req "user_id" S.user_id_encoding)
        (req "token" string)
        (req "user_info" S.user_info_encoding)
        (opt "kind" string)

    let connect_response = union [
        case auth_ok
          (function AuthOk x -> Some x | _ -> None)
          (fun x -> AuthOk x);
        case auth_needed
          (function AuthNeeded x -> Some x | _ -> None)
          (fun x -> AuthNeeded x) ]

    let foreign_message = conv
        (fun {foreign_origin; foreign_token} -> (foreign_origin, foreign_token))
        (fun (foreign_origin, foreign_token) -> {foreign_origin; foreign_token}) @@
      obj2 (req "auth_origin" string) (req "token" string)

    let local_message =
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

    let bad_user_case =
      EzAPI.ErrCase {
        code = 401;
        name = "BadUserOrPassword";
        encoding = (obj1 (req "error" (constant "BadUserOrPassword")));
        select = (function `Bad_user_or_password -> Some () | _ -> None);
        deselect = (fun () -> `Bad_user_or_password);
      }

    let challenge_not_found_case =
      EzAPI.ErrCase {
        code = 401;
        name = "ChallengeNotFoundOrExpired";
        encoding = (obj2
                      (req "error" (constant "ChallengeNotFoundOrExpired"))
                      (req "challenge_id" string));
        select = (function `Challenge_not_found_or_expired s -> Some ((), s) | _ -> None);
        deselect = (fun ((), s) -> `Challenge_not_found_or_expired s);
      }

    let invalid_session_case =
      EzAPI.ErrCase {
        code = 401;
        name = "InvalidSession";
        encoding = (obj1 (req "error" (constant "InvalidSession")));
        select = (function `Invalid_session -> Some () (* | _ -> None *));
        deselect = (fun () -> `Invalid_session);
      }

  end

  module Service = struct

    let section_session = EzAPI.section "Session Requests"

    let param_token =
      EzAPI.Param.string ~name:"token" ~descr:"An authentication token" "token"

    type token_security =
      [ EzAPI.cookie_security | EzAPI.header_security | EzAPI.query_security ]

    let param_security =
      EzAPI.(`Query {
          ref_name = "Token parameter";
          name = param_token
        })

    let header_cookie_security =
      match S.token_kind with
      | `CSRF name ->
        EzAPI.(`Header { ref_name = name ^ " Header"; name })
      | `Cookie name ->
        EzAPI.(`Cookie { ref_name = name ^ " Cookie"; name })

    let security : token_security list = [
      param_security; (* Parameter fisrt *)
      header_cookie_security; (* Header CSRF or Cookie *)
    ]

    let rpc_root =
      List.fold_left (fun path s ->
           EzAPI.Path.( path // s )
        ) EzAPI.Path.root S.rpc_path

    let connect : (auth connect_response, connect_error, token_security) EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"connect"
        ~output:Encoding.connect_response
        ~error_outputs: [Encoding.session_expired_case]
        ~security
        EzAPI.Path.(rpc_root // "connect")

    let login : (login_message, auth, login_error, EzAPI.no_security) EzAPI.post_service0  =
      EzAPI.post_service
        ~section:section_session
        ~name:"login"
        ~input:Encoding.login_message
        ~output:Encoding.auth_ok
        ~error_outputs: [Encoding.bad_user_case;
                         Encoding.challenge_not_found_case]
        EzAPI.Path.(rpc_root // "login")

    let logout : (auth_needed, logout_error, token_security) EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"logout"
        ~meth:Resto1.PUT
        ~output:Encoding.auth_needed
        ~error_outputs: [Encoding.invalid_session_case]
        ~security
        EzAPI.Path.(rpc_root // "logout")
  end

end
