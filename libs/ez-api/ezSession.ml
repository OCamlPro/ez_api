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

    type auth = {
      auth_login : string;
      auth_user_id : user_id;
      auth_token : string;
      auth_user_info : user_info;
    }

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

  type auth_needed = {
    challenge_id : string;
    challenge : string;
  }

  type login_message = {
    login_user : string;
    login_challenge_id : string;
    login_challenge_reply : string;
  }

  type login_error =
    [ `Bad_user_or_password
    | `Challenge_not_found_or_expired of string ]

  type logout_error =
    [ `Invalid_session ]

  type connect_error =
    [ `Auth_needed of auth_needed
    | `Session_expired ]

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

    let auth_needed_case =
      EzAPI.ErrCase {
        code = 401;
        name = "AuthNeeded";
        encoding = (merge_objs
                      (obj1 (req "error" (constant "AuthNeeded")))
                      auth_needed);
        select = (function `Auth_needed s -> Some ((), s) | _ -> None);
        deselect = (fun ((), s) -> `Auth_needed s);
      }

    let session_expired_case =
      EzAPI.ErrCase {
        code = 440;
        name = "SessionExpired";
        encoding = (obj1 (req "error" (constant "sSessionExpired")));
        select = (function `Session_expired -> Some () | _ -> None);
        deselect = (fun () -> `Session_expired);
      }

    let auth_ok =
      let open S in
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

    let login_message =
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

    let bad_user_case =
      EzAPI.ErrCase {
        code = 403;
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
        code = 403;
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
      [ `ApiKey of [ `Cookie | `Header | `Query ] EzAPI.TYPES.apikey_security ]

    let security : token_security list = [
      (let (`CSRF name | `Cookie name) = S.token_kind in
       let in_ = match S.token_kind with
         | `CSRF _ -> `Header
         | `Cookie _ -> `Cookie in
       let ref_name = match S.token_kind with
         | `CSRF name -> name ^ " Header"
         | `Cookie name -> name ^ " Cookie" in
       EzAPI.TYPES.(`ApiKey { ref_name; in_; name}));
      EzAPI.TYPES.(`ApiKey {
          ref_name = "Token parameter";
          in_ = `Query;
          name = param_token.param_value});
    ]

    let rpc_root =
      List.fold_left (fun path s ->
           EzAPI.Path.( path // s )
        ) EzAPI.Path.root S.rpc_path

    let connect : (S.auth, connect_error, token_security) EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"connect"
        ~params:[param_token]
        ~output:Encoding.auth_ok
        ~error_outputs: [Encoding.auth_needed_case;
                         Encoding.session_expired_case]
        ~security
        EzAPI.Path.(rpc_root // "connect")

    let login : (login_message,
                 S.auth,
                 login_error,
                 EzAPI.TYPES.no_security) EzAPI.post_service0  =
      EzAPI.post_service
        ~section:section_session
        ~name:"login"
        ~params:[param_token]
        ~input:Encoding.login_message
        ~output:Encoding.auth_ok
        ~error_outputs: [Encoding.bad_user_case;
                         Encoding.challenge_not_found_case]
        EzAPI.Path.(rpc_root // "login")

    let logout : (auth_needed, logout_error, token_security) EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"logout"
        ~params:[param_token]
        ~meth:"put"
        ~output:Encoding.auth_needed
        ~error_outputs: [Encoding.invalid_session_case]
        ~security
        EzAPI.Path.(rpc_root // "logout")
  end

end
