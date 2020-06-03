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
      case (merge_objs
              (obj1 (req "error" (constant "AuthNeeded")))
              auth_needed)
        (function `Auth_needed s -> Some ((), s) | _ -> None)
        (fun ((), s) -> `Auth_needed s)

    let session_expired_case =
      case (obj1 (req "error" (constant "SessionExpired")))
        (function `Session_expired -> Some () | _ -> None)
        (fun () -> `Session_expired)

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
      case (obj1 (req "error" (constant "BadUserOrPassword")))
        (function `Bad_user_or_password -> Some () | _ -> None)
        (fun () -> `Bad_user_or_password)

    let challenge_not_found_case =
      case (obj2
              (req "error" (constant "BadUserOrPassword"))
              (req "challenge_id" string))
        (function `Challenge_not_found_or_expired s -> Some ((), s) | _ -> None)
        (fun ((), s) -> `Challenge_not_found_or_expired s)

    let invalid_session_case =
      case (obj1 (req "error" (constant "InvalidSession")))
        (function `Invalid_session -> Some () (* | _ -> None *))
        (fun () -> `Invalid_session)

  end

  module Service = struct

    let section_session = EzAPI.section "Session Requests"

    let param_token =
      EzAPI.Param.string ~name:"token" ~descr:"An authentication token" "token"

    let rpc_root =
      List.fold_left (fun path s ->
           EzAPI.Path.( path // s )
        ) EzAPI.Path.root S.rpc_path

    let connect : (S.auth, connect_error) EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"connect"
        ~params:[param_token]
        ~output:Encoding.auth_ok
        ~error_outputs: [401, Encoding.auth_needed_case;
                         440, Encoding.session_expired_case]
        EzAPI.Path.(rpc_root // "connect")

    let login : (login_message,
                 S.auth,
                 login_error) EzAPI.post_service0  =
      EzAPI.post_service
        ~section:section_session
        ~name:"login"
        ~params:[param_token]
        ~input:Encoding.login_message
        ~output:Encoding.auth_ok
        ~error_outputs: [403, Encoding.bad_user_case;
                         401, Encoding.challenge_not_found_case]
        EzAPI.Path.(rpc_root // "login")

    let logout : (auth_needed, logout_error) EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"logout"
        ~params:[param_token]
        ~meth:"put"
        ~output:Encoding.auth_needed
        ~error_outputs: [403, Encoding.invalid_session_case]
        EzAPI.Path.(rpc_root // "logout")
  end

end
