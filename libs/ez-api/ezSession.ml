open StringCompat

(* TODO:
  * Use a better hash fuction than md5 !!!
 *)

let debug = false

module TYPES = struct

  type session = {
      session_cookie : string;
      session_login : string;
      mutable session_variables : string StringMap.t;
      mutable session_last : float;
    }

  module type SessionArg = sig

    type user_info

    val encoding : user_info Json_encoding.encoding
    val rpc_path : string list (* ["v1"] *)

    (*
  Using a cookie (e.g. `Cookie "EZSESSION" `) allows CSRF (Client-Side
  Request Forgery), it is better to use a specific header for security
  (`CSRF "X-Csrf-Token" `).
     *)

    val token_kind : [`Cookie of string | `CSRF of string ]
  end

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

  type s2c_message =
    (* Authentication failed, here is a new challenge *)
    | AuthNeeded of (* challenge_id *) string * (* challenge *) string
    (* Authentication succeeded, here is user info *)
    | AuthOK of
        (* login *) string *
        (* cookie *) string *
        S.user_info

  type login_message = {
      login_user : string;
      login_challenge_id : string;
      login_challenge_reply : string;
    }

  module Encoding = struct
    open Json_encoding

    let auth_needed =
      obj2
        (req "challenge_id" string)
        (req "challenge" string)

    let auth_ok =
      obj3
        (req "login" EzEncoding.encoded_string)
        (req "token" string)
        (req "user_info" S.encoding)

    let s2c_message =
      union [

          case
            auth_needed
            (function
             | AuthNeeded (challenge_id, challenge) ->
                Some (challenge_id, challenge)
             | _ -> None)
            (fun (challenge_id, challenge) ->
              AuthNeeded (challenge_id, challenge)
            );

          case
            auth_ok
            (function
             | AuthOK (login, cookie, user_info) ->
                Some (login, cookie, user_info)
             | _ -> None)
            (fun (login, cookie, user_info) ->
              AuthOK (login, cookie, user_info)
            );

        ]

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
  end

  module Service = struct

    let section_session = EzAPI.section "Session Requests"

    let param_token =
      EzAPI.Param.string ~name:"token" ~descr:"An authentication token" "token"

    let rpc_root =
      List.fold_left (fun path s ->
           EzAPI.Path.( path // s )
        ) EzAPI.Path.root S.rpc_path

    let connect : s2c_message EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"connect"
        ~params:[param_token]
        ~output:Encoding.s2c_message
        EzAPI.Path.(rpc_root // "connect")

    let login : (login_message, s2c_message) EzAPI.post_service0  =
      EzAPI.post_service
        ~section:section_session
        ~name:"login"
        ~params:[param_token]
        ~input:Encoding.login_message
        ~output:Encoding.s2c_message
        EzAPI.Path.(rpc_root // "login")

    let logout : s2c_message EzAPI.service0  =
      EzAPI.service
        ~section:section_session
        ~name:"logout"
        ~params:[param_token]
        ~output:Encoding.s2c_message
        EzAPI.Path.(rpc_root // "logout")
  end

end
