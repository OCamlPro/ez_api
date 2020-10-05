module Types = struct
  type info = {
    app_id : int64;
    token_type : string;
    app_name : string;
    token_exp : int64;
    token_valid : bool;
    token_iss : int64;
    token_meta : string;
    token_scopes : string list;
    user_id : string;
  }
end

module Encoding = struct
  open Types
  open Json_encoding

  let encoding = obj1 @@ req "data" @@
    conv
      (fun {app_id; token_type; app_name; token_exp; token_valid; token_iss;
            token_meta; token_scopes; user_id}
        -> (app_id, token_type, app_name, token_exp, token_valid, token_iss,
            token_meta, token_scopes, user_id))
      (fun (app_id, token_type, app_name, token_exp, token_valid, token_iss,
            token_meta, token_scopes, user_id)
        -> {app_id; token_type; app_name; token_exp; token_valid; token_iss;
            token_meta; token_scopes; user_id}) @@
    obj9
      (req "app_id" int53)
      (req "type" string)
      (req "application" string)
      (req "expires_at" int53)
      (req "is_valid" bool)
      (req "issued_at" int53)
      (req "metadata" (obj1 (req "sso" string)))
      (req "scopes" (list string))
      (req "user_id" string)
end

module Services = struct
  let input_token_param = EzAPI.Param.string ~descr:"input token" "input_token"
  let access_token_param = EzAPI.Param.string ~descr:"access token" "access_token"

  let facebook_auth = EzAPI.TYPES.BASE "https://graph.facebook.com/"

  let debug_token : (Types.info, exn, EzAPI.no_security) EzAPI.service0 =
    EzAPI.service
      ~register:false
      ~name:"debug_token"
      ~params:[input_token_param; access_token_param]
      ~output:Encoding.encoding
      EzAPI.Path.(root // "debug_token")
end

open Types
open Services
open EzRequest_lwt
open Lwt.Infix

let check_token ~app_token ~app_id input_token =
  let params = [
    access_token_param, EzAPI.TYPES.S app_token;
    input_token_param, EzAPI.TYPES.S input_token] in
  ANY.get0 ~params facebook_auth debug_token >|= function
  | Error (KnownError {code; error}) ->
    Error (code, Some (Printexc.to_string error))
  | Error (UnknownError {code; msg}) -> Error (code, msg)
  | Ok token ->
    if token.app_id = app_id && token.token_valid then Ok token.user_id
    else Error (400, Some "Invalid facebook token")
