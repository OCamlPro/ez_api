module Types = struct
  type token_info = {
    idt_iss : string;
    idt_sub : string;
    idt_azp : string;
    idt_aud : string;
    idt_iat : string;
    idt_exp : string;
  }

  type email_info = {
    em_addr : string;
    em_verified : bool;
  }

  type profile_info = {
    pr_picture : string;
    pr_given_name : string;
    pr_family_name : string;
    pr_locale : string;
  }

  type info = {
    token_info : token_info;
    email_info : email_info option;
    profile_info : profile_info option;
  }
end

module Encoding = struct
  open Types
  open Json_encoding

  let token_info = conv
      (fun {idt_iss; idt_sub; idt_azp; idt_aud; idt_iat; idt_exp}
        -> (idt_iss, idt_sub, idt_azp, idt_aud, idt_iat, idt_exp))
      (fun (idt_iss, idt_sub, idt_azp, idt_aud, idt_iat, idt_exp)
        -> {idt_iss; idt_sub; idt_azp; idt_aud; idt_iat; idt_exp}) @@
    obj6
      (req "iss" string)
      (req "sub" string)
      (req "azp" string)
      (req "aud" string)
      (req "iat" string)
      (req "exp" string)

  let email_info = conv
      (fun {em_addr; em_verified} -> (em_addr, em_verified))
      (fun (em_addr, em_verified) -> {em_addr; em_verified}) @@
    obj2
      (req "email" string)
      (req "email_verfied" bool)

  let profile_info = conv
      (fun {pr_picture; pr_given_name; pr_family_name; pr_locale}
        -> (pr_picture, pr_given_name, pr_family_name, pr_locale))
      (fun (pr_picture, pr_given_name, pr_family_name, pr_locale)
        -> {pr_picture; pr_given_name; pr_family_name; pr_locale}) @@
    obj4
      (req "picture" string)
      (req "given_name" string)
      (req "family_name" string)
      (req "locale" string)

  let merge_objs_opt e1 e2 = union [
      case e1
        (function (x, None) -> Some x | _ -> None)
        (fun x -> (x, None));
      case (merge_objs e1 e2)
        (function (x, Some y) -> Some (x, y) | _ -> None)
        (fun (x, y) -> (x, Some y)) ]

  let encoding = EzEncoding.ignore_enc @@ conv
      (fun {token_info; email_info; profile_info}
        -> ((token_info, email_info), profile_info))
      (fun ((token_info, email_info), profile_info)
        -> {token_info; email_info; profile_info}) @@
    merge_objs_opt
      (merge_objs_opt token_info email_info)
      profile_info
end

module Services = struct
  let id_token_param = EzAPI.Param.string ~descr:"ID token" "id_token"

  let google_auth = EzAPI.TYPES.BASE "https://www.googleapis.com/"

  let token_info : (Types.info, exn, EzAPI.no_security) EzAPI.service0 =
    EzAPI.service
      ~register:false
      ~name:"token_info"
      ~params:[id_token_param]
      ~output:Encoding.encoding
      EzAPI.Path.(root // "oauth2" // "v3" // "tokeninfo")
end

open Types
open Services
open EzRequest_lwt
open Lwt.Infix

let check_token ~client_id id_token =
  let params = [id_token_param, EzAPI.TYPES.S id_token] in
  ANY.get0 ~params google_auth token_info >|= function
  | Error (KnownError {code; error}) ->
    Error (code, Some (Printexc.to_string error))
  | Error (UnknownError {code; msg}) -> Error (code, msg)
  | Ok token ->
    if token.token_info.idt_aud = client_id then Ok token.token_info.idt_aud
    else Error (400, Some "this google id_token is not valid for this app")

let get_address ~client_id id_token =
  let params = [id_token_param, EzAPI.TYPES.S id_token] in
  ANY.get0 ~params google_auth token_info >|= function
  | Error (KnownError {code; error}) ->
    Error (code, Some (Printexc.to_string error))
  | Error (UnknownError {code; msg}) -> Error (code, msg)
  | Ok token ->
    if token.token_info.idt_aud = client_id then
      match token.email_info with
      | None -> Error (400, Some "google permission doesn't include email address")
      | Some em -> Ok em.em_addr
    else Error (400, Some "this google id_token is not valid for this app")
