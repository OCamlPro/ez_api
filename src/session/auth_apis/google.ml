module Types = struct
  type info = {
    idt_iss : string;
    idt_sub : string;
    idt_azp : string;
    idt_aud : string;
    idt_iat : string;
    idt_exp : string;
  }

  type profile = {
    go_addr : string;
    go_name : string;
    go_verified : bool option;
    go_picture : string option;
    go_given_name : string option;
    go_family_name : string option;
    go_locale : string option;
  }

  type all = {
    token_info : info;
    profile_info : profile option;
  }
end

module Encoding = struct
  open Types
  open Json_encoding

  let info = conv
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

  let bool_of_string = conv string_of_bool bool_of_string string
  let profile = conv
      (fun {go_addr; go_name; go_verified; go_picture; go_given_name;
            go_family_name; go_locale}
        -> (go_addr, go_name, go_verified, go_picture, go_given_name,
            go_family_name, go_locale))
      (fun (go_addr, go_name, go_verified, go_picture, go_given_name,
            go_family_name, go_locale)
        -> {go_addr; go_name; go_verified; go_picture; go_given_name;
            go_family_name; go_locale}) @@
    obj7
      (req "email" string)
      (req "name" string)
      (opt "email_verified" bool_of_string)
      (opt "picture" string)
      (opt "given_name" string)
      (opt "family_name" string)
      (opt "locale" string)

  let merge_objs_opt e1 e2 = union [
      case (merge_objs e1 e2)
        (function (x, Some y) -> Some (x, y) | _ -> None)
        (fun (x, y) -> (x, Some y));
      case e1
        (function (x, None) -> Some x | _ -> None)
        (fun x -> (x, None));
    ]

  let encoding = EzEncoding.ignore_enc @@ conv
      (fun {token_info; profile_info} -> (token_info, profile_info))
      (fun (token_info, profile_info) -> {token_info; profile_info}) @@
    merge_objs_opt info profile
end

module Services = struct
  open EzAPI

  let id_token_param = Param.string ~descr:"ID token" "id_token"

  let google_auth = BASE "https://www.googleapis.com/"

  let token_info : (Types.all, exn, Security.none) EzAPI.service0 =
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

let handle_error e =
  Error (handle_error (fun exn -> Some (Printexc.to_string exn)) e)

let check_token ~client_id id_token =
  let params = [id_token_param, EzAPI.S id_token] in
  ANY.get0 ~params google_auth token_info >|= function
  | Error e -> handle_error e
  | Ok token ->
    if token.token_info.idt_aud = client_id then Ok token.token_info.idt_sub
    else Error (400, Some "this google id_token is not valid for this app")

let get_info ~client_id id_token =
  let params = [id_token_param, EzAPI.S id_token] in
  ANY.get0 ~params google_auth token_info >|= function
  | Error e -> handle_error e
  | Ok r ->
    if r.token_info.idt_aud = client_id then
      match r.profile_info with
      | None -> Error (400, Some "email or profile not included in google permission")
      | Some p -> Ok p
    else Error (400, Some "this google id_token is not valid for this app")
