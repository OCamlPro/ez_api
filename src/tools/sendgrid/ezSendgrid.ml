(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzSendgrid_types
open EzSendgrid_services

let sendgrid_url = "https://api.sendgrid.com/v3"
let headers key = ["Authorization", "Bearer " ^ key]

let send ?encoding ~api_key input =
  EzReq_lwt.post0
    ~headers:(headers api_key)
    ~input
    sendgrid_host
    (send encoding)

let send_one ~api_key ~dst ~from ~subject content =
  let person = {
    dst = [ {email = fst dst; name = snd dst} ];
    cc = None; bcc = None; psubject = None; data = None } in
  let content =
    List.map
      (fun (content_type, content_value) ->
         {content_type; content_value} ) content in
  let mail = {
    person = [ person ];
    from = {email = fst from; name = snd from};
    subject = Some subject;
    content = Some content;
    template_id = None;
    more_fields = None;
  } in
  send ~api_key mail

let send_template ~api_key ~dst ~from template_id data =
  let person = {
    dst = List.map (fun (email, name) -> {email; name}) dst;
    cc = None; bcc = None; psubject = None;
    data = Some (EzEncoding.destruct Json_encoding.any_value data) } in
  let mail = {
    person = [ person ];
    from = {email = fst from; name = snd from};
    subject = None;
    content = None;
    template_id = Some template_id;
    more_fields = None;
  } in
  send ~api_key mail

let add_contacts ~api_key ?list_ids contacts =
  EzReq_lwt.post0
    ~headers:(headers api_key)
    ~input:(list_ids, contacts)
    sendgrid_host
    add_contacts

let delete_contacts ~api_key ?(all=false) ids =
  let params =
    if all then [delete_all_param, EzAPI.S "true"]
    else [ids_param, EzAPI.S (String.concat "," ids)] in
  EzReq_lwt.post0
    ~headers:(headers api_key)
    ~input:()
    ~params
    sendgrid_host
    delete_contacts

let remove_contact_list ~api_key list_id contact_ids =
  let params = [contact_ids_param, EzAPI.S (String.concat "," contact_ids)] in
  EzReq_lwt.get1
    ~headers:(headers api_key)
    ~params
    sendgrid_host
    remove_contact_list
    list_id

let contacts_count ~api_key =
  EzReq_lwt.get0
    ~headers:(headers api_key)
    sendgrid_host
    contacts_count

let get_contact ~api_key id =
  EzReq_lwt.get1
    ~headers:(headers api_key)
    sendgrid_host
    get_contact
    id

let search_contacts ~api_key input =
  EzReq_lwt.post0
    ~headers:(headers api_key)
    ~input
    sendgrid_host
    search_contacts
