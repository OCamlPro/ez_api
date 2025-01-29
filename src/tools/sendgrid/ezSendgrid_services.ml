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
open EzSendgrid_encoding
open EzAPI

let sendgrid_section = Doc.section "Sendgrid Requests"
let sendgrid_host = BASE "https://api.sendgrid.com/v3"

let arg_id = Arg.string ~example:"base64_hash" "id"

let ids_param = Param.string ~descr:"Sendgrid contact ids" ~name:"ids" "ids"
let contact_ids_param = Param.string ~descr:"Sendgrid contact ids" ~name:"contact_ids" "contact_ids"
let delete_all_param = Param.bool ~descr:"Sendgrid delete all contacts" ~name:"delete_all_contacts" "delete_all_contacts"

let send encoding : ('a mail, unit, string option, Security.bearer) post_service0 =
  raw_service
    ~section:sendgrid_section
    ~name:"sendgrid_send"
    ~input:(Json (mail encoding))
    ~output:Empty
    EzAPI.Path.(root // "mail" // "send")

let add_contacts : (string list option * contact list, string, string option, EzAPI.Security.bearer) EzAPI.post_service0 =
  EzAPI.post_service
    ~register:false
    ~section:sendgrid_section
    ~name:"sendgrid_add_contacts"
    ~input:add_contacts_enc
    ~output:job_output
    ~meth:`PUT
    EzAPI.Path.(root // "marketing" // "contacts")

let delete_contacts : (unit, string, string option, Security.bearer) EzAPI.post_service0 =
  EzAPI.raw_service
    ~register:false
    ~section:sendgrid_section
    ~name:"sendgrid_delete_contacts"
    ~input:Empty
    ~output:(Json job_output)
    ~meth:`DELETE
    ~params:[ids_param; delete_all_param]
    EzAPI.Path.(root // "marketing" // "contacts")

let contacts_count : (int, string option, Security.bearer) EzAPI.service0 =
  EzAPI.service
    ~register:false
    ~section:sendgrid_section
    ~name:"sendgrid_contacts_count"
    ~output:contacts_count
    EzAPI.Path.(root // "marketing" // "contacts" // "count")

let get_contact : (string, contact * contact_more, string option, Security.bearer) EzAPI.service1 =
  EzAPI.service
    ~register:false
    ~section:sendgrid_section
    ~name:"sendgrid_get_contact"
    ~output:get_contact
    EzAPI.Path.(root // "marketing" // "contacts" /: arg_id)

let search_contacts : (string, int * (contact * contact_more) list, string option, Security.bearer) EzAPI.post_service0 =
  EzAPI.post_service
    ~register:false
    ~section:sendgrid_section
    ~name:"sendgrid_search_contacts"
    ~input:query
    ~output:(search_output EzSendgrid_encoding.get_contact)
    EzAPI.Path.(root // "marketing" // "contacts" // "search")

let remove_contact_list : (string, string, string option, Security.bearer) EzAPI.service1 =
  EzAPI.service
    ~register:false
    ~section:sendgrid_section
    ~name:"sendgrid_remove_contact_list"
    ~output:job_output
    ~meth:`DELETE
    ~params:[contact_ids_param]
    EzAPI.Path.(root // "marketing" // "lists" /: arg_id // "contacts")
