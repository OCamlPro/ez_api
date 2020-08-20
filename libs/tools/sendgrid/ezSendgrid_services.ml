open EzSendgrid_types
open EzSendgrid_encoding

let sendgrid_section = EzAPI.section "Sendgrid Requests"
let sendgrid_host = EzAPI.TYPES.BASE "https://api.sendgrid.com/v3"

let arg_id = EzAPI.arg_string "id" "base64_hash"

let ids_param = EzAPI.Param.string ~descr:"Sendgrid contact ids" ~name:"ids" "ids"
let contact_ids_param = EzAPI.Param.string ~descr:"Sendgrid contact ids" ~name:"contact_ids" "contact_ids"
let delete_all_param = EzAPI.Param.bool ~descr:"Sendgrid delete all contacts" ~name:"delete_all_contacts" "delete_all_contacts"

let send encoding : ('a mail, unit, string option, EzAPI.bearer_security) EzAPI.post_service0 =
  EzAPI.post_service
    ~section:sendgrid_section
    ~name:"sendgrid_send"
    ~input:(mail encoding)
    ~output:Json_encoding.empty
    EzAPI.Path.(root // "mail" // "send")

let add_contacts : (string list option * contact list, string, string option, EzAPI.bearer_security) EzAPI.post_service0 =
  EzAPI.post_service
    ~section:sendgrid_section
    ~name:"sendgrid_add_contacts"
    ~input:add_contacts_enc
    ~output:job_output
    ~meth:Resto1.PUT
    EzAPI.Path.(root // "marketing" // "contacts")

let delete_contacts : (unit, string, string option, EzAPI.bearer_security) EzAPI.post_service0 =
  EzAPI.post_service
    ~section:sendgrid_section
    ~name:"sendgrid_delete_contacts"
    ~input:Json_encoding.empty
    ~output:job_output
    ~meth:Resto1.DELETE
    ~params:[ids_param; delete_all_param]
    EzAPI.Path.(root // "marketing" // "contacts")

let contacts_count : (int, string option, EzAPI.bearer_security) EzAPI.service0 =
  EzAPI.service
    ~section:sendgrid_section
    ~name:"sendgrid_contacts_count"
    ~output:contacts_count
    EzAPI.Path.(root // "marketing" // "contacts" // "count")

let get_contact : (string, contact * contact_more, string option, EzAPI.bearer_security) EzAPI.service1 =
  EzAPI.service
    ~section:sendgrid_section
    ~name:"sendgrid_get_contact"
    ~output:get_contact
    EzAPI.Path.(root // "marketing" // "contacts" /: arg_id)

let search_contacts : (string, int * (contact * contact_more) list, string option, EzAPI.bearer_security) EzAPI.post_service0 =
  EzAPI.post_service
    ~section:sendgrid_section
    ~name:"sendgrid_search_contacts"
    ~input:query
    ~output:(search_output EzSendgrid_encoding.get_contact)
    EzAPI.Path.(root // "marketing" // "contacts" // "search")

let remove_contact_list : (string, string, string option, EzAPI.bearer_security) EzAPI.service1 =
  EzAPI.service
    ~section:sendgrid_section
    ~name:"sendgrid_remove_contact_list"
    ~output:job_output
    ~meth:Resto1.DELETE
    ~params:[contact_ids_param]
    EzAPI.Path.(root // "marketing" // "lists" /: arg_id // "contacts")
