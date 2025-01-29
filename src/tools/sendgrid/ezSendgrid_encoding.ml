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
open Json_encoding

let opt_encoding encoding =
  let unit_opt_encoding = case unit (function None -> Some () | Some _ -> None) (fun _ -> None) in
  match encoding with
  | None -> union [ unit_opt_encoding ]
  | Some encoding ->
    union [
      case encoding
        (fun x -> x)
        (fun x -> Some x);
      unit_opt_encoding ]

let email_address =
  conv (fun {email; name} -> (email, name)) (fun (email, name) -> {email; name}) @@
  obj2 (req "email" string) (opt "name" string)

let content_element = conv
    (fun {content_type; content_value} -> (content_type, content_value))
    (fun (content_type, content_value) -> {content_type; content_value}) @@
  obj2 (req "type" string) (req "value" string)

let person = conv
    (fun {dst; cc; bcc; psubject; data} -> (dst, cc, bcc, psubject, data))
    (fun (dst, cc, bcc, psubject, data) -> {dst; cc; bcc; psubject; data}) @@
  obj5
    (req "to" (list email_address))
    (opt "cc" (list email_address))
    (opt "bcc" (list email_address))
    (opt "subject" string)
    (opt "dynamic_template_data" any_value)

let mail more_encoding =
  conv
    (fun {person; from; subject; content; template_id; reply_to; more_fields}
      -> (person, from, subject, content, template_id, reply_to), more_fields)
    (fun ((person, from, subject, content, template_id, reply_to), more_fields)
      -> {person; from; subject; content; template_id; reply_to; more_fields}) @@
  merge_objs
    (obj6
       (req "personalizations" (list person))
       (req "from" email_address)
       (opt "subject" string)
       (opt "content" (list content_element))
       (opt "template_id" string)
       (opt "reply_to" email_address))
    (opt_encoding more_encoding)

let contact = conv
    (fun {addr_line1; addr_line2; alternate_emails; city; country; c_email;
          first_name; last_name; postal_code; state_province_region;
          custom_field }
      -> (c_email, addr_line1, addr_line2, alternate_emails, city, country,
          first_name, last_name, postal_code, state_province_region,
          custom_field))
    (fun (c_email, addr_line1, addr_line2, alternate_emails, city, country,
          first_name, last_name, postal_code, state_province_region,
          custom_field)
      -> {addr_line1; addr_line2; alternate_emails; city; country; c_email;
          first_name; last_name; postal_code; state_province_region;
          custom_field }) @@
  EzEncoding.obj11
    (req "email" string)
    (opt "address_line_1" string)
    (opt "address_line_2" string)
    (opt "alternate_emails" (list string))
    (opt "city" string)
    (opt "country" string)
    (opt "first_name" string)
    (opt "last_name" string)
    (opt "postal_code" string)
    (opt "state_province_region" string)
    (opt "custom_fields" Json_encoding.any_value)

let contact_more = conv
    (fun {c_id; phone_number; whatsapp; line; facebook; unique_name; list_ids;
          segment_ids; created_at; updated_at}
      -> (c_id, phone_number, whatsapp, line, facebook, unique_name, list_ids,
          segment_ids, created_at, updated_at))
    (fun (c_id, phone_number, whatsapp, line, facebook, unique_name, list_ids,
          segment_ids, created_at, updated_at)
      -> {c_id; phone_number; whatsapp; line; facebook; unique_name; list_ids;
          segment_ids; created_at; updated_at}) @@
  obj10
    (req "id" string)
    (opt "phone_number" string)
    (opt "whatsapp" string)
    (opt "line" string)
    (opt "facebook" string)
    (opt "unique_nname" string)
    (req "list_ids" (list string))
    (dft "segment_ids" (option @@ list string) None)
    (req "created_at" string)
    (req "updated_at" string)

let add_contacts_enc = obj2
    (opt "list_ids" (list string))
    (req "contacts" (list contact))

let job_output = obj1 (req "job_id" string)

let contacts_count = EzEncoding.ignore_enc (obj1 (req "contact_count" int))

let get_contact =
  EzEncoding.ignore_enc (merge_objs contact contact_more)

let search_output encoding =
  EzEncoding.ignore_enc @@ obj2
    (req "contact_count" int)
    (dft "result" (list encoding) [])

let query = obj1 (req "query" string)
