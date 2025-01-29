(**************************************************************************)
(*                                                                        *)
(*                 Copyright 2018-2023 OCamlPro                           *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type email_address = {
  email : string;
  name : string option
}

type content_element = {
  content_type : string;
  content_value : string;
}

type person = {
  dst : email_address list;
  cc : email_address list option;
  bcc : email_address list option;
  psubject: string option;
  data : Json_repr.any option;
}

type 'a mail = {
  person: person list;
  from: email_address;
  subject : string option;
  content: content_element list option;
  template_id : string option;
  reply_to: email_address option;
  more_fields : 'a option;
}

type contact = {
  addr_line1 : string option;
  addr_line2 : string option;
  alternate_emails : string list option;
  city : string option;
  country : string option;
  c_email : string;
  first_name : string option;
  last_name : string option;
  postal_code : string option;
  state_province_region : string option;
  custom_field : Json_repr.any option
}

type contact_more = {
  c_id : string;
  phone_number : string option;
  whatsapp : string option;
  line : string option;
  facebook : string option;
  unique_name : string option;
  list_ids : string list;
  segment_ids : string list option;
  created_at : string;
  updated_at : string
}
