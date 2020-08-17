open Json_encoding

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
    (fun {person; from; subject; content; template_id; more_fields}
      -> (person, from, subject, content, template_id), more_fields)
    (fun ((person, from, subject, content, template_id), more_fields)
      -> {person; from; subject; content; template_id; more_fields}) @@
  merge_objs
    (obj5
       (req "personalizations" (list person))
       (req "from" email_address)
       (opt "subject" string)
       (opt "content" (list content_element))
       (opt "template_id" string))
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

let add_contacts_enc = obj2
    (opt "list_ids" (list string))
    (req "contacts" (list contact))
