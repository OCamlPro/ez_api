open EzSendgrid_encoding

let sendgrid_url = "https://api.sendgrid.com/v3"

let post_base ?meth ~api_key encoding data url =
  let content = EzEncoding.construct encoding data in
  EzRequest_lwt.ANY.post ?meth
    ~content
    ~headers:["Authorization", "Bearer " ^ api_key]
    (EzAPI.TYPES.URL (sendgrid_url ^ url))

let send_base ?encoding ~api_key email =
  post_base ~api_key (mail encoding) email "/mail/send"

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
  send_base ~api_key mail

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
  send_base ~api_key mail

let add_contacts ~api_key contacts =
  post_base ~meth:Resto1.PUT ~api_key add_contacts_enc (None, contacts) "/marketing/contacts"

let delete_contacts ~api_key ?(all=false) ids =
  let query =
    if all then "/marketing/contacts?delete_all_contacts=true"
    else "/marketing/contacts?ids=" ^ String.concat "," ids in
  EzRequest_lwt.ANY.get ~meth:Resto1.DELETE
    ~headers:["Authorization", "Bearer " ^ api_key]
    (EzAPI.TYPES.URL (sendgrid_url ^ query))
