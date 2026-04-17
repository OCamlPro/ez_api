open EzAPI

let add_user_agent ?headers () =
  match !Req.user_agent_header, headers with
  | None, _ -> headers
  | Some h, None -> Some [ h ]
  | Some h, Some l ->
    if List.exists (fun (k, _) -> String.lowercase_ascii k = "user-agent") l then Some l
    else Some (h :: l)

let handle_input: type i.
  ?params:(Param.t * param_value) list ->
  ?post:bool ->
  ?url_encode:bool ->
  input:i ->
  EzAPI.base_url ->
  ('arg, i, 'output, 'error, 'security) service ->
  'arg -> ((url -> _) * (?content_type:string -> ?content:string -> url -> _)) -> _ =
  fun ?(params=[]) ?(post=false) ?(url_encode=false) ~input api service arg (get, pos) ->
  match Service.input service.s with
  | Empty ->
    if post then
      let content, content_type = encode_params service.s params, Mime.(to_string url_encoded) in
      pos ~content ~content_type @@ forge api service arg []
    else get @@ forge api service arg params
  | Raw [] ->
    let content, content_type = input, "application/octet-stream" in
    pos ~content ~content_type @@ forge api service arg params
  | Raw [ h ] when h = Mime.url_encoded && input = "" ->
    let content, content_type = encode_params service.s params, Mime.to_string h in
    pos ~content ~content_type @@ forge api service arg []
  | Raw (h :: _) ->
    let content, content_type = input, Mime.to_string h in
    pos ~content ~content_type @@ forge api service arg params
  | Json enc ->
    let content, content_type =
      if url_encode then Url.encode_obj enc input, Mime.(to_string url_encoded)
      else EzEncoding.construct enc input, Mime.(to_string json) in
    pos ~content ~content_type @@ forge api service arg params
