open EzAPIServerUtils

(* RFC 2965 has
    cookie          =  "Cookie:" cookie-version 1*((";" | ",") cookie-value)
    cookie-value    =  NAME "=" VALUE [";" path] [";" domain] [";" port]
    cookie-version  =  "$Version" "=" value
    NAME            =  attr
    VALUE           =  value
    path            =  "$Path" "=" value
    domain          =  "$Domain" "=" value
    port            =  "$Port" [ "=" <"> value <"> ]
  *)

let cookie_re = Re.Str.regexp "[;,][ \t]*"
let equals_re = Re.Str.regexp_string "="

let get ( req : Req.t ) =
  List.fold_left
    (fun acc header ->
      let comps = Re.Str.split_delim cookie_re header in
      (* We don't handle $Path, $Domain, $Port, $Version (or $anything
             $else) *)
      let cookies = List.filter (fun s -> s.[0] != '$') comps in
      let split_pair acc nvp =
        match Re.Str.bounded_split equals_re nvp 2 with
        | [] -> StringMap.add "" "" acc
        | n :: [] -> StringMap.add n "" acc
        | n :: v :: _ -> StringMap.add n v acc
      in
      List.fold_left split_pair acc cookies
    ) StringMap.empty (StringMap.find "cookie" req.Req.req_headers)

let set ?secure ?http_only ?expiration (req : Req.t) ~name ~value =
  let version = req.Req.req_version in
  Cohttp.Cookie.Set_cookie_hdr.serialize ~version @@
  Cohttp.Cookie.Set_cookie_hdr.make ?expiration ?secure ?http_only (name, value)

let clear req ~name =
  set req ~name ~value:"" ~expiration:(`Max_age 0L)

let set ?secure ?http_only req ~name ~value =
  set ?secure ?http_only req ~name ~value
