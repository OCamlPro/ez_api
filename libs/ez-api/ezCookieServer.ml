open EzAPI.TYPES
open StringCompat

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

let cookie_re = Re_str.regexp "[;,][ \t]*"
let equals_re = Re_str.regexp_string "="

let get ( req : EzAPI.request ) =
  List.fold_left
    (fun acc header ->
      let comps = Re_str.split_delim cookie_re header in
      (* We don't handle $Path, $Domain, $Port, $Version (or $anything
             $else) *)
      let cookies = List.filter (fun s -> s.[0] != '$') comps in
      let split_pair acc nvp =
        match Re_str.bounded_split equals_re nvp 2 with
        | [] -> StringMap.add "" "" acc
        | n :: [] -> StringMap.add n "" acc
        | n :: v :: _ -> StringMap.add n v acc
      in
      List.fold_left split_pair acc cookies
    ) StringMap.empty (StringMap.find "cookie" req.req_headers)



let set ?secure ?http_only ?expiration (req : EzAPI.request) ~name ~value =
  let version = match req.req_version with
    | HTTP_1_0 -> `HTTP_1_0
    | HTTP_1_1 -> `HTTP_1_1
  in
  let header =
    Cohttp.Cookie.Set_cookie_hdr.serialize
      ~version
      (Cohttp.Cookie.Set_cookie_hdr.make ?expiration ?secure ?http_only
         (name, value)
      )
  in
  req.rep_headers <- header :: req.rep_headers

let clear req ~name =
  set req ~name ~value:"" ~expiration:(`Max_age 0L)

let set ?secure ?http_only req ~name ~value =
  set ?secure ?http_only req ~name ~value
