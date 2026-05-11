open EzAPI
open Cohttp

let headers req =
  let headers = ref StringMap.empty in
  Header.iter (fun s v ->
      headers :=
        StringMap.add (String.lowercase_ascii s) (String.split_on_char ',' v) !headers)
    (Request.headers req);
  !headers

let meth req = match Request.meth req with
  | #Meth.all as m -> Some m
  | _ -> None

let version req = match Request.version req with
  | #Req.version as v -> Some v
  | _ -> None

let debug ~request req =
  let meth = req |> Request.meth |> Code.string_of_method in
  let target = req |> Request.uri |> Uri.path_and_query in
  let headers = Header.to_list (Request.headers req) in
  request ~meth ~headers target
