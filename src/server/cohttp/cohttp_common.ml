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

let debug req =
  Log.debug "[%t] REQUEST: %s %S" GMTime.pp_now
    (req |> Request.meth |> Code.string_of_method)
    (req |> Request.uri |> Uri.path_and_query);
  Log.debugf ~v:1 (fun () ->
      Header.iter (fun s v ->
          List.iter (fun v -> EzDebug.printf "  %s: %s" s v)
            (String.split_on_char ',' v))
        (Request.headers req))
