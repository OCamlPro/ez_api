module type S = sig
  type 'a t
  val printf : ('a, Format.formatter, unit, unit t) format4 -> 'a
  val ifprintf : ('a, Format.formatter, unit, unit t) format4 -> 'a
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type M = sig
  type 'a t
  val printf : ('a, Format.formatter, unit, unit t) format4 -> 'a
  val debug :
    ?v:int -> ('a, Format.formatter, unit, unit t) format4 -> 'a
  val debugf : ?v:int -> (unit -> unit t) -> unit t
  val headers : (string * string) list -> unit t
  val request :
    meth:string -> headers:(string * string) list -> string -> unit t
  val request_content : ?content_type:string -> string -> unit t
  val response :
    code:int ->
    headers:(string * string) list -> target:string -> string -> unit t
end

let verbose = match Sys.getenv_opt "EZAPISERVER" with
  | None -> ref 0
  | Some s -> match int_of_string_opt s with
    | None -> ref 1
    | Some i -> ref i

let set_verbose i = verbose := i

let rec simple_power x n =
  if n = 0 then 1 else  x * (simple_power x (n-1))

let check ?(v=0) () =
  let mask_version = !verbose >= 8 in
  (not mask_version && !verbose > v) ||
  (mask_version && ((!verbose/8) land (simple_power 2 v)) <> 0)

let content_length_to_string n =
  let rec aux l r d = match l with
    | [] -> assert false
    | [ s ] -> Format.sprintf "%d%s" n s
    | h :: tl ->
      let d2 = d / 1024 in
      if d2 > 10 then aux tl (d mod 1024) d2 else
        let d = if r >= 512 then d+1 else d in
        Format.sprintf "%d%s" d h in
  aux [ ""; "ko"; "Mo"; "To" ] 0 n

let pp_content_length fmt n =
  Format.fprintf fmt "%s" (content_length_to_string n)

module M(S: S) : M with type 'a t = 'a S.t = struct
  type 'a t = 'a S.t

  let printf = S.printf

  let debug ?v fmt =
    if check ?v () then S.printf fmt
    else S.ifprintf fmt

  let debugf ?v f =
    if check ?v () then f () else S.return ()

  let iter f l =
    let rec aux = function
      | [] -> S.return ()
      | h :: tl -> S.bind (f h) @@ fun () -> aux tl in
    aux l

  let headers headers =
    debugf ~v:4 @@ fun () ->
    iter (fun (name, value) -> S.printf "  %s: %s" name value) headers

  let request ~meth ~headers:l target =
    S.bind (debug "[%t] %s %s" GMTime.pp_now meth target) @@ fun () ->
    headers l

  let request_content ?content_type body =
    debugf ~v:2 @@ fun () ->
    if body <> "" && (content_type = Some "application/json" || content_type = Some "text/plain") then
      S.printf "Request content:\n%s" body
    else S.return ()

  let response ~code ~headers:l ~target body =
    S.bind (debugf ~v:(if code >= 200 && code < 300 then 1 else 0) (fun () ->
        let color = if code >= 200 && code < 300 then 32 else 31 in
        S.printf "[%t]\027[0;%dm %d %s\027[0m" GMTime.pp_now color code target)) @@ fun () ->
    S.bind (headers l) @@ fun () ->
    debugf ~v:3 @@ fun () ->
    let content_type = List.assoc_opt "content-type" l in
    if body <> "" && (content_type = Some "application/json" || content_type = Some "text/plain") then
      S.printf "Reply content:\n%s" body
    else S.return ()
end
