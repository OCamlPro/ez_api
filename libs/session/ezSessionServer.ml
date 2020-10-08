open StringCompat
open EzAPI.TYPES
open EzSession.TYPES
open Lwt.Infix

let debug = EzAPIServerUtils.debug

(* WARNINGS:
   * A user might try to fill the table of cookies with useless entries
   * Beware of CSS: user logins should be checked against injection of
       code (HTML, PGSQL)
   * Beware of cookie prediction, use more entropy for randomness
 *)

(* maximal number of stored challenges at any time *)
let max_challenges = 10_000
(* size of challenge_id, challenge and cookie *)
let challenge_size = 30
(* initial size of all hashtbls *)
let initial_hashtbl_size = 100

exception UserAlreadyDefined
exception NoPasswordProvided

let randomChars =
  "abcdefghijklmnopqrstuvxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let randomCharsLen = String.length randomChars

(* challenges should be printable to enforce that they can be directly
  written in URLs*)
let random_challenge () =
  let b = Bytes.create challenge_size in
  for i = 0 to Bytes.length b - 1 do
    Bytes.set b i (randomChars.[Random.int randomCharsLen])
  done;
  Bytes.to_string b

module type SessionStore = sig
  type user_id
  val create_session : ?foreign:foreign_info ->
    login:string -> user_id -> user_id session Lwt.t
  val get_session : cookie:string -> user_id session option Lwt.t
  val remove_session : user_id -> cookie:string -> unit Lwt.t
end

module type Arg = sig
  module SessionArg : EzSession.TYPES.SessionArg
  module SessionStore : SessionStore with type user_id = SessionArg.user_id
  val find_user : login:string ->
    (string option * SessionArg.user_id * SessionArg.user_info) option Lwt.t
  val check_foreign :
    origin:string -> token:string ->
    (string, int * string option) result Lwt.t
end

module Make(S: Arg) : sig

  val register_handlers :
    EzAPI.request EzAPIServerUtils.directory ->
    EzAPI.request EzAPIServerUtils.directory

  val get_request_session :
    EzAPI.request -> S.SessionArg.user_id session option Lwt.t

end = struct

  let find_user = S.find_user
  let check_foreign = S.check_foreign
  open S.SessionStore
  module S = S.SessionArg

  module M = EzSession.Make(S)
  include M

  let cookie_of_param req (`Query { EzAPI.name = param ; _}) =
    EzAPI.find_param param req

  let cookie_of_cookie req (`Cookie { EzAPI.name ; _ }) =
    try Some (StringMap.find name (EzCookieServer.get req))
    with Not_found -> None

  let cookie_of_header req (`Header { EzAPI.name ; _ }) =
    let name = String.lowercase_ascii name in
    match StringMap.find name req.req_headers with
    | exception Not_found -> None
    | [] -> None
    | cookie :: _ -> Some cookie

  let get_request_session security req =
    List.map (function
        | `Query _ as s -> cookie_of_param req s
        | `Cookie _ as s -> cookie_of_cookie req s
        | `Header _ as s -> cookie_of_header req s
      ) security
    |> Lwt_list.fold_left_s (function
        | Some s -> fun _ -> Lwt.return_some s
        | None -> function
          | None -> Lwt.return_none
          | Some cookie -> get_session ~cookie
      ) None

  module Handler = struct

    let challenges = Hashtbl.create initial_hashtbl_size
    let challenge_queue = Queue.create ()

    let rec new_challenge () =
      let challenge_id = random_challenge () in
      if Hashtbl.mem challenges challenge_id then
        new_challenge ()
      else
        let challenge = random_challenge () in
        let t0 = EzAPIServerUtils.req_time () in
        if Queue.length challenge_queue > max_challenges then begin
            let challenge_id = Queue.take challenge_queue in
            Hashtbl.remove challenges challenge_id
          end;
        Hashtbl.add challenges challenge_id (challenge, t0);
        Queue.add challenge_id challenge_queue;
        { challenge_id; challenge }

    let add_auth_header ?cookie req =
      match S.token_kind with
      | `Cookie name ->
        begin match cookie with
          | None -> ()
          | Some cookie ->
            EzCookieServer.set req ~name ~value:cookie
        end
      | `CSRF header ->
        req.rep_headers <-
          ("access-control-allow-headers", header) ::
          req.rep_headers

    let request_auth_base req f =
      add_auth_header req;
      let res, code = f @@ new_challenge () in
      EzAPIServerUtils.return ?code res

    let request_auth req =
      request_auth_base req (fun auth_needed ->
          Ok (AuthNeeded auth_needed), Some 200
        )

    let request_error ~code req msg =
      add_auth_header req;
      EzAPIServerUtils.return ~code (Error msg)

    let return_auth_base req ?cookie ?foreign ~login user_id user_info f =
      begin match cookie with
        | Some cookie -> Lwt.return cookie
        | None ->
          create_session ?foreign ~login user_id >>= function s ->
            Lwt.return s.session_cookie
      end
      >>= function cookie ->
        add_auth_header ~cookie req;
        let auth = {
          auth_login = login;
          auth_user_id = user_id;
          auth_token = cookie;
          auth_user_info = user_info;
        } in
        EzAPIServerUtils.return (f auth)

    let return_auth req ?cookie ?foreign ~login user_id user_info =
      return_auth_base req ?cookie ?foreign ~login user_id user_info
        (fun auth -> Ok auth)

    let connect req security () =
      get_request_session security req >>= function
      | None -> request_auth req
      | Some { session_cookie = cookie; session_login = login; session_foreign = foreign; _ } ->
        find_user ~login >>= function
        | None -> request_error req ~code:440 `Session_expired
        | Some (pwhash, user_id, user_info) ->
          match pwhash, foreign with
          | Some _pwhash, None ->
            return_auth_base req ~cookie ~login user_id user_info (fun a -> Ok (AuthOk a))
          | None, Some {foreign_origin = origin; foreign_token = token} ->
            check_foreign ~origin ~token >>= (function
                | Error _e -> request_error req ~code:440 `Session_expired
                | Ok foreign_id ->
                  if login = foreign_id then
                    return_auth_base req ~cookie ~login ?foreign user_id user_info
                      (fun a -> Ok (AuthOk a))
                  else
                    request_error req ~code:400 (`Invalid_session_connect "wrong user"))
          | _ -> request_error req ~code:400 (`Invalid_session_connect "wrong type of authentication")

    let login req _  = function
      | Local { login_user; login_challenge_id; login_challenge_reply } ->
        begin
          find_user ~login:login_user >>= function
          | Some (Some pwhash, user_id, user_info) ->
            begin match Hashtbl.find challenges login_challenge_id with
              | exception Not_found ->
                debug ~v:1 "/login: could not find challenge\n%!";
                request_error req ~code:401
                  (`Challenge_not_found_or_expired login_challenge_id)
              | (challenge, _t0) ->
                let expected_reply = EzSession.Hash.challenge ~challenge ~pwhash in
                if expected_reply <> login_challenge_reply then begin
                  debug ~v:1 "/login: challenge failed";
                  request_error req ~code:401 `Bad_user_or_password
                end else begin
                  Hashtbl.remove challenges login_challenge_id;
                  return_auth req ~login:login_user user_id user_info
                end
            end
          | _ ->
            debug ~v:1 "/login: could not find user %S" login_user;
            request_error req ~code:401 `Bad_user_or_password

        end
      | Foreign {foreign_origin; foreign_token} ->
        check_foreign ~origin:foreign_origin ~token:foreign_token >>= function
        | Error _ -> request_error req ~code:400 (`Invalid_session_login "foreign authenticatino fail")
        | Ok foreign_login ->
          find_user ~login:foreign_login >>= function
          | Some (_, user_id, user_info) ->
            return_auth req ~login:foreign_login ~foreign:{foreign_origin; foreign_token}
              user_id user_info
          | _ ->
            debug ~v:1 "/login: could not find foreign user";
            request_error req ~code:400 `User_not_registered

    let logout req security () =
       get_request_session security req >>= function
      | None -> EzAPIServerUtils.return ~code:400 (Error (`Invalid_session_logout "session doesn't exist"))
      | Some { session_user_id ; session_cookie = cookie; _ } ->
         remove_session session_user_id ~cookie >>= fun () ->
         request_auth_base req (fun auth_needed -> Ok auth_needed, None)
  end

  let register_handlers dir =
    let open EzAPIServerUtils in
    dir
    |> register Service.connect Handler.connect
    |> register Service.login Handler.login
    |> register Service.logout Handler.logout

  let get_request_session req =
    get_request_session Service.security req


end



module SessionStoreInMemory :
  SessionStore with type user_id = string = struct

(*
    TODO: When crowded, we should:
    * limit the number of sessions by users
    * get rid of oldest sessions in general
 *)

  type user_id = string

  let (session_by_cookie : (string, user_id session) Hashtbl.t) =
    Hashtbl.create initial_hashtbl_size

  let rec create_session ?foreign ~login user_id =
    let cookie = random_challenge () in
    if Hashtbl.mem session_by_cookie cookie then
      create_session ~login user_id
    else begin
      let s = {
        session_login = login;
        session_user_id = user_id;
        session_cookie = cookie;
        session_foreign = foreign;
        session_last = EzAPIServerUtils.req_time ();
      } in
      Hashtbl.add session_by_cookie cookie s;
      Lwt.return s
    end

  let get_session ~cookie =
    match Hashtbl.find session_by_cookie cookie with
    | exception Not_found ->
       Lwt.return None
    | s ->
       Lwt.return (Some { s with session_last = EzAPIServerUtils.req_time () })

  let remove_session user_id ~cookie =
    get_session ~cookie >>= function
    | None -> Lwt.return ()
    | Some s ->
      if s.session_user_id = user_id then
        Hashtbl.remove session_by_cookie cookie;
      Lwt.return ()

end

module UserStoreInMemory(
    S : EzSession.TYPES.SessionArg
    with type user_id = string) : sig

  val create_user :
    ?pwhash:string -> ?password:string -> ?kind:string ->
    login:string -> S.user_info -> unit
  val remove_user : login:string -> unit
  val find_user : login:string -> (string option * S.user_id * S.user_info) option Lwt.t
  val check_foreign : origin:string -> token:string ->
    (string, int * string option) result Lwt.t

  module SessionArg : EzSession.TYPES.SessionArg
    with type user_info = S.user_info
     and type user_id = S.user_id
  module SessionStore : SessionStore with type user_id = S.user_id

end = struct

  module SessionArg = S
  module SessionStore =
    (SessionStoreInMemory : SessionStore with type user_id = S.user_id)

  type user = {
    login : string;
    user_id : S.user_id;
    mutable pwhash : string; (* hash of password *)
    mutable user_info : S.user_info;
    kind : string option;
  }

  let (users : (string, user) Hashtbl.t) =
    Hashtbl.create initial_hashtbl_size

  let create_user ?pwhash ?password ?kind ~login user_info =
    debug ~v:1 "create_user %S ?" login;
    if Hashtbl.mem users login then raise UserAlreadyDefined;
    match kind with
    | Some _ ->
      debug ~v:1 "create_user %S ok" login;
      Hashtbl.add users login
        { login; pwhash = ""; user_id = login; user_info; kind }
    | None ->
      let pwhash = match pwhash with
        | Some pwhash -> pwhash
        | None ->
          match password with
          | None -> raise NoPasswordProvided
          | Some password ->
            EzSession.Hash.password ~login ~password in
      debug ~v:1 "create_user %S ok" login;
      Hashtbl.add users login
        { login; pwhash; user_id = login; user_info; kind }

  let find_user ~login =
    debug ~v:1 "find_user %S ?" login;
    match Hashtbl.find users login with
    | exception Not_found ->
      Lwt.return None
    | u ->
      debug ~v:1 "find_user %S ok" login;
      let pwhash = match u.kind with None -> Some u.pwhash | Some _k -> None in
      Lwt.return ( Some (pwhash, u.user_id, u.user_info) )

  let check_foreign ~origin ~token =
    debug ~v:1 "check_foreign %S ?" (origin ^ "-" ^ token);
    match Hashtbl.find users (origin ^ "-" ^ token) with
    | exception Not_found -> Lwt.return (Error (500, Some "User not found"))
    | u ->
      debug ~v:1 "check_foreign %S ok" (origin ^ "-" ^ token);
      Lwt.return (Ok u.login)


  let remove_user ~login =
    Hashtbl.remove users login

end
