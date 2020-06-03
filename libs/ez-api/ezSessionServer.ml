open StringCompat
open EzAPI.TYPES
open EzSession.TYPES

let (>>=) = Lwt.(>>=)

let verbose = EzAPIServerUtils.verbose

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
  val create_session : login:string -> user_id -> user_id session Lwt.t
  val get_session : cookie:string -> user_id session option Lwt.t
  val remove_session : user_id -> cookie:string -> unit Lwt.t
end

module type Arg = sig
  module SessionArg : EzSession.TYPES.SessionArg
  module SessionStore : SessionStore with type user_id = SessionArg.user_id
  val find_user : login:string ->
    (string * SessionArg.user_id * SessionArg.user_info) option Lwt.t
  (* val error_wrapper : (string -> string) option *)
end

module Make(S: Arg) : sig

  val register_handlers :
    EzAPI.request EzAPIServerUtils.directory ->
    EzAPI.request EzAPIServerUtils.directory

  val get_request_session :
    EzAPI.request -> S.SessionArg.user_id session option Lwt.t

  val register :
           ('arg, 'b, 'input, 'd, 'e) EzAPI.service ->
           ('arg -> 'input -> ('d, 'e) result EzAPIServerUtils.answer Lwt.t) ->
           EzAPI.request EzAPIServerUtils.directory ->
           EzAPI.request EzAPIServerUtils.directory

end = struct

  let find_user = S.find_user
  open S.SessionStore
  module S = S.SessionArg

  let token_kind =
    match S.token_kind with
    | `Cookie name -> `Cookie name
    | `CSRF name -> `CSRF (String.lowercase_ascii name)

  module M = EzSession.Make(S)
  include M

  let get_request_session req =
    begin
      match EzAPI.find_param Service.param_token req with
      | None ->
        Lwt.return None
      | Some cookie ->
        get_session ~cookie >>= function
        | Some s -> Lwt.return (Some s)
        | None -> Lwt.return None
    end >>= function
    | Some _ as res -> Lwt.return res
    | None ->
      match token_kind with
      | `Cookie name ->
        begin
          match StringMap.find name (EzCookieServer.get req) with
          | exception Not_found ->
            Lwt.return None
          | cookie ->
            get_session ~cookie
        end
      | `CSRF name ->
        match StringMap.find name req.req_headers with
        | exception Not_found ->
          Lwt.return None
        | [] -> Lwt.return None
        | cookie :: _ ->
          get_session ~cookie

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
          Error (`Auth_needed auth_needed), Some 401
        )

    let request_error ~code req msg =
      add_auth_header req;
      (* let content = match error_wrapper with None -> msg | Some f -> f msg in *)
      EzAPIServerUtils.return ~code (Error msg)

    let return_auth_base req ?cookie ~login user_id user_info f =
      begin
        match cookie with
        | Some cookie -> Lwt.return cookie
        | None ->
          create_session ~login user_id >>= function s ->
            Lwt.return s.session_cookie
      end
      >>= function cookie ->
        add_auth_header ~cookie req;
        let auth = {
          S.auth_login = login;
          auth_user_id = user_id;
          auth_token = cookie;
          auth_user_info = user_info;
        } in
        EzAPIServerUtils.return (f auth)

    let return_auth req ?cookie ~login user_id user_info =
      return_auth_base req ?cookie ~login user_id user_info
        (fun auth -> Ok auth)

    let connect req () =
      get_request_session req >>= function
      | None ->
        request_auth req
      | Some { session_cookie = cookie;
               session_login = login;
               _ } ->
        find_user ~login >>= function
        | None ->
          request_error req ~code:440 `Session_expired
        | Some (_pwhash, user_id, user_info) ->
          return_auth req ~cookie ~login user_id user_info

    let login req { login_user; login_challenge_id; login_challenge_reply } =
      find_user ~login:login_user >>= function
      | None ->
         if verbose > 1 then
           EzDebug.printf "/login: could not find user %S\n%!" login_user;
         request_error req ~code:401 `Bad_user_or_password
      | Some (pwhash, user_id, user_info) ->
         match Hashtbl.find challenges login_challenge_id with
         | exception Not_found ->
            if verbose > 1 then
              EzDebug.printf "/login: could not find challenge\n%!";
            request_error req ~code:403
              (`Challenge_not_found_or_expired login_challenge_id)
         | (challenge, _t0) ->
            let expected_reply =
              EzSession.Hash.challenge
                ~challenge ~pwhash
            in
            if expected_reply <> login_challenge_reply then begin
                if verbose > 1 then
                  EzDebug.printf "/login: challenge failed\n%!";
                request_error req ~code:401 `Bad_user_or_password
              end else begin
                Hashtbl.remove challenges login_challenge_id;
                return_auth req ~login:login_user user_id user_info
              end

    let logout req () =
       get_request_session req >>= function
      | None -> EzAPIServerUtils.return ~code:403 (Error `Invalid_session)
      | Some { session_user_id ; session_cookie = cookie; _ } ->
         remove_session session_user_id ~cookie >>= fun () ->
         request_auth_base req (fun auth_needed -> Ok auth_needed, None)
  end

  let register service handler =
    let options_headers =
      match S.token_kind with
      | `Cookie _name -> []
      | `CSRF header ->
          ["access-control-allow-headers", header]
    in
    EzAPIServerUtils.register service handler
      ~options_headers

  let register_handlers dir =
    dir
    |> register Service.connect Handler.connect
    |> EzAPIServerUtils.register Service.login Handler.login
    |> register Service.logout Handler.logout

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

  let rec create_session ~login user_id =
    let cookie = random_challenge () in
    if Hashtbl.mem session_by_cookie cookie then
      create_session ~login user_id
    else begin
      let s = {
        session_login = login;
        session_user_id = user_id;
        session_cookie = cookie;
        session_variables = StringMap.empty;
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
       s.session_last <- EzAPIServerUtils.req_time ();
       Lwt.return (Some s)

  let remove_session user_id ~cookie =
    get_session ~cookie >>= function
    | None -> Lwt.return ()
    | Some s ->
      if s.session_user_id = user_id then
        Hashtbl.remove session_by_cookie cookie;
      Lwt.return ()

end

module UserStoreInMemory(S : EzSession.TYPES.SessionArg
                        with type user_id = string) : sig

  type pwhash = string
  val create_user :
    ?pwhash:pwhash ->
    ?password:string -> login:string -> S.user_info -> unit
  val remove_user : login:string -> unit
  val find_user : login:string -> (pwhash * S.user_id * S.user_info) option Lwt.t

  module SessionArg : EzSession.TYPES.SessionArg
    with type user_info = S.user_info
     and type user_id = S.user_id
  module SessionStore : SessionStore with type user_id = S.user_id

end = struct

  type pwhash = string

  module SessionArg = S
  module SessionStore =
    (SessionStoreInMemory : SessionStore with type user_id = S.user_id)

  type user = {
    login : string;
    user_id : S.user_id;
    mutable pwhash : string; (* hash of password *)
    mutable user_info : S.user_info;
  }

  let (users : (string, user) Hashtbl.t) =
    Hashtbl.create initial_hashtbl_size

  let create_user
      ?pwhash ?password ~login
      user_info =
    if verbose > 1 then
      EzDebug.printf "create_user %S ?" login;
    if Hashtbl.mem users login then
      raise UserAlreadyDefined;
    let pwhash = match pwhash with
      | Some pwhash -> pwhash
      | None ->
        match password with
        | None -> raise NoPasswordProvided
        | Some password ->
          EzSession.Hash.password ~login ~password
    in
    if verbose > 1 then
      EzDebug.printf "create_user %S ok" login;
    Hashtbl.add users login
      { login;
        pwhash;
        user_id = login;
        user_info }

  let find_user ~login =
    if verbose > 1 then
      EzDebug.printf "find_user %S ?" login;
    match Hashtbl.find users login with
    | exception Not_found ->
      Lwt.return None
    | u ->
      if verbose > 1 then
        EzDebug.printf "find_user %S ok" login;
      Lwt.return ( Some (u.pwhash, u.user_id, u.user_info) )


  let remove_user ~login =
    Hashtbl.remove users login

end
