open StringCompat
open EzAPI.TYPES
open EzSession.TYPES

let verbose = EzAPIServer.verbose

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
  val create_session : login:string -> session
  val get_session : cookie:string -> session option
  val remove_session : login:string -> cookie:string -> unit
end

module SessionStoreInMemory : SessionStore = struct

(*
    TODO: When crowded, we should:
    * limit the number of sessions by users
    * get rid of oldest sessions in general
 *)

  let (session_by_cookie : (string, session) Hashtbl.t) =
    Hashtbl.create initial_hashtbl_size

  let rec create_session ~login =
    let cookie = random_challenge () in
    if Hashtbl.mem session_by_cookie cookie then
      create_session ~login
    else begin
        let s = {
            session_login = login;
            session_cookie = cookie;
            session_variables = StringMap.empty;
            session_last = EzAPIServer.req_time ();
          } in
        Hashtbl.add session_by_cookie cookie s;
        s
      end

  let get_session ~cookie =
    match Hashtbl.find session_by_cookie cookie with
    | exception Not_found ->
       None
    | s ->
       s.session_last <- EzAPIServer.req_time ();
       Some s

  let remove_session ~login ~cookie =
    match get_session ~cookie with
    | None -> ()
    | Some s ->
       if s.session_login = login then
         Hashtbl.remove session_by_cookie cookie

end

module UserStoreInMemory(S : EzSession.TYPES.SessionArg) : sig

  type pwhash = string
  val create_user :
    ?pwhash:pwhash ->
    ?password:string -> login:string -> S.user_info -> unit
  val remove_user : login:string -> unit
  val find_user : login:string -> (pwhash * S.user_info) option

  module SessionArg : EzSession.TYPES.SessionArg
         with type user_info = S.user_info
  module SessionStore : SessionStore
end = struct

  type pwhash = string

  module SessionArg = S
  module SessionStore = SessionStoreInMemory

  type user = {
      login : string;
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
                  user_info }

  let find_user ~login =
    if verbose > 1 then
      EzDebug.printf "find_user %S ?" login;
    match Hashtbl.find users login with
    | exception Not_found -> None
    | u ->
       if verbose > 1 then
         EzDebug.printf "find_user %S ok" login;
       Some (u.pwhash, u.user_info)


  let remove_user ~login =
    Hashtbl.remove users login

end


module Make(S: sig
                module SessionArg : EzSession.TYPES.SessionArg
                module SessionStore : SessionStore
                val find_user : login:string ->
                                         (string *
                                            SessionArg.user_info) option
              end
           ) : sig

  val register_handlers :
    EzAPI.request EzAPIServer.directory ->
    EzAPI.request EzAPIServer.directory

  val get_request_session : EzAPI.request -> session option

end = struct

  let find_user = S.find_user
  open S.SessionStore
  module S = S.SessionArg

  let token_kind =
    match S.token_kind with
    | `Cookie name -> `Cookie name
    | `CSRF name -> `CSRF (String.lowercase name)

  module M = EzSession.Make(S)
  include M

  let get_request_session req =
    match
      match EzAPI.find_param Service.param_token req with
      | None ->
         None
      | Some cookie ->
         match get_session ~cookie with
         | Some s -> Some s
         | None -> None
    with
    | Some _ as res -> res
    | None ->
       match token_kind with
       | `Cookie name ->
          begin
            match StringMap.find name (EzCookieServer.get req) with
            | exception Not_found ->
               None
            | cookie ->
               get_session ~cookie
          end
       | `CSRF name ->
          match StringMap.find name req.req_headers with
          | exception Not_found ->
             None
          | [] -> None
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
        let t0 = EzAPIServer.req_time () in
        if Queue.length challenge_queue > max_challenges then begin
            let challenge_id = Queue.take challenge_queue in
            Hashtbl.remove challenges challenge_id
          end;
        Hashtbl.add challenges challenge_id (challenge, t0);
        Queue.add challenge_id challenge_queue;
        AuthNeeded (challenge_id, challenge)

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

    let request_auth req =
      add_auth_header req;
      EzAPIServer.return (new_challenge ())

    let return_auth req ?cookie ~login user_info =
      let cookie = match cookie with
        | None ->
           let s = create_session ~login in
           s.session_cookie
        | Some cookie -> cookie
      in
      add_auth_header ~cookie req;
      EzAPIServer.return
        (AuthOK (login, cookie, user_info))

    let connect req () =
      match get_request_session req with
      | None ->
         request_auth req
      | Some { session_cookie = cookie;
               session_login = login;
               _ } ->
         match find_user ~login with
         | None ->
            request_auth req
         | Some (_pwhash, user_info) ->
            return_auth req ~cookie ~login user_info

    let login req { login_user; login_challenge_id; login_challenge_reply } =
      match find_user ~login:login_user with
      | None ->
         if verbose > 1 then
           EzDebug.printf "/login: could not find user %S\n%!" login_user;
         request_auth req
      | Some (pwhash, user_info) ->
         match Hashtbl.find challenges login_challenge_id with
         | exception Not_found ->
            if verbose > 1 then
              EzDebug.printf "/login: could not find challenge\n%!";
            request_auth req
         | (challenge, _t0) ->
            let expected_reply =
              EzSession.Hash.challenge
                ~challenge ~pwhash
            in
            if expected_reply <> login_challenge_reply then begin
                if verbose > 1 then
                  EzDebug.printf "/login: challenge failed\n%!";
                request_auth req
              end else begin
                Hashtbl.remove challenges login_challenge_id;
                return_auth req ~login:login_user user_info
              end

    let logout req () =
      match get_request_session req with
      | None ->
         request_auth req
      | Some { session_login=login; session_cookie = cookie; _ } ->
         remove_session ~login ~cookie;
         request_auth req
  end

  let register_handlers dir =
    dir
    |> EzAPIServer.register Service.connect Handler.connect
    |> EzAPIServer.register Service.login Handler.login
    |> EzAPIServer.register Service.logout Handler.logout

end
