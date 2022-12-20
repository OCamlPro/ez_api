open EzAPIServerUtils
open EzSession.TYPES
open Lwt.Infix

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
  String.init challenge_size (fun _ -> randomChars.[Random.int randomCharsLen])

module type SessionStore = sig
  type user_id
  val create_session : ?foreign:foreign_info ->
    login:string -> req:Req.t -> user_id -> user_id session Lwt.t
  val get_session : ?req:Req.t -> string -> user_id session option Lwt.t
  val remove_session : user_id -> token:string -> unit Lwt.t
end

module type Arg = sig
  module SessionArg : EzSession.TYPES.SessionArg
  module SessionStore : SessionStore with type user_id = SessionArg.user_id
  val find_user : login:string ->
    (string option * SessionArg.user_id * SessionArg.user_info) option Lwt.t
  val check_foreign :
    origin:string -> token:string ->
    (string, int * string option) result Lwt.t
  val register_foreign :
    origin:string -> token:string ->
    (SessionArg.user_id * SessionArg.user_info option, int * string option) result Lwt.t
end

(** Dummy implementation for [Arg.check_foreign] *)
let default_check_foreign ~origin ~token =
  ignore (origin, token);
  Lwt.return (Error (400, Some "Foreign authentication not implemented"))

(** Dummy implementation for [Arg.register_foreign] *)
let default_register_foreign ~origin ~token =
  ignore (origin, token);
  Lwt.return (Error (400, Some "Foreign registration not implemented"))

module Make(S: Arg) : sig

  module Service : EzSession.M with type user_id = S.SessionArg.user_id
                                and type user_info = S.SessionArg.user_info

  val register_handlers :
    Directory.t -> Directory.t

  val get_request_session :
    Req.t -> S.SessionArg.user_id session option Lwt.t

end = struct

  let find_user = S.find_user
  let check_foreign = S.check_foreign
  let register_foreign = S.register_foreign
  open S.SessionStore
  module S = S.SessionArg

  module M = EzSession.Make(S)
  include M

  (** Searches in the given request for the parameter indicated in security configuration. 
  Returns concatenated with ',' strting containing all values associated to the parameter,
  if exists. *)
  let cookie_of_param req (`Query { EzAPI.Security.name = param ; _}) =
    Req.find_param param req

  (** Searches in the given request for the cookie with the name as indicated in security 
  configuration. Cookies aree implemented only for given Cohttp request implementation. *)
  let cookie_of_cookie req (`Cookie { EzAPI.Security.name ; _ }) =
    try Some (StringMap.find name (EzCookieServer.get req))
    with Not_found -> None

  (** Searches in the given request for the header with the name indicated in security
  configuration. Returns the first occurence of the value for the given header. *)
  let cookie_of_header req (`Header { EzAPI.Security.name ; _ }) =
    let name = String.lowercase_ascii name in
    match StringMap.find name req.Req.req_headers with
    | exception Not_found -> None
    | [] -> None
    | cookie :: _ -> Some cookie

  (** Extracts token from then given request by applying one from the defined security
  configurations. If token was succesfully retreived, then it looks up and returns 
  associated to it session. *)
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
          | Some cookie -> get_session ~req cookie
      ) None

  module Handler = struct
    (** Hash map of challenges' id associated with challenge itself and with a time it 
    was created (client's request time). *)
    let challenges = Hashtbl.create initial_hashtbl_size
    
    (** Queue of challenge ids, that allows to remove the oldest one when maximal size 
    of challenge is achieved. *)
    let challenge_queue = Queue.create ()

    (** Create new [auth_needed] that contains random challenge with its random id. If
    maximal size of id challenges is reached, the oldest id is reused to store new
    challenge *)
    let rec new_challenge req =
      let challenge_id = random_challenge () in
      if Hashtbl.mem challenges challenge_id then
        new_challenge req
      else
        let challenge = random_challenge () in
        if Queue.length challenge_queue > max_challenges then begin
            let challenge_id = Queue.take challenge_queue in
            Hashtbl.remove challenges challenge_id
          end;
        Hashtbl.add challenges challenge_id (challenge, req.Req.req_time);
        Queue.add challenge_id challenge_queue;
        { challenge_id; challenge }

    (** Returns authentication header that sould be then added inside the server response.
    If [S.token_kind] is a cookie, then create corresponding {i Set-Cookie} header with 
    [token] as a cookie value. Otherwise creates header {i Access-control-allow-headers} 
    and mention CSRF header name that should be present for every client's request for 
    authentication purpose. *)
    let add_auth_header ?token req =
      match S.token_kind with
      | `Cookie name ->
        begin match token with
          | None -> []
          | Some value ->
            [ EzCookieServer.set req ~name ~value ]
        end
      | `CSRF header ->
        [ "access-control-allow-headers", header ]

    let request_auth_base req f =
      let headers = add_auth_header req in
      let res, code = f @@ new_challenge req in
      return ?code ~headers res

    (** Creates authentification response that returns challenge to resolve. Adds 
    authentification header with [add_auth_header] *)
    let request_auth req =
      request_auth_base req (fun auth_needed ->
          Ok (AuthNeeded auth_needed), Some 200
        )
    (** Creates response that contains error with specified code. Adds authentification 
    header with [add_auth_header]*)
    let request_error ~code req msg =
      let headers = add_auth_header req in
      return ~code ~headers (Error msg)

    let return_auth_base req ?token ?foreign ~login user_id user_info f =
      begin match token with
        | Some token -> Lwt.return token
        | None ->
          create_session ?foreign ~login ~req user_id >>= function s ->
            Lwt.return s.session_token
      end
      >>= function token ->
        let headers = add_auth_header ~token req in
        let auth = {
          auth_login = login;
          auth_user_id = user_id;
          auth_token = token;
          auth_user_info = user_info } in
        return ~headers (f auth)

    (** Creates login response, that returns authentification information. Return [LoginWait]
    if user info isn't provided. If token isn't provided then the new session is created and 
    new session's token is used. *)
    let return_auth req ?token ?foreign ~login user_id user_info =
      match user_info with
      | Some user_info ->
        return_auth_base req ?token ?foreign ~login user_id user_info
          (fun auth -> Ok (LoginOk auth))
      | None ->
        return (Ok (LoginWait user_id))

    (** Connection service handler. It performs next actions:
        - Looks up for the session that is associated to the token extracted from request (returned by [get_request_session]). If session or token don't exist then returns challenge to resolve with
        [request_auth].
        - If session was found, then it extracts user's login related to the session and search for the corresponding user information. If user doesn't exists, it means that session is expired and it responds with an error.
        - If user exists then checks if it contains password. If it does, then returns response containing authentification information and authentification headers. 
        - If it doesn't checks if foreign user with given login exists and returns its information.
        - Otherwise returns an error {!Invalid_session_connect}. *)
    let connect req security () =
      get_request_session security req >>= function
      | None -> request_auth req
      | Some { session_token = token; session_login = login; session_foreign = foreign; _ } ->
        find_user ~login >>= function
        | None -> request_error req ~code:440 `Session_expired
        | Some (pwhash, user_id, user_info) ->
          match pwhash, foreign with
          | Some _pwhash, None ->
            return_auth_base req ~token ~login user_id user_info (fun a -> Ok (AuthOk a))
          | None, Some {foreign_origin = origin; foreign_token = token} ->
            check_foreign ~origin ~token >>= (function
                | Error _e -> request_error req ~code:440 `Session_expired
                | Ok foreign_id ->
                  if login = foreign_id then
                    return_auth_base req ~token ~login ?foreign user_id user_info
                      (fun a -> Ok (AuthOk a))
                  else
                    request_error req ~code:400 (`Invalid_session_connect "wrong user"))
          | _ -> request_error req ~code:400 (`Invalid_session_connect "wrong type of authentication")

    (** Login service handler. It performs next actions for local users (have password):
        - Search for the user with the provided login.
        - Verify password by comparing challenge reply send by client with expected one computed by server.
        - Discard used challenge.
        - Create session with given user and storee it.
        - Returns authentification information (if login sucessed).
        And for foreign (without password) users:
        - Checks if foreign user exists and get its login.
        - Searching for user information with the given login.
        - If user exists, create session and storee it.
        - Otherwise, register foreign user and returns authentification information. *)
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
                  return_auth req ~login:login_user user_id (Some user_info)
                end
            end
          | _ ->
            debug ~v:1 "/login: could not find user %S" login_user;
            request_error req ~code:401 `Bad_user_or_password

        end
      | Foreign {foreign_origin; foreign_token} ->
        check_foreign ~origin:foreign_origin ~token:foreign_token >>= function
        | Error _ -> request_error req ~code:400 (`Invalid_session_login "foreign authentication fail")
        | Ok foreign_login ->
          find_user ~login:foreign_login >>= function
          | Some (_, user_id, user_info) ->
            return_auth req ~login:foreign_login ~foreign:{foreign_origin; foreign_token}
              user_id (Some user_info)
          | None ->
            register_foreign ~origin:foreign_origin ~token:foreign_token >>= function
            | Ok (user_id, user_info) ->
              return_auth req ~login:foreign_login ~foreign:{foreign_origin; foreign_token}
                user_id user_info
            | Error _ ->
              debug ~v:1 "/login: could not register foreign user";
              request_error req ~code:400 `User_not_registered

    (** Connection service handler that at the end returns new challenge to make possible 
    further connections. It checks for authentification token within request and if it 
    exists then remove current session associtated to the token. Otherwise returns an error. *)
    let logout req security () =
       get_request_session security req >>= function
      | None -> return ~code:400 (Error (`Invalid_session_logout "session doesn't exist"))
      | Some { session_user_id ; session_token = token; _ } ->
         remove_session session_user_id ~token >>= fun () ->
         request_auth_base req (fun auth_needed -> Ok auth_needed, None)
  end

  let register_handlers dir =
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

  (** Hash map that stores sessions by token *)
  let (session_by_token : (string, user_id session) Hashtbl.t) =
    Hashtbl.create initial_hashtbl_size

  let rec create_session ?foreign ~login ~req user_id =
    let token = random_challenge () in
    if Hashtbl.mem session_by_token token then
      create_session ~login ~req user_id
    else begin
      let s = {
        session_login = login;
        session_user_id = user_id;
        session_token = token;
        session_foreign = foreign;
        session_last = req.Req.req_time;
      } in
      Hashtbl.add session_by_token token s;
      Lwt.return s
    end

  let get_session ?req token =
    match Hashtbl.find session_by_token token with
    | exception Not_found ->
       Lwt.return None
    | s ->
      let s = match req with None -> s | Some req -> { s with session_last = req.Req.req_time } in
      Lwt.return (Some s)

  let remove_session user_id ~token =
    get_session token >>= function
    | None -> Lwt.return ()
    | Some s ->
      if s.session_user_id = user_id then
        Hashtbl.remove session_by_token token;
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
  val register_foreign : origin:string -> token:string ->
    (S.user_id * S.user_info option, int * string option) result Lwt.t

  module SessionArg : EzSession.TYPES.SessionArg
    with type user_info = S.user_info
     and type user_id = S.user_id
  module SessionStore : SessionStore with type user_id = S.user_id

end = struct

  module SessionArg = S
  module SessionStore =
    (SessionStoreInMemory : SessionStore with type user_id = S.user_id)

  (** User information *)
  type user = {
    login : string;
    user_id : S.user_id;
    pwhash : string; (* hash of password *)
    user_info : S.user_info;
    kind : string option;
  }

  (** Hash map of users by their login *)
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

  let register_foreign = default_register_foreign

  let remove_user ~login =
    Hashtbl.remove users login

end
