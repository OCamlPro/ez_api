open EzAPI
open Lwt.Infix

module StringMap = Map.Make(String)
module MethMap = Map.Make(struct type t = Meth.t let compare = compare end)

module Step = struct
  type t =
    | Static of string
    | Dynamic of Arg.descr

  let to_string = function
    | Static s -> s
    | Dynamic a -> Format.sprintf "{%s}" a.Arg.name

  let list_to_string l =
    Format.sprintf "/%s" @@ String.concat "/" @@ List.map to_string l

end

type conflict =
  | CService of Meth.t
  | CTypes of Arg.descr * Arg.descr

and 'a directory = {
  services: 'a registered_service MethMap.t ;
  subdirs: ('a static_subdirectories option * 'a variable_subdirectories option)
}

and 'a static_subdirectories = 'a directory StringMap.t

and _ variable_subdirectories =
  | Arg: 'a1 Arg.t * ('a * 'a1) directory -> 'a variable_subdirectories

and _ registered_service =
  | Http : {
      service : ('a, 'i, 'o, 'e, 's) Service.t;
      handler : ('a -> 'i -> ('o, 'e) result Answer.t Lwt.t);
    } -> 'a registered_service
  | Websocket : {
      service : ('a, 'i, 'o, 'e, 's) Service.t;
      react : ('a -> 'i -> ('o, 'e) result Lwt.t);
      bg : ('a -> (('o, 'e) result -> unit) -> unit Lwt.t);
      onclose : ('a -> unit Lwt.t) option;
      step : float option;
    } -> 'a registered_service

let empty = { services = MethMap.empty ; subdirs = (None, None) }

type t = Req.t directory

type resolved_directory =
    Dir: 'a directory * 'a -> resolved_directory

type lookup_error = [
  | `Not_found
  | `Cannot_parse of Arg.descr * string * string list
  | `Method_not_allowed ]

type handler_error = [
  | EzEncoding.destruct_error
  | `unsupported of string option
  | `handler_exn of exn
  | `handler_error of string ]

type ws_frame = [ `binary of string | `text of string | `none ]

type lookup_ok = [
  | `head | `options of (string * string) list
  | `http of (string -> (string Answer.t, handler_error) result Lwt.t)
  | `ws of ((string -> (ws_frame, handler_error) result Lwt.t) *
            (((ws_frame, handler_error) result -> unit) -> unit Lwt.t) *
            (unit -> unit Lwt.t) option * float option) ]

let conflict_to_string = function
  | CService m -> Format.sprintf "Duplicated service (%s)" (Meth.to_string m)
  | CTypes (arg1, arg2) ->
    Format.sprintf "Conflicing dynamic arguments: %s <> %s" arg1.Arg.name arg2.Arg.name

let rec resolve :
  type a. string list -> a directory -> a -> string list ->
  (resolved_directory, lookup_error) result Lwt.t =
  fun prefix dir args path ->
  match path, dir with
  | [], dir -> 
    Lwt.return_ok (Dir (dir, args))
  | _name :: _path, { subdirs = None, None; _ } -> Lwt.return_error `Not_found
  | name :: path, { subdirs = Some static, None; _ } ->
    begin match StringMap.find_opt name static with
      | None -> Lwt.return_error `Not_found
      | Some dir -> resolve (name :: prefix) dir args path
    end
  | name :: path, { subdirs = None, Some (Arg (arg, dir)); _ } ->
    begin match arg.Arg.destruct name with
      | Ok x -> resolve (name :: prefix) dir (args, x) path
      | Error msg -> Lwt.return_error @@
        `Cannot_parse (arg.Arg.description, msg, name :: prefix)
    end
  | name :: path, { subdirs = Some static, Some (Arg (arg, dir)); _ } ->
    match StringMap.find_opt name static with
    | Some dir -> resolve (name :: prefix) dir args path
    | None ->
      match arg.Arg.destruct name with
      | Ok x -> resolve (name :: prefix) dir (args, x) path
      | Error msg -> Lwt.return_error @@
        `Cannot_parse (arg.Arg.description, msg, name :: prefix)

(* Note : headers are merged with predefined headers *)
let io_to_answer : type a. code:int -> headers:(string * string) list -> a io -> a -> string Answer.t = 
fun ~code ~headers io body ->
  match io with
  | Empty -> {Answer.code; body=""; headers}
  | Raw l ->
    let content_type = match l with
      | [] -> "application/octet-stream"
      | h :: _ -> Mime.to_string h in
    {Answer.code; body; headers=("content-type", content_type)::headers}
  | Json enc ->
    {Answer.code; body = EzEncoding.construct enc body;
     headers=("content-type", "application/json")::headers}

let ser_handler :
  type i o e. ?content_type:string -> access_control:(string * string) list 
  -> ('a -> i -> (o, e) result Answer.t Lwt.t) -> 'a ->
  i io -> o io -> e Json_encoding.encoding ->
  string -> (string Answer.t, handler_error) result Lwt.t =
  fun ?content_type ~access_control handler args input output errors ->
  let handle_result {Answer.code; body; headers} = 
    match body with
    | Ok o -> io_to_answer ~code ~headers:(headers @ access_control) output o
    | Error e ->
      {Answer.code; body = EzEncoding.construct errors e;
       headers=("content-type", "application/json")::access_control } 
  in
  match input with
  | Empty -> (fun _ ->
      Lwt.catch
        (fun () -> handler args () >|= fun r -> Ok (handle_result r))
        (fun exn -> Lwt.return_error (`handler_exn exn)))
  | Raw mimes -> (fun s ->
      if not (Mime.allowed mimes content_type) then
        Lwt.return_error (`unsupported content_type)
      else
        Lwt.catch
          (fun () -> handler args s >|= fun r -> Ok (handle_result r))
          (fun exn -> Lwt.return_error (`handler_exn exn)))
  | Json enc -> (fun (s : string) ->
      match EzEncoding.destruct_res enc s with
      | Ok i ->
        Lwt.catch
          (fun () -> handler args i >|= fun r -> Ok (handle_result r))
          (fun exn -> Lwt.return_error (`handler_exn exn))
      | Error e -> Lwt.return_error e)

let io_to_ws_frame : type a. a io -> a -> ws_frame = fun io a ->
  match io with
  | Empty -> `none
  | Raw _ -> `binary a
  | Json enc -> `text (EzEncoding.construct enc a)

let ser_websocket react bg args input output errors =
  let handle_result r = match r with
    | Ok x -> Ok (io_to_ws_frame output x)
    | Error e -> Error (`handler_error (EzEncoding.construct errors e)) in
  let bg send = bg args (fun r -> send @@ handle_result r) in
  let react s =
    Lwt.catch
      (fun () ->
         match IO.from_string input (fun i -> react args i >|= handle_result) s with
         | Ok p -> p
         | Error e -> Lwt.return_error e)
      (fun exn -> Lwt.return_error (`handler_exn exn)) in
  react, bg

let lookup ?meth ?content_type dir r path : (lookup_ok, lookup_error) result Lwt.t =
  resolve [] dir r path >>= function
  | Error _ as err -> Lwt.return err
  | Ok (Dir (dir, args)) ->
    match meth with
    | None | Some `OPTIONS ->
      begin match MethMap.bindings dir.services with
        | [] -> Lwt.return_error `Not_found
        | l ->
          (* Todo : combine access control headers correctly. *)
          let access_control = List.fold_left (fun acc (_,rs) -> match rs with
              | Http {service; _} when acc = [] -> Service.access_control service
              | _ -> acc) [] l in 
          let meths = Meth.headers @@ List.map fst l in
          let sec_set = List.fold_left (fun acc (_, rs) -> match rs with
              | Http {service; _} -> Security.StringSet.union acc (Security.headers (Service.security service))
              | Websocket _ -> acc) Security.StringSet.empty l in
          Lwt.return_ok @@ `options (access_control @ meths @ (Security.header sec_set))
      end
    | Some `HEAD -> Lwt.return_ok `head
    | Some (#Meth.t as m) ->
      match m, MethMap.find_opt m dir.services with
      | _, Some (Http {service; handler}) ->
        let input = Service.input service in
        let output = Service.output service in
        let errors = Service.errors_encoding service in
        let access_control = Service.access_control service in
        let h = ser_handler ?content_type ~access_control handler args input output errors in
        Lwt.return_ok @@ `http h
      | `GET, Some (Websocket {service; react; bg; onclose; step}) ->
        let input = Service.input service in
        let output = Service.output service in
        let errors = Service.errors_encoding service in
        let react, bg = ser_websocket react bg args input output errors in
        let onclose = match onclose with None -> None | Some f -> Some (fun () -> f args) in
        Lwt.return_ok @@ `ws (react, bg, onclose, step)
      | _ -> Lwt.return_error `Method_not_allowed

let step_of_path path =
  let rec aux : type r p. (r, p) Path.t -> Step.t list -> Step.t list = fun path acc ->
    match path with
    | Path.Root -> acc
    | Path.Static (path, name) -> aux path (Step.Static name :: acc)
    | Path.Dynamic (path, arg) -> aux path (Step.Dynamic arg.Arg.description :: acc) in
  aux path []

let conflict path kind = Error (step_of_path path, kind)

let rec insert
  : type r a.
    (r, a) Path.t -> r directory ->
    (a directory * (a directory -> r directory), Step.t list * conflict) result
  = fun path dir ->
    match path with
    | Path.Root -> Ok (dir, (fun x -> x))
    | Path.Static (subpath, name) -> begin
        match insert subpath dir with
        | Error c -> Error c
        | Ok (subdir, rebuild) ->
          let dirmap, dir, services = match subdir with
            | { subdirs = None, _; services } -> StringMap.empty, empty, services
            | { subdirs = Some m, _; services } ->
              let dir = match StringMap.find_opt name m with
                | None -> empty
                | Some dir -> dir in
              m, dir, services in
          let rebuild s =
            let subdirs = Some (StringMap.add name s dirmap), snd subdir.subdirs in
            rebuild { subdirs; services } in
          Ok (dir, rebuild)
      end
    | Path.Dynamic (subpath, arg) -> begin
        match insert subpath dir with
        | Error c -> Error c
        | Ok (subdir, rebuild) ->
          let r = match subdir with
            | { subdirs = static, None ; services } -> Ok (static, empty, services)
            | { subdirs = static, Some (Arg (arg', dir)); services } ->
              try
                let Arg.Ty.Eq = Arg.Ty.eq arg.Arg.id arg'.Arg.id in
                Ok (static, (dir :> a directory), services)
              with Arg.Ty.Not_equal ->
                conflict path (CTypes (arg.Arg.description, arg'.Arg.description)) in
          match r with
          | Error c -> Error c
          | Ok (static, dir, services) ->
            let rebuild s =
              let subdirs = static, Some (Arg (arg, s)) in
              rebuild { subdirs ; services } in
            Ok (dir, rebuild)
      end

let register :
  type a.
  t -> (a, _, _, _, _) Service.t ->
  ((a, _, _, _, _) Service.t -> a registered_service) ->
  (t, Step.t list * conflict) result =
  fun root service f ->
  let path = Service.path service in
  match insert path root with
  | Error c -> Error c
  | Ok (dir, insert) ->
    let rs = f service in
    let meth = Service.meth service in
    match dir with
    | { services ; subdirs = _ } as dir when not (MethMap.mem meth services) ->
      Ok (insert { dir with services = MethMap.add meth rs services })
    | _ -> conflict path @@ CService meth

let register_http root service handler =
  register root service (fun service -> Http {service; handler})

let register_ws root ?onclose ?step ~react ~bg service =
  register root service (fun service -> Websocket {service; react; bg; onclose; step})
