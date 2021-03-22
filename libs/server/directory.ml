(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open EzAPI
open Lwt.Infix

module StringMap = Map.Make(String)
module MethMap = Map.Make(struct type t = Meth.t let compare = compare end)

module Step = struct
  type t =
    | Static of string
    | Dynamic of Arg.descr
end

type conflict =
  | CService
  | CDir
  | CBuilder
  | CCustom
  | CTypes of Arg.descr * Arg.descr
  | CType of Arg.descr * string list

and 'a directory = {
  services: 'a registered_service MethMap.t ;
  subdirs: 'a subdirectories option
}

and _ subdirectories =
  | Suffixes: 'a directory StringMap.t -> 'a subdirectories
  | Arg: 'a1 Arg.t * ('a * 'a1) directory -> 'a subdirectories

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

let empty = { services = MethMap.empty ; subdirs = None }

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

let rec resolve :
  type a. string list -> a directory -> a -> string list ->
  (resolved_directory, lookup_error) result Lwt.t =
  fun prefix dir args path ->
  match path, dir with
  | [], dir -> Lwt.return_ok (Dir (dir, args))
  | _name :: _path, { subdirs = None; _ } -> Lwt.return_error `Not_found
  | name :: path, { subdirs = Some (Suffixes static); _ } ->
    begin match StringMap.find_opt name static with
      | None -> Lwt.return_error `Not_found
      | Some dir -> resolve (name :: prefix) dir args path
    end
  | name :: path, { subdirs = Some (Arg (arg, dir)); _ } ->
    match arg.Arg.destruct name with
    | Ok x -> resolve (name :: prefix) dir (args, x) path
    | Error msg -> Lwt.return_error @@
      `Cannot_parse (arg.Arg.description, msg, name :: prefix)

let io_to_answer : type a. code:int -> a io -> a -> string Answer.t = fun ~code io body ->
  match io with
  | Empty -> {Answer.code; body=""; headers=[]}
  | Raw l ->
    let content_type = match l with
      | [] -> "application/octet-stream"
      | h :: _ -> Mime.to_string h in
    {Answer.code; body; headers=["content-type", content_type]}
  | Json enc ->
    {Answer.code; body = EzEncoding.construct enc body;
     headers=["content-type", "application/json"]}

let ser_handler :
  type i o e. ?content_type:string -> ('a -> i -> (o, e) result Answer.t Lwt.t) -> 'a ->
  i io -> o io -> e Json_encoding.encoding ->
  string -> (string Answer.t, handler_error) result Lwt.t =
  fun ?content_type handler args input output errors ->
  let handle_result {Answer.code; body; _} = match body with
    | Ok o -> io_to_answer ~code output o
    | Error e ->
      {Answer.code; body = EzEncoding.construct errors e;
       headers=["content-type", "application/json"]} in
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
          let meths = Meth.headers @@ List.map fst l in
          let sec_set = List.fold_left (fun acc (_, rs) -> match rs with
              | Http {service; _} -> Security.StringSet.union acc (Security.headers (Service.security service))
              | Websocket _ -> acc) Security.StringSet.empty l in
          Lwt.return_ok @@ `options (meths @ (Security.header sec_set))
      end
    | Some `HEAD -> Lwt.return_ok `head
    | Some (#Meth.t as m) ->
      match m, MethMap.find_opt m dir.services with
      | _, Some (Http {service; handler}) ->
        let input = Service.input service in
        let output = Service.output service in
        let errors = Service.errors_encoding service in
        let h = ser_handler ?content_type handler args input output errors in
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
          let r = match subdir with
            | { subdirs = None; services } -> Ok (StringMap.empty, services)
            | { subdirs = Some (Suffixes m); services } -> Ok (m, services)
            | { subdirs = Some (Arg (arg, _)); _ } ->
              conflict path (CType (arg.Arg.description, [name])) in
          match r with
          | Error c -> Error c
          | Ok (dirmap, services) ->
            let dir = match StringMap.find_opt name dirmap with
              | None -> empty
              | Some dir -> dir in
            let rebuild s =
              let subdirs = Some (Suffixes (StringMap.add name s dirmap)) in
              rebuild ({ subdirs ; services }) in
            Ok (dir, rebuild)
      end
    | Path.Dynamic (subpath, arg) -> begin
        match insert subpath dir with
        | Error c -> Error c
        | Ok (subdir, rebuild) ->
          let r = match subdir with
            | { subdirs = None ; services } -> Ok (empty, services)
            | { subdirs = Some (Arg (arg', dir)); services } -> begin
                try
                  let Arg.Ty.Eq = Arg.Ty.eq arg.Arg.id arg'.Arg.id in
                  Ok ((dir :> a directory), services)
                with Arg.Ty.Not_equal ->
                  conflict path (CTypes (arg.Arg.description, arg'.Arg.description))
              end
            | { subdirs = Some (Suffixes m); _ } ->
              conflict path
                (CType (arg.Arg.description, List.map fst (StringMap.bindings m))) in
          match r with
          | Error c -> Error c
          | Ok (dir, services) ->
            let rebuild s =
              let subdirs = Some (Arg (arg, s)) in
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
  | Error c-> Error c
  | Ok (dir, insert) ->
    let rs = f service in
    let meth = Service.meth service in
    match dir with
    | { services ; subdirs = _ } as dir when not (MethMap.mem meth services) ->
      Ok (insert { dir with services = MethMap.add meth rs services })
    | _ -> conflict path CService

let register_http root service handler =
  register root service (fun service -> Http {service; handler})

let register_ws root ?onclose ?step ~react ~bg service =
  register root service (fun service -> Websocket {service; react; bg; onclose; step})
