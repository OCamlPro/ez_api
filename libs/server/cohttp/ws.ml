open Lwt.Infix
open Websocket.Frame
open EzAPIServerUtils.Directory

let ws_react f pong rsend fr =
  match fr.opcode with
  | Opcode.Ping ->
    !rsend @@ Some (create ~opcode:Opcode.Pong ~content:fr.content ())
  | Opcode.Close ->
    if String.length fr.content >= 2 then
      let content = String.sub fr.content 0 2 in
      !rsend @@ Some (create ~opcode:Opcode.Close ~content ())
    else
      !rsend @@ Some (close 1000)
  | Opcode.Pong -> pong fr.content
  | Opcode.Text | Opcode.Binary ->
    Lwt.async (fun () ->
        f fr.content >|= function
        | Ok `none -> !rsend None
        | Ok (`binary content) ->
          !rsend @@ Some (create ~opcode:Opcode.Binary ~content ())
        | Ok (`text content) ->
          !rsend @@ Some (create ~opcode:Opcode.Text ~content ())
        | Error (`handler_error content) ->
          !rsend @@ Some (create ~opcode:Opcode.Text ~content ())
        | Error _ ->
          !rsend @@ Some (close 1011))
  | _ ->
    !rsend @@ Some (close 1002)

let ws_loop bg send =
  let send : (ws_frame, handler_error) result -> unit = function
    | Error _ -> send (Some (close 1000))
    | Ok `none -> send None
    | Ok (`binary content) ->
      send @@ Some (create ~opcode:Opcode.Binary ~content ())
    | Ok (`text content) ->
      send @@ Some (create ~opcode:Opcode.Text ~content ()) in
  bg send

let ping_table : (string, CalendarLib.Fcalendar.Precise.t) Hashtbl.t = Hashtbl.create 1024

let check_ping ?(step=30.) id key =
  let open CalendarLib.Fcalendar.Precise in
  match Hashtbl.find_opt ping_table (id ^ key) with
  | None -> false
  | Some t ->
    let step = Period.second (Time.Second.from_float step) in
    let now = now () in
    if compare (add t step) now < 0 then (
      Hashtbl.remove ping_table (id ^ key);
      false)
    else true

let ping_pong ?(step=30.) rsend =
  let id = string_of_int @@ Random.int 1_000_000_000 in
  let content = string_of_int @@ Random.int 1_000_000_000 in
  let rec loop () =
    !rsend (Some (create ~opcode:Opcode.Ping ~content ()));
    EzLwtSys.sleep step >>= fun () ->
    if check_ping ~step id content then loop ()
    else Lwt.return_unit in
  let fill content =
    let now = CalendarLib.Fcalendar.Precise.now () in
    Hashtbl.replace ping_table (id ^ content) now in
  loop, fill

let ws ?step req ~react ~bg =
  let rsend = ref (fun _ -> ()) in
  let ping_loop, pong_fill = ping_pong ?step rsend in
  Websocket_cohttp_lwt.upgrade_connection
    req (ws_react react pong_fill rsend) >>= fun (r, send) ->
  rsend := send;
  Lwt.async (fun () -> Lwt.pick [ws_loop bg send; ping_loop ()]);
  Lwt.return r
