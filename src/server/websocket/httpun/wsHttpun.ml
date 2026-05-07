open Httpun_ws

module type S = sig
  type 'a m
  val async : (unit -> unit m) -> unit
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m
  val sleep : float -> unit m
end

let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string

let close ?(code=`Normal_closure) wsd = Wsd.close ~code wsd

let send ~kind ~content wsd =
  let len, off = String.length content, 0 in
  let bs = Bigstringaf.of_string ~off ~len content in
  Wsd.schedule wsd bs ~kind ~off ~len

let read_payload ~payload f =
  let b = Buffer.create 100 in
  let on_eof () = f (Buffer.contents b) in
  let rec on_read bs ~off ~len =
    Buffer.add_string b (Bigstringaf.substring bs ~off ~len);
    Payload.schedule_read payload ~on_eof ~on_read in
  Payload.schedule_read payload ~on_eof ~on_read

let make_data content =
  let len, off = String.length content, 0 in
  let buffer = Bigstringaf.of_string ~off ~len content in
  { IOVec.buffer; off; len }

let ping_table : (string, float) Hashtbl.t = Hashtbl.create 1024

let check_ping ?(step=30.) id key =
  match Hashtbl.find_opt ping_table (id ^ key) with
  | None -> false
  | Some t ->
    let now = Unix.gettimeofday () in
    if t +. step < now then (
      Hashtbl.remove ping_table (id ^ key);
      false)
    else true

module Make(S: S) = struct

  let react_content react notify_close wsd content =
    S.async @@ fun () ->
    S.bind (react content) @@ fun r ->
    let () = match r with
      | Ok `none -> ()
      | Ok (`binary content) -> send ~kind:`Binary ~content wsd
      | Ok (`text content) -> send ~kind:`Text ~content wsd
      | Error (`handler_error content) -> send ~kind:`Text ~content wsd
      | Error _ ->
        close ~code:`Internal_server_error wsd;
        notify_close () in
    S.return ()

  let react react pong notify_close ~opcode ~payload wsd = match opcode with
    | `Ping ->
      read_payload ~payload @@ fun content ->
      let application_data = make_data content in
      Wsd.send_pong ~application_data wsd
    | `Connection_close -> Wsd.close wsd; notify_close ()
    | `Pong -> read_payload ~payload pong
    | `Text | `Binary -> read_payload ~payload (react_content react notify_close wsd)
    | _ -> close ~code:`Protocol_error wsd

  let ping_pong ?(step=30.) id wsd =
    let id_str = Uuidm.to_string id in
    let content = string_of_int @@ Random.int 1_000_000_000 in
    let len, off = String.length content, 0 in
    let buffer = Bigstringaf.of_string ~off ~len content in
    let application_data = { IOVec.buffer; off; len } in
    let rec loop () =
      Wsd.send_ping ~application_data wsd;
      S.bind (S.sleep (step /. 2.)) @@ fun () ->
      if check_ping ~step id_str content then
        S.bind (S.sleep (step /. 2.)) @@ fun () -> loop ()
      else (
        close ~code:`Going_away wsd;
        S.return ()) in
    let fill content =
      let now = Unix.gettimeofday () in
      Hashtbl.replace ping_table (id_str ^ content) now in
    loop, fill
end
