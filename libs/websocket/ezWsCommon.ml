module type S = sig
  type 'a rp = ('a, string) result Lwt.t

  type 'a action = {
    send : 'a -> unit rp;
    close : int option -> unit rp
  }

  type 'a ws = {
    action : 'a action;
    conn : unit rp
  }
end

module Types = struct
  type 'a rp = ('a, string) result Lwt.t

  type 'a action = {
    send : 'a -> unit rp;
    close : int option -> unit rp
  }

  type 'a ws = {
    action : 'a action;
    conn : unit rp
  }
end

let log ?(action="recv") url =
  Option.iter @@ fun msg -> EzDebug.printf "[>%s %s %s]" msg action url

let res_encoding err ok = Json_encoding.(union [
    case ok Result.to_option Result.ok;
    case err (function Error e -> Some e | _ -> None) Result.error
  ])
