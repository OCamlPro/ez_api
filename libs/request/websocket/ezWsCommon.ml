module type S = sig
  type 'a res = ('a, int * string option) result Lwt.t

  type ws_res = {
    send : string -> unit res;
    close : unit -> unit res;
    react : unit res;
  }
end

module Types = struct
  type 'a res = ('a, int * string option) result Lwt.t

  type ws_res = {
    send : string -> unit res;
    close : unit -> unit res;
    react : unit res;
  }
end

let catch f =
  Lwt.catch f (fun exn -> Lwt.return_error (0, Some (Printexc.to_string exn)))

let log ?(action="recv") url =
  Option.iter @@ fun msg -> EzDebug.printf "[>%s %s %s]" msg action url
