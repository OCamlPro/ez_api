module type S = sig
  type 'a r = ('a, string) result

  type 'a action = {
    send : 'a -> unit r;
    close : int option -> unit r;
  }

  type 'a ws = {
    action : 'a action;
    conn : unit r Eio.Promise.t;
  }
end

module Types = struct
  type 'a r = ('a, string) result

  type 'a action = {
    send : 'a -> unit r;
    close : int option -> unit r;
  }

  type 'a ws = {
    action : 'a action;
    conn : unit r Eio.Promise.t;
  }
end
