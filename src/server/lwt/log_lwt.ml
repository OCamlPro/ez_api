module S : Log.S with type 'a t = 'a Lwt.t = struct
  type 'a t = 'a Lwt.t
  let printf = Lwt_fmt.eprintf
  let ifprintf (fmt: ('a, Format.formatter, unit, unit t) format4) = Lwt_fmt.ifprintf Lwt_fmt.stderr fmt
  let bind = Lwt.bind
  let return = Lwt.return
end

module M : Log.M with type 'a t = 'a Lwt.t = Log.M(S)
include M
