module S : Log.S with type 'a t = 'a = struct
  type 'a t = 'a
  let printf = Format.eprintf
  let ifprintf (fmt: ('a, Format.formatter, unit, unit t) format4) = Format.ifprintf Format.err_formatter fmt
  let bind p f = f p
  let return = Fun.id
end

module M : Log.M with type 'a t = 'a = Log.M(S)
include M
