open EzAPI

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
