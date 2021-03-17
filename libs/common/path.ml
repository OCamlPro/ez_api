type (_, _) t =
  | Root : ('r, 'r) t
  | Static : ('r, 'key) t * string -> ('r, 'key) t
  | Dynamic : ('r, 'key) t * 'a Arg.t -> ('r, 'key * 'a) t

let root = Root

let add_suffix path name = Static (path, name)

let add_arg path arg = Dynamic (path, arg)

let (//) = add_suffix
let (/:) = add_arg

let to_list ?(root=[]) ?(wrap=(fun s -> "{" ^ s ^ "}")) path =
  let rec aux : type r a. (r, a) t -> string list = function
    | Root -> root
    | Static (path, name) -> name :: aux path
    | Dynamic (path, arg) -> wrap arg.Arg.description.Arg.name :: aux path in
  List.rev @@ aux path

let args path =
  let rec aux : type r a. (r, a) t -> Arg.descr list = function
    | Root -> []
    | Static (path, _) -> aux path
    | Dynamic (path, arg) -> (Arg.descr arg) :: aux path in
  List.rev @@ aux path

let to_string ?(root="") ?wrap path =
  let root = String.split_on_char '/' root in
  String.concat "/" @@ to_list ~root ?wrap path

let forge path args =
  let rec aux : type r k. (r, k) t -> k -> string list -> string list =
    fun path args acc -> match path, args with
      | Root, _ -> acc
      | Static (path, name), args -> aux path args (name :: acc)
      | Dynamic (path, arg), (args, x) -> aux path args (arg.Arg.construct x :: acc) in
  aux path args []

let rec get_root : type r a. (r, a) t -> a -> r =
  fun p a ->
  match p, a with
  | Root, _ -> a
  | Static (p, _), _ -> get_root p a
  | Dynamic (p, _), (a, _) -> get_root p a
