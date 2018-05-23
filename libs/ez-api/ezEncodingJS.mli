
val init : unit -> unit

(* Prefer these functions to the ones of Ezjsonm *)
val string_of_json : ?minify:bool -> Ezjsonm.t -> string
val json_of_string : string -> [> Ezjsonm.t ]
