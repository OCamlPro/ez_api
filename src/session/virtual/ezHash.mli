(** Hash function implementation depending, on imported library. Currently supported ones:
    md5, sha2, sha3 and blake2s. *)
val hash : string -> string
