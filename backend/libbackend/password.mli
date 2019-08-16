type t

val to_bytes : t -> string

val from_plaintext : string -> t

val from_hash : string -> t

val invalid : t
