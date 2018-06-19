open Core_kernel

val digest384 : string -> string
val digest256 : string -> string

val date_of_isostring : string -> Time.t
val date_to_isostring : Time.t -> string

val date_to_sqlstring : Time.t -> string
val date_of_sqlstring : string -> Time.t


