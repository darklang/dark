type t

val of_int : int -> t

val to_int : t -> int option

val to_int_exn : t -> int

val of_float : float -> t

val to_float : t -> float

val of_string_exn : string -> t

val to_string : t -> string

val random : t -> t

val init : unit -> unit

val ( % ) : t -> t -> t

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( / ) : t -> t -> t

val ( * ) : t -> t -> t

val pow : t -> t -> t

val one : t

val zero : t

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result
