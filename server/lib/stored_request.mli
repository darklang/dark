open Core

type t = string * Cohttp_lwt_unix.Request.t [@@deriving sexp]

val store : string -> string -> int -> Cohttp_lwt_unix.Request.t -> unit
val load_all : string -> int -> t list
