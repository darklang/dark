open Core

open Types.RuntimeT

type t

val from_request : Cohttp_lwt_unix.Request.t -> string -> Uri.t -> t
val to_dval : t -> dval
val sample : t

