open Core
open Types
open Types.RuntimeT

val conn : Postgresql.connection

(* Low-level API *)
val run_sql : ?quiet:bool -> string -> unit
val fetch_via_sql : ?quiet:bool -> string -> string list list
val exists_via_sql : ?quiet:bool -> string -> bool
val escape : string -> string

(* Saving canvases to the DB *)
val save_oplists : string -> string -> string -> unit
val load_oplists : string -> string -> string option
val all_oplists : string -> string list

(* Per user pg_schemas *)
val create_namespace : host -> unit
val run_sql_in_ns : ?quiet: bool -> host:host -> string -> unit
val fetch_via_sql_in_ns : ?quiet:bool -> host:host -> string -> string list list
val ns_name : host -> string

(* Misc *)
val delete_testdata : unit -> unit
