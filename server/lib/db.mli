open Core
open Types
open Types.RuntimeT

(* Low-level API *)
val run_sql : ?quiet:bool -> string -> unit
val fetch_via_sql : ?quiet:bool -> string -> string list list
val exists_via_sql : ?quiet:bool -> string -> bool

(* Saving canvases to the DB *)
val save_oplists : string -> string -> string -> unit
val load_oplists : string -> string -> string option
val all_oplists : string -> string list

(* Per user pg_schemas *)
val create_namespace : host -> unit
val run_sql_in_ns : ?quiet: bool -> host:host -> string -> unit
val fetch_via_sql_in_ns : ?quiet:bool -> host:host -> string -> string list list
val ns_name : host -> string

(* TODO: Remove these soon *)
val escape : string -> string
val escapea : string -> string

(* Misc *)
val delete_testdata : unit -> unit
val literal_of_uuid : Uuid.t -> string
