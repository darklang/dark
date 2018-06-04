open Core
open Types
open Types.RuntimeT

(* Low-level API *)
val run_sql : ?quiet:bool -> string -> unit
val fetch_via_sql : ?quiet:bool -> string -> string list list
val exists_via_sql : ?quiet:bool -> string -> bool

(* Saving canvases to the DB *)
val save_oplists : host:string -> digest:string -> string -> unit
val load_oplists : host:string -> digest:string -> string option
val load_json_oplists : host:string -> string option
val save_json_oplists : host:string -> digest:string -> string -> unit
val all_oplists : digest:string -> string list

(* Per user pg_schemas *)
val create_namespace : host -> unit
val run_sql_in_ns : ?quiet: bool -> host:host -> string -> unit
val fetch_via_sql_in_ns : ?quiet:bool -> host:host -> string -> string list list
val ns_name : host -> string

(* Misc *)
val delete_testdata : unit -> unit
