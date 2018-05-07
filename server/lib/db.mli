open Core
open Types
open Types.RuntimeT

val conn : Postgresql.connection

(* Low-level API *)
val run_sql : ?quiet:bool -> string -> unit
val fetch_via_sql : ?quiet:bool -> string -> string list list

(* DB struct functions *)
val cols_for : DbT.db -> (string * tipe) list

(* DB runtime functions *)
val initialize_migrations : host -> unit
val insert : tables:(DbT.db list) -> DbT.db -> dval_map -> Uuid.t
val fetch_all : tables:(DbT.db list) -> DbT.db -> dval
val fetch_by : tables:(DbT.db list) -> DbT.db -> string -> dval -> dval
val delete : tables:(DbT.db list) -> DbT.db -> dval_map -> unit
val delete_all : tables:(DbT.db list) -> DbT.db -> unit
val update : tables:(DbT.db list) -> DbT.db -> dval_map -> unit
val count : DbT.db -> int
val drop : DbT.db -> unit

(* DB schema modifications *)
val create : host -> string -> tlid -> DbT.db
val init_storage : DbT.db -> unit
val to_display_name : string -> string
val add_col : id -> id -> DbT.db -> DbT.db
val set_col_name : id -> string -> bool -> DbT.db -> DbT.db
val set_col_type : id -> tipe -> bool ->  DbT.db -> DbT.db
val change_col_name : id -> string -> bool -> DbT.db -> DbT.db
val change_col_type : id -> tipe -> bool -> DbT.db -> DbT.db
val initialize_migration : id -> DbT.migration_kind -> DbT.db -> DbT.db
val unlocked : DbT.db list -> DbT.db list
val db_locked : DbT.db -> bool

(* DBs as values for execution *)
val dbs_as_env : DbT.db list -> dval_map
val dbs_as_exe_env : DbT.db list -> dval_map


(* Saving canvases to the DB *)
val save_oplists : string -> string -> string -> unit
val load_oplists : string -> string -> string option
val all_oplists : string -> string list


(* Per user pg_schemas *)
val create_namespace : host -> unit
val run_sql_in_ns : ?quiet: bool -> host:host -> string -> unit
val fetch_via_sql_in_ns : ?quiet:bool -> host:host -> string -> string list list
val ns_name : host -> string
