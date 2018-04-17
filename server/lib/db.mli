open Core
open Types

val conn : Postgresql.connection
val init : unit -> unit
val with_postgres : (unit -> 'b) -> 'b

(* Low-level API *)
val run_sql : ?quiet:bool -> string -> unit
val fetch_via_sql : ?quiet:bool -> string -> string list list

(* DB struct functions *)
val cols_for : DbT.db -> (string * RuntimeT.tipe) list

(* DB runtime functions *)
val insert : tables:(DbT.db list) -> DbT.db -> RuntimeT.dval_map -> Uuid.t
val fetch_all : tables:(DbT.db list) -> DbT.db -> RuntimeT.dval
val fetch_by : tables:(DbT.db list) -> DbT.db -> string -> RuntimeT.dval -> RuntimeT.dval
val delete : tables:(DbT.db list) -> DbT.db -> RuntimeT.dval_map -> unit
val update : tables:(DbT.db list) -> DbT.db -> RuntimeT.dval_map -> unit
val count : DbT.db -> int

(* DB schema modifications *)
val create_new_db : DbT.db -> unit
val to_display_name : string -> string
val add_db_col : id -> id -> DbT.db -> DbT.db
val set_col_name : id -> string -> bool -> DbT.db -> DbT.db
val change_col_name : id -> string -> bool -> DbT.db -> DbT.db
val set_db_col_type : id -> RuntimeT.tipe -> bool ->  DbT.db -> DbT.db
val unlocked : DbT.db list -> DbT.db list

(* DBs as values for execution *)
val dbs_as_env : DbT.db list -> RuntimeT.dval_map
val dbs_as_exe_env : DbT.db list -> RuntimeT.dval_map


(* Saving canvases to the DB *)
val save_oplists : string -> string -> string -> unit
val load_oplists : string -> string -> string option
