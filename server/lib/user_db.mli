open Core
open Types
open Types.RuntimeT

(* DB struct functions *)
val cols_for : DbT.db -> (string * tipe) list

(* DB runtime functions *)
val insert : tables:(DbT.db list) -> DbT.db -> dval_map -> Uuid.t
val fetch_all : tables:(DbT.db list) -> DbT.db -> dval
val fetch_by : tables:(DbT.db list) -> DbT.db -> string -> dval -> dval
val delete : tables:(DbT.db list) -> DbT.db -> dval_map -> unit
val delete_all : tables:(DbT.db list) -> DbT.db -> unit
val update : tables:(DbT.db list) -> DbT.db -> dval_map -> unit
val count : DbT.db -> int

(* DB schema modifications *)
val create : host -> string -> tlid -> DbT.db
val add_col : id -> id -> DbT.db -> DbT.db
val set_col_name : id -> string -> bool -> DbT.db -> DbT.db
val set_col_type : id -> tipe -> bool ->  DbT.db -> DbT.db
val change_col_name : id -> string -> bool -> DbT.db -> DbT.db
val change_col_type : id -> tipe -> bool -> DbT.db -> DbT.db
val initialize_migration : id -> id -> id -> DbT.migration_kind -> DbT.db -> DbT.db
val unlocked : DbT.db list -> DbT.db list
val db_locked : DbT.db -> bool

(* DBs as values for execution *)
val dbs_as_env : DbT.db list -> dval_map
val dbs_as_exe_env : DbT.db list -> dval_map


