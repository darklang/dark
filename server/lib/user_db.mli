open Core
open Types
open Types.RuntimeT

(* DB struct functions *)
val cols_for : DbT.db -> (string * tipe) list

(* DB runtime functions *)
val insert : exec_state -> DbT.db -> dval_map -> Uuid.t
val fetch_all : exec_state -> DbT.db -> dval
val fetch_by : exec_state -> DbT.db -> string -> dval -> dval
val find : exec_state -> DbT.db -> Uuid.t -> dval
val find_many : exec_state -> DbT.db -> Uuid.t list -> dval
val delete : exec_state -> DbT.db -> dval_map -> unit
val delete_all : exec_state -> DbT.db -> unit
val update : exec_state -> DbT.db -> dval_map -> unit
val count : DbT.db -> int

(* DB schema modifications *)
val create : host -> string -> tlid -> DbT.db
val add_col : id -> id -> DbT.db -> DbT.db
val set_col_name : id -> string -> DbT.db -> DbT.db
val set_col_type : id -> tipe -> DbT.db -> DbT.db
val change_col_name : id -> string -> DbT.db -> DbT.db
val change_col_type : id -> tipe -> DbT.db -> DbT.db
val initialize_migration : id -> id -> id -> DbT.migration_kind -> DbT.db -> DbT.db
val unlocked : DbT.db list -> DbT.db list
val db_locked : DbT.db -> bool

(* DBs as values for execution *)
val dbs_as_env : DbT.db list -> dval_map
val dbs_as_exe_env : DbT.db list -> dval_map


