open Core
open Libexecution
open Types
open Types.RuntimeT

(* DB struct functions *)
val cols_for : DbT.db -> (string * tipe) list

(* DB runtime functions *)
val set : state:exec_state -> upsert:bool -> DbT.db -> string -> dval_map -> Uuidm.t
val fetch_all : state:exec_state -> DbT.db -> dval
val fetch_by_key : state:exec_state -> DbT.db -> string -> dval
val fetch_many_by_key : state:exec_state -> DbT.db -> string list -> dval
val fetch_by : state:exec_state -> DbT.db -> string -> dval -> dval
val fetch_by_many : state:exec_state -> DbT.db -> (string * dval) list -> dval
val delete : state:exec_state -> DbT.db -> string -> unit
val delete_all : state:exec_state -> DbT.db -> unit
val update : state:exec_state -> DbT.db -> dval_map -> unit
val count : DbT.db -> int

(* DB schema modifications *)
val create : host -> string -> tlid -> DbT.db
val add_col : id -> id -> DbT.db -> DbT.db
val set_col_name : id -> string -> DbT.db -> DbT.db
val set_col_type : id -> tipe -> DbT.db -> DbT.db
val change_col_name : id -> string -> DbT.db -> DbT.db
val change_col_type : id -> tipe -> DbT.db -> DbT.db
val initialize_migration : id -> id -> id -> DbT.migration_kind -> DbT.db -> DbT.db
val unlocked : Uuidm.t -> Uuidm.t -> DbT.db list -> DbT.db list
val db_locked : DbT.db -> bool

val find_db : DbT.db list -> string -> DbT.db option
val find_db_exn : DbT.db list -> string -> DbT.db

(* DBs as values for execution *)
val dbs_as_env : DbT.db list -> dval_map
val dbs_as_exe_env : DbT.db list -> dval_map


