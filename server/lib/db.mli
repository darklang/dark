open Types

val with_postgres : (unit -> 'b) -> 'b

(* DB struct functions *)
val cols_for : DbT.db -> (string * RuntimeT.tipe) list

(* DB runtime functions *)
val insert : DbT.db -> RuntimeT.dval_map -> unit
val fetch_all : DbT.db -> RuntimeT.dval
val fetch_by : DbT.db -> string -> RuntimeT.dval -> RuntimeT.dval
val delete : DbT.db -> RuntimeT.dval_map -> unit
val update : DbT.db -> RuntimeT.dval_map -> unit

(* DB schema modifications *)
val create_new_db : tlid -> DbT.db -> unit
val add_db_col : id -> id -> DbT.db -> DbT.db
val set_col_name : id -> string -> DbT.db -> DbT.db
val set_db_col_type : id -> RuntimeT.tipe -> DbT.db -> DbT.db

(* DBs as values for execution *)
val dbs_as_env : DbT.db list -> RuntimeT.dval_map
val dbs_as_exe_env : DbT.db list -> RuntimeT.dval_map
