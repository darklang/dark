open Types
open Types.RuntimeT

val with_postgres : (unit -> 'b) -> 'b

(* DB runtime functions *)
val insert : DbT.db -> RuntimeT.dval_map -> unit
val fetch_all : DbT.db -> RuntimeT.dval
val fetch_by : DbT.db -> string -> RuntimeT.dval -> RuntimeT.dval
val delete : DbT.db -> RuntimeT.dval -> unit
val keys : DbT.db -> dval
val update : DbT.db -> RuntimeT.dval -> unit

(* DB schema modifications *)
val create_new_db : tlid -> string -> unit
val add_db_col : id -> id -> DbT.db -> DbT.db
val set_col_name : id -> string -> DbT.db -> DbT.db
val set_db_col_type : id -> tipe -> DbT.db -> DbT.db

(* DB schema modifications *)
val dbs_as_env : DbT.db list -> RuntimeT.dval_map
val dbs_as_exe_env : DbT.db list -> RuntimeT.dval_map
