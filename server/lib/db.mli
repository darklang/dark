open Types

val with_postgres : DbT.db -> (DbT.db -> 'a) -> 'a

(* DB runtime functions *)
val insert : DbT.db -> RuntimeT.dval_map -> unit
val fetch_all : DbT.db -> RuntimeT.dval

(* DB schema modifications *)
val create_new_db : tlid -> string -> unit
val add_db_row : id -> id -> DbT.db -> DbT.db
val set_row_name : id -> string -> DbT.db -> DbT.db
val set_db_row_type : id -> string -> DbT.db -> DbT.db

(* DB schema modifications *)
val dbs_as_env : DbT.db list -> RuntimeT.dval_map
val dbs_as_exe_env : DbT.db list -> RuntimeT.dval_map
