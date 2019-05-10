open Core
open Libexecution
open Types
open Types.RuntimeT

(* DB struct functions *)
val cols_for : DbT.db -> (string * tipe) list

(* DB runtime functions *)
val set :
  state:exec_state -> upsert:bool -> DbT.db -> string -> dval_map -> Uuidm.t

val get : state:exec_state -> DbT.db -> string -> dval

val get_many : state:exec_state -> DbT.db -> string list -> dval

val get_all : state:exec_state -> DbT.db -> dval

val query : state:exec_state -> DbT.db -> dval -> dval

val query_by_one : state:exec_state -> DbT.db -> string -> dval -> dval

val count : state:exec_state -> DbT.db -> int

val delete : state:exec_state -> DbT.db -> string -> unit

val delete_all : state:exec_state -> DbT.db -> unit

(* Stats fns *)

val stats_count : account_id:Uuidm.t -> canvas_id:Uuidm.t -> DbT.db -> int

val stats_pluck : account_id:Uuidm.t -> canvas_id:Uuidm.t -> DbT.db -> dval

(* Deprecated: only used in legacy in deprecated libdb *)
val update : state:exec_state -> DbT.db -> dval_map -> unit

val coerce_key_value_pair_to_legacy_object : dval list -> dval

val coerce_dlist_of_kv_pairs_to_legacy_object : dval -> dval

(* DB schema modifications *)
val create : string -> tlid -> DbT.db

val create2 : string -> tlid -> id -> DbT.db

val add_col : id -> id -> DbT.db -> DbT.db

val set_col_name : id -> string -> DbT.db -> DbT.db

val set_col_type : id -> tipe -> DbT.db -> DbT.db

val change_col_name : id -> string -> DbT.db -> DbT.db

val change_col_type : id -> tipe -> DbT.db -> DbT.db

val delete_col : id -> DbT.db -> DbT.db

val unlocked : Uuidm.t -> Uuidm.t -> DbT.db list -> DbT.db list

val find_db : DbT.db list -> string -> DbT.db option

val find_db_exn : DbT.db list -> string -> DbT.db

val create_migration : id -> id -> DbT.col list -> DbT.db -> DbT.db

val add_col_to_migration : id -> id -> DbT.db -> DbT.db

val set_col_name_in_migration : id -> string -> DbT.db -> DbT.db

val set_col_type_in_migration : id -> tipe -> DbT.db -> DbT.db

val abandon_migration : DbT.db -> DbT.db

val delete_col_in_migration : id -> DbT.db -> DbT.db

val rename_db : string -> DbT.db -> DbT.db
