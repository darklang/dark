open Core
open Libexecution
open Types
open Types.RuntimeT

(* DB struct functions *)
val cols_for : 'expr_type DbT.db -> (string * tipe) list

(* DB runtime functions *)
val set :
     state:exec_state
  -> upsert:bool
  -> 'expr_type DbT.db
  -> string
  -> dval_map
  -> Uuidm.t

val get_option : state:exec_state -> 'expr_type DbT.db -> string -> dval option

val get_many :
  state:exec_state -> 'expr_type DbT.db -> string list -> (string * dval) list

val get_many_with_keys :
  state:exec_state -> 'expr_type DbT.db -> string list -> (string * dval) list

val get_all : state:exec_state -> 'expr_type DbT.db -> (string * dval) list

val get_all_keys : state:exec_state -> 'expr_type DbT.db -> string list

val query_exact_fields :
  state:exec_state -> 'expr_type DbT.db -> dval -> (string * dval) list

val query :
  state:exec_state -> 'expr_type DbT.db -> dblock_args -> (string * dval) list

val count : state:exec_state -> 'expr_type DbT.db -> int

val delete : state:exec_state -> 'expr_type DbT.db -> string -> unit

val delete_all : state:exec_state -> 'expr_type DbT.db -> unit

(* Stats fns *)

val stats_count :
  account_id:Uuidm.t -> canvas_id:Uuidm.t -> 'expr_type DbT.db -> int

val stats_pluck :
     account_id:Uuidm.t
  -> canvas_id:Uuidm.t
  -> 'expr_type DbT.db
  -> (dval * string) option

(* DB schema modifications *)
val create : string -> tlid -> 'expr_type DbT.db

val create2 : string -> tlid -> id -> 'expr_type DbT.db

val add_col : id -> id -> 'expr_type DbT.db -> 'expr_type DbT.db

val set_col_name : id -> string -> 'expr_type DbT.db -> 'expr_type DbT.db

val set_col_type : id -> tipe -> 'expr_type DbT.db -> 'expr_type DbT.db

val change_col_name : id -> string -> 'expr_type DbT.db -> 'expr_type DbT.db

val change_col_type : id -> tipe -> 'expr_type DbT.db -> 'expr_type DbT.db

val delete_col : id -> 'expr_type DbT.db -> 'expr_type DbT.db

val unlocked :
  Uuidm.t -> Uuidm.t -> 'expr_type DbT.db list -> 'expr_type DbT.db list

val find_db : 'expr_type DbT.db list -> string -> 'expr_type DbT.db option

val create_migration :
  id -> id -> DbT.col list -> RuntimeT.expr DbT.db -> RuntimeT.expr DbT.db

val add_col_to_migration :
  id -> id -> RuntimeT.expr DbT.db -> RuntimeT.expr DbT.db

val set_col_name_in_migration :
  id -> string -> 'expr_type DbT.db -> 'expr_type DbT.db

val set_col_type_in_migration :
  id -> tipe -> 'expr_type DbT.db -> 'expr_type DbT.db

val abandon_migration : 'expr_type DbT.db -> 'expr_type DbT.db

val delete_col_in_migration : id -> 'expr_type DbT.db -> 'expr_type DbT.db

val rename_db : string -> 'expr_type DbT.db -> 'expr_type DbT.db
