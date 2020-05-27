open Core
open Libexecution
open Types
open Types.RuntimeT

(* DB struct functions *)
val cols_for : 'expr_type DbT.db -> (string * tipe) list

(* DB runtime functions *)
val set :
     state:'expr_type exec_state
  -> upsert:bool
  -> 'expr_type DbT.db
  -> string
  -> 'expr_type dval_map
  -> Uuidm.t

val get_option :
     state:'expr_type exec_state
  -> 'expr_type DbT.db
  -> string
  -> 'expr_type dval option

val get_many :
     state:'expr_type exec_state
  -> 'expr_type DbT.db
  -> string list
  -> (string * 'expr_type dval) list

val get_many_with_keys :
     state:'expr_type exec_state
  -> 'expr_type DbT.db
  -> string list
  -> (string * 'expr_type dval) list

val get_all :
     state:'expr_type exec_state
  -> 'expr_type DbT.db
  -> (string * 'expr_type dval) list

val get_all_keys :
  state:'expr_type exec_state -> 'expr_type DbT.db -> string list

val query_exact_fields :
     state:'expr_type exec_state
  -> 'expr_type DbT.db
  -> 'expr_type dval
  -> (string * 'expr_type dval) list

val query :
     state:Types.RuntimeT.expr exec_state
  -> Types.RuntimeT.expr DbT.db
  -> Types.RuntimeT.expr dblock_args
  -> (string * Types.RuntimeT.expr dval) list

val count : state:'expr_type exec_state -> 'expr_type DbT.db -> int

val delete : state:'expr_type exec_state -> 'expr_type DbT.db -> string -> unit

val delete_all : state:'expr_type exec_state -> 'expr_type DbT.db -> unit

(* Stats fns *)

val stats_count :
  account_id:Uuidm.t -> canvas_id:Uuidm.t -> 'expr_type DbT.db -> int

val stats_pluck :
     account_id:Uuidm.t
  -> canvas_id:Uuidm.t
  -> 'expr_type DbT.db
  -> ('expr_type dval * string) option

(* DB schema modifications *)
val create : string -> tlid -> 'expr_type DbT.db

val create2 : string -> tlid -> id -> 'expr_type DbT.db

val add_col : id -> id -> 'expr_type DbT.db -> 'expr_type DbT.db

val set_col_name : id -> string -> 'expr_type DbT.db -> 'expr_type DbT.db

val set_col_type : id -> tipe -> 'expr_type DbT.db -> 'expr_type DbT.db

val change_col_name : id -> string -> 'expr_type DbT.db -> 'expr_type DbT.db

val change_col_type : id -> tipe -> 'expr_type DbT.db -> 'expr_type DbT.db

val delete_col : id -> 'expr_type DbT.db -> 'expr_type DbT.db

val unlocked : canvas_id:Uuidm.t -> account_id:Uuidm.t -> tlid list

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
