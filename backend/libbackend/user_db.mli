open Core
open Libexecution
open Types
open Types.RuntimeT

(* DB struct functions *)
val cols_for : fluid_expr DbT.db -> (string * tipe) list

(* DB runtime functions *)
val set :
     state:fluid_expr exec_state
  -> upsert:bool
  -> fluid_expr DbT.db
  -> string
  -> fluid_expr dval_map
  -> Uuidm.t

val get_option :
     state:fluid_expr exec_state
  -> fluid_expr DbT.db
  -> string
  -> fluid_expr dval option

val get_many :
     state:fluid_expr exec_state
  -> fluid_expr DbT.db
  -> string list
  -> (string * fluid_expr dval) list

val get_many_with_keys :
     state:fluid_expr exec_state
  -> fluid_expr DbT.db
  -> string list
  -> (string * fluid_expr dval) list

val get_all :
     state:fluid_expr exec_state
  -> fluid_expr DbT.db
  -> (string * fluid_expr dval) list

val get_all_keys :
  state:fluid_expr exec_state -> fluid_expr DbT.db -> string list

val query_exact_fields :
     state:fluid_expr exec_state
  -> fluid_expr DbT.db
  -> fluid_expr dval
  -> (string * fluid_expr dval) list

val query :
     state:Types.fluid_expr exec_state
  -> Types.fluid_expr DbT.db
  -> Types.fluid_expr dblock_args
  -> (string * Types.fluid_expr dval) list

val query_count :
     state:Types.fluid_expr exec_state
  -> Types.fluid_expr DbT.db
  -> Types.fluid_expr dblock_args
  -> int

val count : state:fluid_expr exec_state -> fluid_expr DbT.db -> int

val delete : state:fluid_expr exec_state -> fluid_expr DbT.db -> string -> unit

val delete_all : state:fluid_expr exec_state -> fluid_expr DbT.db -> unit

(* Stats fns *)

val stats_count :
  account_id:Uuidm.t -> canvas_id:Uuidm.t -> fluid_expr DbT.db -> int

val stats_pluck :
     account_id:Uuidm.t
  -> canvas_id:Uuidm.t
  -> fluid_expr DbT.db
  -> (fluid_expr dval * string) option

(* DB schema modifications *)
val create : string -> tlid -> fluid_expr DbT.db

val create2 : string -> tlid -> id -> fluid_expr DbT.db

val add_col : id -> id -> fluid_expr DbT.db -> fluid_expr DbT.db

val set_col_name : id -> string -> fluid_expr DbT.db -> fluid_expr DbT.db

val set_col_type : id -> tipe -> fluid_expr DbT.db -> fluid_expr DbT.db

val change_col_name : id -> string -> fluid_expr DbT.db -> fluid_expr DbT.db

val change_col_type : id -> tipe -> fluid_expr DbT.db -> fluid_expr DbT.db

val delete_col : id -> fluid_expr DbT.db -> fluid_expr DbT.db

val unlocked : canvas_id:Uuidm.t -> account_id:Uuidm.t -> tlid list

val find_db : fluid_expr DbT.db list -> string -> fluid_expr DbT.db option

val create_migration :
  id -> id -> DbT.col list -> fluid_expr DbT.db -> fluid_expr DbT.db

val add_col_to_migration : id -> id -> fluid_expr DbT.db -> fluid_expr DbT.db

val set_col_name_in_migration :
  id -> string -> fluid_expr DbT.db -> fluid_expr DbT.db

val set_col_type_in_migration :
  id -> tipe -> fluid_expr DbT.db -> fluid_expr DbT.db

val abandon_migration : fluid_expr DbT.db -> fluid_expr DbT.db

val delete_col_in_migration : id -> fluid_expr DbT.db -> fluid_expr DbT.db

val rename_db : string -> fluid_expr DbT.db -> fluid_expr DbT.db
