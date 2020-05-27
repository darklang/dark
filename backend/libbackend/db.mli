open Core
open Libexecution

(* Low-level API *)

type 'expr_type param =
  | Int of int
  | Int63 of Int63.t
  | ID of Types.id
  | String of string
  | Uuid of Uuidm.t
  | Float of float
  | Binary of string
  (* only works for passed params *)
  | Secret of string
  | RoundtrippableDval of 'expr_type Types.RuntimeT.dval
  | RoundtrippableDvalmap of 'expr_type Types.RuntimeT.dval_map
  (* Queryable are stored as jsonb so that they can be queried. *)
  | QueryableDval of 'expr_type Types.RuntimeT.dval
  | QueryableDvalmap of 'expr_type Types.RuntimeT.dval_map
  | Time of Types.RuntimeT.time
  | Null
  | List of 'expr_type param list
  | Bool of bool
[@@deriving show]

(* only works for in-script params *)

type result =
  | TextResult
  (* bytea will be returned as hex strings *)
  | BinaryResult

(* unparsed binary data *)

(* NOTE: run is not allowed to receive multiple commands. If you
 * want multiple statements, use [transaction] *)
val run :
     params:'expr_type param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> unit

val transaction : name:string -> (unit -> unit) -> unit

val delete :
     params:'expr_type param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> int

val fetch :
     params:'expr_type param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> string list list

val fetch_one :
     params:'expr_type param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> string list

val fetch_one_option :
     params:'expr_type param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> string list option

val iter_with_cursor :
     name:string
  -> params:'expr_type param list
  -> ?result:result
  -> f:(string list -> unit)
  -> string
  -> unit

val exists :
     params:'expr_type param list
  -> name:string
  -> ?subject:string
  -> string
  -> bool

(* Occasionally, we're trying to do something dynamic, or maybe multiple
 * things in a single sql statement and then the above statements don't
 * work, so we need to escape manually *)
val escape : 'expr_type param -> string

val escape_string : string -> string

val array_separator : string

val date_of_sqlstring : string -> Core_kernel.Time.t

(* Misc *)
val delete_benchmarking_data : unit -> unit

exception DBQueryException of string

val dbQueryExceptionToString : exn -> string

type table_stats_row =
  { relation : string
  ; disk : float
  ; rows : float
  ; disk_human : string
  ; rows_human : string }

val table_stats : unit -> table_stats_row list
