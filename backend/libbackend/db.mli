open Core
open Libexecution

(* Low-level API *)

type param =
  | Int of int
  | Int63 of Int63.t
  | ID of Types.id
  | String of string
  | Uuid of Uuidm.t
  | Float of float
  | Binary of string
  (* only works for passed params *)
  | Secret of string
  | RoundtrippableDval of Types.RuntimeT.dval
  | RoundtrippableDvalmap of Types.RuntimeT.dval_map
  (* Queryable are stored as jsonb so that they can be queried. *)
  | QueryableDval of Types.RuntimeT.dval
  | QueryableDvalmap of Types.RuntimeT.dval_map
  | Time of Types.RuntimeT.time
  | Null
  | List of param list
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
     params:param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> unit

val transaction : name:string -> (unit -> unit) -> unit

val delete :
     params:param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> int

val fetch :
     params:param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> string list list

val fetch_one :
     params:param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> string list

(** This is fetch_one, but with SQL returning a single int field. The intent is
 * to make it easy to swap Db.fetch_count and Db.delete so we can evaluate the
 * performance of GC queries using fetch_count, with basically the same code as
 * we use for delete - see Stored_events.trim_events_for_handler for an example. *)
val fetch_count :
     params:param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> int

val fetch_one_option :
     params:param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> string list option

val iter_with_cursor :
     name:string
  -> params:param list
  -> ?result:result
  -> f:(string list -> unit)
  -> string
  -> unit

val exists :
  params:param list -> name:string -> ?subject:string -> string -> bool

(* Occasionally, we're trying to do something dynamic, or maybe multiple
 * things in a single sql statement and then the above statements don't
 * work, so we need to escape manually *)
val escape : param -> string

val escape_string : string -> string

val array_separator : string

val date_of_sqlstring : string -> Core_kernel.Time.t

(* Misc *)
val delete_benchmarking_data : unit -> unit

exception DBQueryException of string

val dbQueryExceptionToString : exn -> string

type table_stats_row =
  { relation : string
  ; disk_bytes : int
  ; rows : int
  ; disk_human : string
  ; rows_human : string }

(** Queries the database to get approximate sizes (both in bytes and in # of rows) for each
 * table in the postgres DB, as a list of [table_stats_rows].
 *
 * Primary use case here is to run in a cron and be logged to honeycomb, but
 * there is also human-readable output. *)
val table_stats : unit -> table_stats_row list
