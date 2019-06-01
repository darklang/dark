open Core
open Libexecution

(* Low-level API *)

type param =
  | Int of int
  | ID of Types.id
  | String of string
  | Uuid of Uuidm.t
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

(* only works for in-script params *)

type result =
  | TextResult
  (* bytea will be returned as hex strings *)
  | BinaryResult

(* unparsed binary data *)

(* NOTE: run is not allowed to receive multiple commands. If you
 * want multiple statements, put a BEGIN/END around them. *)
val run :
     params:param list
  -> ?result:result
  -> name:string
  -> ?subject:string
  -> string
  -> unit

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

val array_separator : string

val date_of_sqlstring : string -> Core_kernel.Time.t

(* Misc *)
val delete_benchmarking_data : unit -> unit

(* exposed to test it *)
val escape_single : string -> string
