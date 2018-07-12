open Core
open Libexecution

(* Low-level API *)

type param = Int of int
           | String of string
           | Uuid of Uuidm.t
           | Binary of string (* only works for passed params *)
           | Secret of string
           | DvalJson of Types.RuntimeT.dval
           | DvalmapJsonb of Types.RuntimeT.dval_map
           | Null
           | List of param list (* only works for in-script params *)

type result = TextResult (* bytea will be returned as hex strings *)
            | BinaryResult (* unparsed binary data *)

(* NOTE: run is not allowed to receive multiple commands. If you
 * want multiple statements, put a BEGIN/END around them. *)
val run : params: param list -> ?result : result -> name:string -> ?subject:string -> string -> unit
val fetch : params: param list -> ?result : result -> name:string -> ?subject:string -> string -> string list list
val fetch_one : params: param list -> ?result : result -> name:string ->  ?subject:string -> string -> string list
val fetch_one_option : params: param list -> ?result : result -> name:string -> ?subject:string -> string -> string list option
val exists : params: param list -> name:string -> ?subject:string -> string -> bool

(* Occasionally, we're trying to do something dynamic, or maybe multiple
 * things in a single sql statement and then the above statements don't
 * work, so we need to escape manually *)
val escape : param -> string
val cast_expression_for : Types.RuntimeT.dval -> string option


(* Misc *)
val delete_benchmarking_data : unit -> unit

(* exposed to test it *)
val escape_single : string -> string
val escape_double : string -> string
