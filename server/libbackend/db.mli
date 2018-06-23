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

(* NOTE: run is not allowed to receive multiple commands. If you
 * want multiple statements, put a BEGIN/END around them. *)
val run : params: param list -> name:string -> string ->
  unit
val fetch : params: param list -> name:string -> string ->
  string list list
val fetch_one : params: param list -> name:string -> string ->
  string list
val fetch_one_option : params: param list -> name:string -> string ->
  string list option
val exists : params: param list -> name:string -> string ->
  bool

(* Occasionally, we're trying to do something dynamic, or maybe multiple
 * things in a single sql statement and then the above statements don't
 * work, so we need to escape manually *)
val escape : param -> string
val cast_expression_for : Types.RuntimeT.dval -> string option



(* Saving canvases to the DB *)
val save_oplists : host:string -> digest:string -> string -> unit
val load_oplists : host:string -> digest:string -> string option
val load_json_oplists : host:string -> string option
val save_json_oplists : host:string -> digest:string -> string -> unit
val all_oplists : digest:string -> string list

(* Misc *)
val delete_benchmarking_data : unit -> unit

(* exposed to test it *)
val escape_single : string -> string
val escape_double : string -> string
