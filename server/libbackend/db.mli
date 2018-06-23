open Core
open Libexecution

(* Low-level API *)
val exists_via_sql : ?quiet:bool -> string -> bool

type param = Int of int
           | String of string
           | Uuid of Uuidm.t
           | Binary of string
           | Secret of string
           | DvalJson of Types.RuntimeT.dval
           | DvalmapJsonb of Types.RuntimeT.dval_map
           | Null

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
(* val exists_via_sql2 : ?quiet:bool -> string -> bool *)

(* Saving canvases to the DB *)
val save_oplists : host:string -> digest:string -> string -> unit
val load_oplists : host:string -> digest:string -> string option
val load_json_oplists : host:string -> string option
val save_json_oplists : host:string -> digest:string -> string -> unit
val all_oplists : digest:string -> string list

(* Misc *)
val delete_benchmarking_data : unit -> unit
