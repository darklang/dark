open Core
open Libexecution

(* Low-level API *)
val fetch_via_sql : ?quiet:bool -> string -> string list list
val exists_via_sql : ?quiet:bool -> string -> bool

type sql = Int of int
         | String of string
         | Uuid of Uuidm.t
         | Binary of string
         | Secret of string
         | DvalJson of Types.RuntimeT.dval
         | DvalmapJsonb of Types.RuntimeT.dval_map
         | Null

val run_sql2 : params: sql list -> name:string -> string -> unit
(* val fetch_via_sql2 : ?quiet:bool -> string -> string list list *)
(* val exists_via_sql2 : ?quiet:bool -> string -> bool *)

(* Saving canvases to the DB *)
val save_oplists : host:string -> digest:string -> string -> unit
val load_oplists : host:string -> digest:string -> string option
val load_json_oplists : host:string -> string option
val save_json_oplists : host:string -> digest:string -> string -> unit
val all_oplists : digest:string -> string list

(* Misc *)
val delete_benchmarking_data : unit -> unit
