open Core
open Libexecution

val store :
     canvas_id:Uuidm.t
  -> trace_id:Uuidm.t
  -> Types.tlid
  -> Types.RuntimeT.dval_map
  -> unit

val load_for_analysis :
     canvas_id:Uuidm.t
  -> Types.tlid
  -> Uuidm.t
  -> Analysis_types.input_vars option

val load_traceids : canvas_id:Uuidm.t -> Types.tlid -> Uuidm.t list
