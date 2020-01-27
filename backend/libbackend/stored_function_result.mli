open Core
open Libexecution

val store :
     canvas_id:Uuidm.t
  -> trace_id:Uuidm.t
  -> Types.RuntimeT.function_desc
  -> Types.RuntimeT.dval list
  -> Types.RuntimeT.dval
  -> unit

val load :
     canvas_id:Uuidm.t
  -> trace_id:Uuidm.t
  -> Types.tlid
  -> Analysis_types.function_result list

val trim_results : unit -> int

val trim_results_for_canvas : Uuidm.t -> int
