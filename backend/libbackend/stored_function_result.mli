open Core
open Libexecution

val store :
     canvas_id:Uuidm.t
  -> trace_id:Uuidm.t
  -> Types.RuntimeT.function_desc
  -> Types.fluid_expr Types.RuntimeT.dval list
  -> Types.fluid_expr Types.RuntimeT.dval
  -> unit

val load :
     canvas_id:Uuidm.t
  -> trace_id:Uuidm.t
  -> Types.tlid
  -> Types.fluid_expr Analysis_types.function_result list

val trim_results : unit -> int

val trim_results_for_canvas : Uuidm.t -> int

type trim_results_action = Stored_function_arguments.trim_arguments_action

val trim_results_for_canvas :
     Libcommon.Telemetry.Span.t
  -> trim_results_action
  -> limit:int
  -> ?canvas_name:string
  -> Uuidm.t
  -> int
