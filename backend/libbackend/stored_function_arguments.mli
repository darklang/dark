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
  -> (Analysis_types.input_vars * Types.RuntimeT.time) option

val load_traceids : canvas_id:Uuidm.t -> Types.tlid -> Uuidm.t list

(* GC old function arguments *)
val trim_arguments : unit -> int

type trim_arguments_action = Stored_event.trim_events_action

val trim_arguments_for_canvas :
     Libcommon.Telemetry.Span.t
  -> trim_arguments_action
  -> limit:int
  -> canvas_name:string
  -> Uuidm.t
  -> int

val trim_arguments_for_handler :
     Libcommon.Telemetry.Span.t
  -> trim_arguments_action
  -> limit:int
  -> canvas_name:string
  -> tlid:string
  -> Uuidm.t
  -> int
