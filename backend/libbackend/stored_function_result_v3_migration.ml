open Core_kernel
open Libexecution
open Analysis_types
open Types
module RTT = Types.RuntimeT
open Libcommon
module Db = Libbackend_basics.Db

let get_handler_traces
    ~canvas_id
    ~(tlid : tlid)
    ~(path : string)
    ~(module_ : string)
    ~(modifier : string)
    (count : int) : Uuidm.t list =
  let builtin_trace = Analysis.traceid_of_tlid tlid in
  Db.fetch
    ~name:"handler_valid_traces"
    "SELECT trace_id from stored_events_v2
      WHERE canvas_id = $1
        AND module = $2
        AND path = $3
        AND modifier = $4
     GROUP BY trace_id
     ORDER BY MAX(timestamp) DESC
     LIMIT $5"
    ~params:
      [ Db.Uuid canvas_id
      ; Db.String module_
      ; Db.String path
      ; Db.String modifier
      ; Db.Int count ]
  |> List.map ~f:(fun uuid ->
         uuid |> List.hd_exn |> Uuidm.of_string |> Option.value_exn)
  |> List.cons builtin_trace
