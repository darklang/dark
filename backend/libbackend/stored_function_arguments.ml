open Core_kernel
open Libexecution
open Types
module RTT = Types.RuntimeT

(* ------------------------- *)
(* External *)
(* ------------------------- *)

let store ~canvas_id ~trace_id tlid args =
  Db.run
    ~name:"stored_function_arguments.store"
    "INSERT INTO function_arguments
     (canvas_id, trace_id, tlid, timestamp, arguments_json)
     VALUES ($1, $2, $3, CURRENT_TIMESTAMP, $4)"
    ~params:[Uuid canvas_id; Uuid trace_id; ID tlid; RoundtrippableDvalmap args]


let load_for_analysis ~canvas_id tlid (trace_id : Uuidm.t) :
    Analysis_types.input_vars option =
  Db.fetch
    ~name:"stored_function_arguments.load_for_analysis"
    "SELECT arguments_json
     FROM function_arguments
     WHERE canvas_id = $1
       AND tlid = $2
       AND trace_id = $3
     LIMIT 1"
    ~params:[Db.Uuid canvas_id; Db.ID tlid; Db.Uuid trace_id]
  |> List.hd
  |> Option.map ~f:(function
         | [args] ->
             args
             |> Dval.of_internal_roundtrippable_v0
             |> Dval.to_dval_pairs_exn
         | _ ->
             Exception.internal
               "Bad format for stored_functions.load_for_analysis" )


let load_traceids ~(canvas_id : Uuidm.t) (tlid : Types.tlid) : Uuidm.t list =
  Db.fetch
    ~name:"stored_function_arguments.load_traceids"
    "SELECT trace_id
     FROM function_arguments
     WHERE canvas_id = $1
       AND tlid = $2
     ORDER BY timestamp DESC
       LIMIT 10"
    ~params:[Db.Uuid canvas_id; Db.ID tlid]
  |> List.map ~f:(function
         | [trace_id] ->
             Util.uuid_of_string trace_id
         | _ ->
             Exception.internal
               "Bad DB format for stored_functions.load_for_analysis" )
