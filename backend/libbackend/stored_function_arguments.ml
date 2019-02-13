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
    ~params:
      [ Uuid canvas_id
      ; Uuid trace_id
      ; ID tlid
      ; String
          ( args
          |> Dval.unsafe_dvalmap_to_yojson ~redact:false
          |> Yojson.Safe.to_string ) ]


let load_for_analysis ~canvas_id tlid :
    (Uuidm.t * Analysis_types.input_vars) list =
  Db.fetch
    ~name:"stored_function_arguments.load_for_analysis"
    "SELECT trace_id, arguments_json, timestamp
     FROM function_arguments
     WHERE canvas_id = $1
       AND tlid = $2
     ORDER BY timestamp DESC
       LIMIT 10"
    ~params:[Db.Uuid canvas_id; Db.ID tlid]
  |> List.map ~f:(function
         | [trace_id; args; _] ->
             let trace_id =
               if trace_id = ""
               then Util.create_uuid ()
               else Util.uuid_of_string trace_id
             in
             let input_vars =
               args
               |> Yojson.Safe.from_string
               |> Dval.unsafe_dvalmap_of_yojson
               |> RTT.DvalMap.to_alist
             in
             (trace_id, input_vars)
         | _ ->
             Exception.internal
               "Bad DB format for stored_functions.load_for_analysis" )


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
