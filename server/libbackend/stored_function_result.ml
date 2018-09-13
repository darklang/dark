open Core_kernel
open Libexecution
open Analysis_types

open Types
module RTT = Types.RuntimeT

(* ------------------------- *)
(* External *)
(* ------------------------- *)

let store ~canvas_id ~trace_id (tlid, fnname, id) arglist result =
  Db.run
    ~name:"stored_function_result.store"
    "INSERT INTO function_results
     (canvas_id, trace_id, tlid, fnname, id, hash, timestamp, value)
     VALUES ($1, $2, $3, $4, $5, $6, CURRENT_TIMESTAMP, $7)"
    ~params:[ Uuid canvas_id
            ; Uuid trace_id
            ; ID tlid
            ; String fnname
            ; ID id
            ; String (Dval.hash arglist)
            ; DvalJson result]

let load ~canvas_id ~trace_id tlid : function_result list =
  Db.fetch
    ~name:"sfr_load"
    "SELECT fnname, id, hash, value, timestamp
     FROM function_results
     WHERE canvas_id = $1
       AND trace_id = $2
       AND tlid = $3"
    ~params:[ Db.Uuid canvas_id
            ; Db.Uuid trace_id
            ; Db.ID tlid
            ]
  |> List.map ~f:(function
      | [fnname; id; hash; dval; ts] ->
        (fnname, id_of_string id, hash, Dval.unsafe_dval_of_json_string dval)
      | _ -> Exception.internal "Bad DB format for stored_functions_results.load")

