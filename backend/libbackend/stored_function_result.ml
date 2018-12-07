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
    "INSERT INTO function_results_v2
     (canvas_id, trace_id, tlid, fnname, id, hash, timestamp, value)
     VALUES ($1, $2, $3, $4, $5, $6, CURRENT_TIMESTAMP, $7)"
    ~params:
      [ Uuid canvas_id
      ; Uuid trace_id
      ; ID tlid
      ; String fnname
      ; ID id
      ; String (Dval.hash arglist)
      ; DvalJson result ]


let load ~canvas_id ~trace_id tlid : function_result list =
  (* Right now, we don't allow the user to see multiple results when a function
   * is called in a loop. But, there's a lot of data when functions are called
   * in a loop, so avoid massive responses. *)
  Db.fetch
    ~name:"sfr_load"
    "SELECT
       DISTINCT ON (fnname, id, hash)
       fnname, id, hash, value, timestamp
     FROM function_results_v2
     WHERE canvas_id = $1
       AND trace_id = $2
       AND tlid = $3
     ORDER BY fnname, id, hash, timestamp DESC"
    ~params:[Db.Uuid canvas_id; Db.Uuid trace_id; Db.ID tlid]
  |> List.map ~f:(function
         | [fnname; id; hash; dval; ts] ->
             ( fnname
             , id_of_string id
             , hash
             , Dval.unsafe_dval_of_json_string dval )
         | _ ->
             Exception.internal
               "Bad DB format for stored_functions_results.load" )


let trim_results
    ~(canvas_id : Uuidm.t) ~(keep : Analysis_types.traceid list) () =
  Db.run
    ~name:"stored_function_result.trim_results"
    "DELETE FROM function_results_v2
     WHERE canvas_id = $1
       AND timestamp < CURRENT_TIMESTAMP
       AND NOT (trace_id = ANY (string_to_array($2, $3)::uuid[]))"
    ~subject:(Uuidm.to_string canvas_id)
    ~params:
      [ Uuid canvas_id
      ; List (List.map ~f:(fun u -> Db.Uuid u) keep)
      ; String Db.array_separator ]
