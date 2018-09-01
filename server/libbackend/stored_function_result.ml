open Core_kernel
open Libexecution
open Analysis_types

open Types
module RTT = Types.RuntimeT

(* By hashing the filename, it's cheap to know if anything has changed,
 * without security implication of saving passwords to disk *)
let hash (arglist : RTT.dval list) : string =
  arglist
  |> List.map ~f:Dval.to_internal_repr
  |> String.concat
  |> Crypto.hash

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
            ; String (hash arglist)
            ; DvalJson result]

let load ~canvas_id ~trace_id tlid : (RTT.dval function_result) list =
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
        (fnname, id_of_string id, hash, Dval.dval_of_json_string dval)
      | _ -> Exception.internal "Bad DB format for stored_functions_results.load")

