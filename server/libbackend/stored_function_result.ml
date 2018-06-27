open Core_kernel
open Libexecution

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

let store (canvas_id, tlid, fnname, id) arglist result =
  Db.run
    ~name:"stored_function_result.store"
    "INSERT INTO function_results
     (canvas_id, tlid, fnname, id, hash, timestamp, value)
     VALUES ($1, $2, $3, $4, $5, CURRENT_TIMESTAMP, $6)"
    ~params:[ Uuid canvas_id
            ; Int tlid
            ; String fnname
            ; Int id
            ; String (hash arglist)
            ; DvalJson result]

let load (canvas_id, tlid, fnname, id) arglist
  : (RTT.dval * Time.t) option =
  (* TODO: sort by timestamp *)
  Db.fetch_one_option
    ~name:"sfr_load"
    "SELECT value, timestamp
     FROM function_results
     WHERE canvas_id = $1
       AND tlid = $2
       AND fnname = $3
       AND id = $4
       AND hash = $5
     ORDER BY timestamp DESC
       LIMIT 1"
    ~params:[ Db.Uuid canvas_id
            ; Db.Int tlid
            ; Db.String fnname
            ; Db.Int id
            ; Db.String (hash arglist)]
  |> Option.map ~f:(function
      | [dval; ts] ->
        (Dval.dval_of_json_string dval, Dval.date_of_sqlstring ts)
      | _ -> Exception.internal "Bad DB format for stored_functions")


