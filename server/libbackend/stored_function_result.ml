open Core_kernel
open Libexecution

open Types
module RTT = Types.RuntimeT

module Dbp = Dbprim

(* By hashing the filename, it's cheap to know if anything has changed,
 * without security implication of saving passwords to disk *)
let hash (arglist : RTT.dval list) : string =
  arglist
  |> List.map ~f:Dval.to_internal_repr
  |> String.concat
  |> Util.hash

(* ------------------------- *)
(* External *)
(* ------------------------- *)

let store (canvas_id, _, tlid, fnname, id) arglist result =
  Printf.sprintf
    "INSERT INTO function_results
    (canvas_id, tlid, fnname, id, hash, timestamp, value)
    VALUES (%s, %s, %s, %s, %s, CURRENT_TIMESTAMP, %s)"
    (Dbp.uuid canvas_id)
    (Dbp.tlid tlid)
    (Dbp.string fnname)
    (Dbp.id id)
    (Dbp.string (hash arglist))
    (Dbp.dvaljson result)
  |> Db.run_sql

let load (canvas_id, _, tlid, fnname, id) arglist
  : (RTT.dval * Time.t) option =
  Printf.sprintf
    "SELECT value, timestamp
    FROM function_results
    WHERE canvas_id = %s
      AND tlid = %s
      AND fnname = %s
      AND id = %s
      AND hash = %s
      LIMIT 1
      "
    (* TODO: sort by timestamp *)
    (Dbp.uuid canvas_id)
    (Dbp.tlid tlid)
    (Dbp.string fnname)
    (Dbp.id id)
    (Dbp.string (hash arglist))
  |> Db.fetch_via_sql
  |> List.hd
  |> Option.map ~f:(function
      | [dval; ts] ->
        (Dval.dval_of_json_string dval, Dval.date_of_sqlstring ts)
      | _ -> Exception.internal "Bad DB format for stored_functions")


