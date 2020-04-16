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
     (canvas_id, trace_id, tlid, fnname, id, hash, hash_version, timestamp, value)
     VALUES ($1, $2, $3, $4, $5, $6, $7, CURRENT_TIMESTAMP, $8)"
    ~params:
      [ Uuid canvas_id
      ; Uuid trace_id
      ; ID tlid
      ; String fnname
      ; ID id
      ; String (Dval.hash Dval.current_hash_version arglist)
      ; Int Dval.current_hash_version
      ; RoundtrippableDval result ]


let load ~canvas_id ~trace_id tlid : RTT.expr function_result list =
  (* Right now, we don't allow the user to see multiple results when a function
   * is called in a loop. But, there's a lot of data when functions are called
   * in a loop, so avoid massive responses. *)
  Db.fetch
    ~name:"sfr_load"
    "SELECT
       DISTINCT ON (fnname, id, hash, hash_version)
       fnname, id, hash, hash_version, value, timestamp
     FROM function_results_v2
     WHERE canvas_id = $1
       AND trace_id = $2
       AND tlid = $3
     ORDER BY fnname, id, hash, hash_version, timestamp DESC"
    ~params:[Db.Uuid canvas_id; Db.Uuid trace_id; Db.ID tlid]
  |> List.map ~f:(function
         | [fnname; id; hash; hash_version; dval; ts] ->
             ( fnname
             , id_of_string id
             , hash
               (* hash_version is nullable, nulls come back as empty string *)
             , (match hash_version with "" -> 0 | hv -> hv |> int_of_string)
             , Dval.of_internal_roundtrippable_v0 dval )
         | _ ->
             Exception.internal
               "Bad DB format for stored_functions_results.load")


(** trim_results removes all function_results_v2 records older than a week,
 * leaving at minimum 10 records for each tlid on a canvas regardless of age.
 *
 * Returns the number of rows deleted.
 *
 * CAVEAT: in order to keep our DB from bursting into flames given a large
 * number of records to cleanup, we cap the maximum number of records deleted
 * per call to 10_000.
 *
 * See also
 * - Stored_event.trim_events
 * - Stored_function_arguments.trim_arguments
 * which are nearly identical queries on different tables *)
let trim_results () : int =
  Db.delete
    ~name:"stored_function_result.trim_results"
    "WITH indexed_results AS (
       SELECT trace_id, row_number() OVER (
         PARTITION BY canvas_id, tlid
         ORDER BY timestamp DESC
       ) AS rownum
       FROM function_results_v2
       WHERE timestamp < (NOW() - interval '1 week')
    )
    DELETE FROM function_results_v2 WHERE trace_id IN (
      SELECT trace_id FROM indexed_results
      WHERE rownum > 10
      LIMIT 10000
    )"
    ~params:[]


(** trim_results_for_canvas is like trim_results but for a single canvas.
 *
 * All the comments and warnings there apply. Please read them. *)
let trim_results_for_canvas (canvas_id : Uuidm.t) : int =
  Db.delete
    ~name:"stored_function_result.trim_results_for_canvas"
    "WITH indexed_results AS (
       SELECT trace_id, row_number() OVER (
         PARTITION BY canvas_id, tlid
         ORDER BY timestamp DESC
       ) AS rownum
       FROM function_results_v2
       WHERE canvas_id = $1
       AND timestamp < (NOW() - interval '1 week')
    )
    DELETE FROM function_results_v2 WHERE trace_id IN (
      SELECT trace_id FROM indexed_results
      WHERE rownum > 10
      LIMIT 10000
    )"
    ~params:[Uuid canvas_id]
