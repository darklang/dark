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
      ; RoundtrippableDval result ]


let load ~canvas_id ~trace_id tlid : function_result list =
  (* Right now, we don't allow the user to see multiple results when a function
   * is called in a loop. But, there's a lot of data when functions are called
   * in a loop, so avoid massive responses.
   *
   * The wrapping SELECT DISTINCT ON (fnname, id) ensures we only get one per caller,
   * and the inner SELECT DISTINCT ensures we get the most recent one.
   * *)
  Db.fetch
    ~name:"sfr_load"
    "SELECT DISTINCT ON (fnname, id) fnname, id, hash, value, timestamp
     FROM (
      SELECT DISTINCT ON (fnname, id, hash) fnname, id, hash, value, timestamp
      FROM function_results_v2
      WHERE canvas_id = $1
        AND trace_id = $2
        AND tlid = $3
      ORDER BY fnname, id, hash, timestamp DESC
      ) AS q
      ORDER BY fnname, id, timestamp DESC"
    ~params:[Db.Uuid canvas_id; Db.Uuid trace_id; Db.ID tlid]
  |> List.map ~f:(function
         | [fnname; id; hash; dval; ts] ->
             ( fnname
             , id_of_string id
             , hash
             , Dval.of_internal_roundtrippable_v0 dval )
         | _ ->
             Exception.internal
               "Bad DB format for stored_functions_results.load" )


(* in the previous iteration of this, we did two queries:
 * - get most recent 10
 * - delete all older than a week, but not in the first resultset
 *
 * turns out, it's cheaper to delete anything that's older than a week, and not
 * in the most recent ten of that set. This means we keep more traces (we'll
 * keep the top 10 older than a week, even if there are > 10 more recent than
 * that), but this is an okay heuristic for garbage collection
 * *)
let trim_results () : int =
  Db.delete
    ~name:"stored_function_result.trim_results"
    "DELETE FROM function_results_v2
    WHERE trace_id IN (
      SELECT trace_id FROM (
        SELECT row_number()
        OVER (PARTITION BY canvas_id, tlid ORDER BY timestamp
desc) as rownum, t.trace_id
        FROM function_results_v2 t
        WHERE timestamp < (NOW() - interval '1 week')
        LIMIT 10000) as u
      WHERE rownum > 10
      LIMIT 10000
    )"
    ~params:[]
