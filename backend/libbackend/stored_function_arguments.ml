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
    (RTT.expr Analysis_types.input_vars * RTT.time) option =
  (* We need to alias the subquery (here aliased as `q`) because Postgres
   * requires inner SELECTs to be aliased. *)
  Db.fetch
    ~name:"stored_function_arguments.load_for_analysis"
    "SELECT arguments_json, timestamp FROM (
      SELECT DISTINCT ON (trace_id) trace_id, timestamp, arguments_json
      FROM function_arguments
      WHERE canvas_id = $1 AND tlid = $2 AND trace_id = $3
      ORDER BY trace_id, timestamp DESC
      ) AS q
      ORDER BY timestamp DESC
      LIMIT 1"
    ~params:[Db.Uuid canvas_id; Db.ID tlid; Db.Uuid trace_id]
  |> List.hd
  |> Option.map ~f:(function
         | [args; timestamp] ->
             ( args
               |> Dval.of_internal_roundtrippable_v0
               |> Dval.to_dval_pairs_exn
             , Util.date_of_isostring timestamp )
         | _ ->
             Exception.internal
               "Bad format for stored_functions.load_for_analysis")


let load_traceids ~(canvas_id : Uuidm.t) (tlid : Types.tlid) : Uuidm.t list =
  (* We need to alias the subquery (here aliased as `q`) because Postgres
   * requires inner SELECTs to be aliased. *)
  Db.fetch
    ~name:"stored_function_arguments.load_traceids"
    "SELECT trace_id FROM (
      SELECT DISTINCT ON (trace_id) trace_id, timestamp
      FROM function_arguments
      WHERE canvas_id = $1 AND tlid = $2
      ORDER BY trace_id, timestamp DESC
      ) AS q
      ORDER BY timestamp DESC
      LIMIT 10"
    ~params:[Db.Uuid canvas_id; Db.ID tlid]
  |> List.map ~f:(function
         | [trace_id] ->
             Util.uuid_of_string trace_id
         | _ ->
             Exception.internal
               "Bad DB format for stored_functions.load_for_analysis")


(** trim_arguments removes all function_arguments records older than a week,
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
 * - Stored_function_result.trim_results
 * which are nearly identical queries on different tables *)
let trim_arguments () : int =
  Db.delete
    ~name:"stored_function_argument.trim_arguments"
    "WITH indexed_arguments AS (
       SELECT trace_id, row_number() OVER (
         PARTITION BY canvas_id, tlid
         ORDER BY timestamp DESC
       ) AS rownum
       FROM function_arguments
       WHERE timestamp < (NOW() - interval '1 week')
    )
    DELETE FROM function_arguments WHERE trace_id IN (
      SELECT trace_id FROM indexed_arguments
      WHERE rownum > 10
      LIMIT 10000
    )"
    ~params:[]


(** trim_arguments_for_canvas is like trim_arguments_for_canvas but for a single canvas.
 *
 * All the comments and warnings there apply. Please read them. *)
let trim_arguments_for_canvas (canvas_id : Uuidm.t) : int =
  Db.delete
    ~name:"stored_function_argument.trim_arguments_for_canvas"
    "WITH indexed_arguments AS (
       SELECT trace_id, row_number() OVER (
         PARTITION BY canvas_id, tlid
         ORDER BY timestamp DESC
       ) AS rownum
       FROM function_arguments
       WHERE canvas_id = $1
       AND timestamp < (NOW() - interval '1 week')
    )
    DELETE FROM function_arguments WHERE trace_id IN (
      SELECT trace_id FROM indexed_arguments
      WHERE rownum > 10
      LIMIT 10000
    )"
    ~params:[Uuid canvas_id]
