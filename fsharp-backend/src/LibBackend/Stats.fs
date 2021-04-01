module LibBackend.Stats

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module PT = LibBackend.ProgramTypes

// -------------------------
// Non-execution analysis
// -------------------------

type DBStat = { count : int; example : Option<RT.Dval * string> }

type DBStats = Map<tlid, DBStat>

let dbStats (c : Canvas.T) (tlids : tlid list) : Task<DBStats> =
  let canvasID = c.meta.id
  let ownerID = c.meta.owner

  tlids
  |> List.filterMap
       (fun tlid -> Map.get tlid c.dbs |> Option.map (fun db -> (tlid, db)))
  |> List.map
       (fun (tlid, db) ->
         task {
           // CLEANUP this is a lot of reqs
           let! count = UserDB.statsCount canvasID ownerID (db.toRuntimeType ())
           let! example = UserDB.statsPluck canvasID ownerID (db.toRuntimeType ())
           return (tlid, { count = count; example = example })
         })
  |> Task.flatten
  |> Task.map Map.ofList

//
// type worker_stat = {count : int} [@@deriving show, yojson]
//
// let worker_stats (canvas_id : Uuidm.t) (tlid : tlid) : worker_stat =
//   let count =
//     Db.fetch_one
//       ~name:"count_workers"
//       ~subject:(show_tlid tlid)
//       "SELECT COUNT(1) AS num
//       FROM events E
//       INNER JOIN toplevel_oplists TL
//         ON TL.canvas_id = E.canvas_id
//         AND TL.module = E.space
//         AND TL.name = E.name
//       WHERE TL.tlid = $1
//       AND TL.canvas_id = $2
//       AND E.status IN('new', 'scheduled')"
//       ~params:[Db.ID tlid; Db.Uuid canvas_id]
//     |> List.hd_exn
//     |> int_of_string
//   in
//   {count}
//
//
