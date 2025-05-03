/// Non-execution analysis
module LibCloud.Stats

open System.Threading.Tasks
open FSharp.Control.Tasks

open Microsoft.Data.Sqlite
open Fumble
open Db

open Prelude

module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

type DBStat = { count : int; example : Option<RT.Dval * string> }

type DBStats = Map<tlid, DBStat>

// let dbStats (c : Canvas.T) (tlids : tlid list) : Task<DBStats> =
//   tlids
//   |> List.filterMap (fun tlid ->
//     Map.get tlid c.dbs |> Option.map (fun db -> (tlid, db)))
//   |> List.map (fun (tlid, db) ->
//     task {
//       let db = PT2RT.DB.toRT db
//       // CLEANUP this is a lot of DB requests (2 per user DB)
//       let! count = UserDB.statsCount c.id db
//       let! example = UserDB.statsPluck c.id db
//       return (tlid, { count = count; example = example })
//     })
//   |> Task.flatten
//   |> Task.map Map.ofList

let workerStats (canvasID : CanvasID) (tlid : tlid) : Task<int> =
  Sql.query
    "SELECT COUNT(1) AS num
     FROM queue_events_v0 E
     INNER JOIN toplevels_v0 TL
        ON TL.canvas_id = E.canvas_id
       AND TL.module = E.module
       AND TL.name = E.name
     WHERE TL.tlid = @tlid
       AND TL.canvas_id = @canvasID"
  |> Sql.parameters [ "tlid", Sql.tlid tlid; "canvasID", Sql.uuid canvasID ]
  |> Sql.executeRowAsync (fun read -> read.int "num")

type Something =
  { relation : string
    rows : int64
    diskBytes : int64
    diskHuman : string
    rowsHuman : string }

// // In LibCloud.Db
// let sqliteTableStats () : Task<List<Something>> =
//   task {
//     let! tables = Sql.listOnlyColumn "SELECT name FROM sqlite_master WHERE type='table'" []

//     return!
//       tables
//       |> List.mapTask (fun (DString name) ->
//         task {
//           let! rowCount = Sql.queryInt64 $"SELECT COUNT(*) FROM \"{name}\"" []
//           let! pageCount = Sql.queryInt64 $"PRAGMA page_count" []
//           let! pageSize = Sql.queryInt64 $"PRAGMA page_size" []

//           let diskBytes = pageCount * pageSize
//           return {
//             relation = name
//             rows = rowCount
//             diskBytes = diskBytes
//             diskHuman = FileSize.humanReadable diskBytes
//             rowsHuman = rowCount.ToString("N0")
//           }
//         })
//   }
