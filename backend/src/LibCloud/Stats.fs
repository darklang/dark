/// Non-execution analysis
module LibCloud.Stats

open System.Threading.Tasks
open FSharp.Control.Tasks

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

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
