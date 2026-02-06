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
