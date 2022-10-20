/// API endpoints related to DBs
module ApiServer.DBs

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module AT = LibExecution.AnalysisTypes
module CTRuntime = ClientTypes.Runtime
module CTApi = ClientTypes.Api
module CT2Runtime = ClientTypes2ExecutionTypes.Runtime

module Stats = LibBackend.Stats
module Canvas = LibBackend.Canvas
module RT = LibExecution.RuntimeTypes
module TI = LibBackend.TraceInputs
module Telemetry = LibService.Telemetry

module Unlocked =
  module Types = CTApi.DB.Unlocked

  /// API endpoint to fetch a list of unlocked User DBs
  ///
  /// A 'locked' database cannot have its fields/types changed
  let get (ctx : HttpContext) : Task<Types.Response> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx

      t.next "getUnlocked"
      let! unlocked = LibBackend.UserDB.unlocked canvasInfo.owner canvasInfo.id

      return { unlocked_dbs = unlocked }
    }

module DBStatsV1 =
  module Types = CTApi.DB.StatsV1

  /// API endpoint to get statistical data regarding User DBs
  let getStats (ctx : HttpContext) : Task<Types.Response.T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadVanillaJsonAsync<Types.Request>()
      Telemetry.addTags [ "tlids", p.tlids ]

      t.next "load-canvas"
      let! c = Canvas.loadAllDBs canvasInfo

      t.next "load-db-stats"
      let! result = Stats.dbStats c p.tlids

      t.next "write-api"
      // TODO: move this mapping to CT2Api?
      let (result : Types.Response.T) =
        result
        |> Map.toList
        |> List.map (fun (k, (s : Stats.DBStat)) ->
          (string k),
          { Types.Response.Stat.count = s.count
            Types.Response.Stat.example =
              Option.map (fun (dv, s) -> (CT2Runtime.Dval.toCT dv, s)) s.example })
        |> Map

      return result
    }
