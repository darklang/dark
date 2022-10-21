/// API endpoints to function and Handler execution
module ApiServer.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module AT = LibExecution.AnalysisTypes
module CTRuntime = ClientTypes.Runtime
module CTApi = ClientTypes.Api
module CT2Runtime = ClientTypes2ExecutionTypes.Runtime

module Canvas = LibBackend.Canvas
module RealExe = LibRealExecution.RealExecution
module Exe = LibExecution.Execution
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated
module Telemetry = LibService.Telemetry


module FunctionV1 =

  /// API endpoint to execute a User Function and return the result
  let execute (ctx : HttpContext) : Task<CTApi.Execution.FunctionV1.Response> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadVanillaJsonAsync<CTApi.Execution.FunctionV1.Request>()
      let args = List.map CT2Runtime.Dval.fromCT p.args
      Telemetry.addTags [ "tlid", p.tlid
                          "trace_id", p.trace_id
                          "caller_id", p.caller_id
                          "fnname", p.fnname ]

      t.next "load-canvas"
      let! c = Canvas.loadTLIDsWithContext canvasInfo [ p.tlid ]
      let program = Canvas.toProgram c

      t.next "execute-function"
      let fnname = p.fnname |> PTParser.FQFnName.parse |> PT2RT.FQFnName.toRT


      let! (result, traceResults) =
        RealExe.reexecuteFunction
          c.meta
          program
          p.tlid
          p.caller_id
          p.trace_id
          fnname
          args

      t.next "get-unlocked"
      let! unlocked = LibBackend.UserDB.unlocked canvasInfo.owner canvasInfo.id

      t.next "write-api"
      let hashVersion = DvalReprInternalDeprecated.currentHashVersion
      let hash = DvalReprInternalDeprecated.hash hashVersion args

      let result : CTApi.Execution.FunctionV1.Response =
        { result = CT2Runtime.Dval.toCT result
          hash = hash
          hashVersion = hashVersion
          touched_tlids = HashSet.toList traceResults.tlids
          unlocked_dbs = unlocked }

      return result
    }

module HandlerV1 =
  /// API endpoint to trigger the execution of a Handler
  ///
  /// Handlers are handled asynchronously, so the result is not returned. The result
  /// is instead added to the trace, which is then loaded by the client again.
  let trigger (ctx : HttpContext) : Task<CTApi.Execution.HandlerV1.Response> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadVanillaJsonAsync<CTApi.Execution.HandlerV1.Request>()
      Telemetry.addTags [ "tlid", p.tlid; "trace_id", p.trace_id ]

      let inputVars =
        p.input
        |> List.map (fun (name, var) -> (name, CT2Runtime.Dval.fromCT var))
        |> Map

      t.next "load-canvas"
      let! c = Canvas.loadTLIDsWithContext canvasInfo [ p.tlid ]
      let program = Canvas.toProgram c
      let handler = c.handlers[p.tlid] |> PT2RT.Handler.toRT

      t.next "execute-handler"
      let! (_, traceResults) =
        RealExe.executeHandler
          ClientTypes2BackendTypes.Pusher.eventSerializer
          c.meta
          handler
          program
          p.trace_id
          inputVars
          RealExe.ReExecution

      t.next "write-api"
      return { touched_tlids = traceResults.tlids |> HashSet.toList }
    }
