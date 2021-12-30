module ApiServer.Execution

// Execution API endpoints

open Microsoft.AspNetCore.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibExecution.OCamlTypes.Convert

module Canvas = LibBackend.Canvas
module RealExe = LibRealExecution.RealExecution
module Exe = LibExecution.Execution
module DvalRepr = LibExecution.DvalRepr
module Telemetry = LibService.Telemetry

module Function =
  type Params =
    { tlid : tlid
      trace_id : AT.TraceID
      caller_id : id
      args : ORT.dval list
      fnname : string }

  type T =
    { result : ORT.dval
      hash : string
      hashVersion : int
      touched_tlids : tlid list
      unlocked_dbs : tlid list }

  let execute (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let executionID = loadExecutionID ctx
      let! p = ctx.ReadJsonAsync<Params>()
      let args = List.map Convert.ocamlDval2rt p.args
      Telemetry.addTags [ "tlid", p.tlid
                          "trace_id", p.trace_id
                          "caller_id", p.caller_id
                          "fnname", p.fnname ]

      t.next "load-canvas"
      let! c = Canvas.loadTLIDsWithContext canvasInfo [ p.tlid ]

      t.next "load-execution-state"
      let program = Canvas.toProgram c
      let! (state, touchedTLIDs) =
        RealExe.createState executionID p.trace_id p.tlid program

      t.next "execute-function"
      let fnname = p.fnname |> PT.FQFnName.parse
      let! result = Exe.executeFunction state p.caller_id args fnname

      t.next "get-unlocked"
      let! unlocked = LibBackend.UserDB.unlocked canvasInfo.owner canvasInfo.id

      t.next "write-api"
      let hashVersion = DvalRepr.currentHashVersion
      let hash = DvalRepr.hash hashVersion args

      let result =
        { result = Convert.rt2ocamlDval result
          hash = hash
          hashVersion = hashVersion
          touched_tlids = HashSet.toList touchedTLIDs
          unlocked_dbs = unlocked }

      return result
    }

module Handler =
  type Params =
    { tlid : tlid
      trace_id : AT.TraceID
      input : List<string * ORT.dval> }

  type T = { touched_tlids : tlid list }

  let trigger (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let executionID = loadExecutionID ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()
      Telemetry.addTags [ "tlid", p.tlid; "trace_id", p.trace_id ]

      let inputVars =
        p.input
        |> List.map (fun (name, var) -> (name, Convert.ocamlDval2rt var))
        |> Map.ofList

      t.next "load-canvas"
      let! c = Canvas.loadTLIDsWithContext canvasInfo [ p.tlid ]
      let program = Canvas.toProgram c
      let expr = c.handlers[ p.tlid ].ast.toRuntimeType ()

      t.next "load-execution-state"
      let! state, touchedTLIDs =
        RealExe.createState executionID p.trace_id p.tlid program

      t.next "execute-handler"
      // CLEANUP
      // since this ignores the result, it doesn't go through the http result
      // handling function. This might not matter
      let! (_result : RT.Dval) = Exe.executeHandler state inputVars expr

      t.next "write-api"
      return { touched_tlids = touchedTLIDs |> HashSet.toList }
    }
