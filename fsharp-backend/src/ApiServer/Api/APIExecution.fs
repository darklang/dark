module ApiServer.Execution

// Execution API endpoints

open Microsoft.AspNetCore.Http
open Giraffe

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OT = LibBackend.OCamlInterop.OCamlTypes
module ORT = LibBackend.OCamlInterop.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibBackend.OCamlInterop.Convert

module Canvas = LibBackend.Canvas
module RealExe = LibBackend.RealExecution
module Exe = LibExecution.Execution
module DvalRepr = LibExecution.DvalRepr

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
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      let! p = ctx.BindModelAsync<Params>()
      let args = List.map Convert.ocamlDval2rt p.args
      t "read-api"

      let! c = Canvas.loadTLIDsWithContext canvasInfo [ p.tlid ]
      let c = Result.unwrapUnsafe c
      t "load-canvas"

      let program = Canvas.toProgram c
      let! (state, touchedTLIDs) = RealExe.createState p.trace_id p.tlid program
      t "load-execution-state"

      let fnname = p.fnname |> PT.FQFnName.parse
      let! result = Exe.executeFunction state p.caller_id args fnname
      t "execute-function"

      let! unlocked = LibBackend.UserDB.unlocked canvasInfo.owner canvasInfo.id
      t "get-unlocked"

      let hashVersion = DvalRepr.currentHashVersion
      let hash = DvalRepr.hash hashVersion args

      let result =
        { result = Convert.rt2ocamlDval result
          hash = hash
          hashVersion = hashVersion
          touched_tlids = HashSet.toList touchedTLIDs
          unlocked_dbs = unlocked }

      t "write-api"
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
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      let! p = ctx.BindModelAsync<Params>()

      let inputVars =
        p.input
        |> List.map (fun (name, var) -> (name, Convert.ocamlDval2rt var))
        |> Map.ofList

      t "read-api"

      let! c = Canvas.loadTLIDsWithContext canvasInfo [ p.tlid ]
      let c = Result.unwrapUnsafe c
      let program = Canvas.toProgram c
      let expr = c.handlers.[p.tlid].ast.toRuntimeType ()
      t "load-canvas"

      let! state, touchedTLIDs = RealExe.createState p.trace_id p.tlid program
      t "load-execution-state"

      // since this ignores the result, it doesn't go through the error rail
      // handling function. This might not matter
      let! (_result : RT.Dval) = Exe.executeHttpHandler state inputVars expr

      t "execute-handler"

      let result = { touched_tlids = touchedTLIDs |> HashSet.toList }

      t "write-api"

      return result
    }
