module HttpMiddleware.MiddlewareV0

// V0 of the Dark HTTP Middleware. Designed to move as much of the Http framework into "user space" (or something which could potentially be user space in the future) as possible.

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module RealExe = LibRealExecution.RealExecution
module Exe = LibExecution.Execution
module Interpreter = LibExecution.Interpreter
module Req = RequestV0
module Resp = ResponseV0


let createRequest
  (allowUnparseable : bool)
  (url : string)
  (headers : List<string * string>)
  (query : List<string * string list>)
  (body : byte array)
  : RT.Dval =
  Req.fromRequest allowUnparseable url headers query body

let executeRequest
  (program : RT.ProgramContext)
  (tlid : tlid)
  (traceID : LibExecution.AnalysisTypes.TraceID)
  (routeVars : List<string * RT.Dval>)
  (request : RT.Dval)
  (expr : RT.Expr)
  : Task<Resp.HttpResponse * HashSet.T<tlid>> =
  task {
    let! state, touchedTLIDs = RealExe.createState traceID tlid program

    // Build request
    let symtable =
      Map.ofList routeVars
      |> Interpreter.withGlobals state
      |> Map.add "request" request

    // Execute
    let! result = Interpreter.eval state symtable expr

    return (Resp.toHttpResponse result, touchedTLIDs)
  }
