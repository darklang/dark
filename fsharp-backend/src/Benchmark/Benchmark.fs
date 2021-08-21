module Benchmark

// A benchmark executable to measure the performance of Dark code. See --help to see
// options.

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module RealExe = LibRealExecution.RealExecution
module Exe = LibExecution.Execution
module Interpreter = LibExecution.Interpreter
module Account = LibBackend.Account
module Canvas = LibBackend.Canvas

let run (expr : RT.Expr) : Task<RT.Dval> =
  task {
    let traceID = System.Guid.NewGuid()
    let! c = TestUtils.testCanvasInfo "benchmark"
    let program : RT.ProgramContext =
      { canvasID = c.id
        accountID = c.owner
        dbs = Map.empty
        userFns = Map.empty
        secrets = []
        userTypes = Map.empty }
    let! state, _ = RealExe.createState traceID 7UL program
    let symtable = Interpreter.withGlobals state Map.empty
    let! dval = (Interpreter.eval state symtable expr) |> TaskOrValue.toTask
    return dval
  }

[<EntryPoint>]
let main _ =
  print "Starting Benchmark"
  LibBackend.Init.init "Benchmark"
  0
