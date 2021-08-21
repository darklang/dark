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

let runCode
  (program : RT.ProgramContext)
  (traceID : System.Guid)
  (expr : RT.Expr)
  : Task<RT.Dval> =
  task {
    let! state, _ = RealExe.createState traceID 7UL program
    let symtable = Interpreter.withGlobals state Map.empty
    return! (Interpreter.eval state symtable expr) |> TaskOrValue.toTask
  }

let runBenchmark (filename : string) (warmUpCount : uint) : Task<unit> =
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
    print "Running warmups"
    let! code =
      System.IO.File.ReadAllTextAsync $"src/Benchmark/benchmarks/{filename}"
    let expr = FSharpToExpr.parseRTExpr code
    for i = 1u to warmUpCount do
      let stopWatch = System.Diagnostics.Stopwatch.StartNew()
      do! runCode program traceID expr |> Task.map (ignore<RT.Dval>)
      stopWatch.Stop()
      print $"  Warmup {i}: {stopWatch.ElapsedMilliseconds}ms"

    print "Starting benchmark"
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let! dval = runCode program traceID expr
    stopWatch.Stop()
    print $"{dval}"
    print $"Done: {stopWatch.ElapsedMilliseconds}ms"
    return ()
  }

[<EntryPoint>]
let main _ =
  print "Starting Benchmark"
  let filename = "fizzbuzz.fs"
  let warmUps = 3u
  LibBackend.Init.init "Benchmark"
  (runBenchmark filename warmUps).GetAwaiter().GetResult()
  0
