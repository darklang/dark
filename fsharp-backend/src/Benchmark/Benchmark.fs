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

let programContext () : Task<RT.ProgramContext> =
  task {
    let! c = TestUtils.TestUtils.initializeTestCanvas "benchmark"
    return
      { canvasID = c.id
        canvasName = c.name
        accountID = c.owner
        dbs = Map.empty
        userFns = Map.empty
        secrets = []
        userTypes = Map.empty }
  }

let runFSharp
  (program : RT.ProgramContext)
  (traceID : System.Guid)
  (expr : RT.Expr)
  : Task<RT.Dval> =
  task {
    let executionID = ExecutionID "benchmark"
    let! state, _ = RealExe.createState executionID traceID 7UL program
    let symtable = Interpreter.withGlobals state Map.empty
    return! (Interpreter.eval state symtable expr) |> Ply.TplPrimitives.runPlyAsTask
  }

let runFSharpBenchmark
  (expr : RT.Expr)
  (iterations : uint)
  (warmUpCount : uint)
  : Task<unit> =
  task {
    print "Running F# Benchmark"
    let traceID = System.Guid.NewGuid()
    let! program = programContext ()

    print "  Running warmups"
    for i = 1u to warmUpCount do
      let stopWatch = System.Diagnostics.Stopwatch.StartNew()
      do! runFSharp program traceID expr |> Task.map (ignore<RT.Dval>)
      stopWatch.Stop()
      print $"    Warmup {i}: {stopWatch.ElapsedMilliseconds}ms"

    print $"  Starting F# benchmark ({iterations})"
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let! dval = runFSharp program traceID expr
    for i = 1u to iterations - 1u do
      let! (_ : RT.Dval) = runFSharp program traceID expr
      ()
    stopWatch.Stop()
    print $"{dval}"
    print $"  Done: {stopWatch.ElapsedMilliseconds / int64 iterations}ms"
    return ()
  }



let runOCaml (expr : PT.Expr) : Task<float * RT.Dval> =
  task {
    let! program = programContext ()
    return!
      LibBackend.OCamlInterop.benchmark
        program.accountID
        program.canvasID
        expr
        Map.empty
        []
        []
  }

let runOCamlBenchmark
  (expr : PT.Expr)
  (iterations : uint)
  (warmUpCount : uint)
  : Task<unit> =
  task {
    print "Running OCaml Benchmark"
    print "  Running warmups"
    for i = 1u to warmUpCount do
      let! (elapsed : float, _ : RT.Dval) = runOCaml expr
      print $"    Warmup {i}: {elapsed}ms"

    print $"  Starting OCaml benchmark ({iterations})"
    let! (elapsed, dval) = runOCaml expr
    let mutable elapsedTotal = elapsed
    for i = 1u to iterations - 1u do
      let! (elapsed, _dval : RT.Dval) = runOCaml expr
      elapsedTotal <- elapsed + elapsedTotal
      ()
    print $"{dval}"
    print $"  Done: {int64 elapsedTotal / int64 iterations}ms"
    return ()
  }




let runBenchmark
  (filename : string)
  (iterations : uint)
  (warmUpCount : uint)
  : Task<unit> =
  task {
    let filename = if filename.EndsWith(".fs") then filename else filename + ".fs"
    let! code =
      System.IO.File.ReadAllTextAsync $"src/Benchmark/benchmarks/{filename}"
    let ptExpr = FSharpToExpr.parsePTExpr code
    let rtExpr = ptExpr.toRuntimeType ()
    do! runOCamlBenchmark ptExpr iterations warmUpCount
    do! runFSharpBenchmark rtExpr iterations warmUpCount
    return ()
  }

open Argu

type Arguments =
  | [<Mandatory; MainCommand; ExactlyOnce>] Filename of BENCHMARK : string
  | [<AltCommandLine("-w")>] Warmups of COUNT : uint
  | [<AltCommandLine("-i")>] Iterations of COUNT : uint

  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Filename _ ->
        "the name of the benchmark file to test (in src/Benchmarks/benchmarks)"
      | Warmups _ -> "the number of warmups to run"
      | Iterations _ -> "the number of times to run the test (the mean is reported)"

let parser = ArgumentParser.Create<Arguments>(programName = "Benchmark")

[<EntryPoint>]
let main args : int =
  try
    let cliArgs = parser.ParseCommandLine args
    print "Starting Benchmark"
    (LibBackend.Init.init "Benchmark" false).Result
    let filename = cliArgs.GetResult Filename
    let warmUpCount = cliArgs.GetResult(Warmups, 3u)
    let iterations = cliArgs.GetResult(Iterations, 3u)
    (runBenchmark filename iterations warmUpCount).GetAwaiter().GetResult()
    0
  with
  | e ->
    print e.Message
    print e.StackTrace
    1
