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
    let! c = TestUtils.testCanvasInfo "benchmark"
    return
      { canvasID = c.id
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
    let! state, _ = RealExe.createState traceID 7UL program
    let symtable = Interpreter.withGlobals state Map.empty
    return! (Interpreter.eval state symtable expr) |> TaskOrValue.toTask
  }

let runFSharpBenchmark (expr : RT.Expr) (warmUpCount : uint) : Task<unit> =
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

    print "  Starting benchmark"
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let! dval = runFSharp program traceID expr
    stopWatch.Stop()
    print $"{dval}"
    print $"  Done: {stopWatch.ElapsedMilliseconds}ms"
    return ()
  }



let runOCaml (expr : PT.Expr) : Task<RT.Dval> =
  task {
    let! program = programContext ()
    let! result =
      // FSTODO: Even though this goes through serialization and a HTTP call, this is
      // still massively faster than the F# version, but we should remove that at some point.
      LibBackend.OCamlInterop.execute
        program.accountID
        program.canvasID
        expr
        Map.empty
        []
        []
    return result
  }

let runOCamlBenchmark (expr : PT.Expr) (warmUpCount : uint) : Task<unit> =
  task {
    print "Running OCaml Benchmark"
    print "  Running warmups"
    for i = 1u to warmUpCount do
      let stopWatch = System.Diagnostics.Stopwatch.StartNew()
      do! runOCaml expr |> Task.map (ignore<RT.Dval>)
      stopWatch.Stop()
      print $"    Warmup {i}: {stopWatch.ElapsedMilliseconds}ms"

    print "  Starting benchmark"
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let! dval = runOCaml expr
    stopWatch.Stop()
    print $"{dval}"
    print $"  Done: {stopWatch.ElapsedMilliseconds}ms"
    return ()
  }




let runBenchmark (filename : string) (warmUpCount : uint) : Task<unit> =
  task {
    let filename = if filename.EndsWith(".fs") then filename else filename + ".fs"
    let! code =
      System.IO.File.ReadAllTextAsync $"src/Benchmark/benchmarks/{filename}"
    let ptExpr = FSharpToExpr.parsePTExpr code
    let rtExpr = ptExpr.toRuntimeType ()
    do! runOCamlBenchmark ptExpr warmUpCount
    do! runFSharpBenchmark rtExpr warmUpCount
    return ()
  }

open Argu

type Arguments =
  | [<Mandatory; MainCommand; ExactlyOnce>] Filename of BENCHMARK : string
  | [<AltCommandLine("-w")>] Warmups of COUNT : uint

  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Filename _ ->
        "the name of the benchmark file to test (in src/Benchmarks/benchmarks)"
      | Warmups _ -> "the number of warmups to run"

let parser = ArgumentParser.Create<Arguments>(programName = "Benchmark")

[<EntryPoint>]
let main args : int =
  try
    let cliArgs = parser.ParseCommandLine args
    print "Starting Benchmark"
    LibBackend.Init.init "Benchmark"
    let filename = cliArgs.GetResult Filename
    let warmups = cliArgs.GetResult(Warmups, 3u)
    (runBenchmark filename warmups).GetAwaiter().GetResult()
    0
  with
  | e ->
    print e.Message
    1
