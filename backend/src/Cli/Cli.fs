module Cli.Main

open System
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module Exe = LibExecution.Execution
module PackageRefs = LibExecution.PackageRefs
module BuiltinCli = Builtins.Cli.Builtin

// Dual logging (console + cli.log file)
let private logError (message : string) : unit =
  // Always write to stderr for immediate feedback
  System.Console.Error.WriteLine message

  // Also try to log to file (best effort - don't fail if we can't)
  try
    let logPath = System.IO.Path.Combine(LibConfig.Config.logDir, "cli.log")
    let logDir = System.IO.Path.GetDirectoryName(logPath)
    if not (IO.Directory.Exists logDir) then
      System.IO.Directory.CreateDirectory logDir |> ignore<IO.DirectoryInfo>

    let timestamp = System.DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss"
    let logEntry = $"[{timestamp}] {message}\n"
    System.IO.File.AppendAllText(logPath, logEntry)
  with _ ->
    () // Silently ignore logging errors - don't make things worse

// ---------------------
// Version information
// ---------------------

type VersionInfo = { hash : string; buildDate : string; inDevelopment : bool }

#if DEBUG
let inDevelopment : bool = true
#else
let inDevelopment : bool = false
#endif

open System.Reflection

let info () =
  let buildAttributes =
    Assembly.GetEntryAssembly().GetCustomAttribute<AssemblyMetadataAttribute>()
  // This reads values created during the build in Cli.fsproj
  // It doesn't feel like this is how it's supposed to be used, but it works. But
  // what if we wanted more than two parameters?
  let buildDate = buildAttributes.Key
  let gitHash = buildAttributes.Value
  { hash = gitHash; buildDate = buildDate; inDevelopment = inDevelopment }


// ---------------------
// Execution
// ---------------------

let builtins : RT.Builtins =
  // Outer CLI uses main branch for its own execution context.
  // User scripts get branch-specific context via cliParseAndExecuteScript.
  LibExecution.Builtin.combine
    [ Builtins.CliHost.Libs.Cli.builtinsToUse ()
      Builtins.CliHost.Builtin.builtins ()
      BuiltinCli.builtins () ]
    []



let state (packageManager : RT.PackageManager) =
  let program : RT.Program = { dbs = Map.empty }

  let notify
    (_state : RT.ExecutionState)
    (_vm : RT.VMState)
    (_msg : string)
    (_metadata : Metadata)
    =
    uply { return () }

  let sendException
    (_ : RT.ExecutionState)
    (_ : RT.VMState)
    (metadata : Metadata)
    (exn : exn)
    =
    uply { printException "Internal error" metadata exn }

  Exe.createState
    builtins
    packageManager
    Exe.noTracing
    sendException
    notify
    PT.mainBranchId
    program




let execute
  (packageManager : RT.PackageManager)
  (args : List<string>)
  : Task<RT.ExecutionResult> =
  task {
    // Split out because `cli.execute` turned out to be nearly identical for `status` and `help` despite help
    // running 5x the instructions, which means most of it is a FIXED cost, not the Dark code running.
    // `state` builds the builtins map and the execution state; this says how much of the fixed cost is that.
    let state = Telemetry.time "cli.buildState" [] (fun () -> state packageManager)
    let fnName = RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.executeCliCommand ())
    let args =
      args |> List.map RT.DString |> Dval.list RT.KTString |> NEList.singleton
    let! result = Exe.executeFunction state fnName [] args
    return result
  }

let initSerializers () = ()

[<EntryPoint>]
let main (args : string[]) =
  try
    // How long the process took to reach here. `cli.total` starts after resource extraction and can't
    // see runtime init, assembly loading or JIT of the startup path, which is a large share of a short
    // command. Wall clock rather than Stopwatch, because the only fixed point is when the OS started us.
    // Read the switch before doing any measuring: `Process.GetCurrentProcess()` reads /proc and
    // initialises the Process machinery, which is not free on a command this short, and the only thing
    // it feeds is a telemetry event.
    let telemetryEnabled =
      match System.Environment.GetEnvironmentVariable "DARK_TELEMETRY" with
      | "1" -> true
      | _ -> false

    let preMainMs =
      if telemetryEnabled then
        let processStart =
          System.Diagnostics.Process.GetCurrentProcess().StartTime.ToUniversalTime()
        int64 (System.DateTime.UtcNow - processStart).TotalMilliseconds
      else
        0L

    // Extract embedded resources FIRST — this sets DARK_CONFIG_RUNDIR
    // which LibConfig.Config needs to resolve paths correctly.
    let extractStart = System.Diagnostics.Stopwatch.GetTimestamp()
    EmbeddedResources.extract ()
    let extractTicks = System.Diagnostics.Stopwatch.GetTimestamp() - extractStart
    initSerializers ()

    // Now safe to access LibConfig paths.
    //
    // Gated on DARK_TELEMETRY (read above), the same switch the Dark side reads (see `initState` in
    // cli/core.dark), so both halves of the instrument turn on together. Unconditional init would append
    // to telemetry.jsonl on every invocation, and, since `InterpreterStats.create()` keys off this, would
    // also leave per-instruction counting on in the hot loop for every run.
    if telemetryEnabled then
      Telemetry.init (
        System.IO.Path.Combine(LibConfig.Config.logDir, "telemetry.jsonl")
      )
    // Emitted now rather than at the top, because telemetry has no output path until `init` above.
    Telemetry.event "cli.preMain" [ "ms", string preMainMs ]
    let ticksToMs (t : int64) = t * 1000L / System.Diagnostics.Stopwatch.Frequency
    Telemetry.event "cli.extractResources" [ "ms", string (ticksToMs extractTicks) ]
    // Drained here rather than logged in place: `extract` runs before telemetry has an output path.
    for (label, ticks) in EmbeddedResources.timings do
      Telemetry.event $"cli.{label}" [ "ms", string (ticksToMs ticks) ]

    use _totalSpan = Telemetry.span "cli.total" []

    // Named so the phases inside `cli.total` sum to it. Without this most of the total lands in "the
    // rest", which is not a useful place for time to live.
    Telemetry.time "cli.seedCheck" [] (fun () ->
      // If data.db is missing but seed.db exists, copy seed as data.db
      let dbPath = LibConfig.Config.dbPath
      let seedPath = System.IO.Path.Combine(LibConfig.Config.runDir, "seed.db")
      if not (System.IO.File.Exists dbPath) && System.IO.File.Exists seedPath then
        System.Console.Error.WriteLine "Copying seed.db as data.db"
        System.IO.File.Copy(seedPath, dbPath))

    // Force the module-level `builtins` binding here, so its cost is attributed to a span of its own
    // rather than to whichever phase happens to touch it first.
    Telemetry.time "cli.builtinsInit" [] (fun () ->
      builtins.fns.Count |> ignore<int>)

    // Separated from `growIfNeeded` so the first connection open and its PRAGMA round trip are attributable
    // to themselves rather than to whichever query happened to run first.
    Telemetry.time "cli.dbConnect" [] LibDB.Sqlite.Sql.warm

    // Grow the database: apply any unapplied ops and evaluate values.
    let cliPackageManager =
      Telemetry.time "cli.createPM" [] (fun () -> LibDB.PackageManager.rt)

    Telemetry.time "cli.growIfNeeded" [] (fun () ->
      (LibDB.Seed.growIfNeeded (fun () -> builtins) cliPackageManager (fun msg ->
        System.Console.Error.WriteLine msg))
        .Result
      |> ignore<bool>)

    Telemetry.time "cli.pmInit" [] (fun () -> cliPackageManager.init.Result)

    let result =
      Telemetry.time "cli.execute" [] (fun () ->
        let result = execute cliPackageManager (Array.toList args)
        result.Result)

    Telemetry.time "cli.consoleWait" [] NonBlockingConsole.wait

    // Startup instrumentation. All of it is inert when telemetry is off; read it with
    // `scripts/perf/view-telemetry.py`.

    // How many package items this run actually decoded. Emitted alongside the spans because per-item
    // cost and item count are useless separately.
    Telemetry.counterSnapshot ()
    |> List.iter (fun (name, n) -> Telemetry.event name [ "count", string n ])

    // Total the interpreter counters across every VM this run created. A VM is per-`executeFunction`
    // and the stats hang off it, so without the sink the object is gone before anything could ask.
    if Telemetry.isEnabled () then
      let stats =
        RT.InterpreterStatsSink.all
        |> Seq.choose (fun o ->
          match o with
          | :? RT.InterpreterStats as s -> Some s
          | _ -> None)
        |> Seq.toList

      // Per-opcode allocation, summed across every VM. Names come from reflection over the Instruction DU
      // so tag order can't drift out of sync with a hand-written list.
      let opcodeNames = RT.Opcode.names
      let totalAlloc = Array.zeroCreate 32
      let totalCount = Array.zeroCreate 32
      for s in stats do
        for i in 0..31 do
          totalAlloc[i] <- totalAlloc[i] + s.allocByOpcode[i]
          totalCount[i] <- totalCount[i] + s.countByOpcode[i]
      for i in 0..31 do
        if totalCount[i] > 0L then
          let name = if i < opcodeNames.Length then opcodeNames[i] else string i
          Telemetry.event
            $"opcode.{name}"
            [ "count", string totalCount[i]
              "allocBytes", string totalAlloc[i]
              "bytesPerOp", string (totalAlloc[i] / totalCount[i]) ]

      // Per-builtin allocation, summed across VMs. This is what named the cost: ~99% of everything the
      // process allocates happens inside builtin bodies, not the interpreter around them.
      let byBuiltin = System.Collections.Generic.Dictionary<string, int64>()
      for s in stats do
        for kv in s.builtinAlloc do
          match byBuiltin.TryGetValue kv.Key with
          | true, v -> byBuiltin[kv.Key] <- v + kv.Value
          | false, _ -> byBuiltin[kv.Key] <- kv.Value
      let callsByBuiltin = System.Collections.Generic.Dictionary<string, int64>()
      for s in stats do
        for kv in s.builtinCallsByName do
          match callsByBuiltin.TryGetValue kv.Key with
          | true, v -> callsByBuiltin[kv.Key] <- v + kv.Value
          | false, _ -> callsByBuiltin[kv.Key] <- kv.Value
      byBuiltin
      |> Seq.sortByDescending (fun kv -> kv.Value)
      |> Seq.truncate 20
      |> Seq.iter (fun kv ->
        let calls =
          match callsByBuiltin.TryGetValue kv.Key with
          | true, c -> c
          | false, _ -> 0L
        Telemetry.event
          $"builtinAlloc.{kv.Key}"
          [ "bytes", string kv.Value
            "calls", string calls
            "bytesPerCall", string (if calls = 0L then 0L else kv.Value / calls) ])

      for i in 0 .. min (RT.ApplyStage.names.Length - 1) 31 do
        let total = stats |> List.sumBy (fun s -> s.allocByStage[i])
        let runs = stats |> List.sumBy (fun s -> s.countByStage[i])
        if total > 0L then
          Telemetry.event
            $"applyStage.{RT.ApplyStage.names[i]}"
            [ "bytes", string total
              "runs", string runs
              "bytesPerRun", string (if runs = 0L then 0L else total / runs) ]

      Telemetry.event
        "vm.stats"
        [ "vms", string (List.length stats)
          "instructions", string (stats |> List.sumBy (fun s -> s.instructionCount))
          "builtinCalls", string (stats |> List.sumBy (fun s -> s.builtinCallCount))
          "packageCalls", string (stats |> List.sumBy (fun s -> s.packageCallCount))
          "framePushes", string (stats |> List.sumBy (fun s -> s.framePushCount))
          "registersAllocated",
          string (stats |> List.sumBy (fun s -> s.registersAllocated))
          "builtinBodyAlloc",
          string (stats |> List.sumBy (fun s -> s.builtinBodyAlloc))
          "tstSizeSum", string (stats |> List.sumBy (fun s -> s.tstSizeSum))
          "tstSizeMax",
          string (stats |> List.map (fun s -> s.tstSizeMax) |> List.fold max 0L) ]

    // Allocation per instruction, to separate "we allocate a Dval per operation" from "the async state
    // machine costs per operation". Process-total, so it costs one call at exit rather than anything per
    // instruction. GC counts come along because collection pauses would show up as neither.
    if Telemetry.isEnabled () then
      Telemetry.event
        "gc.stats"
        [ "totalAllocatedBytes", string (System.GC.GetTotalAllocatedBytes(false))
          "gen0", string (System.GC.CollectionCount 0)
          "gen1", string (System.GC.CollectionCount 1)
          "gen2", string (System.GC.CollectionCount 2) ]

    Telemetry.timerSnapshot ()
    |> List.iter (fun (name, us) ->
      Telemetry.event name [ "us", string us; "ms", string (us / 1000L) ])

    // Exit codes are bounded; narrow safely rather than letting an out-of-Int32
    // result throw an uncaught host OverflowException.
    let intToExitCode (i : RT.DarkInt) : int =
      match RT.DarkInt.toInt32 i with
      | Some n -> n
      | None ->
        logError
          $"main function returned an Int outside exit-code range: {RT.DarkInt.toBigInt i}"
        1

    match result with
    | Error(rte, callStack) ->
      let state = state cliPackageManager

      let errorCallStackStr =
        (LibExecution.Execution.callStackString state callStack).Result

      match (LibExecution.Execution.runtimeErrorToString state rte).Result with
      | Ok(RT.DString s) ->
        logError $"Encountered a Runtime Error:\n{s}\n\n{errorCallStackStr}\n  "

      | Ok otherVal ->
        logError
          $"Encountered a Runtime Error, stringified it, but somehow a non-string was returned.\nRuntime Error: {rte}\n'Stringified':\n{otherVal}\n{errorCallStackStr}"

      | Error newErr ->
        logError
          $"Encountered a Runtime Error, tried to stringify it, and then _that_ failed.\nOriginal Error: {rte}\n{errorCallStackStr}\n\nError encountered when trying to stringify:\n{newErr}"

      1
    | Ok(RT.DInt64 i) -> intToExitCode (RT.DarkInt.Finite i)
    | Ok(RT.DInt i) -> intToExitCode i
    | Ok dval ->
      let state = state cliPackageManager
      let output = (Exe.dvalToRepr state dval).Result
      logError $"Error: main function must return an int (returned {output})"
      1


  with e ->
    let rec describe (depth : int) (ex : exn) : unit =
      let indent = String.replicate depth "  "
      System.Console.Error.WriteLine $"{indent}{ex.GetType().FullName}: {ex.Message}"
      match ex with
      | :? System.AggregateException as agg ->
        for inner in agg.InnerExceptions do
          describe (depth + 1) inner
      | _ ->
        if not (isNull ex.InnerException) then describe (depth + 1) ex.InnerException
      if depth = 0 && not (isNull ex.StackTrace) then
        System.Console.Error.WriteLine $"Stack trace:\n{ex.StackTrace}"
    System.Console.Error.WriteLine "Error starting Darklang CLI:"
    describe 0 e
    1
