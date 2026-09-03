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

// Log to stderr and, when possible, to cli.log.
let private logError (message : string) : unit =
  System.Console.Error.WriteLine message

  // Logging must not make the command fail.
  try
    let logPath = System.IO.Path.Combine(LibConfig.Config.logDir, "cli.log")
    let logDir = System.IO.Path.GetDirectoryName(logPath)
    if not (IO.Directory.Exists logDir) then
      System.IO.Directory.CreateDirectory logDir |> ignore<IO.DirectoryInfo>

    let timestamp = System.DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss"
    let logEntry = $"[{timestamp}] {message}\n"
    System.IO.File.AppendAllText(logPath, logEntry)
  with _ ->
    ()

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
  // These values are written by the build in Cli.fsproj.
  let buildDate = buildAttributes.Key
  let gitHash = buildAttributes.Value
  { hash = gitHash; buildDate = buildDate; inDevelopment = inDevelopment }


// ---------------------
// Execution
// ---------------------

/// Build builtins after package seeding has refreshed PackageRefs.
/// Keeping this lazy prevents F# module initialization from resolving hashes
/// before the package database exists.
let private builtinsLazy : Lazy<RT.Builtins> =
  lazy
    // The CLI itself runs against the main branch. User scripts choose their
    // branch in cliParseAndExecuteScript.
    (LibExecution.Builtin.combine
      [ Builtins.CliHost.Libs.Cli.builtinsToUse ()
        Builtins.CliHost.Builtin.builtins ()
        BuiltinCli.builtins () ]
      [])



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
    uply {
      // A store condition already carries a sentence written for whoever ran the command, and
      // `printException` buries it under a stack trace. The exception still surfaces as a runtime
      // error, which prints that sentence once.
      match exn with
      | :? Exception.StoreConditionException -> ()
      | _ -> printException "Internal error" metadata exn
    }

  Exe.createState
    (builtinsLazy.Force())
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
    // Keep state construction separate so startup cost is measurable.
    let state = Telemetry.time "cli.buildState" [] (fun () -> state packageManager)
    // Load bundled Darklang function hashes once for package-approval checks.
    let! bundled = LibDB.ProgramTypes.Fn.hashesOwnedBy "Darklang" |> Ply.toTask
    let state =
      // CLI control code is trusted; guest `run`/`eval` create restricted states.
      { Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll state with
          // CLI control code may manage the instance policy.
          canManagePolicies = true
          // Only the trusted `sync` command may reach private-network HTTP targets.
          canUsePrivateNetworkHttp =
            match args with
            | "sync" :: _ -> true
            | _ -> false
          isBundledPackageFn = fun (RT.Hash h) -> bundled.Contains h }
    let fnName = RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.executeCliCommand ())
    let args =
      args |> List.map RT.DString |> Dval.list RT.KTString |> NEList.singleton
    let! result = Exe.executeFunction state fnName [] args
    return result
  }

let initSerializers () = ()

/// Record host-operation decisions for troubleshooting and review in
/// `rundir/logs/host-audit.jsonl`. Set `DARK_AUDIT=off` to skip this audit file.
let private installAuditLog () : unit =
  if System.Environment.GetEnvironmentVariable "DARK_AUDIT" <> "off" then
    let logPath =
      System.IO.Path.Combine(LibConfig.Config.runDir, "logs", "host-audit.jsonl")
    let lockObj = obj ()
    LibExecution.Host.setAuditSink (fun op outcome ->
      try
        let decision, layer, detail =
          match outcome with
          | LibExecution.HostTypes.Outcome.Success _ -> "allowed", "", ""
          | LibExecution.HostTypes.Outcome.Denied(layer, _, resource, _) ->
            "denied", string layer, resource
          | LibExecution.HostTypes.Outcome.Failed failure ->
            "failed", "", failure.message
          | LibExecution.HostTypes.Outcome.Rejected m -> "rejected", "", m
        let detail = LibExecution.HostTypes.redactAuditDetail op detail
        let line =
          System.Text.Json.JsonSerializer.Serialize(
            {| ts = System.DateTime.UtcNow.ToString("o")
               op = LibExecution.HostTypes.describeOperation op
               decision = decision
               layer = layer
               detail = detail |}
          )
        lock lockObj (fun () -> System.IO.File.AppendAllText(logPath, line + "\n"))
      with _ ->
        ())

[<EntryPoint>]
let main (args : string[]) =
  try
    // Sampled before anything else in the process, including the environment read below. It is one
    // half of `cli.preMain`, and the other half -- the process start time -- costs milliseconds to
    // obtain, so taking this first is what keeps the measurement from including itself.
    let mainEntry = System.DateTime.UtcNow

    // Measure startup before cli.total, including runtime and assembly loading.
    let telemetryEnabled =
      match System.Environment.GetEnvironmentVariable "DARK_TELEMETRY" with
      | "1" -> true
      | _ -> false

    let preMainMs =
      if telemetryEnabled then
        // Keep Process initialization out of the startup measurement.
        let processStart =
          System.Diagnostics.Process.GetCurrentProcess().StartTime.ToUniversalTime()
        int64 (mainEntry - processStart).TotalMilliseconds
      else
        0L

    // Extract embedded resources FIRST — this sets DARK_CONFIG_RUNDIR
    // which LibConfig.Config needs to resolve paths correctly.
    let extractStart = System.Diagnostics.Stopwatch.GetTimestamp()
    EmbeddedResources.extract ()
    let extractTicks = System.Diagnostics.Stopwatch.GetTimestamp() - extractStart
    initSerializers ()

    // Extraction established the rundir, so policy paths are safe to use.
    try
      LibDB.PolicyStore.seedInstanceIfMissing
        LibExecution.Permissions.Policy.defaultInstance
    with e ->
      eprintfn
        "warning: could not initialize ~/.darklang/policy (%s); host effects are denied this run"
        e.Message

    // Prevent scoped guest file operations from targeting the package store.
    LibExecution.HostSecurity.setPackageDbPath LibConfig.Config.dbPath

    // Record host-operation decisions at the boundary.
    installAuditLog ()

    // Use the same DARK_TELEMETRY switch as the Dark-side instrumentation.
    if telemetryEnabled then
      Telemetry.init (
        System.IO.Path.Combine(LibConfig.Config.logDir, "telemetry.jsonl")
      )
    // The output path is available only after telemetry.init.
    Telemetry.event "cli.preMain" [ "ms", string preMainMs ]
    let ticksToMs (t : int64) = t * 1000L / System.Diagnostics.Stopwatch.Frequency
    Telemetry.event "cli.extractResources" [ "ms", string (ticksToMs extractTicks) ]
    // Resource extraction happened before telemetry had an output path.
    for (label, ticks) in EmbeddedResources.timings do
      Telemetry.event $"cli.{label}" [ "ms", string (ticksToMs ticks) ]

    use _totalSpan = Telemetry.span "cli.total" []

    // Keep database setup phases visible in cli.total.
    Telemetry.time "cli.seedCheck" [] (fun () ->
      // If data.db is missing but seed.db exists, copy seed as data.db
      let dbPath = LibConfig.Config.dbPath
      let seedPath = System.IO.Path.Combine(LibConfig.Config.runDir, "seed.db")
      if not (System.IO.File.Exists dbPath) && System.IO.File.Exists seedPath then
        System.Console.Error.WriteLine "Copying seed.db as data.db"
        System.IO.File.Copy(seedPath, dbPath))

    // Open the connection separately so its setup cost is measured on its own.
    Telemetry.time "cli.dbConnect" [] LibDB.Sqlite.Sql.warm

    // Grow the database: apply any unapplied ops and evaluate values.
    let cliPackageManager =
      Telemetry.time "cli.createPM" [] (fun () -> LibDB.PackageManager.rt)

    Telemetry.time "cli.growIfNeeded" [] (fun () ->
      (LibDB.Seed.growIfNeeded
        (fun () -> builtinsLazy.Force())
        cliPackageManager
        (fun msg -> System.Console.Error.WriteLine msg))
        .Result
      |> ignore<bool>)

    // Force builtins after seeding so initialization is measured separately.
    Telemetry.time "cli.builtinsInit" [] (fun () ->
      builtinsLazy.Force().fns.Count |> ignore<int>)

    Telemetry.time "cli.pmInit" [] (fun () -> cliPackageManager.init.Result)

    let result =
      Telemetry.time "cli.execute" [] (fun () ->
        let result = execute cliPackageManager (Array.toList args)
        result.Result)

    Telemetry.time "cli.consoleWait" [] NonBlockingConsole.wait

    // Startup metrics are emitted only when telemetry is enabled.

    // Pair the decoded item count with the timing spans.
    Telemetry.counterSnapshot ()
    |> List.iter (fun (name, n) -> Telemetry.event name [ "count", string n ])

    // Collect counters from every VM created by executeFunction.
    if Telemetry.isEnabled () then
      let stats =
        RT.InterpreterStatsSink.all
        |> Seq.choose (fun o ->
          match o with
          | :? RT.InterpreterStats as s -> Some s
          | _ -> None)
        |> Seq.toList

      // Per-opcode allocation across all VMs. Names come from the Instruction DU.
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

      // Per-builtin allocation across all VMs.
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

    // Report allocation and GC totals after the run, not during each instruction.
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
      // Error formatting uses trusted Darklang functions shipped with the CLI.
      let state =
        state cliPackageManager
        |> Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll

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
      // Result formatting uses trusted Darklang functions shipped with the CLI.
      let state =
        state cliPackageManager
        |> Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll
      let output = (Exe.dvalToRepr state dval).Result
      logError $"Error: main function must return an int (returned {output})"
      1


  with e ->
    // A store that cannot be used is an ENVIRONMENT, not a bug: a read-only mount, a store owned by
    // another user, a full disk. `LibDB.Sqlite` raises a `StoreConditionException` carrying a
    // sentence written for whoever ran the command, so all that is left is to print it.
    let rec storeCondition (ex : exn) : Exception.StoreConditionException option =
      match ex with
      | :? Exception.StoreConditionException as s -> Some s
      | :? System.AggregateException as agg ->
        agg.InnerExceptions |> Seq.tryPick storeCondition
      | _ ->
        if isNull ex.InnerException then None else storeCondition ex.InnerException

    match storeCondition e with
    | Some s ->
      System.Console.Error.WriteLine s.Message
      1
    | None ->

      let rec describe (depth : int) (ex : exn) : unit =
        let indent = String.replicate depth "  "
        System.Console.Error.WriteLine
          $"{indent}{ex.GetType().FullName}: {ex.Message}"
        match ex with
        | :? System.AggregateException as agg ->
          for inner in agg.InnerExceptions do
            describe (depth + 1) inner
        | _ ->
          if not (isNull ex.InnerException) then
            describe (depth + 1) ex.InnerException
        if depth = 0 && not (isNull ex.StackTrace) then
          System.Console.Error.WriteLine $"Stack trace:\n{ex.StackTrace}"
      System.Console.Error.WriteLine "Error starting Darklang CLI:"
      describe 0 e
      1
