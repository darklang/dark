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
    uply {
      // A read-only store or a full disk is not an internal error, and dumping the exception tree for one
      // buries the sentence that says what to do about it under thirty lines of stack. Say nothing here
      // rather than say it better: the exception still surfaces as a runtime error, which prints the same
      // sentence, and printing it here too just said everything twice.
      match exn with
      | :? Exception.StoreConditionException -> ()
      | _ -> printException "Internal error" metadata exn
    }

  Exe.createState builtins packageManager Exe.noTracing sendException notify program




/// The CLI entry point is a stored, per-install pointer — `config_v0` key
/// `entry_point`, a package location like `Darklang.Cli.executeCliCommand` —
/// defaulting to the shipped CLI. Malleable + per-user: Feriel and Stachu each `dark
/// config set-entry Mine.myCli` on their own machine and get their own CLI next
/// launch. Stored as a NAME (resolved to a hash here) so it follows the latest
/// content. Any miss falls back to the default — a recovery affordance for a bad
/// pointer.
let private resolveEntryPoint () : RT.FQFnName.FQFnName =
  let defaultFn = RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.executeCliCommand ())
  try
    match (LibDB.Config.get "entry_point").Result with
    | None
    | Some "" -> defaultFn
    | Some loc ->
      match List.rev (loc.Split('.') |> Array.toList) with
      | name :: revRest ->
        let owner, modules =
          match List.rev revRest with
          | o :: mods -> o, mods
          | [] -> "Darklang", []
        let location : PT.PackageLocation =
          { owner = owner; modules = modules; name = name }
        match (LibDB.PackageManager.pt.findFn location).Result with
        | Some fqPkg ->
          RT.FQFnName.Package(
            LibExecution.ProgramTypesToRuntimeTypes.FQFnName.Package.toRT fqPkg
          )
        | None ->
          System.Console.Error.WriteLine
            $"entry point '{loc}' didn't resolve; running the default CLI"
          defaultFn
      | [] -> defaultFn
  with e ->
    System.Console.Error.WriteLine
      $"entry point lookup failed ({e.Message}); running the default CLI"
    defaultFn

let execute
  (packageManager : RT.PackageManager)
  (args : List<string>)
  : Task<RT.ExecutionResult> =
  task {
    // Split out because `cli.execute` turned out to be nearly identical for `status`
    // and `help` despite help running 5x the instructions -- which means most of it
    // is a FIXED cost, not the Dark code running. `state` builds the builtins map
    // and the execution state; this says how much of the fixed cost is that. The
    // branch was resolved from the flag / DARK_BRANCH / config before this ran; hand
    // it to the execution state so everything under it -- including the pretty
    // printers, which turn hashes back into names -- answers for the branch this run
    // is on rather than for main.
    let state =
      Telemetry.time "cli.buildState" [] (fun () ->
        { state packageManager with
            branchId =
              LibDB.PackageManager.currentBranchId () |> Option.defaultValue "" })
    // `--safe` is the recovery floor: ignore the stored `entry_point` and run the
    // shipped default CLI, so a custom root that resolves-but-misbehaves can always
    // be escaped. (A bad/unresolvable pointer already falls back on its own.) The
    // flag is stripped before the args reach the fn.
    let safeMode = List.contains "--safe" args
    // `--safe` is boot-level; strip it so it doesn't reach the entry-point fn as a
    // command arg.
    let args = args |> List.filter (fun a -> a <> "--safe")
    let fnName =
      if safeMode then
        System.Console.Error.WriteLine
          "running in --safe mode: the shipped default CLI"
        RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.executeCliCommand ())
      else
        resolveEntryPoint ()
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
      builtins.fns |> Map.count |> ignore<int>)

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

    // `--branch <id>` / `--branch=<id>`: pick the branch for THIS process. Its delta
    // ops (stored effective=0 in the shared log) overlay core for parse + execute,
    // so `dark --branch <id> eval "<branch fn>"` resolves the branch's own code
    // without switching anything persistently.
    //
    // Both spellings, because every other CLI takes either and supporting one
    // silently makes the other look like a typo rather than a gap. A missing value
    // is an ERROR, not a fall-through to `current_branch`: "you asked for a branch
    // and got main" is the failure this whole surface is supposed to stop producing.
    let branchFlag =
      args
      |> Array.mapi (fun i a -> (i, a))
      |> Array.tryPick (fun (i, a) ->
        if a = "--branch" then
          // The next token, unless it's another flag. `--branch --json status` used
          // to create a branch NAMED `--json` and run a non-JSON command on it; a
          // forgotten value should be the error the missing-value case already
          // gives.
          if i + 1 < args.Length && not (args[i + 1].StartsWith "-") then
            Some(Ok(args[i + 1]), i, 2)
          else
            Some(Error(), i, 1)
        elif a.StartsWith "--branch=" then
          let v = a.Substring "--branch=".Length
          if v <> "" then Some(Ok v, i, 1) else Some(Error(), i, 1)
        else
          None)

    match branchFlag with
    | Some(Error(), _, _) ->
      System.Console.Error.WriteLine
        "--branch needs a branch id: `dark --branch <id> <command>` (or `--branch=<id>`)"
      exit 1
    | _ -> ()

    // All three tiers below name a branch the way a person does, by NAME. This is
    // where a name becomes the id everything underneath refers to; nothing past this
    // point resolves a name again.
    //
    // A name we don't have is CREATED, not refused: spinning up a branch without a
    // separate `switch` is the point of the flag, and it's how concurrent agents are
    // meant to work. It says so on stderr, though. Running silently on a branch that
    // doesn't exist yet is indistinguishable from running on the one you meant, so a
    // typo would otherwise read as success.
    let resolveName (name : string) (announceAs : string) : Option<string> =
      // `main` is spelled as the absence of a branch, so accept it as the way back.
      if name = "main" then
        None
      else
        // Same three spellings `dark switch` takes: a name, a full id, or an
        // unambiguous id PREFIX. That last one matters because every listing
        // abbreviates ids to 8 characters, so the thing you would paste matched
        // nothing here and created a new empty branch named after the abbreviation.
        match (LibDB.Branches.lookupRef name).Result with
        | Some id -> Some id
        | None ->
          let (id, created) = (LibDB.Branches.resolveOrCreate name "main").Result
          if created then
            System.Console.Error.WriteLine $"created branch '{name}'{announceAs}"
          Some id

    let branchId =
      match branchFlag with
      | Some(Ok name, _, _) -> resolveName name ""
      | _ ->
        // No --branch: DARK_BRANCH, then the persistent `current_branch`.
        //
        // Three tiers, each scoped tighter than the one below, which is the point of
        // having three: the FLAG is this command, DARK_BRANCH is this SHELL, and the
        // config is this machine. The env tier is what lets several agents work on
        // several branches at once without fighting over one config key -- each
        // exports DARK_BRANCH in its own shell and they never stomp each other.
        // `dark switch` writes the config, so without the env tier the only way to
        // run concurrent branches is to pass the flag on literally every command.
        let fromEnv =
          match System.Environment.GetEnvironmentVariable "DARK_BRANCH" with
          | null -> None
          | "" -> None
          | name -> Some name

        match fromEnv with
        | Some name -> resolveName name " (DARK_BRANCH)"
        | None ->
          // The config holds the branch's ID, and `current_branch_name` holds its
          // name alongside for anyone reading the file. The id is what makes this
          // tier safe: names are deliberately NOT unique, including across
          // instances, and resolution by name takes the most recent branch under
          // that name -- so a sync that brought in a peer's `fix-auth` could
          // silently move you onto theirs.
          //
          // A NAME is still accepted, because stores written before this hold one.
          // It is a fallback, not the format.
          //
          // Either way this resolves without creating, and degrades to main rather
          // than failing every command, but says so: an edit landing somewhere you
          // didn't ask for is worse than an error.
          match (LibDB.Config.get "current_branch").Result with
          | Some stored when stored <> "" && stored <> "main" ->
            if (LibDB.Branches.isLive stored).Result then
              Some stored
            else
              match (LibDB.Branches.liveIdForName stored).Result with
              | Some id -> Some id
              | None ->
                let label =
                  match (LibDB.Config.get "current_branch_name").Result with
                  | Some name when name <> "" -> name
                  | _ -> stored
                System.Console.Error.WriteLine
                  $"current branch '{label}' is gone (archived or merged); running on main"
                None
          | _ -> None

    // Strip the flag (and its value, for the space form) so it never reaches the
    // entry-point fn as a positional argument.
    let args =
      match branchFlag with
      | Some(_, i, width) ->
        Array.append
          (Array.sub args 0 i)
          (Array.sub args (i + width) (args.Length - i - width))
      | None -> args

    LibDB.PackageManager.selectBranch branchId

    let result =
      Telemetry.time "cli.execute" [] (fun () ->
        let result = execute cliPackageManager (Array.toList args)
        result.Result)

    Telemetry.time "cli.consoleWait" [] NonBlockingConsole.wait

    // Startup instrumentation. All of it is inert when telemetry is off; read it with
    // `scripts/testing/view-telemetry.py`.

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
      byBuiltin
      |> Seq.sortByDescending (fun kv -> kv.Value)
      |> Seq.truncate 20
      |> Seq.iter (fun kv ->
        Telemetry.event $"builtinAlloc.{kv.Key}" [ "bytes", string kv.Value ])

      for i in 0 .. min (RT.ApplyStage.names.Length - 1) 31 do
        let total = stats |> List.sumBy (fun s -> s.allocByStage[i])
        if total > 0L then
          Telemetry.event
            $"applyStage.{RT.ApplyStage.names[i]}"
            [ "bytes", string total ]

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

    // Startup instrumentation. All of it is inert when telemetry is off; read it with
    // `scripts/testing/view-telemetry.py`. What the numbers said, and what's still
    // unexplained, is in notes/fresh-arch/cli-startup-cost.md -- deliberately not
    // inlined here, because a measurement pinned to one commit goes stale in a
    // comment and nobody updates it.

    // How many package items this run actually decoded. Emitted alongside the spans
    // because per-item cost and item count are useless separately.
    Telemetry.counterSnapshot ()
    |> List.iter (fun (name, n) -> Telemetry.event name [ "count", string n ])

    // Total the interpreter counters across every VM this run created. A VM is
    // per-`executeFunction` and the stats hang off it, so without the sink the
    // object is gone before anything could ask.
    if Telemetry.isEnabled () then
      let stats =
        RT.InterpreterStatsSink.all
        |> Seq.choose (fun o ->
          match o with
          | :? RT.InterpreterStats as s -> Some s
          | _ -> None)
        |> Seq.toList

      Telemetry.event
        "vm.stats"
        [ "vms", string (List.length stats)
          "instructions", string (stats |> List.sumBy (fun s -> s.instructionCount))
          "builtinCalls", string (stats |> List.sumBy (fun s -> s.builtinCallCount))
          "packageCalls", string (stats |> List.sumBy (fun s -> s.packageCallCount))
          "framePushes", string (stats |> List.sumBy (fun s -> s.framePushCount)) ]

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
        // "Function <64 hex chars> couldn't be found" almost always means the STORE
        // is older than the binary: package code was reloaded, every hash moved, and
        // this database still points at the old ones. The raw message sends people
        // looking for a missing function that was never missing. It's the most
        // common failure in the dev loop and it costs an hour every time until
        // you've seen it once.
        let staleStoreHint =
          if
            s.Contains "couldn't be found"
            && System.Text.RegularExpressions.Regex.IsMatch(s, "[0-9a-f]{32}")
          then
            "\n\nThis usually means the store is older than the binary: package code was reloaded and the "
            + "hashes moved.\n  Run `scripts/build/reload-packages` to bring the store up to this binary, "
            + "or point DARK_CONFIG_RUNDIR at a freshly-cloned store."
          else
            ""

        logError
          $"Encountered a Runtime Error:\n{s}{staleStoreHint}\n\n{errorCallStackStr}\n  "

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
    // A store that cannot be used is an ENVIRONMENT, not a bug: a read-only mount, a store owned by
    // another user, a full disk. `LibDB.Sqlite` recognises those three and raises a
    // `StoreConditionException` carrying a sentence written for whoever ran the command, so all that is
    // left here is to print it. Everything else falls through to the dump below, which is still the right
    // response to something genuinely unexpected.
    let rec storeCondition (ex : exn) : Exception.StoreConditionException option =
      match ex with
      | :? Exception.StoreConditionException as s -> Some s
      | :? System.AggregateException as agg ->
        agg.InnerExceptions |> Seq.tryPick storeCondition
      | _ -> if isNull ex.InnerException then None else storeCondition ex.InnerException

    match storeCondition e with
    | Some s ->
      System.Console.Error.WriteLine s.Message
      1
    | None ->

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
