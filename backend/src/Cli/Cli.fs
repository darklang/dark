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
  // These two values are created during the build, in Cli.fsproj.
  let buildDate = buildAttributes.Key
  let gitHash = buildAttributes.Value
  { hash = gitHash; buildDate = buildDate; inDevelopment = inDevelopment }


// ---------------------
// Execution
// ---------------------

/// Deferred deliberately, and this must stay a `lazy`.
///
/// Constructing the builtins resolves PackageRefs, and on a first run the hash file is still empty
/// at that point -- `Seed.growIfNeeded` regenerates it. A plain module-level value is built by F#'s
/// per-file static initializer, before `main` runs at all, so every ref would resolve to "" and the
/// builtins would disagree with the freshly grown package DB. Force it after the grow.
let private builtinsLazy : Lazy<RT.Builtins> =
  lazy
    // User scripts get branch-specific context via cliParseAndExecuteScript.
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
      // A read-only store or a full disk is not an internal error, and dumping the exception tree
      // buries the sentence that says what to do about it. The exception still surfaces as a runtime
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
    program




/// The CLI entry point is a stored, per-install pointer: `config_v0` key `entry_point`, a package
/// location like `Darklang.Cli.executeCliCommand`, defaulting to the shipped CLI. Stored as a NAME
/// (resolved to a hash here) so it follows the latest content. Any miss falls back to the default.
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
    // The branch is resolved from the flag / DARK_BRANCH / config before this runs; handing it to
    // the execution state is what makes everything underneath -- including the pretty printers,
    // which turn hashes back into names -- answer for this run's branch rather than for main.
    let state =
      Telemetry.time "cli.buildState" [] (fun () ->
        { state packageManager with
            branchId = LibDB.PackageManager.currentBranchId () })
    // `--safe` is the recovery floor: ignore the stored `entry_point` and run the shipped default
    // CLI, so a custom root that resolves-but-misbehaves can always be escaped. (A bad pointer
    // already falls back on its own.)
    let safeMode = List.contains "--safe" args
    // Boot-level; strip it so it doesn't reach the entry-point fn as a command arg.
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
    // Sampled before anything else in the process, including the environment read below. It is one
    // half of `cli.preMain`, and the other half -- the process start time -- costs milliseconds to
    // obtain, so taking this first is what keeps the measurement from including itself.
    let mainEntry = System.DateTime.UtcNow

    // How long the process took to reach here. `cli.total` starts after resource extraction and can't
    // see runtime init, assembly loading or JIT of the startup path, which is a large share of a short
    // command. Wall clock rather than Stopwatch, because the only fixed point is when the OS started us.
    let telemetryEnabled =
      match System.Environment.GetEnvironmentVariable "DARK_TELEMETRY" with
      | "1" -> true
      | _ -> false

    let preMainMs =
      if telemetryEnabled then
        // `Process.GetCurrentProcess()` reads /proc and initialises the Process machinery, which is
        // not free on a command this short. It sits after `mainEntry` deliberately: it is the cost of
        // reading the clock, not part of the startup being measured.
        let processStart =
          System.Diagnostics.Process.GetCurrentProcess().StartTime.ToUniversalTime()
        int64 (mainEntry - processStart).TotalMilliseconds
      else
        0L

    // Extract embedded resources FIRST: this sets DARK_CONFIG_RUNDIR, which
    // LibConfig.Config needs to resolve paths correctly.
    let extractStart = System.Diagnostics.Stopwatch.GetTimestamp()
    EmbeddedResources.extract ()
    let extractTicks = System.Diagnostics.Stopwatch.GetTimestamp() - extractStart
    initSerializers ()

    // Now safe to access LibConfig paths. Gated on DARK_TELEMETRY, the same switch the Dark side
    // reads (`initState` in cli/core.dark), so both halves turn on together. Unconditional init would
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

    // Named so the phases inside `cli.total` sum to it, rather than landing in "the rest".
    Telemetry.time "cli.seedCheck" [] (fun () ->
      // If data.db is missing but seed.db exists, copy seed as data.db
      let dbPath = LibConfig.Config.dbPath
      let seedPath = System.IO.Path.Combine(LibConfig.Config.runDir, "seed.db")
      if not (System.IO.File.Exists dbPath) && System.IO.File.Exists seedPath then
        System.Console.Error.WriteLine "Copying seed.db as data.db"
        System.IO.File.Copy(seedPath, dbPath))

    // Separated from `growIfNeeded` so the first connection open and its PRAGMA round trip are attributable
    // to themselves rather than to whichever query happened to run first.
    Telemetry.time "cli.dbConnect" [] LibDB.Sqlite.Sql.warm

    // Where the unguarded HTTP transport may be pointed. Two sources, both of them local intent: a URL on
    // this command line, and the relay this instance stored. Everything else -- notably a package pulled
    // from a peer -- gets the guarded client, which bans loopback and RFC-1918 as usual.
    //
    // The stored side is a lookup rather than a snapshot because `dark sync setup` writes the relay and
    // pushes to it inside one process, so a value read here would be one command too late.
    LibExecution.SyncTargets.setFromArgv args
    LibExecution.SyncTargets.setStoredLookup (fun () ->
      [ "sync.relay" ]
      |> List.choose (fun key ->
        try
          (LibDB.Config.get key).Result
        with _ ->
          None)
      |> List.filter (fun v -> v <> ""))

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

    // After the grow, never before: see the comment on `builtinsLazy`. Forced explicitly so its cost
    // lands in a span of its own rather than inside `cli.execute`.
    Telemetry.time "cli.builtinsInit" [] (fun () ->
      builtinsLazy.Force().fns.Count |> ignore<int>)

    // After the fold, and only after it: an edit of yours that the build also ships has just lost
    // the name to the build's newer stamp, and this puts it back. Said out loud, since it is a
    // default rather than anything you asked for.
    Telemetry.time "cli.keepLocalEdits" [] (fun () ->
      match EmbeddedResources.locallyAuthored with
      | [] -> ()
      | held ->
        let kept = (LibDB.UpgradeKeep.restore held).Result

        // Named separately because one edit to a core function repoints hundreds of callers, and a list
        // led by arbitrary repoints reads like a disaster rather than like "your draft survived".
        let edited = kept |> List.filter (fun k -> k.source <> "propagation")
        let followed = List.length kept - List.length edited

        if kept <> [] then
          let name (k : LibDB.UpgradeKeep.Kept) =
            let mods = String.concat "." k.location.modules
            if mods = "" then
              $"{k.location.owner}.{k.location.name}"
            else
              $"{k.location.owner}.{mods}.{k.location.name}"

          let shown =
            (if edited = [] then kept else edited)
            |> List.map name
            |> List.truncate 3
            |> String.concat ", "

          let more =
            let n =
              (if edited = [] then List.length kept else List.length edited) - 3
            if n > 0 then $" and {n} more" else ""

          let cascade =
            if followed > 0 && edited <> [] then
              let it = if List.length edited = 1 then "it" else "them"
              $", plus {followed} that followed {it}"
            else
              ""

          System.Console.Error.WriteLine
            $"This build ships a different version of {shown}{more}{cascade}. Kept yours; \
              the build's is still in the log.")

    Telemetry.time "cli.pmInit" [] (fun () -> cliPackageManager.init.Result)

    // `--branch <id>` / `--branch=<id>`: pick the branch for THIS process. Its delta ops (stored
    // effective=0 in the shared log) overlay core for parse and execute, so nothing is switched
    // persistently. Both spellings, because every other CLI takes either. A missing value is an
    // ERROR, not a fall-through to `current_branch`.
    let branchFlag =
      args
      |> Array.mapi (fun i a -> (i, a))
      |> Array.tryPick (fun (i, a) ->
        if a = "--branch" then
          // The next token, unless it's another flag: `--branch --json status` must
          // not create a branch NAMED `--json`.
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

    // All three tiers below name a branch by NAME. This is where a name becomes the id everything
    // underneath refers to; nothing past this point resolves a name again. A name we don't have is
    // CREATED, not refused, and says so on stderr, because a typo would otherwise read as success.
    let resolveName (name : string) (announceAs : string) : Option<PT.BranchId> =
      // `main` is spelled as the absence of a branch, so accept it as the way back.
      if name = PT.BranchId.MainName then
        None
      else
        // Same three spellings `dark switch` takes: a name, a full id, or an unambiguous
        // id PREFIX. Listings abbreviate ids to 8 characters, so the prefix is what you
        // would actually paste.
        match (LibDB.Branches.lookupRef name).Result with
        | Some id -> Some id
        | None ->
          let (id, created) =
            (LibDB.Branches.resolveOrCreate name PT.BranchId.Main).Result
          if created then
            System.Console.Error.WriteLine $"created branch '{name}'{announceAs}"
          Some id

    let branchId =
      match branchFlag with
      | Some(Ok name, _, _) -> resolveName name ""
      | _ ->
        // No --branch: DARK_BRANCH, then the persistent `current_branch`. Three tiers,
        // each scoped tighter than the one below: the FLAG is this command, DARK_BRANCH
        // is this SHELL, and the config is this machine. The env tier is what lets
        // several agents work on several branches at once without fighting over one
        // config key.
        let fromEnv =
          match System.Environment.GetEnvironmentVariable "DARK_BRANCH" with
          | null -> None
          | "" -> None
          | name -> Some name

        match fromEnv with
        | Some name -> resolveName name " (DARK_BRANCH)"
        | None ->
          // The config holds the branch's ID; `current_branch_name` holds its name
          // alongside for anyone reading the file. The id is what makes this tier safe:
          // names are deliberately NOT unique across instances, so resolving by name
          // could silently move you onto a peer's `fix-auth`. A NAME is still accepted,
          // as a fallback. Either way this resolves without creating, and degrades to
          // main rather than failing, but says so.
          match (LibDB.Config.get "current_branch").Result with
          | Some stored when stored <> "" ->
            // Stored text, so it can be an id, a name, or something a previous build wrote. Anything
            // that isn't a live branch degrades to main and says so.
            let asId = PT.BranchId.Parse stored

            match asId with
            | Some id when id = PT.BranchId.Main -> None
            | Some id when (LibDB.Branches.isLive id).Result -> Some id
            | _ ->
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

    LibDB.PackageManager.selectBranch (
      branchId |> Option.defaultValue PT.BranchId.Main
    )

    let result =
      Telemetry.time "cli.execute" [] (fun () ->
        let result = execute cliPackageManager (Array.toList args)
        result.Result)

    Telemetry.time "cli.consoleWait" [] NonBlockingConsole.wait

    // Startup instrumentation, inert when telemetry is off; read it with
    // `scripts/perf/view-telemetry.py`. The counters say how many package items this run decoded,
    // which is only useful next to the spans.
    Telemetry.counterSnapshot ()
    |> List.iter (fun (name, n) -> Telemetry.event name [ "count", string n ])

    // Total the interpreter counters across every VM. A VM is per-`executeFunction` and the stats
    // hang off it, so without the sink the object is gone before anything could ask.
    if Telemetry.isEnabled () then
      let stats =
        RT.InterpreterStatsSink.all
        |> Seq.choose (fun o ->
          match o with
          | :? RT.InterpreterStats as s -> Some s
          | _ -> None)
        |> Seq.toList

      // Per-opcode allocation. Names come from reflection over the Instruction DU, so tag order
      // can't drift out of sync with a hand-written list.
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

      // Per-builtin allocation. Nearly all of what the process allocates happens inside builtin
      // bodies, not the interpreter around them.
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

    // Allocation per instruction, to separate "a Dval per operation" from "the async state machine
    // per operation". Process-total, so it costs one call at exit; GC counts come along because
    // collection pauses would show up as neither.
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

    // Exit codes are bounded; narrow safely rather than letting an out-of-Int32 result throw.
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
        // "Function <64 hex chars> couldn't be found" almost always means the STORE is
        // older than the binary: package code was reloaded, every hash moved, and this
        // database still points at the old ones.
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
