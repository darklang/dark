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
    // Load bundled Darklang function hashes once for package-approval checks.
    let! bundled = LibDB.ProgramTypes.Fn.hashesOwnedBy "Darklang" |> Ply.toTask
    let state =
      // CLI control code is trusted; guest `run`/`eval` create restricted states.
      { Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll state with
          // CLI control code may manage the instance policy.
          canManagePolicies = true
          // The sync transport runs from many entry points -- the sync verbs, `branch
          // push/pull`, the auto-sync daemon (launched as `eval`), and the workbench --
          // so the host state grants it broadly. Guest isolation does not rest on this
          // flag: `PolicyStore.guestState` always clears it, and `requireBundledCaller`
          // additionally demands every frame be bundled Darklang code.
          canUsePrivateNetworkHttp = true
          isBundledPackageFn = fun (RT.Hash h) -> bundled.Contains h }
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

    // Extract embedded resources FIRST: this sets DARK_CONFIG_RUNDIR, which
    // LibConfig.Config needs to resolve paths correctly.
    let extractStart = System.Diagnostics.Stopwatch.GetTimestamp()
    EmbeddedResources.extract ()
    let extractTicks = System.Diagnostics.Stopwatch.GetTimestamp() - extractStart
    initSerializers ()

    // Extraction established the rundir, so policy paths are safe to use. The policy belongs to
    // the INSTANCE (see `setPolicyDirectory`), which is what the rundir is; for an ordinary
    // install that is `~/.darklang`, so this is the same file it always was.
    LibExecution.HostSecurity.setPolicyDirectory (
      System.IO.Path.Combine(LibConfig.Config.runDir, "policy")
    )

    try
      LibDB.PolicyStore.seedInstanceIfMissing
        LibExecution.Permissions.Policy.defaultInstance
    with e ->
      eprintfn
        "warning: could not initialize %s (%s); host effects are denied this run"
        (System.IO.Path.Combine(LibConfig.Config.runDir, "policy"))
        e.Message

    // Prevent scoped guest file operations from targeting the package store.
    LibExecution.HostSecurity.setPackageDbPath LibConfig.Config.dbPath

    // Record host-operation decisions at the boundary.
    installAuditLog ()


    // Now safe to access LibConfig paths. Gated on DARK_TELEMETRY, the same switch the Dark side
    // reads (`initState` in cli/core.dark), so both halves turn on together. Unconditional init would
    // also leave per-instruction counting on in the hot loop for every run.
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

    // Named so the phases inside `cli.total` sum to it, rather than landing in "the rest".
    Telemetry.time "cli.seedCheck" [] (fun () ->
      // If data.db is missing but seed.db exists, copy seed as data.db
      let dbPath = LibConfig.Config.dbPath
      let seedPath = System.IO.Path.Combine(LibConfig.Config.runDir, "seed.db")
      if not (System.IO.File.Exists dbPath) && System.IO.File.Exists seedPath then
        System.Console.Error.WriteLine "Copying seed.db as data.db"
        System.IO.File.Copy(seedPath, dbPath))

    // Open the connection separately so its setup cost is measured on its own.
    Telemetry.time "cli.dbConnect" [] LibDB.Sqlite.Sql.warm

    // The transport attaches the write secret itself: the credential must not reach
    // Dark, where any code the CLI runs could read it. Keyed by ORIGIN so it matches
    // however the caller spelled the url, and read per request because
    // `dark sync setup` stores it and pushes in one process. (WHERE the transport may
    // reach is the instance policy's decision now; the old origin allowlist is gone.)
    let storedRelay () : Option<string> =
      try
        match (LibDB.Config.get "sync.relay").Result with
        | Some url when url <> "" -> Some url
        | _ -> None
      with _ ->
        None

    LibExecution.UnguardedOrigins.setSecretLookup (fun origin ->
      try
        match storedRelay () with
        | Some stored when
          LibExecution.UnguardedOrigins.originOf stored = Some origin
          ->
          (LibDB.Config.get (LibDB.Config.secretPrefix + stored)).Result
        | _ -> None
      with _ ->
        None)

    // Grow the database: apply any unapplied ops and evaluate values.
    let cliPackageManager =
      Telemetry.time "cli.createPM" [] (fun () -> LibDB.PackageManager.rt)

    // Reads elsewhere TOLERATE an op they cannot decode, because a synced store legitimately holds a
    // peer's newer ops. This is the other case: everything here is unreadable, so there is nothing to
    // tolerate and the only honest answer is to say so and stop.
    let rec isBinaryFormatFailure (e : exn) : bool =
      match e with
      | null -> false
      | :? LibSerialization.Binary.BaseFormat.BinaryFormatException -> true
      | :? System.AggregateException as agg ->
        agg.InnerExceptions |> Seq.exists isBinaryFormatFailure
      | e -> isBinaryFormatFailure e.InnerException

    Telemetry.time "cli.growIfNeeded" [] (fun () ->
      try
        (LibDB.Seed.growIfNeeded
          // Bounded by the operator's instance policy. The store can hold values
          // that arrived by import or sync, and this is where they first run.
          LibDB.Seed.EvaluationAuthority.underInstancePolicy
          (fun () -> builtinsLazy.Force())
          cliPackageManager
          (fun msg -> System.Console.Error.WriteLine msg))
          .Result
        |> ignore<bool>
      with ex when isBinaryFormatFailure ex ->
        let path = LibConfig.Config.dbPath

        [ "This store was written by a build whose serialization format differs from this one, so"
          "none of it can be read. Retrying will not help."
          ""
          "  Move it aside and a fresh store grows from this build's seed:"
          ""
          $"    mv {path} {path}.old"
          ""
          "  Anything only in that store is still in the `.old` copy, readable by the build that"
          "  wrote it." ]
        |> List.iter System.Console.Error.WriteLine

        exit 1)

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

    // `--branch <branch>` / `--branch=<branch>`, a name, an id or an id prefix: pick the branch for
    // THIS process. Its delta ops (stored effective=0 in the shared log) overlay core for parse and
    // execute, so nothing is switched persistently. Both spellings, because every other CLI takes
    // either. A missing value is an ERROR, not a fall-through to `current_branch`.
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
        "--branch needs a branch name or id: `dark --branch <branch> <command>` (or `--branch=<branch>`)"
      exit 1
    | _ -> ()

    // Which branch this process runs on: `--branch`, then `DARK_BRANCH`, then the stored
    // `current_branch`. The order lives in `LibDB.BranchSelection`, where it has a test; this is where
    // the outcome gets SAID. A name we don't have is created and announced, because a typo would
    // otherwise read as success; a foreign uuid or an ambiguous prefix is refused, for the same reason.
    let flagName =
      match branchFlag with
      | Some(Ok name, _, _) -> Some name
      | _ -> None

    let envName =
      match System.Environment.GetEnvironmentVariable "DARK_BRANCH" with
      | null
      | "" -> None
      | name -> Some name

    let branchId =
      match (LibDB.BranchSelection.select flagName envName).Result with
      | Error(LibDB.BranchSelection.AmbiguousPrefix prefix) ->
        System.Console.Error.WriteLine
          $"'{prefix}' matches more than one branch; use more of the id, or its name"
        exit 1
      | Error(LibDB.BranchSelection.UnknownId id) ->
        System.Console.Error.WriteLine
          $"no branch with id {id} in this store; `dark branches` lists yours"
        exit 1
      | Ok selection ->
        selection.created
        |> Option.iter (fun name ->
          let via =
            if selection.tier = LibDB.BranchSelection.Env then
              " (DARK_BRANCH)"
            else
              ""
          System.Console.Error.WriteLine $"created branch '{name}'{via}")
        selection.goneStored
        |> Option.iter (fun label ->
          System.Console.Error.WriteLine
            $"current branch '{label}' is gone (archived or merged); now on main")
        selection.branchId

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
      // Error formatting uses trusted Darklang functions shipped with the CLI.
      let state =
        state cliPackageManager
        |> Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll

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
