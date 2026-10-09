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




/// A dotted package name (`Darklang.Stdlib.Exec.Policy.roundRobin`) as the function it names,
/// or `None` when nothing in the store has it. Owner `Darklang` when the name has no owner.
let private resolvePackageFn (dotted : string) : Option<RT.FQFnName.FQFnName> =
  match List.rev (dotted.Split('.') |> Array.toList) with
  | name :: revRest ->
    let owner, modules =
      match List.rev revRest with
      | o :: mods -> o, mods
      | [] -> "Darklang", []
    let location : PT.PackageLocation =
      { owner = owner; modules = modules; name = name }
    (LibDB.PackageManager.pt.findFn location).Result
    |> Option.map (fun fqPkg ->
      RT.FQFnName.Package(
        LibExecution.ProgramTypesToRuntimeTypes.FQFnName.Package.toRT fqPkg
      ))
  | [] -> None

/// The CLI entry point is a stored, per-install pointer: `config_v0` key `entry_point`, a package
/// location like `Darklang.Cli.executeCliCommand`, defaulting to the shipped CLI. Stored as a NAME
/// (resolved to a hash here) so it follows the latest content. Any miss falls back to the default.
let private resolveEntryPoint
  (settings : Map<string, string>)
  : RT.FQFnName.FQFnName =
  let defaultFn = RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.executeCliCommand ())
  try
    match Map.tryFind "entry_point" settings with
    | None
    | Some "" -> defaultFn
    | Some loc ->
      match resolvePackageFn loc with
      | Some fn -> fn
      | None ->
        System.Console.Error.WriteLine
          $"entry point '{loc}' didn't resolve; running the default CLI"
        defaultFn
  with e ->
    System.Console.Error.WriteLine
      $"entry point lookup failed ({e.Message}); running the default CLI"
    defaultFn

/// The store-change source for the scheduler's poll: `LibDB.Sqlite.DataVersion`, one held
/// connection per store, since `PRAGMA data_version` answers per connection.
let private installStoreVersionSource () : unit =
  LibExecution.HostEvents.sources.storeVersion <-
    Some LibDB.Sqlite.DataVersion.current

/// The store's startup settings, read in one query (each `Config.get` is a round trip of about
/// 8 KB, and the allocation gate counts startup): the entry point, the `exec.*` knobs and the
/// `trace.*` caps. Empty when the store cannot answer. All are `dark config set`; no environment
/// variable shadows any of them.
let private startupSettings () : Map<string, string> =
  try
    (LibDB.Config.getMany
      [ "entry_point"
        "exec.workers"
        "exec.policy"
        "exec.maxInstructions"
        "exec.maxBytes"
        "exec.storePollMs"
        "exec.spreadCrossover"
        "exec.spreadMinChunk"
        "exec.spreadPredict"
        "trace.keep"
        "trace.maxMb"
        "trace.record" ])
      .Result
  with _ ->
    Map.empty

/// How many worker schedulers this run may start: the store's `exec.workers`, else one per
/// core. Never below one.
let private workerCount (settings : Map<string, string>) : int =
  match Map.tryFind "exec.workers" settings with
  | Some s ->
    match System.Int32.TryParse s with
    | true, n when n >= 1 -> n
    | _ -> max 1 System.Environment.ProcessorCount
  | None -> max 1 System.Environment.ProcessorCount

/// The scheduling policy, an expert setting: `exec.policy` names a Dark function
/// (`Darklang.Stdlib.Exec.Policy.youngestFirst`, say) that is asked which runnable process to
/// step next whenever there is a choice; unset, the scheduler round-robins in F# and never
/// asks. A name that does not resolve is said and ignored, like a bad entry point.
let private installPolicy
  (settings : Map<string, string>)
  (state : RT.ExecutionState)
  : unit =
  let named = Map.tryFind "exec.policy" settings |> Option.defaultValue ""
  if named <> "" then
    match resolvePackageFn named with
    | Some fn ->
      LibExecution.Scheduler.policy <-
        LibExecution.Scheduler.Chooser(
          Builtins.Language.Libs.Exec.chooserFor state fn
        )
    | None ->
      System.Console.Error.WriteLine(
        Builtins.Language.Libs.Exec.policyComplaint
          named
          "did not resolve to a function"
      )

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
          isBundledPackageFn = fun (RT.Hash h) -> bundled.Contains h
          // What a list op asks before spreading a package fn across cores (`LibExecution.Spread`).
          fnPurity =
            LibDB.PackagePermissions.purity
              LibDB.PackagePermissions.Load.fromStore
              state.fns.builtIn }
    // `--safe` is the recovery floor: ignore the stored `entry_point` and run the shipped default
    // CLI, so a custom root that resolves-but-misbehaves can always be escaped. (A bad pointer
    // already falls back on its own.)
    let safeMode = List.contains "--safe" args
    // Boot-level; strip it so it doesn't reach the entry-point fn as a command arg.
    let args = args |> List.filter (fun a -> a <> "--safe")
    let settings = startupSettings ()
    let fnName =
      if safeMode then
        System.Console.Error.WriteLine
          "running in --safe mode: the shipped default CLI"
        RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.executeCliCommand ())
      else
        resolveEntryPoint settings
    let args =
      args |> List.map RT.DString |> Dval.list RT.KTString |> NEList.singleton
    // The CLI's top level is a process: the scheduler runs on this thread until it finishes,
    // stepping whatever else gets spawned meanwhile (a script under `eval`, an `apps` daemon).
    // `DARK_SCHEDULER=off` is a bisect switch back to a plain run, for finding out whether an
    // oddity is the scheduler's; not a setting, not documented for users.
    if System.Environment.GetEnvironmentVariable "DARK_SCHEDULER" = "off" then
      let! result = Exe.executeFunction state fnName [] args
      return result
    else
      installStoreVersionSource ()
      LibExecution.Scheduler.defaultWorkers <- workerCount settings
      let cap (key : string) : int64 =
        match Map.tryFind key settings with
        | Some v ->
          match System.Int64.TryParse v with
          | true, n when n >= 0L -> n
          | _ -> 0L
        | None -> 0L
      // How often a live view looks for a change. Left alone it is the 200 ms the scheduler
      // ships with; a value under 10 ms is refused there rather than here, because a busy loop
      // on a timer is not a setting anyone means.
      match Map.tryFind "exec.storePollMs" settings with
      | Some v ->
        match System.Int32.TryParse v with
        | true, n when n >= 10 -> LibExecution.Scheduler.storePollMs <- n
        | _ -> ()
      | None -> ()
      // List ops spreading across cores (`LibExecution.Spread`): interpreted instructions of
      // projected serial work before a spread pays, and the least a chunk should carry. Expert
      // settings, for measuring; a negative crossover turns spreading off.
      match Map.tryFind "exec.spreadCrossover" settings with
      | Some v ->
        match System.Int64.TryParse v with
        | true, n -> LibExecution.Spread.crossover <- (if n < 0L then -1L else n)
        | _ -> ()
      | None -> ()
      match Map.tryFind "exec.spreadPredict" settings with
      | Some "on" -> LibExecution.Spread.predicting <- true
      | Some "off" -> LibExecution.Spread.predicting <- false
      | _ -> ()
      match Map.tryFind "exec.spreadMinChunk" settings with
      | Some v ->
        match System.Int64.TryParse v with
        | true, n when n >= 0L -> LibExecution.Spread.minChunk <- n
        | _ -> ()
      | None -> ()
      // `DARK_SPREAD_REPORT=1` says at exit how many spreads ran and how many fell back: the way to
      // tell a run that spread from one that matched serial without spreading. A diagnostic
      // switch like `DARK_SCHEDULER`, not a setting.
      if System.Environment.GetEnvironmentVariable "DARK_SPREAD_REPORT" = "1" then
        // Not `eprintfn`: printf formats by reflection, which the AOT binary does not have.
        System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
          System.Console.Error.WriteLine(
            $"spread: {LibExecution.Spread.spreads} spreads, "
            + $"{LibExecution.Spread.fallbacks} fell back; predicted "
            + $"{LibExecution.Spread.predictedPure} pure, "
            + $"{LibExecution.Spread.predictedImpure} impure, "
            + $"{LibExecution.Spread.predictedUnknown} unknown, "
            + $"{LibExecution.Spread.predictionFailures} analyses failed"
          ))
      LibExecution.Scheduler.maxInstructions <- cap "exec.maxInstructions"
      LibExecution.Scheduler.maxBytes <- cap "exec.maxBytes"
      LibDB.Tracing.TraceRetention.configure
        (Map.tryFind "trace.keep" settings)
        (Map.tryFind "trace.maxMb" settings)
      LibDB.Tracing.TraceDetail.configure (Map.tryFind "trace.record" settings)
      installPolicy settings state
      return LibExecution.Scheduler.executeFunction state fnName [] args
  }

let initSerializers () = ()

/// Ctrl-C while a traced `run` or `eval` is in the foreground: stop what it spawned, store its
/// log as it stands, mark the run suspended, say how to take it up again, and leave.
/// `dark traces resume <id>` then runs the same input, answering every call the log has instead
/// of performing it, and goes live where the log ends. With nothing in the foreground (no
/// traced run, or a TUI reading keys, which takes Ctrl-C as input and never gets here), the
/// process just ends as it always did.
///
/// The children go first, and politely. A process cancelled this way finishes what it already
/// handed the host -- the write in flight lands -- and then stops at its next turn, which is
/// what `Exec.cancel` means everywhere else. Without this, Ctrl-C flushed the log while
/// children were mid-call and then killed them by process exit, so the log ended at an
/// arbitrary point and a resume re-did work that had half happened. A quarter of a second is
/// the whole budget: this is an interrupt, and the person is waiting.
let private stopChildrenPolitely () : unit =
  try
    let sched = LibExecution.Scheduler.Scheduler.CurrentOrShared
    let live =
      sched.Snapshot()
      |> List.filter (fun p ->
        match p.status with
        | LibExecution.Scheduler.Runnable
        | LibExecution.Scheduler.Parked _ -> true
        | _ -> false)
    for p in live do
      sched.Cancel p.id |> ignore<bool>
    if not (List.isEmpty live) then
      let deadline = System.DateTime.UtcNow.AddMilliseconds 250.0
      let stillGoing () =
        sched.Snapshot()
        |> List.exists (fun p ->
          match p.status with
          | LibExecution.Scheduler.Runnable
          | LibExecution.Scheduler.Parked _ -> true
          | _ -> false)
      while System.DateTime.UtcNow < deadline && stillGoing () do
        System.Threading.Thread.Sleep 10
  with _ ->
    ()

let private installSuspendOnInterrupt () : unit =
  System.Console.CancelKeyPress.Add(fun args ->
    let suspended =
      try
        stopChildrenPolitely ()
        (LibDB.Traces.Foreground.suspend ()).Result
      with _ ->
        None
    match suspended with
    | Some id ->
      args.Cancel <- true
      let prefix = (string id).Substring(0, 8)
      System.Console.Error.WriteLine ""
      System.Console.Error.WriteLine
        $"stopped; the run is kept. Take it up again with: dark traces resume {prefix}"
      exit 130
    | None -> ())

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

/// The title a system monitor shows (`setProcessTitle`): `dark` plus the command, plus the one
/// argument that tells commands of the same kind apart (a daemon's slug, a served port), within
/// the kernel's 15 bytes. Global flags (`--branch <b>`, `--safe`) are skipped.
let private processTitle (args : string list) : string =
  let rec drop (args : string list) =
    match args with
    // `--branch` takes a value; `--trace` does NOT, so matching it here swallowed the
    // following argument and `dark --trace run app.dark` was titled `dark app.dark`.
    // The generic flag arm below drops it.
    | "--branch" :: _ :: rest -> drop rest
    | flag :: rest when flag.StartsWith "--" -> drop rest
    | _ -> args
  let portOf (rest : string list) =
    let rec go (xs : string list) =
      match xs with
      | "--port" :: p :: _ -> Some p
      | _ :: xs -> go xs
      | [] -> None
    go rest
  match drop args with
  | [] -> "dark"
  | "apps" :: "daemon-main" :: slug :: _ -> $"dark {slug}"
  | "apps" :: sub :: slug :: _ when sub = "start" || sub = "view" || sub = "run" ->
    $"dark {slug}"
  | "serve" :: rest ->
    match portOf rest with
    | Some p -> $"dark serve :{p}"
    | None -> "dark serve"
  | "run" :: path :: _ -> $"dark {System.IO.Path.GetFileNameWithoutExtension path}"
  | cmd :: _ -> $"dark {cmd}"

/// Everything the CLI does. Called on a thread of our own; see `main`.
let private runCli (args : string[]) : int =
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
    installSuspendOnInterrupt ()
    let title = processTitle (List.ofArray args)
    LibExecution.HostProcess.setProcessTitle title
    let commandLine = "dark " + String.concat " " args

    // Loopback for guest HTTP, off unless asked for BY NAME. The guest default blocks loopback,
    // RFC-1918, link-local and cloud-metadata as an SSRF guard, and a developer pointing the CLI
    // at a server on this machine needs the first of those and nothing else; `devLoopbackConfig`
    // widens exactly that one range and says why there.
    //
    // Deliberately NOT read from the instance policy, which can already express
    // `http LOCALHOST *`. A policy may also say `all`, and guest code includes `darklang serve`
    // handlers and package initialisation, so honouring the policy here would let a broad grant
    // switch the guard off.
    //
    // Stored setting beats the environment, because the environment is a container-wide default
    // and the stored value is a decision somebody made in this install. Anything other than
    // `on` leaves it off: an unreadable opt-in has not happened.
    let loopbackRequested () : bool =
      let isOn (s : string) = s.Trim().ToLower() = "on"
      let stored =
        try
          (LibDB.Config.get "http.loopback").Result
        with _ ->
          None
      match stored with
      | Some v -> isOn v
      | None ->
        match
          System.Environment.GetEnvironmentVariable "DARK_CONFIG_HTTP_LOOPBACK"
        with
        | null
        | "" -> false
        | v -> isOn v

    if loopbackRequested () then
      LibExecution.HostHttp.setGuestConfig LibExecution.HostHttp.devLoopbackConfig


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
      // Under the upgrade lock: right after an upgrade every dark started at once finds the same ops
      // unapplied, and folding them side by side fails some with "database is locked". One folds;
      // the rest find nothing left.
      use _growing = EmbeddedResources.upgradeLock LibConfig.Config.dbPath
      try
        (LibDB.Seed.growIfNeeded
          // Bounded by the operator's instance policy. The store can hold values
          // that arrived by import or sync, and this is where they first run.
          LibDB.Seed.EvaluationAuthority.underInstancePolicy
          (fun () -> builtinsLazy.Force())
          cliPackageManager
          EmbeddedResources.progress)
          .Result
        |> ignore<bool>
        // Still under the lock: the stamp says "this store is current", so it is written only after the
        // fold, and only if the folded store holds what this build needs.
        EmbeddedResources.finishUpgrade LibConfig.Config.dbPath
      with
      | ex when EmbeddedResources.upgradePending () ->
        EmbeddedResources.abandonUpgrade LibConfig.Config.dbPath ex.Message
      | ex when isBinaryFormatFailure ex ->
        let path = LibConfig.Config.dbPath

        // The dark that wrote it comes first: after an upgrade rewrites every blob it can read, a store
        // this dark cannot read is most likely one a NEWER dark wrote, and moving it aside would leave
        // that work where this dark cannot see it.
        [ "This store was written in a format this dark cannot read, so it has not been opened."
          "  The dark that wrote it still can. If that was a newer dark, `dark update` gets you back to it."
          ""
          "  To start this dark with an empty store instead, move this one aside; your work stays in"
          "  the moved copy, which the dark that wrote it can open:"
          ""
          $"    mv '{path}' '{path}.old'" ]
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

    // `--trace` / `--no-trace`: whether THIS run is recorded, whatever the store and the
    // environment say, and touching nothing persistent. Recording is a yes or no, so these are
    // bare flags rather than `--trace <value>`; a value is refused by name, because
    // `--trace on run thing.dark` would otherwise try to run a command called `on`.
    let valueAfterTrace =
      args
      |> Array.mapi (fun i a -> (i, a))
      |> Array.tryPick (fun (i, a) ->
        if a.StartsWith "--trace=" then
          Some(a.Substring "--trace=".Length)
        elif a = "--trace" && i + 1 < args.Length then
          match args[i + 1] with
          | "on"
          | "off"
          | "io"
          | "complete"
          | "all"
          | "none" -> Some args[i + 1]
          | _ -> None
        else
          None)

    match valueAfterTrace with
    | Some v ->
      System.Console.Error.WriteLine
        $"--trace takes no value ('{v}'). Recording is on or off: `dark --trace <command>`"
      System.Console.Error.WriteLine
        "records this one, `dark --no-trace <command>` does not, and `dark traces record on`"
      System.Console.Error.WriteLine "changes the setting itself."
      exit 1
    | None -> ()

    if Array.contains "--trace" args && Array.contains "--no-trace" args then
      System.Console.Error.WriteLine
        "--trace and --no-trace ask for opposite things; pick one"
      exit 1
    elif Array.contains "--trace" args then
      LibDB.Tracing.TraceDetail.setForRun LibDB.Tracing.TraceDetail.On
    elif Array.contains "--no-trace" args then
      LibDB.Tracing.TraceDetail.setForRun LibDB.Tracing.TraceDetail.Off

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

    // Strip the boot flags so none of them reaches the entry-point fn as a positional
    // argument. `--branch` takes a value in its space form, so it goes by index and width;
    // the recording flags are bare and go by name.
    let args =
      let withoutBranch =
        match branchFlag with
        | Some(_, i, width) ->
          Array.append
            (Array.sub args 0 i)
            (Array.sub args (i + width) (args.Length - i - width))
        | None -> args
      withoutBranch |> Array.filter (fun a -> a <> "--trace" && a <> "--no-trace")

    LibDB.PackageManager.selectBranch (
      branchId |> Option.defaultValue PT.BranchId.Main
    )

    // Listed for `dark ps` from any shell, once the branch is known; the file goes when this
    // process does. The branch is its name where one was given, else the stored id, else main.
    LibExecution.HostRegistry.setDirectory LibConfig.Config.runDir
    LibExecution.HostRegistry.register
      title
      commandLine
      (match flagName, envName, branchId with
       | Some name, _, _
       | None, Some name, _ -> name
       | None, None, Some id -> string id
       | None, None, None -> "")

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
          "traitDispatches",
          string (stats |> List.sumBy (fun s -> s.traitDispatchCount))
          "traitDispatchMisses",
          string (stats |> List.sumBy (fun s -> s.traitDispatchMissCount))
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

      match rte with
      // A condition is a refusal with the sentence that resolves it: print that and nothing else,
      // with no header calling it a runtime error and no call stack under it.
      | RT.RuntimeError.Condition message -> logError message
      | _ ->
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
          // The code that describes errors is package code too, so when it fails as well the usual reason is
          // a store that does not hold what this binary needs. Say THAT, which a person can act on, before
          // anything raw.
          match LibDB.Seed.missingPins () with
          | [] ->
            logError
              $"dark hit an error, and the code that describes errors failed as well.\n  error: {rte}\n  describing it failed with: {newErr}\n{errorCallStackStr}"
          | missing ->
            let path = LibConfig.Config.dbPath
            let total = List.length (LibExecution.PackageRefs.pinned ())
            let some = missing |> List.truncate 3 |> String.concat ", "
            logError (
              $"dark could not run: this store does not hold {List.length missing} of the {total} package items this dark needs ({some}).\n"
              + "  It was most likely written by a different version of Darklang; the one that wrote it can still open it.\n"
              + $"  To start fresh with this version instead, move it aside:  mv '{path}' '{path}.old'"
            )

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
    match Exception.findStoreCondition e with
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


[<EntryPoint>]
let main (args : string[]) : int =
  // Run on a thread we start, for its STACK SIZE, and not for concurrency.
  //
  // The interpreter recurses over nested types behind
  // `RuntimeHelpers.EnsureSufficientExecutionStack()`, which throws on how much stack is LEFT
  // rather than on how deep you already are, and on musl the runtime does not see the main
  // thread's real stack. Every command died at trivial depth on Alpine while glibc, macOS and arm
  // were green.
  //
  // Measured in Alpine: the main thread and a default-size thread both throw, while 8 MB and 16 MB
  // recurse 100,000 frames. `ulimit -s` is 8192 on BOTH platforms and 256 KB throws on both, so it
  // is neither the limit nor explicitness. It is that nothing sizes those two.
  //
  // `Scheduler.executeFunction` runs its loop on the CALLING thread, which is why sizing the
  // scheduler's own threads did not fix a plain `dark eval`. They still need their sizes, for
  // `serve` and for a worker group, so do not revert that.
  //
  // Threadpool threads are unsized, the runtime making them. No interpreter work runs on one today.
  let mutable exitCode = 1

  let thread =
    System.Threading.Thread(
      (fun () ->
        exitCode <-
          try
            runCli args
          with e ->
            System.Console.Error.WriteLine
              $"Error starting Darklang CLI: {e.Message}"
            1),
      LibExecution.HostEvents.threadStackBytes,
      IsBackground = false,
      Name = "dark-main"
    )

  thread.Start()
  thread.Join()
  exitCode
