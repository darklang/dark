/// Tests for the platform model: what `PlatformSet.make` refuses, and what the fingerprint notices.
///
/// The fingerprint tests carry the most weight. The qualified form `Files#fileRead` is DERIVED from
/// the manifest rather than stored in `ProgramTypes`, and the only thing making that safe is that
/// the fingerprint changes when ownership moves. If `fingerprintNoticesAnOwnershipMove` ever goes
/// red, the derived form is unsound and the qualifier has to be stored, which costs a rehash of
/// every package that calls a builtin. Read that failure as a design change, not a broken test.
module Tests.Platform

open Expecto

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Platform

module Builtin = LibExecution.Builtin
module Effects = LibExecution.Effects
module Permission = LibExecution.Permissions
module PT = LibExecution.ProgramTypes
module PackageSurface = TestUtils.PackageSurface
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module RT = LibExecution.RuntimeTypes


// ── fixtures ──────────────────────────────────────────────────────────────────

/// A builtin that does nothing, so a test can talk about names, signatures and effects without
/// caring what runs.
let private stubFn (name : string) (effects : Set<Effects.Effect>) : BuiltInFn =
  { name = FQFnName.builtin name 0
    typeParams = []
    parameters = [ BuiltInParam.make "x" TUnit "" ]
    returnType = TUnit
    description = "stub"
    previewable = Pure
    deprecated = NotDeprecated
    sqlSpec = NotQueryable
    callEffects = effects
    fn =
      (function
      | struct (_, _, _, [| _ |]) -> Ply DUnit
      | _ -> incorrectArgs ()) }

let private stubPlatform
  (name : string)
  (requires : List<string>)
  (fns : List<BuiltInFn>)
  : Platform =
  { name = name
    version = 0
    description = "stub"
    builtins = Builtin.make [] fns
    requires = requires
    dynamicEffects = Set.empty
    requiresStore = false }


// ── what `make` refuses ───────────────────────────────────────────────────────

let collisionIsRefused =
  test "two platforms claiming one builtin name is refused" {
    let a = stubPlatform "A" [] [ stubFn "sharedName" Set.empty ]
    let b = stubPlatform "B" [] [ stubFn "sharedName" Set.empty ]
    Expect.throws
      (fun () -> PlatformSet.make [ a; b ] [] |> ignore<PlatformSet>)
      "a name claimed by two platforms must not compose silently"
  }

let missingRequirementIsRefused =
  test "a platform missing a requirement is refused" {
    let a = stubPlatform "A" [ "NotPresent" ] [ stubFn "onlyName" Set.empty ]
    Expect.throws
      (fun () -> PlatformSet.make [ a ] [] |> ignore<PlatformSet>)
      "a set missing something a member requires must not compose"
  }

let satisfiedRequirementComposes =
  test "a satisfied requirement composes" {
    let core = stubPlatform "Core" [] [ stubFn "coreName" Set.empty ]
    let host = stubPlatform "Host" [ "Core" ] [ stubFn "hostName" Set.empty ]
    let set = PlatformSet.make [ core; host ] []
    Expect.equal set.builtins.fns.Count 2 "both platforms' builtins are present"
  }


// ── the fingerprint ───────────────────────────────────────────────────────────

let fingerprintIsStable =
  test "the same set fingerprints the same twice" {
    let build () =
      PlatformSet.make [ stubPlatform "A" [] [ stubFn "stubName" Set.empty ] ] []
    Expect.equal (build ()).fingerprint (build ()).fingerprint "deterministic"
  }

let fingerprintNoticesAnEffectChange =
  test "a changed effect changes the fingerprint" {
    let pure_ =
      PlatformSet.make [ stubPlatform "A" [] [ stubFn "stubName" Set.empty ] ] []
    let effectful =
      PlatformSet.make
        [ stubPlatform
            "A"
            []
            [ stubFn "stubName" (Set.singleton Effects.Effect.FileRead) ] ]
        []
    Expect.notEqual
      pure_.fingerprint
      effectful.fingerprint
      "a builtin gaining an effect must invalidate approvals"
  }

/// The property the derived `Platform#name` form rests on. Both sets contain the same platform
/// names and the same builtin with the same signature and the same effects; the only difference is
/// WHICH platform declares it. Nothing else in the manifest distinguishes them.
let fingerprintNoticesAnOwnershipMove =
  test "moving a builtin between platforms changes the fingerprint" {
    let shared = stubFn "moved" Set.empty
    let filler = stubFn "filler" Set.empty
    let ownedByA =
      PlatformSet.make
        [ stubPlatform "A" [] [ shared ]; stubPlatform "B" [] [ filler ] ]
        []
    let ownedByB =
      PlatformSet.make
        [ stubPlatform "A" [] [ filler ]; stubPlatform "B" [] [ shared ] ]
        []
    Expect.notEqual
      ownedByA.fingerprint
      ownedByB.fingerprint
      "ownership is part of the manifest, or the qualified name cannot be derived"
  }


// ── ownership queries ─────────────────────────────────────────────────────────

let ownershipResolves =
  test "a builtin resolves to its declaring platform, both ways" {
    let set =
      PlatformSet.make
        [ stubPlatform "Core" [] [ stubFn "coreName" Set.empty ]
          stubPlatform "Host" [ "Core" ] [ stubFn "hostName" Set.empty ] ]
        []
    Expect.equal
      (PlatformSet.ownerOf "hostName" set |> Option.map _.name)
      (Some "Host")
      "ownerOf finds the declaring platform"
    Expect.equal
      (PlatformSet.qualify "hostName" set)
      "Host#hostName"
      "qualify renders it"
    Expect.equal
      (PlatformSet.resolveQualified "Host#hostName" set |> Option.map _.name)
      (Some "hostName")
      "resolveQualified reads it back"
    Expect.isNone
      (PlatformSet.resolveQualified "Core#hostName" set)
      "a name attributed to the wrong platform does not resolve"
    Expect.isNone
      (PlatformSet.resolveQualified "Absent#hostName" set)
      "a platform outside the set does not resolve"
  }

let effectOriginsNameThePlatforms =
  test "effect origins say which platform introduced each effect" {
    let set =
      PlatformSet.make
        [ stubPlatform "Core" [] [ stubFn "pureName" Set.empty ]
          stubPlatform
            "Host"
            [ "Core" ]
            [ stubFn "readName" (Set.singleton Effects.Effect.FileRead) ] ]
        []
    let origins = PlatformSet.effectOrigins set
    Expect.equal
      (Map.get Effects.Effect.FileRead origins)
      (Some [ "Host" ])
      "file-read comes from Host"
    Expect.equal
      (PlatformSet.unreachableEffects (Set.singleton Effects.Effect.Http) set)
      [ Effects.Effect.Http ]
      "an effect no platform produces is named as unreachable"
    Expect.equal
      (PlatformSet.unreachableEffects (Set.singleton Effects.Effect.FileRead) set)
      []
      "an effect a platform produces is not"
  }


// ── the real catalog ──────────────────────────────────────────────────────────

let catalogComposes =
  test "the shipped catalog composes" {
    // `make` is where collisions and missing requirements raise, so composing IS the assertion.
    let set = Platforms.Sets.everythingFor PT.PackageManager.empty
    Expect.isGreaterThan set.builtins.fns.Count 300 "the whole floor is present"
    Expect.equal
      (set.platforms |> List.map _.name |> List.distinct |> List.length)
      (List.length set.platforms)
      "platform names are unique"
  }

/// `Core` alone has to be composable, or the boundary is decoration rather than a seam.
let sealedComputeIsPure =
  test "Core composes alone, with no effects" {
    let set = Platforms.Sets.sealedCompute ()
    Expect.equal (PlatformSet.effectSurface set) Set.empty "Core reaches nothing"
    Expect.isFalse (PlatformSet.needsStore set) "Core needs no store"
    Expect.isGreaterThan set.builtins.fns.Count 150 "and it is not a stub"
  }

let everyPlatformDeclaresItsStoreNeed =
  test "a platform reaching the package store says it needs one" {
    let set = Platforms.Sets.everythingFor PT.PackageManager.empty
    let inconsistent =
      set.platforms
      |> List.filter (fun p ->
        let reachesStore =
          Platform.effectSurface p
          |> Set.exists (fun e ->
            e = Effects.Effect.PackageRead || e = Effects.Effect.PackageWrite)
        reachesStore && not p.requiresStore)
      |> List.map _.name
    Expect.equal inconsistent [] "these read or write packages but claim no store"
  }


/// What each shipped platform is allowed to reach, written by hand.
///
/// It lives here rather than on the `Platform` record on purpose. Nothing at runtime reads it, and
/// carrying it there cost 32 KB of `Set` construction on every CLI invocation to hold a constant a
/// test consults once. The declaration has to be somewhere a person will disagree with it, and a
/// failing test that names the platform and the effect is that place.
///
/// Every platform in the catalog must appear. A new one with no entry fails, which is the point:
/// deciding what it may reach should not be something you can forget.
let private promisedEffects : Map<string, Set<Effects.Effect>> =
  Map.ofList
    [ // Arithmetic, text, collections. The floor, and the only platform that reaches nothing.
      "Core", Set.empty

      // Reading the wall clock, and generating entropy.
      "Clock", Set.singleton Effects.Effect.Clock
      "Random", Set.singleton Effects.Effect.Random

      // Parser, reflection, instrumentation. Statically pure, which is worth knowing: the platform
      // that makes Darklang able to read itself needs no host access to do it.
      "Lang", Set.empty

      // Four platforms over one assembly. They share the `Host.perform` door; what they do not
      // share is a grant. `Terminal` reaching exactly stdout and stdin is the reason for the cut.
      "Terminal", set [ Effects.Effect.Stdout; Effects.Effect.Stdin ]

      "Files", set [ Effects.Effect.FileRead; Effects.Effect.FileWrite ]

      "Process", set [ Effects.Effect.Process; Effects.Effect.Native ]

      // Raw descriptors and the environment. Wide on purpose: a descriptor names a number rather
      // than a resource, so it is granted whole or not at all.
      "Posix",
      set
        [ Effects.Effect.FileRead
          Effects.Effect.FileWrite
          Effects.Effect.EnvRead
          Effects.Effect.EnvWrite
          Effects.Effect.Process
          Effects.Effect.Native ]

      "HttpClient", Set.singleton Effects.Effect.Http

      // `httpServerServe` is one builtin that also prints and reads the clock, so the platform is
      // wider than its name. Splitting the builtin would narrow it; nothing has yet.
      "HttpServer",
      set [ Effects.Effect.HttpServer; Effects.Effect.Stdout; Effects.Effect.Clock ]

      // The package store: what every program needs in order to resolve a name, so every effect
      // here is one every program carries. It is now exactly the two it is named for, which is the
      // narrowest a package platform can be. `file-write` left with `pmSeedExport` (to `Admin`),
      // `native` with `pmEvaluateValue` (to `Darklang`), and `random` was a stale declaration on
      // `pmPropagate` describing a `Guid.NewGuid` that no longer exists on that path.
      "Store", set [ Effects.Effect.PackageRead ]

      // The write half. Split off `Store` because every program links `Store` to resolve a name,
      // so `package-write` was being granted to every command that merely lists packages.
      //
      // It reaches `package-read` as well, which is not a leak and not laziness: writing a name
      // means reading what is there first. The asymmetry is the point. Reading never needs to
      // write, so the read half can be narrow; writing always needs to read, so the write half
      // cannot be.
      "Authoring", set [ Effects.Effect.PackageRead; Effects.Effect.PackageWrite ]

      // Three platforms over one assembly: deliberate, person-initiated operations on this
      // installation. `file-write` and `native` each belong to exactly one of them, which is what
      // keeps `dark config` from having to take either.
      "Instance", set [ Effects.Effect.PackageRead; Effects.Effect.PackageWrite ]

      "Seed",
      set
        [ Effects.Effect.PackageRead
          Effects.Effect.FileWrite
          Effects.Effect.Native ]

      // Every builtin here is `Native`, and honestly so: policy gates every other platform, so the
      // ability to edit it is the ability to do anything.
      "Policy", set [ Effects.Effect.Native ]

      // Four platforms over one assembly. `Native` and the package effects belong to two builtins
      // out of the set, and the cut is what makes them say so.
      "Db", set [ Effects.Effect.DbRead; Effects.Effect.DbWrite ]

      "Traces", set [ Effects.Effect.TraceRead; Effects.Effect.TraceWrite ]

      "Accounts", set [ Effects.Effect.DbRead ]

      // From `Libs.Sqlite.dynamicEffects`, which the static declarations cannot show: what a query
      // may reach depends on the database file and on whether the SQL can leave it.
      "Sqlite",
      set
        [ Effects.Effect.PackageRead
          Effects.Effect.PackageWrite
          Effects.Effect.Native ]

      // eval and script execution: running arbitrary Dark, which no rule can scope. `package-read`
      // arrived with `pmEvaluateValue`, which reads a stored value in order to run it.
      "Darklang", set [ Effects.Effect.Native; Effects.Effect.PackageRead ] ]


/// The contract test. Compares what a platform may actually reach against `promisedEffects`, and
/// fails on a difference in EITHER direction.
///
/// Read a failure as one of two things. **Extra** means a builtin gained an effect the platform was
/// not promised to have, and the usual cause is a new builtin in the wrong assembly: a pure helper
/// belongs in `Builtins.Pure`, and one that reaches the filesystem belongs in `Builtins.Cli`.
/// **Missing** means a promise no builtin needs any more, which is a line to delete. An over-broad
/// promise nobody revisits is what makes an install-time review meaningless, so both directions
/// fail rather than only the alarming one.
let declaredEffectsMatchReality =
  test "each platform reaches exactly what it is promised to reach" {
    let set_ = Platforms.Sets.everythingFor PT.PackageManager.empty
    let problems =
      set_.platforms
      |> List.collect (fun p ->
        match Map.get p.name promisedEffects with
        | None ->
          [ $"{p.name}: no entry in promisedEffects; decide what it may reach" ]
        | Some promised ->
          let actual = Platform.effectSurface p
          let name (e : Effects.Effect) = Effects.name e
          let extra =
            Set.difference actual promised
            |> Set.toList
            |> List.map (fun e ->
              $"{p.name}: reaches {name e}, which it is not promised")
          let missing =
            Set.difference promised actual
            |> Set.toList
            |> List.map (fun e ->
              $"{p.name}: promised {name e}, which nothing needs")
          extra @ missing)
    Expect.equal problems [] "platform effect promises are out of date"
  }


/// Which builtin declares each effect, for the platforms that claim to be narrow.
///
/// `Native` is the one worth watching: it means "no rule could honestly scope this", so a platform
/// that acquires one quietly stops being scopable. This does not forbid it; it names them, so the
/// list is something someone chose rather than something that accumulated.
let nativeBuiltinsAreInventoried =
  test "the Native builtins are the ones we expect" {
    let set = Platforms.Sets.everythingFor PT.PackageManager.empty
    let natives =
      set.platforms
      |> List.collect (fun p ->
        p.builtins.fns.Values
        |> Seq.filter (fun fn -> Set.contains Effects.Effect.Native fn.callEffects)
        |> Seq.map (fun fn -> $"{p.name}#{fn.name.name}")
        |> List.ofSeq)
      |> List.sort
    // Not a fixed list: it moves with real work, and pinning it would make every legitimate change
    // a two-file edit. What is pinned is that no platform OUTSIDE this set acquires one, since those
    // are the platforms whose whole value is being scopable.
    let scopablePlatforms =
      [ "Core"; "Clock"; "Random"; "HttpClient"; "HttpServer"; "Lang" ]
    let leaked =
      natives
      |> List.filter (fun n ->
        scopablePlatforms |> List.exists (fun p -> n.StartsWith(p + "#")))
    Expect.equal
      leaked
      []
      "a platform that is supposed to be scopable acquired an unscopable builtin"
  }


// ── layering: where a platform's builtins are wrapped ─────────────────────────

/// Where each platform's Dark wrappers are SUPPOSED to live, as `owner/module` prefixes under
/// `packages/`.
///
/// Declared narrowly on purpose. Listing every area a platform is currently wrapped in would make
/// this test assert the status quo and nothing else; listing the intended homes and pinning the
/// exceptions by name (`knownStrays`) makes the pin list a backlog that can shrink.
let private platformHomes : Map<string, List<string>> =
  Map.ofList
    [ "Core", [ "darklang/stdlib" ]
      "Clock", [ "darklang/stdlib" ]
      "Random", [ "darklang/stdlib" ]
      "Terminal", [ "darklang/stdlib"; "darklang/cli" ]
      "Files", [ "darklang/stdlib" ]
      "Process", [ "darklang/stdlib" ]
      "Posix", [ "darklang/stdlib" ]
      "HttpClient", [ "darklang/stdlib"; "darklang/cli" ]
      "HttpServer", [ "darklang/stdlib" ]
      "Lang", [ "darklang/languageTools" ]
      "Store", [ "darklang/scm"; "darklang/sync"; "darklang/languageTools" ]
      "Authoring", [ "darklang/scm"; "darklang/sync" ]
      "Instance", [ "darklang/cli" ]
      "Seed", [ "darklang/cli" ]
      "Policy", [ "darklang/cli" ]
      "Db", [ "darklang/stdlib" ]
      "Traces", [ "darklang/tracing" ]
      "Accounts", [ "darklang/scm" ]
      "Sqlite", [ "darklang/stdlib" ]
      "Darklang", [ "darklang/cli" ] ]


/// Wrappers that live outside their platform's home and are known about.
///
/// Each entry is `Platform#builtin@area`. This is the layering backlog: every line is a primitive
/// reachable from a module that does not own it. Some are fine and will never move (the LSP reads
/// stdin, so `Terminal#stdinReadExactly` under `languageTools` is right); some are the finding
/// (`Store#pmEvaluateValue` under `stdlib` puts "run this stored Dark value" in the standard
/// library). Deleting a line is progress; adding one should need a reason.
let private knownStrays : Set<string> =
  Set.ofList
    [ // ── fine, and unlikely ever to move ──────────────────────────────────────
      // The CLI browses and edits packages, so it names the store's read builtins directly.
      "Store#applicableByName@darklang/cli"
      "Store#atRestCheckBranch@darklang/cli"
      "Store#pmFnInstructions@darklang/cli"
      "Store#pmGetCurrentDeprecation@darklang/cli"
      "Store#pmGetDeprecationSets@darklang/cli"
      // `dark db` is a CLI command over the user database.
      "Db#dbCreate@darklang/cli"
      "Db#dbDrop@darklang/cli"
      "Db#dbListAll@darklang/cli"
      // The LSP speaks over stdio.
      "Terminal#stdinReadExactly@darklang/languageTools"
      // Sync compares this build's ABI fingerprint against a peer's before ingesting ops.
      "Lang#getKernelHash@darklang/sync"
      // The CLI's authoring path parses and reflects.
      "Lang#parserParsePackageToWrittenTypes@darklang/cli"
      "Lang#reflect@darklang/cli"
      // `Stdlib.toRepr` is a real standard-library function that happens to be reflection.
      "Lang#toRepr@darklang/stdlib"
      // The CLI times itself.
      "Clock#timeNowMs@darklang/cli"
      // `dark version` reports which build it is. Surfaced by splitting `Host`: under one coarse
      // platform whose home was stdlib AND cli, a lone wrapper in cli looked like the home. The
      // builtin then moved from `Posix` to `Lang`, which is where the fact belongs.
      "Lang#getBuildHash@darklang/cli"

      // ── findings: a capability reachable from a module that should not own it ──
      // "Run this stored Dark value", from the standard library. It lives in `Darklang` rather
      // than `Store` precisely so that `Store` need not declare `native`; moving it back would
      // put that effect on every program that resolves a name.
      "Darklang#pmEvaluateValue@darklang/stdlib"
      "Store#pmFindValuesByValueType@darklang/stdlib"
      // Instance administration from the standard library: backing up and restoring the store,
      // and reading and writing config. `stdlib/localStore.dark` wraps all five.
      "Instance#configGet@darklang/stdlib"
      "Instance#configSet@darklang/stdlib"
      "Instance#localDbBackupTo@darklang/stdlib"
      "Instance#localDbPath@darklang/stdlib"
      "Instance#localDbRestoreFrom@darklang/stdlib" ]


let builtinsAreWrappedInTheirPlatformsHome =
  test "every builtin is wrapped in its platform's home, or is a known stray" {
    let set_ = Platforms.Sets.everythingFor PT.PackageManager.empty
    let files = PackageSurface.packageFiles.Value
    let strays =
      set_.platforms
      |> List.collect (fun p ->
        let homes = Map.get p.name platformHomes |> Option.defaultValue []
        p.builtins.fns.Values
        |> Seq.filter (fun fn ->
          not (Set.contains fn.name.name PackageSurface.languageIdioms))
        |> Seq.collect (fun fn ->
          files
          |> List.filter (fun (_, contents) ->
            PackageSurface.referencesBuiltin contents fn.name.name)
          |> List.map (fun (path, _) -> PackageSurface.area path)
          |> List.distinct
          |> List.filter (fun a -> not (List.contains a homes))
          |> List.map (fun a -> $"{p.name}#{fn.name.name}@{a}"))
        |> List.ofSeq)
      |> List.sort
      |> List.distinct
    let unexpected =
      strays |> List.filter (fun s -> not (Set.contains s knownStrays))
    let goneAway =
      knownStrays
      |> Set.toList
      |> List.filter (fun s -> not (List.contains s strays))
    Expect.equal
      unexpected
      []
      ("a builtin is wrapped outside its platform's home. Move the wrapper, widen "
       + "`platformHomes` if the module really does own that platform, or add the line to "
       + "`knownStrays` with a reason")
    Expect.equal
      goneAway
      []
      "these `knownStrays` no longer exist; delete them (this is what progress looks like)"
  }


// ── does the seam actually work ──────────────────────────────────────────────

/// Composing `Core` alone is easy to assert and easy to get wrong: `PlatformSet.make` will happily
/// hand back a set nothing can execute against. This runs a program on one.
///
/// It is the claim the whole exercise rests on. Everything else here says platforms CAN be composed
/// apart; only this says the result is a runtime rather than a data structure. Deny-all access on
/// purpose: `Core` reaches nothing, so the strictest possible policy has to be enough for it.
let coreOnlyRuntimeComputes =
  testTask "a Core-only runtime runs a program, under deny-all" {
    let set = Platforms.Sets.sealedCompute ()
    let pmRT =
      PT2RT.PackageManager.toRT
        set.builtins.values
        LibExecution.ProgramTypes.PackageManager.empty
    let state =
      Exe.createState
        set.builtins
        pmRT
        Exe.noTracing
        RT.consoleReporter
        RT.consoleNotifier
        { dbs = Map.empty }

    let! ptExpr = TestUtils.TestUtils.parsePTExpr "\"ab\" ++ \"cd\""
    let! result = Exe.executeExpr state (PT2RT.Expr.toRT Map.empty 0 None ptExpr)
    match result with
    | Ok(RT.DString "abcd") -> ()
    | Ok other -> failtest $"Core-only runtime computed the wrong thing: {other}"
    | Error(rte, _) -> failtest $"Core-only runtime raised: {rte}"
  }

/// The other half of "composed apart": a set without `Host` must not contain `Host`'s builtins.
///
/// Obvious, and worth asserting anyway. A `combine` that quietly pulled in a transitive assembly
/// would leave `sealedCompute` reaching the filesystem while still passing every other test here,
/// because every other test asks about platforms rather than about what ended up in the dictionary.
let coreOnlySetExcludesEverythingElse =
  test "a Core-only set contains no other platform's builtins" {
    let set = Platforms.Sets.sealedCompute ()
    let has (n : string) = set.builtins.fns.Keys |> Seq.exists (fun k -> k.name = n)
    Expect.isTrue (has "stringAppend") "Core's own builtins are there"
    Expect.isFalse (has "fileRead") "Host's are not"
    Expect.isFalse (has "httpClientRequest") "HttpClient's are not"
    Expect.isFalse (has "sqliteQuery") "Sqlite's are not"
    Expect.isFalse (has "cliEvaluateExpression") "Darklang's are not"
  }


/// Every platform except `Core` requires exactly `Core`, and that is the composability claim holding
/// up rather than a field that lost its meaning.
///
/// `requires` is about BUILTINS needing other builtins. At that level the platforms turn out to be
/// independent: the only cross-platform dependency any of them has is that its builtins return
/// `Option`/`Result`, whose Dark definitions are Core's. Everything that looks like a dependency
/// between platforms is really a dependency between PACKAGE MODULES, which `platformHomes` covers.
///
/// Pinned so that a new `requires` entry has to be justified. If one appears, either a builtin
/// genuinely started calling another platform's, or someone read the field the loose way, and the
/// loose reading makes every platform require every other one.
let requiresIsJustCore =
  test "every platform but Core requires exactly Core" {
    let set = Platforms.Sets.everythingFor PT.PackageManager.empty
    let unexpected =
      set.platforms
      |> List.filter (fun p ->
        if p.name = "Core" then p.requires <> [] else p.requires <> [ "Core" ])
      |> List.map (fun p ->
        let reqs = String.concat ", " p.requires
        $"{p.name} requires {reqs}")
      |> List.sort
    Expect.equal
      unexpected
      []
      "a platform's builtins claim to need another platform's; name the builtin that does"
  }

/// The compositions the tightened `requires` newly permits. Each is a set someone might plausibly
/// want, and none of them composed before.
let smallSetsCompose =
  test "two-platform sets compose" {
    let pm = PT.PackageManager.empty
    let core = Builtins.Pure.Builtin.platform
    for (name, other) in
      [ "Lang", Builtins.Language.Builtin.platform
        "Instance", Builtins.Admin.Builtin.instancePlatform
        "Seed", Builtins.Admin.Builtin.seedPlatform
        "Policy", Builtins.Admin.Builtin.policyPlatform
        "Darklang", Builtins.CliHost.Builtin.platform
        "HttpClient", Builtins.Http.Client.Builtin.platform
        "Store", Builtins.Store.Builtin.platform pm
        "Authoring", Builtins.Store.Builtin.authoringPlatform pm
        // The four over `Builtins.Cli`: the composition the cut exists to allow.
        // The four over `Builtins.Data`.
        "Db", Builtins.Data.Builtin.dbPlatform
        "Traces", Builtins.Data.Builtin.tracesPlatform
        "Accounts", Builtins.Data.Builtin.accountsPlatform
        "Sqlite", Builtins.Data.Builtin.sqlitePlatform
        "Terminal", Builtins.Cli.Builtin.terminalPlatform
        "Files", Builtins.Cli.Builtin.filesPlatform
        "Process", Builtins.Cli.Builtin.processPlatform
        "Posix", Builtins.Cli.Builtin.posixPlatform ] do
      let set = PlatformSet.make [ core; other ] []
      Expect.isGreaterThan
        set.builtins.fns.Count
        (Platform.fnCount core)
        $"Core + {name} composes and adds something"
  }


/// The one executable that composes a strict subset, and the reason the subset claim is testable
/// rather than aspirational. Every other binary we ship links every platform, so nothing
/// would notice if the seams grew back. This notices.
///
/// It reads the `.fsproj` rather than the compiled assembly on purpose: what matters is what the
/// project is ALLOWED to reference, and the failure this catches is somebody adding a line to the
/// project file because they wanted one function from it. Reference-count linting inside a compiled
/// closure would pass long after the separation was gone.
let sealedHostLinksOnlyCore =
  test "the sealed host's project references stay a short list" {
    let path =
      System.IO.Path.Combine(
        TestUtils.PackageSurface.findRepoRoot (),
        "backend/src/SealedHost/SealedHost.fsproj"
      )
    let text = System.IO.File.ReadAllText path
    let refs =
      System.Text.RegularExpressions.Regex.Matches(
        text,
        "ProjectReference Include=\"([^\"]+)\""
      )
      |> Seq.map (fun m -> m.Groups[1].Value.Split('/') |> Array.last)
      |> Seq.sort
      |> Seq.toList

    // Prelude and LibExecution are the runtime. LibDB is the package reader, and is the one line
    // here worth removing some day: it is what drags SQLite, and its native binaries for every RID,
    // into a binary whose whole point is being small. Builtins.Pure is `Core`.
    let expected =
      [ "Builtins.Pure.fsproj"
        "LibDB.fsproj"
        "LibExecution.fsproj"
        "Prelude.fsproj" ]

    Expect.equal
      refs
      expected
      "SealedHost references exactly these. Adding one is a decision about what a minimal Dark host \
       is, not a build detail: `Platforms.fsproj` in particular references every platform we ship \
       and would make the whole exercise a lie."
  }


/// An invariant the CAPABILITY SYSTEM rests on, which nothing was checking.
///
/// `a + b` is an `EInfix` in ProgramTypes and lowers to `Builtin.add` only at RT.
/// `CallGraph.analyze` records `EFnName` and `EPipeFnCall` and nothing else, so an
/// operator-dispatched builtin is invisible to every static walk built on it. That includes
/// `LibDB.PackagePermissions.permissionRequirements`, which is what decides the permissions a
/// package function is APPROVED for.
///
/// That was sound for exactly one reason: every builtin in the infix table is pure. An effectful
/// operator would have been a hole -- a function whose approval analysis reports fewer permissions
/// than it can use, silently, with no error anywhere.
///
/// `analysisVersion` 4 closed it: `analyze` records `EInfix` now, so an effectful operator would be
/// seen. This test is therefore no longer the only thing standing between that hole and us, and it
/// is kept anyway. It states an intent that is worth stating (an operator should not act), it costs
/// nothing, and it is what made the hole visible in the first place: the report that listed all
/// thirteen operators as unreachable is what sent me to look at `analyze`.
let operatorDispatchedBuiltinsArePure =
  test "operator-dispatched builtins declare no effects" {
    let set = Platforms.Sets.everythingFor PT.PackageManager.empty

    let offenders =
      set.builtins.fns
      |> Seq.filter (fun (KeyValue(k, _)) -> PT.InfixFnName.isOperatorDispatched k.name)
      |> Seq.filter (fun (KeyValue(_, fn)) -> not (Set.isEmpty fn.callEffects))
      |> Seq.map (fun (KeyValue(k, fn)) ->
        let es =
          fn.callEffects
          |> Set.toList
          |> List.map Effects.name
          |> List.sort
          |> String.concat " "
        $"{k.name} declares {es}")
      |> List.ofSeq

    Expect.equal
      offenders
      []
      "An operator-dispatched builtin declares effects. `CallGraph` cannot see it, so \
       `PackagePermissions` will not report those effects as required and an approval will \
       understate what the code can do. Either make the builtin pure, teach `CallGraph.analyze` to \
       record `EInfix`, or both."
  }


// ── a platform we did not ship ────────────────────────────────────────────────
//
// The goal these tests exist for: a library, written by someone else, that brings BOTH a set of
// builtins and a capability of its own that it advertises and you can control. Everything below
// composes a platform this repo does not contain, declaring an effect this runtime has never heard
// of, and asks whether the seams hold.
//
// It is deliberately not wired into any shipped set. NativeAOT is a closed world, so a real third
// party composes at publish time; what these assert is that the composition API, the fingerprint,
// the effect surface and the permission check all accept a stranger.

/// The capability a vendor's platform declares. Namespaced, so two vendors cannot collide on
/// `serial` and neither can shadow a well-known effect.
let private serial : Effects.Effect =
  match Effects.custom "acme/serial" with
  | Some effect -> effect
  | None -> Exception.raiseInternal "acme/serial should be a valid custom effect name" []

let private foreignPlatform (effect : Effects.Effect) : Platform =
  { name = "AcmeSerial"
    version = 0
    description = "Talking to a serial port, from a vendor we do not ship."
    builtins = Builtin.make [] [ stubFn "acmeSerialWrite" (Set.singleton effect) ]
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }

let aForeignPlatformComposes =
  test "a platform declaring an effect this runtime never heard of composes" {
    let core = stubPlatform "Core" [] [ stubFn "coreName" Set.empty ]
    let set = PlatformSet.make [ core; foreignPlatform serial ] []
    Expect.equal
      (PlatformSet.effectSurface set)
      (Set.singleton serial)
      "the vendor's capability is in the set's effect surface, under its own name"
    Expect.equal
      (PlatformSet.qualify "acmeSerialWrite" set)
      "AcmeSerial#acmeSerialWrite"
      "and its builtin resolves to it"
  }

let aForeignEffectIsInTheFingerprint =
  test "renaming a custom effect changes the fingerprint" {
    // The fingerprint is what answers "is the primitive floor the same as when this was approved".
    // A capability quietly becoming a different capability under the same builtin name is exactly
    // the change an approval must not survive.
    let core = stubPlatform "Core" [] [ stubFn "coreName" Set.empty ]
    let other =
      match Effects.custom "acme/gpio" with
      | Some effect -> effect
      | None -> Exception.raiseInternal "acme/gpio should be valid" []
    let before = PlatformSet.make [ core; foreignPlatform serial ] []
    let after = PlatformSet.make [ core; foreignPlatform other ] []
    Expect.notEqual
      before.fingerprint
      after.fingerprint
      "the same builtin reaching a different capability is a different floor"
  }

let aForeignEffectCannotShadowAWellKnownOne =
  test "a custom effect name must be namespaced, and cannot be a well-known one" {
    // Without this an unvalidated `Custom "http"` would be a second, unrelated value that renders
    // as `http`, matches no `Rule.Effect Http`, and reads in every message as though it did.
    Expect.isNone (Effects.custom "http") "a bare well-known name is refused"
    Expect.isNone (Effects.custom "native") "including native"
    Expect.isNone (Effects.custom "serial") "an unnamespaced name is refused"
    Expect.isNone (Effects.custom "Acme/Serial") "so is upper case"
    Expect.isNone (Effects.custom "acme/serial/extra") "and a third segment"
    Expect.isSome (Effects.custom "acme/serial") "owner/name is the shape"
  }

let aForeignEffectResolvesByName =
  test "a custom effect survives a round trip through its name" {
    // Every serializer crosses effects by name through `fromName`, so this IS the wire format.
    Expect.equal (Effects.name serial) "acme/serial" "renders as itself"
    Expect.equal (Effects.fromName "acme/serial") (Some serial) "and resolves back"
    Expect.equal
      (Effects.fromName "file-read")
      (Some Effects.Effect.FileRead)
      "well-known names still win"
  }

let aForeignEffectIsGranted =
  test "a policy can allow, and refuse, a capability the runtime never heard of" {
    // The half of the goal that is about CONTROL: a vendor advertising a capability is only
    // useful if the person installing it can say no to that capability specifically.
    let request =
      match Permission.Request.custom serial with
      | Ok request -> request
      | Error e -> Exception.raiseInternal e []

    let allowed = Permission.Policy.create [ Permission.Rule.Effect serial ] []
    Expect.isTrue
      (Permission.Policy.allows request allowed)
      "a rule naming the capability allows it"

    // Allow-everything-else is the interesting case: the vendor's capability must not be reachable
    // through any of the effects we DID ship, or advertising it separately would be theatre.
    let everythingElse =
      Permission.Policy.create
        (Effects.all |> List.map Permission.Rule.Effect)
        []
    Expect.isFalse
      (Permission.Policy.allows request everythingElse)
      "every well-known effect granted still does not reach a custom one"

    let denied =
      Permission.Policy.create
        [ Permission.Rule.All ]
        [ Permission.Rule.Effect serial ]
    Expect.isFalse
      (Permission.Policy.allows request denied)
      "and an explicit deny beats allow-all"
  }

let aForeignEffectSuggestsItsOwnRule =
  test "a denied custom capability names the rule that would grant it" {
    // `Native` has no useful rule text, because it is keyed per builtin and grantable only whole.
    // A custom effect is different: its name IS the grantable unit, so the denial can tell you
    // exactly what to type.
    let request =
      match Permission.Request.custom serial with
      | Ok request -> request
      | Error e -> Exception.raiseInternal e []
    Expect.equal
      (Permission.Request.suggestRule request)
      (Some "acme/serial")
      "the suggestion is the effect's own name"
  }

let aCustomRequestNeedsACustomEffect =
  test "a custom request cannot be built from a well-known effect" {
    Expect.isError
      (Permission.Request.custom Effects.Effect.Native)
      "Native is not a custom capability"
    Expect.isError
      (Permission.Request.custom Effects.Effect.FileRead)
      "nor is a scoped one"
  }


/// The always-on floor is stated in F# (`Platforms.Sets.alwaysOn`, which builds the guest set) and
/// again in Dark (`Cli.Platforms.alwaysOn`, which refuses to switch one off). Two copies with
/// nothing checking them is a pair that drifts, and the drift is silent in the worst direction:
/// the CLI would let you switch off something the runtime forces back on, and then print `off`
/// next to a platform that is running.
///
/// Same shape as the `effectCases` pin in `Builtin.Tests`, and for the same reason.
let alwaysOnListsAgree =
  test "the Dark always-on list matches the runtime's" {
    let source =
      System.IO.File.ReadAllText(
        System.IO.Path.Combine(
          PackageSurface.findRepoRoot (),
          "packages",
          "darklang",
          "cli",
          "platforms.dark"
        )
      )
    let start = source.IndexOf "val alwaysOn ="
    Expect.isGreaterThan start -1 "platforms.dark should define alwaysOn"
    let stop = source.IndexOf("]", start)
    let block = source.Substring(start, stop - start)
    let darkNames =
      System.Text.RegularExpressions.Regex.Matches(block, "\"([A-Za-z]+)\"")
      |> Seq.map (fun m -> m.Groups[1].Value)
      |> List.ofSeq
    Expect.equal
      darkNames
      Platforms.Sets.alwaysOn
      "Dark's alwaysOn must match Platforms.Sets.alwaysOn"
  }

/// The floor has to be able to render its own failure. `Core` alone cannot: the pretty printer is
/// Dark, and turning a hash back into a name is `package-read`. This is what makes `Store` part of
/// the floor rather than a convenience, and it is affordable only because `Store` reaches exactly
/// that one effect.
let theFloorCanNameThings =
  test "the always-on floor reaches package-read and nothing more" {
    let set = Platforms.Sets.byNames Platforms.Sets.alwaysOn
    Expect.equal
      (PlatformSet.effectSurface set)
      (Set.singleton Effects.Effect.PackageRead)
      "a floor that needed file-write or native to name things would not be a floor worth having"
  }


// ── a platform described rather than written ───────────────────────────────────
//
// Every platform in this repo is hand-written `BuiltInFn` records. One that arrives as an artifact
// cannot be: nothing in this binary knows its names or signatures until it says so. `External`
// turns that description into the same `Builtins`, and these ask whether the rest of the runtime
// can tell the difference. It should not be able to.

/// The capability our described platform claims. Nothing in this runtime ships it.
let private describedEffect : Effects.Effect =
  match Effects.custom "acme/serial" with
  | Some effect -> effect
  | None -> Exception.raiseInternal "acme/serial should be valid" []

/// `readTag : Unit -> String`, described, answering from a counter the runtime cannot see.
let private describedFns (effects : Set<Effects.Effect>) : List<External.Fn> =
  [ { name = "acmeReadTag"
      version = 0
      parameters = [ ("unit", TUnit) ]
      returnType = TString
      effects = effects
      description = "described, not written" } ]

let private describedPlatform (effects : Set<Effects.Effect>) (answer : string) : Platform =
  { name = "AcmeSerial"
    version = 0
    description = "A platform this repo does not contain."
    builtins =
      External.builtins
        // Stands in for the pipe. The point is that the runtime cannot tell.
        (fun _index _args -> Ply(RT.DString answer))
        (describedFns effects)
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }

/// Call the described builtin against Core plus that platform, under `policy`.
///
/// By name through `executeFunction` rather than by parsing `Builtin.acmeReadTag ()`. The parser
/// resolves builtin names against the state IT runs under, which is the real catalog and not the
/// set composed here, so a described platform's builtins are invisible to it. That is a real gap
/// and it is the next item on the plan; it is not what these tests are about.
let private runDescribed
  (effects : Set<Effects.Effect>)
  (policy : Permission.Policy)
  =
  task {
    let core = Platforms.Sets.sealedCompute ()
    let set =
      PlatformSet.make (core.platforms @ [ describedPlatform effects "TAG-42" ]) []
    let pmRT =
      PT2RT.PackageManager.toRT
        set.builtins.values
        LibExecution.ProgramTypes.PackageManager.empty
    let state =
      Exe.createState
        set.builtins
        pmRT
        Exe.noTracing
        RT.consoleReporter
        RT.consoleNotifier
        { dbs = Map.empty }
      |> Exe.setInstancePolicy policy
    let name = RT.FQFnName.FQFnName.Builtin(RT.FQFnName.builtin "acmeReadTag" 0)
    return!
      Exe.executeFunction state name [] (NEList.singleton RT.DUnit)
  }

let aDescribedPlatformComposes =
  test "a described platform composes and is indistinguishable in the manifest" {
    let core = Platforms.Sets.sealedCompute ()
    let described = describedPlatform (Set.singleton describedEffect) "TAG-42"
    let set = PlatformSet.make (core.platforms @ [ described ]) []
    Expect.equal
      (PlatformSet.qualify "acmeReadTag" set)
      "AcmeSerial#acmeReadTag"
      "the described builtin resolves to its platform"
    Expect.isTrue
      (Set.contains describedEffect (PlatformSet.effectSurface set))
      "and its capability is in the set's effect surface"
    Expect.equal (Platform.fnCount described) 1 "one builtin, counted like any other"

    // The fingerprint comes out of the description for free, since `manifestText` hashes names,
    // signatures and effects and the description is where a synthesized builtin's come from. Worth
    // asserting rather than assuming: it is what makes a described platform pinnable.
    let sameAgain = describedPlatform (Set.singleton describedEffect) "TAG-42"
    Expect.equal
      (Platform.fingerprint described)
      (Platform.fingerprint sameAgain)
      "the same description fingerprints the same"

    let otherEffect =
      match Effects.custom "acme/gpio" with
      | Some effect -> effect
      | None -> Exception.raiseInternal "acme/gpio should be valid" []
    Expect.notEqual
      (Platform.fingerprint described)
      (Platform.fingerprint (describedPlatform (Set.singleton otherEffect) "TAG-42"))
      "a description claiming a different capability fingerprints differently"

    // And the body is NOT in it, which is the honest limit. Two platforms answering differently
    // through the same declared surface are the same fingerprint, so the hash pins the CONTRACT.
    // Pinning the artifact is the content hash's job, separately.
    Expect.equal
      (Platform.fingerprint described)
      (Platform.fingerprint (describedPlatform (Set.singleton describedEffect) "TAG-99"))
      "the answer is not in the fingerprint; the artifact hash is what pins that"
  }

let aDescribedBuiltinRuns =
  testTask "a described builtin runs, and its declared effect is enforced" {
    // The two halves that matter. It executes at all, and the ambient gate checks the effect it
    // CLAIMED rather than anything about its body, which is the only thing a runtime can check
    // about code it did not compile.
    let granted = Permission.Policy.create [ Permission.Rule.Effect describedEffect ] []
    let! allowed =
      runDescribed (Set.singleton describedEffect) granted
    match allowed with
    | Ok(RT.DString "TAG-42") -> ()
    | Ok other -> failtest $"described builtin answered wrongly: {other}"
    | Error(rte, _) -> failtest $"described builtin raised: {rte}"
  }

let aDescribedBuiltinIsDeniedWithoutTheGrant =
  testTask "a described builtin is refused when its capability is not granted" {
    // Allow-everything-else on purpose: if the custom effect were reachable through any effect we
    // already ship, declaring it separately would be theatre.
    let everythingElse =
      Permission.Policy.create (Effects.all |> List.map Permission.Rule.Effect) []
    let! denied =
      runDescribed (Set.singleton describedEffect) everythingElse
    match denied with
    | Error _ -> ()
    | Ok other ->
      failtest $"described builtin ran without a grant for its capability: {other}"
  }

/// Parse AND run `Builtin.acmeReadTag ()` under a state whose catalog includes the described
/// platform, which is what shipping one would actually mean.
///
/// The other described-platform tests call by name through `executeFunction`, because the shared
/// parse helper runs under its own state carrying the stock catalog. This one asks the question
/// that matters: does a platform composed into the HOST's set become visible to name resolution?
let aDescribedBuiltinResolvesByName =
  testTask "a described platform's builtin resolves at parse time when the host has it" {
    let pm = TestUtils.TestUtils.pmPT
    let described = describedPlatform (Set.singleton describedEffect) "TAG-42"
    let set = PlatformSet.make ((Platforms.Sets.everythingFor pm).platforms @ [ described ]) []
    let pmRT = PT2RT.PackageManager.toRT set.builtins.values pm
    let state =
      Exe.createState
        set.builtins
        pmRT
        Exe.noTracing
        RT.consoleReporter
        RT.consoleNotifier
        { dbs = Map.empty }
      |> Exe.setInstancePolicy
        (Permission.Policy.create [ Permission.Rule.All ] [])

    // Parse under THIS state, not the shared helper's.
    let parser =
      RT.FQFnName.fqPackage (LibExecution.PackageRefs.Fn.LanguageTools.Parser.parsePTExpr ())
    let! parsed =
      Exe.executeFunction
        state
        parser
        []
        (NEList.singleton (RT.DString "Builtin.acmeReadTag ()"))

    match parsed with
    | Error(rte, _) -> failtest $"parsing raised: {rte}"
    | Ok dval ->
      match
        LibExecution.CommonToDarkTypes.Result.fromDT
          LibExecution.ProgramTypesToDarkTypes.Expr.fromDT
          dval
          identity
      with
      | Error _ -> failtest "the parser refused a described builtin's name"
      | Ok ptExpr ->
        let! ran = Exe.executeExpr state (PT2RT.Expr.toRT Map.empty 0 None ptExpr)
        match ran with
        | Ok(RT.DString "TAG-42") -> ()
        | Ok other -> failtest $"resolved but answered wrongly: {other}"
        | Error(rte, _) -> failtest $"resolved but raised: {rte}"
  }


let tests =
  testList
    "platform"
    [ collisionIsRefused
      missingRequirementIsRefused
      satisfiedRequirementComposes
      fingerprintIsStable
      fingerprintNoticesAnEffectChange
      fingerprintNoticesAnOwnershipMove
      ownershipResolves
      effectOriginsNameThePlatforms
      declaredEffectsMatchReality
      nativeBuiltinsAreInventoried
      builtinsAreWrappedInTheirPlatformsHome
      requiresIsJustCore
      smallSetsCompose
      catalogComposes
      sealedComputeIsPure
      coreOnlyRuntimeComputes
      coreOnlySetExcludesEverythingElse
      sealedHostLinksOnlyCore
      operatorDispatchedBuiltinsArePure
      everyPlatformDeclaresItsStoreNeed
      aForeignPlatformComposes
      aForeignEffectIsInTheFingerprint
      aForeignEffectCannotShadowAWellKnownOne
      aForeignEffectResolvesByName
      aForeignEffectIsGranted
      aForeignEffectSuggestsItsOwnRule
      aCustomRequestNeedsACustomEffect
      alwaysOnListsAgree
      theFloorCanNameThings
      aDescribedPlatformComposes
      aDescribedBuiltinRuns
      aDescribedBuiltinIsDeniedWithoutTheGrant
      aDescribedBuiltinResolvesByName ]
