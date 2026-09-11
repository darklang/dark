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

      // `policy-read` and `policy-write` rather than `Native`. The distinction these exist to draw:
      // reading your own instance policy is not the same grant as handing over the machine, and
      // `Native` said it was. Editing policy is still powerful, since policy gates every other
      // platform, but powerful and unscopeable are different claims and only one of them is true
      // here. `canManagePolicies` is what refuses guest code; the effect is what a POLICY can
      // reason about.
      "Policy", set [ Effects.Effect.PolicyRead; Effects.Effect.PolicyWrite ]

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
        (fun _state _name _args -> Ply(RT.DString answer))
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

// ── the manifest ──────────────────────────────────────────────────────────────

let private goodManifest : External.Manifest =
  { owner = "acme"
    name = "AcmeSerial"
    version = 0
    description = "A platform this repo does not contain."
    requires = [ "Core" ]
    requiresStore = false
    artifacts = []
    types = []
    fns = describedFns (Set.singleton describedEffect) }

let private rejectionOf (m : External.Manifest) : List<string> =
  match External.Manifest.toPlatform (fun _ _ _ -> Ply RT.DUnit) m with
  | Ok _ -> failtest "expected this manifest to be refused"
  | Error r -> r.problems

let aGoodManifestBecomesAPlatform =
  test "a well-formed manifest becomes a platform that composes" {
    match External.Manifest.toPlatform (fun _ _ _ -> Ply(RT.DString "TAG-42")) goodManifest with
    | Error r -> failtest $"refused a good manifest: {r.problems}"
    | Ok platform ->
      let core = Platforms.Sets.sealedCompute ()
      let set = PlatformSet.make (core.platforms @ [ platform ]) []
      Expect.equal
        (PlatformSet.qualify "acmeReadTag" set)
        "AcmeSerial#acmeReadTag"
        "composes like any other"
      Expect.isTrue
        (Set.contains describedEffect (PlatformSet.effectSurface set))
        "and its declared capability is in the surface"
  }

let aManifestNamesEveryProblemAtOnce =
  test "a bad manifest reports every problem, not the first" {
    // Plural on purpose. One problem per attempt is a bad afternoon for whoever is writing the
    // manifest, and the checks are independent so there is no reason to stop at the first.
    let bad =
      { goodManifest with
          owner = "acme corp"
          name = "Acme Serial"
          version = -1 }
    let problems = rejectionOf bad
    Expect.hasLength problems 3 "three independent problems, three messages"
    Expect.isTrue
      (problems |> List.exists (fun p -> p.Contains "owner"))
      "the owner is named"
    Expect.isTrue
      (problems |> List.exists (fun p -> p.Contains "version"))
      "so is the version"
  }

let aManifestRefusesWhatCannotCross =
  test "a manifest refuses parameters that cannot cross a process boundary" {
    // A bare function parameter is the obvious case. The nested ones matter more, because the
    // failure would otherwise wait until somebody actually passed a lambda.
    let withParam (typ : TypeReference) =
      { goodManifest with
          fns =
            [ { name = "acmeReadTag"
                version = 0
                parameters = [ ("f", typ) ]
                returnType = TString
                effects = Set.empty
                description = "" } ] }

    let fnType = TFn(NEList.singleton TInt64, TInt64)
    Expect.isNonEmpty (rejectionOf (withParam fnType)) "a bare function"
    Expect.isNonEmpty (rejectionOf (withParam (TList fnType))) "a list of functions"
    Expect.isNonEmpty
      (rejectionOf (withParam (TDict(TString, fnType))))
      "a dict holding functions"
    Expect.isNonEmpty (rejectionOf (withParam (TDB TString))) "a database handle"
    Expect.isNonEmpty (rejectionOf (withParam (TStream TString))) "a stream"

    // And the control: ordinary nesting is fine, so the check is not just refusing everything.
    match
      External.Manifest.toPlatform
        (fun _ _ _ -> Ply RT.DUnit)
        (withParam (TList(TDict(TString, TInt64))))
    with
    | Ok _ -> ()
    | Error r -> failtest $"refused a type that travels fine: {r.problems}"
  }

let aManifestRefusesADuplicateBuiltin =
  test "a manifest declaring one builtin twice is refused" {
    // `Builtin.make` is last-write-wins over a dictionary, so without this the second silently
    // wins and the manifest describes something the platform does not provide.
    let fn =
      match goodManifest.fns with
      | [ only ] -> only
      | other -> failtest $"expected one described fn, got {List.length other}"
    let problems = rejectionOf { goodManifest with fns = [ fn; fn ] }
    Expect.isTrue
      (problems |> List.exists (fun p -> p.Contains "more than once"))
      "the duplicate is named"
  }

// ── types a manifest names ────────────────────────────────────────────────────

let private parseType (s : string) : External.NamedType =
  match External.NamedType.parse s with
  | Ok t -> t
  | Error e -> failtest $"failed to parse '{s}': {e}"

let namedTypesRoundTrip =
  test "a named type parses and renders back to itself" {
    // Round trip rather than structural assertions: the point of the syntax is that a person can
    // write it and see it again, and `dark platforms` will show what was declared.
    let cases =
      [ "String"
        "Unit"
        "List<Int64>"
        "Dict<String, Int64>"
        "Tuple<Int64, String>"
        "Tuple<Int64, String, Bool>"
        "Stdlib.Result<String, String>"
        "List<Stdlib.Option<Dict<String, List<UInt8>>>>"
        "Acme.Serial.Config" ]
    for case in cases do
      Expect.equal (External.NamedType.render (parseType case)) case $"round trip of {case}"
  }

let namedTypeParseRefusesNonsense =
  test "a named type refuses what it cannot read, with the offset" {
    let refuses (s : string) =
      match External.NamedType.parse s with
      | Ok t ->
        failtest $"parsed '{s}' as {External.NamedType.render t} when it should have refused"
      | Error _ -> ()
    refuses "List<"
    refuses "List<Int64"
    refuses "Dict<String>"
    refuses "Dict<String, Int64, Bool>"
    refuses "List<Int64> trailing"
    refuses "Tuple<Int64>"
    refuses ""
  }

let namedTypeResolvesAgainstTheConsumersStore =
  test "a named type resolves package types through the consumer, and says so when it cannot" {
    // The whole reason a manifest names types instead of carrying hashes. `lookup` stands in for
    // the consumer's store: the SAME manifest resolves differently on two instances, and that is
    // correct rather than alarming.
    let hash = "0123456789abcdef"
    let lookup name =
      if name = "Stdlib.Result" then Some(RT.FQTypeName.fqPackage hash) else None

    match External.NamedType.resolve lookup (parseType "Stdlib.Result<String, String>") with
    | Error e -> failtest $"should have resolved: {e}"
    | Ok(RT.TCustomType(nr, [ RT.TString; RT.TString ])) ->
      match nr.resolved with
      | Ok(RT.FQTypeName.Package(RT.Hash h)) ->
        Expect.equal h hash "resolved to the consumer's hash for that type"
      | Error e -> failtest $"name resolution carried an error: {e}"
    | Ok other -> failtest $"resolved to the wrong thing: {other}"

    match External.NamedType.resolve lookup (parseType "Acme.Unknown") with
    | Ok _ -> failtest "resolved a type this instance does not have"
    | Error e ->
      Expect.stringContains e "Acme.Unknown" "the unresolvable name is in the message"

    // And nesting resolves all the way down rather than only at the top.
    match External.NamedType.resolve lookup (parseType "List<Acme.Unknown>") with
    | Ok _ -> failtest "resolved a missing type nested inside a list"
    | Error _ -> ()

    match External.NamedType.resolve lookup (parseType "List<Dict<String, Int64>>") with
    | Ok(RT.TList(RT.TDict(RT.TString, RT.TInt64))) -> ()
    | Ok other -> failtest $"built the wrong type: {other}"
    | Error e -> failtest $"should have resolved: {e}"
  }

// ── the text manifest ─────────────────────────────────────────────────────────

let private manifestText =
  """DARK-PLATFORM-MANIFEST 1
owner acme
name AcmeSerial
version 0
description Talking to a serial port.
requires Core
store no
artifact linux-x64 e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
artifact osx-arm64 a3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b856

# the one thing it does
fn acmeReadTag 0
param port String
returns String
effect acme/serial
doc Read the tag at a port.

fn acmeWriteTag 0
param port String
param value String
returns Stdlib.Result<Unit, String>
effect acme/serial
effect clock
"""

/// Stands in for the consumer's store.
let private lookupResult (name : string) : Option<RT.FQTypeName.FQTypeName> =
  if name = "Stdlib.Result" then
    Some(RT.FQTypeName.fqPackage "0123456789abcdef")
  else
    None

let aTextManifestParsesAndRoundTrips =
  test "a written manifest parses, and rendering it back parses the same" {
    match Written.parse manifestText with
    | Error problems -> failtest $"failed to parse: {problems}"
    | Ok written ->
      Expect.equal written.owner "acme" "owner"
      Expect.equal written.name "AcmeSerial" "name"
      Expect.equal written.requires [ "Core" ] "requires"
      Expect.isFalse written.requiresStore "store no"
      Expect.hasLength written.artifacts 2 "one executable per target"
      Expect.hasLength written.fns 2 "two functions"

      match written.fns with
      | [ readTag; writeTag ] ->
        Expect.hasLength readTag.parameters 1 "readTag takes one"
        Expect.equal readTag.effects [ "acme/serial" ] "and declares its capability"
        Expect.equal readTag.description "Read the tag at a port." "doc line attaches"
        Expect.hasLength writeTag.parameters 2 "writeTag takes two"
        Expect.equal writeTag.effects [ "acme/serial"; "clock" ] "two effects, in order"
      | other -> failtest $"unexpected fns: {other}"

      // Render and reparse rather than comparing text: the format is a contract about MEANING, and
      // insisting the bytes match would pin the comment and blank-line layout too.
      match Written.parse (Written.render written) with
      | Error problems -> failtest $"rendered form did not parse: {problems}"
      | Ok again -> Expect.equal again written "render then parse is identity"
  }

let aTextManifestCollectsEveryProblem =
  test "a written manifest reports every bad line, with line numbers" {
    let bad =
      """DARK-PLATFORM-MANIFEST 1
owner acme
name AcmeSerial
version zero
store maybe
nonsense here
param orphan String
fn goodFn 0
param p List<
"""
    match Written.parse bad with
    | Ok _ -> failtest "parsed a manifest full of problems"
    | Error problems ->
      // Every bad line, not the first, and each one says where.
      Expect.hasLength problems 5 "five bad lines, five messages"
      Expect.isTrue
        (problems |> List.forall (fun p -> p.StartsWith "line "))
        "every problem names its line"
      Expect.isTrue
        (problems |> List.exists (fun p -> p.Contains "unknown key 'nonsense'"))
        "an unknown key is an error rather than ignored"
      Expect.isTrue
        (problems |> List.exists (fun p -> p.Contains "before any 'fn'"))
        "a param with no function above it is caught"
  }

let aTextManifestNeedsItsHeader =
  test "a written manifest without its header is refused outright" {
    // Fail on the first line rather than trying to parse an arbitrary file as a manifest, so
    // pointing this at the wrong path says so instead of reporting forty unknown keys.
    match Written.parse "owner acme\nname AcmeSerial\n" with
    | Ok _ -> failtest "parsed something with no header"
    | Error problems -> Expect.hasLength problems 1 "one problem: the header"
  }

let aWrittenManifestResolvesToAPlatform =
  testTask "a written manifest resolves against the consumer and runs" {
    // The whole path in one test: text in, a platform out, a call through it.
    match Written.parse manifestText with
    | Error problems -> failtest $"parse: {problems}"
    | Ok written ->
      match Written.resolve lookupResult written with
      | Error r -> failtest $"resolve: {r.problems}"
      | Ok manifest ->
        match
          External.Manifest.toPlatform (fun _ _ _ -> Ply(RT.DString "TAG-42")) manifest
        with
        | Error r -> failtest $"toPlatform: {r.problems}"
        | Ok platform ->
          let core = Platforms.Sets.sealedCompute ()
          let set = PlatformSet.make (core.platforms @ [ platform ]) []
          Expect.equal
            (PlatformSet.qualify "acmeReadTag" set)
            "AcmeSerial#acmeReadTag"
            "composed from text"
          Expect.isTrue
            (Set.contains describedEffect (PlatformSet.effectSurface set))
            "its named capability became a real effect"
  }

let aWrittenManifestReportsUnresolvableNames =
  test "a written manifest naming things this instance lacks says which" {
    let written =
      match Written.parse manifestText with
      | Ok w -> w
      | Error problems -> failtest $"parse: {problems}"
    // A store without `Stdlib.Result`, and a capability that is not an effect at all.
    let withBadEffect =
      { written with
          fns =
            written.fns
            |> List.map (fun fn -> { fn with effects = [ "Acme/Serial" ] }) }
    match Written.resolve (fun _ -> None) withBadEffect with
    | Ok _ -> failtest "resolved against a store that has neither"
    | Error r ->
      Expect.isTrue
        (r.problems |> List.exists (fun p -> p.Contains "Stdlib.Result"))
        "the missing type is named"
      Expect.isTrue
        (r.problems |> List.exists (fun p -> p.Contains "is not an effect"))
        "and so is the thing that is not an effect"
  }

// ── resolving against a real store ────────────────────────────────────────────

let manifestLocationsAreFullyQualified =
  test "a manifest's type names are split fully, with no implicit owner" {
    // Dark source has an implicit owner; a manifest deliberately does not, because a third
    // party's manifest should not depend on whose shortcuts are in play.
    // Note the doubled `Result`: the type lives in a module of the same name, so its fully
    // qualified name really is `Darklang.Stdlib.Result.Result`. A manifest has to write that,
    // which looks odd and is unambiguous, which is the trade a manifest should take.
    match LibDB.PlatformInstall.location "Darklang.Stdlib.Result.Result" with
    | Some loc ->
      Expect.equal loc.owner "Darklang" "owner"
      Expect.equal loc.modules [ "Stdlib"; "Result" ] "modules"
      Expect.equal loc.name "Result" "name"
    | None -> failtest "should have split a fully qualified name"

    match LibDB.PlatformInstall.location "Acme.Serial.Deep.Config" with
    | Some loc -> Expect.equal loc.modules [ "Serial"; "Deep" ] "nested modules"
    | None -> failtest "should have split a deeper name"

    // A bare name is refused rather than guessed at.
    Expect.isNone (LibDB.PlatformInstall.location "Result") "a bare name has no owner"
  }

let aManifestResolvesAgainstTheRealStore =
  testTask "a manifest naming a real package type resolves against the store" {
    // The point of naming rather than hashing: the SAME text resolves here, and would fail on an
    // instance without that type.
    let text =
      """DARK-PLATFORM-MANIFEST 1
owner acme
name AcmeSerial
version 0
store no

fn acmeReadTag 0
param port String
returns Darklang.Stdlib.Result.Result<String, String>
effect acme/serial
"""
    let written =
      match Written.parse text with
      | Ok w -> w
      | Error problems -> failtest $"parse: {problems}"

    let! (resolved : Result<External.Manifest, External.Rejection>) =
      LibDB.PlatformInstall.resolve TestUtils.TestUtils.pmPT written |> Ply.toTask
    match resolved with
    | Error r -> failtest $"should have resolved against the real store: {r.problems}"
    | Ok manifest ->
      match manifest.fns with
      | [ fn ] ->
        match fn.returnType with
        | RT.TCustomType(nr, [ RT.TString; RT.TString ]) ->
          match nr.resolved with
          | Ok _ -> ()
          | Error e -> failtest $"resolved to an unresolved name: {e}"
        | other -> failtest $"return type resolved wrongly: {other}"
      | other -> failtest $"expected one fn, got {List.length other}"
  }

let aManifestNamingAMissingTypeSaysSo =
  testTask "a manifest naming a type this instance lacks names it" {
    let text =
      """DARK-PLATFORM-MANIFEST 1
owner acme
name AcmeSerial
version 0
store no

fn acmeReadTag 0
param port String
returns Acme.Serial.NoSuchType
effect acme/serial
"""
    let written =
      match Written.parse text with
      | Ok w -> w
      | Error problems -> failtest $"parse: {problems}"

    let! (resolved : Result<External.Manifest, External.Rejection>) =
      LibDB.PlatformInstall.resolve TestUtils.TestUtils.pmPT written |> Ply.toTask
    match resolved with
    | Ok _ -> failtest "resolved a type the store does not have"
    | Error r ->
      Expect.isTrue
        (r.problems |> List.exists (fun p -> p.Contains "Acme.Serial.NoSuchType"))
        "the missing type is named in the problem"
  }

// ── artifacts ─────────────────────────────────────────────────────────────────

let private withArtifacts (artifacts : List<string * string>) =
  { goodManifest with artifacts = artifacts }

let aManifestAddressesItsExecutablesByHash =
  test "a manifest names one executable per target, by hash" {
    let real = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    let m = withArtifacts [ ("linux-x64", real); ("osx-arm64", real) ]
    Expect.isEmpty (External.Manifest.problems m) "two targets is fine"
    Expect.equal
      (External.Manifest.artifactFor "linux-x64" m)
      (Some real)
      "and the one for this target is findable"

    // Not building for a target is an ordinary answer, not a problem. A platform may simply not
    // exist for your machine, which is worth saying at install rather than at spawn.
    Expect.isNone
      (External.Manifest.artifactFor "win-x64" m)
      "a target it does not build for answers None"
    Expect.isEmpty
      (External.Manifest.problems (withArtifacts []))
      "and a manifest with no artifacts at all is still well formed"
  }

let aManifestRefusesABadArtifactLine =
  test "a manifest refuses an artifact that is not a target and a hash" {
    let real = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    Expect.isNonEmpty
      (External.Manifest.problems (withArtifacts [ ("linux", real) ]))
      "a runtime identifier has two parts"
    Expect.isNonEmpty
      (External.Manifest.problems (withArtifacts [ ("Linux-X64", real) ]))
      "and is lower case"
    Expect.isNonEmpty
      (External.Manifest.problems (withArtifacts [ ("linux-x64", "deadbeef") ]))
      "a short hash is not a SHA-256"
    Expect.isNonEmpty
      (External.Manifest.problems (withArtifacts [ ("linux-x64", real.ToUpper()) ]))
      "nor is an upper case one, since the hash is the name and names are exact"

    // Two executables for one target would mean the manifest does not say which runs.
    Expect.isNonEmpty
      (External.Manifest.problems
        (withArtifacts [ ("linux-x64", real); ("linux-x64", real) ]))
      "one target, one executable"
  }

// ── the artifact cache ────────────────────────────────────────────────────────

/// The cache lives under the policy directory, so these run under an isolated one.
let private withArtifactCache (f : unit -> unit) =
  let dir =
    System.IO.Path.Combine(
      System.IO.Path.GetTempPath(),
      $"dark-artifacts-{System.Guid.NewGuid()}"
    )
  System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
  let restore = LibExecution.HostSecurity.policyDirectoryForTesting dir
  try
    f ()
  finally
    restore.Dispose()
    System.IO.Directory.Delete(dir, true)

let anArtifactIsCachedUnderItsOwnHash =
  test "an artifact lands under its hash and verifies" {
    withArtifactCache (fun () ->
      let bytes = System.Text.Encoding.UTF8.GetBytes "#!/bin/sh\necho hi\n"
      let hash = LibExecution.Blob.sha256Hex bytes

      match LibDB.PlatformArtifacts.verified hash with
      | Ok present -> Expect.isFalse present "not there before it is written"
      | Error e -> failtest e

      match LibDB.PlatformArtifacts.materialize hash bytes with
      | Error e -> failtest $"should have written: {e}"
      | Ok file ->
        Expect.isTrue (System.IO.File.Exists file) "the file is there"
        Expect.stringEnds file hash "and the hash is the whole filename"

      match LibDB.PlatformArtifacts.verified hash with
      | Ok present -> Expect.isTrue present "and now it verifies"
      | Error e -> failtest e)
  }

let anArtifactThatLiesIsRefusedBeforeTheWrite =
  test "bytes that are not what they claim never reach the disk" {
    withArtifactCache (fun () ->
      let real = System.Text.Encoding.UTF8.GetBytes "the real thing"
      let hash = LibExecution.Blob.sha256Hex real
      let other = System.Text.Encoding.UTF8.GetBytes "something else entirely"

      match LibDB.PlatformArtifacts.materialize hash other with
      | Ok _ -> failtest "wrote bytes that do not match their hash"
      | Error e -> Expect.stringContains e hash "the expected hash is in the message"

      // Checked before the write, so nothing lands under a name that would later be trusted.
      match LibDB.PlatformArtifacts.path hash with
      | Ok file -> Expect.isFalse (System.IO.File.Exists file) "and nothing was written"
      | Error e -> failtest e)
  }

let aTamperedArtifactFailsVerification =
  test "an artifact swapped on disk stops verifying" {
    // The reason verification is not just an install-time check. Anything that can write the cache
    // could otherwise replace the executable a hash approved, and the filename would still agree.
    withArtifactCache (fun () ->
      let bytes = System.Text.Encoding.UTF8.GetBytes "the approved binary"
      let hash = LibExecution.Blob.sha256Hex bytes
      match LibDB.PlatformArtifacts.materialize hash bytes with
      | Error e -> failtest e
      | Ok file ->
        System.IO.File.WriteAllBytes(file, System.Text.Encoding.UTF8.GetBytes "not that")
        match LibDB.PlatformArtifacts.verified hash with
        | Ok present -> Expect.isFalse present "a swapped file does not verify"
        | Error e -> failtest e)
  }

let anArtifactHashCannotBeAPath =
  test "a hash that is not a hash cannot name a file" {
    // The hash reaches here from a manifest, which came from outside, so it is checked rather than
    // trusted. Otherwise a manifest could name '../../../etc/cron.d/whatever'.
    withArtifactCache (fun () ->
      Expect.isError (LibDB.PlatformArtifacts.path "../../etc/passwd") "no traversal"
      Expect.isError (LibDB.PlatformArtifacts.path "deadbeef") "too short"
      Expect.isError
        (LibDB.PlatformArtifacts.path (String.replicate 64 "A"))
        "upper case is not the hash we store under")
  }

/// An instance that provides nothing, so an install is judged on the manifest alone.
let private nothingProvidesIt : LibDB.InstalledPlatforms.Provider = fun _ _ -> None

// ── a manifest stored as a package value ──────────────────────────────────────

let private manifestLocation : PT.PackageLocation =
  { owner = "acme"; modules = [ "AcmeSerial" ]; name = "manifest" }

/// A package manager holding one manifest value, the way the store would after installing.
let private pmWithManifest (body : PT.Expr) : PT.PackageManager =
  let hash = PT.FQValueName.package "manifest-under-test"
  { TestValues.pm with
      findValue =
        fun loc ->
          if loc = manifestLocation then Ply(Some hash) else TestValues.pm.findValue loc
      getValue =
        fun h ->
          if h = hash then
            Ply(
              Some
                ({ hash = h; description = "a platform manifest"; body = body }
                 : PT.PackageValue.PackageValue)
            )
          else
            TestValues.pm.getValue h }

let private stringLiteral (text : string) : PT.Expr =
  PT.EString(0UL, [ PT.StringText text ])

let aManifestInTheStoreResolves =
  testTask "a manifest stored as a package value reads back and resolves" {
    // Manifests ride sync this way rather than through package_blobs, which does not sync. A value
    // is text, content-addressed, approvable and pinnable, and authored like everything else.
    let text =
      """DARK-PLATFORM-MANIFEST 1
owner acme
name AcmeSerial
version 0
store no
artifact linux-x64 e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855

fn acmeReadTag 0
param port String
returns String
effect acme/serial
"""
    let! (result : Result<External.Manifest, External.Rejection>) =
      LibDB.PlatformInstall.manifestFrom (pmWithManifest (stringLiteral text)) manifestLocation
      |> Ply.toTask
    match result with
    | Error r -> failtest $"should have read the manifest: {r.problems}"
    | Ok manifest ->
      Expect.equal manifest.name "AcmeSerial" "the platform it describes"
      Expect.equal
        (External.Manifest.artifactFor "linux-x64" manifest)
        (Some "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        "and the executable it names for this target"
  }

let aManifestMustBeALiteral =
  testTask "a manifest that is computed rather than written is refused" {
    // Running package code to find out what a platform CLAIMS is the wrong order. The manifest is
    // what you read before deciding to trust it, so it has to be a literal rather than an
    // expression that produces one.
    let computed = PT.EApply(0UL, stringLiteral "not", [], NEList.singleton (stringLiteral "ok"))
    let! (result : Result<External.Manifest, External.Rejection>) =
      LibDB.PlatformInstall.manifestFrom (pmWithManifest computed) manifestLocation |> Ply.toTask
    match result with
    | Ok _ -> failtest "accepted a manifest that was not a literal"
    | Error r ->
      Expect.isTrue
        (r.problems |> List.exists (fun p -> p.Contains "literal"))
        "and says why"
  }

let aMissingManifestSaysSo =
  testTask "asking for a manifest that is not there says so rather than raising" {
    let! (result : Result<External.Manifest, External.Rejection>) =
      LibDB.PlatformInstall.manifestFrom TestValues.pm manifestLocation |> Ply.toTask
    match result with
    | Ok _ -> failtest "found a manifest in an empty store"
    | Error r -> Expect.isNonEmpty r.problems "with a reason"
  }

// ── fetching an artifact ──────────────────────────────────────────────────────

let anArtifactIsFetchedOnceAndReusedAfter =
  testTask "an artifact is fetched when missing and reused when present" {
    // Reused, not refetched: the point of the cache. Counting the source's calls is the only way
    // to tell those apart from outside.
    let bytes = System.Text.Encoding.UTF8.GetBytes "the platform executable"
    let hash = LibExecution.Blob.sha256Hex bytes
    let mutable fetches = 0
    let source : LibDB.PlatformArtifacts.Source =
      fun _ ->
        fetches <- fetches + 1
        Ply(Some bytes)

    let dir =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-artifacts-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
    let restore = LibExecution.HostSecurity.policyDirectoryForTesting dir
    try
      let! first = LibDB.PlatformArtifacts.ensure source hash |> Ply.toTask
      match first with
      | Error e -> failtest $"first fetch failed: {e}"
      | Ok file -> Expect.isTrue (System.IO.File.Exists file) "it landed"
      Expect.equal fetches 1 "fetched once"

      let! second = LibDB.PlatformArtifacts.ensure source hash |> Ply.toTask
      Expect.isOk second "and is there the second time"
      Expect.equal fetches 1 "without asking the source again"

      // A swapped file is refetched rather than trusted, which is the whole reason the cache check
      // verifies instead of testing for existence.
      match LibDB.PlatformArtifacts.path hash with
      | Ok file ->
        System.IO.File.WriteAllBytes(file, System.Text.Encoding.UTF8.GetBytes "swapped")
      | Error e -> failtest e
      let! third = LibDB.PlatformArtifacts.ensure source hash |> Ply.toTask
      Expect.isOk third "a swapped artifact is replaced"
      Expect.equal fetches 2 "by fetching it again"
    finally
      restore.Dispose()
      System.IO.Directory.Delete(dir, true)
  }

let anUnavailableArtifactSaysWhatItMeans =
  testTask "an artifact nobody has says what that means" {
    let dir =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-artifacts-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
    let restore = LibExecution.HostSecurity.policyDirectoryForTesting dir
    try
      let hash = LibExecution.Blob.sha256Hex (System.Text.Encoding.UTF8.GetBytes "absent")
      let! result = LibDB.PlatformArtifacts.ensure (fun _ -> Ply None) hash |> Ply.toTask
      match result with
      | Ok _ -> failtest "produced a file for bytes nobody has"
      | Error e ->
        // Two plausible causes and the message names both, because at this layer we cannot tell
        // them apart and guessing wrong sends someone looking in the wrong place.
        Expect.stringContains e "fetched" "not fetched yet"
        Expect.stringContains e "this machine" "or not built for this target"
    finally
      restore.Dispose()
      System.IO.Directory.Delete(dir, true)
  }

let aLyingSourceIsRefused =
  testTask "a source that returns the wrong bytes is refused" {
    // The source is transport, and transport is not trusted. Whatever it hands back is checked
    // against the hash that was asked for before anything touches the disk.
    let dir =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-artifacts-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
    let restore = LibExecution.HostSecurity.policyDirectoryForTesting dir
    try
      let wanted = LibExecution.Blob.sha256Hex (System.Text.Encoding.UTF8.GetBytes "wanted")
      let source : LibDB.PlatformArtifacts.Source =
        fun _ -> Ply(Some(System.Text.Encoding.UTF8.GetBytes "something else"))
      let! result = LibDB.PlatformArtifacts.ensure source wanted |> Ply.toTask
      match result with
      | Ok _ -> failtest "accepted bytes that were not what was asked for"
      | Error e -> Expect.stringContains e "does not match its hash" "and says so"
      match LibDB.PlatformArtifacts.path wanted with
      | Ok file -> Expect.isFalse (System.IO.File.Exists file) "nothing was written"
      | Error e -> failtest e
    finally
      restore.Dispose()
      System.IO.Directory.Delete(dir, true)
  }

// ── a platform in another process ─────────────────────────────────────────────
//
// The fixture is Python, so it is a text file with nothing to build, and it knows nothing about
// Dark beyond the wire. What these assert is that a described platform backed by a real process
// composes, runs, is permission-checked, and fails like a value rather than a hang.

let private echoPlatformPath () =
  System.IO.Path.Combine(
    PackageSurface.findRepoRoot (),
    "backend",
    "testfiles",
    "platforms",
    "echo-platform.py"
  )

/// What the host tells the fixture at startup: the type names its manifest used, and what this
/// store made of them. `Result` is the whole point, since an enum on the wire carries a hash.
let private echoTypes : List<string * RT.FQTypeName.FQTypeName> =
  [ ("Darklang.Stdlib.Result.Result", LibExecution.Dval.resultType ()) ]

let private spawnedFns : List<External.Fn> =
  [ { name = "echoCounter"
      version = 0
      parameters = [ ("unit", TUnit) ]
      returnType = TInt64
      effects = Set.singleton describedEffect
      description = "state living outside the runtime" }
    { name = "echoShout"
      version = 0
      parameters = [ ("text", TString) ]
      returnType = TString
      effects = Set.singleton describedEffect
      description = "reads a Dval as well as writing one" }
    { name = "echoReach"
      version = 0
      parameters = [ ("unit", TUnit) ]
      returnType = TString
      effects = Set.singleton describedEffect
      description = "tries the network it never asked for" }
    { name = "echoResult"
      version = 0
      parameters = [ ("text", TString) ]
      returnType = TypeReference.result TString TString
      effects = Set.singleton describedEffect
      description = "returns something that is not a primitive" }
    { name = "echoBytes"
      version = 0
      parameters = [ ("bytes", TBlob) ]
      returnType = TBlob
      effects = Set.singleton describedEffect
      description = "bytes in and bytes out" }
    { name = "echoCrash"
      version = 0
      parameters = [ ("unit", TUnit) ]
      returnType = TUnit
      effects = Set.singleton describedEffect
      description = "exits without answering" } ]

let private spawnedPlatform (handle : LibDB.PlatformSpawn.Handle) : Platform =
  { name = "EchoPlatform"
    version = 0
    description = "a platform in another process"
    builtins = External.builtins (LibDB.PlatformSpawn.invoke handle) spawnedFns
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }

/// Call one builtin of the spawned platform, under a policy that grants its capability.
let private callSpawned (handle : LibDB.PlatformSpawn.Handle) (name : string) (arg : RT.Dval) =
  task {
    let core = Platforms.Sets.sealedCompute ()
    let set = PlatformSet.make (core.platforms @ [ spawnedPlatform handle ]) []
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
      |> Exe.setInstancePolicy
        (Permission.Policy.create [ Permission.Rule.Effect describedEffect ] [])
    let fnName = RT.FQFnName.FQFnName.Builtin(RT.FQFnName.builtin name 0)
    return! Exe.executeFunction state fnName [] (NEList.singleton arg)
  }

let aSpawnedPlatformAnswers =
  testTask "a platform in another process answers, and keeps its own state" {
    let handle =
      LibDB.PlatformSpawn.handleFor
        "EchoPlatform"
        (echoPlatformPath ())
        (Set.singleton describedEffect)
        echoTypes
    try
      // Its counter lives outside this runtime, so two calls to one process differ. That is the
      // proof it is really another process and not a clever closure.
      let! first = callSpawned handle "echoCounter" RT.DUnit
      let! second = callSpawned handle "echoCounter" RT.DUnit
      match first, second with
      | Ok(RT.DInt64 a), Ok(RT.DInt64 b) ->
        Expect.equal a 1L "the first call"
        Expect.equal b 2L "and the second, from the same process"
      | other -> failtest $"unexpected answers: %A{other}"

      // And it READS a Dval as well as writing one.
      let! shouted = callSpawned handle "echoShout" (RT.DString "quiet")
      match shouted with
      | Ok(RT.DString "QUIET") -> ()
      | other -> failtest $"unexpected: {other}"
    finally
      LibDB.PlatformSpawn.stop handle
  }

let theSandboxFollowsTheDeclaration =
  test "what a platform declared decides how its process is confined" {
    // Three branches, and the one that matters most is the third: a platform that can reach
    // anything gets no confinement and is told so, rather than a namespace that would read as
    // more than it is.
    let confined = LibDB.PlatformSandbox.plan (Set.singleton describedEffect) "/x"
    let asked =
      LibDB.PlatformSandbox.plan (Set.singleton Effects.Effect.Http) "/x"
    let unscopeable =
      LibDB.PlatformSandbox.plan (Set.singleton Effects.Effect.Native) "/x"

    // Only meaningful where a namespace can actually be entered; elsewhere all three say why not,
    // which is the correct answer on that machine and not a failure.
    if confined.confinement.StartsWith "no network" then
      Expect.notEqual confined.executable "/x" "a confined platform is started through a wrapper"
      Expect.equal asked.executable "/x" "one that asked for the network is started directly"
      Expect.stringContains
        asked.confinement
        "asked for the network"
        "and is told why it is not confined"

    Expect.equal unscopeable.executable "/x" "`native` gets no wrapper"
    Expect.stringContains
      unscopeable.confinement
      "not confined"
      "and says so, rather than implying a sandbox it does not have"
  }

let theShippedFetchManifestStillResolves =
  testTask "the Fetch pilot's manifest still parses and resolves against this store" {
    // The pilot names `Stdlib.HttpClient.Response` and `Stdlib.Result.Result` symbolically, and
    // resolves them against whatever store installs it. That is the design working, and it is also
    // how the shipped manifest rots: rename either type and this manifest stops resolving, with
    // nothing else to notice.
    let path =
      System.IO.Path.Combine(
        PackageSurface.findRepoRoot (),
        "backend",
        "testfiles",
        "platforms",
        "fetch.manifest"
      )
    match LibExecution.Platform.Written.parse (System.IO.File.ReadAllText path) with
    | Error problems -> failtest $"the shipped manifest does not parse: {problems}"
    | Ok written ->
      let! (resolved : Result<External.Manifest, External.Rejection>) =
        LibDB.PlatformInstall.resolve TestUtils.TestUtils.pmPT written |> Ply.toTask
      match resolved with
      | Error r -> failtest $"the shipped manifest does not resolve here: {r.problems}"
      | Ok manifest ->
        Expect.equal manifest.name "Fetch" "the platform it describes"
        // The handshake table, which is the thing that lets the plugin build a `Result` at all. An
        // empty one would mean the plugin gets no hashes and can only answer with primitives.
        Expect.isNonEmpty manifest.types "it resolved type names, and kept what they resolved to"
        let declared = manifest.fns |> List.map _.effects |> Set.unionMany
        Expect.equal
          declared
          (Set.singleton Effects.Effect.Http)
          "and asks for exactly the network, which is why it is not confined"
  }

let theFirstPartyListMatchesTheSource =
  test "the first-party-only list is exactly the builtins that check caller trust" {
    // Two gates exist and only one of them was ever visible. A builtin's effects are declared,
    // checked before the body runs, printed by `dark permissions` and carried in a manifest. Caller
    // trust is a call INSIDE a body, so nothing outside that body knew it was there.
    //
    // Pinned against the source rather than maintained by hand: a list of names beside the calls
    // that enforce them is a list that drifts, and the drift is silent in the direction that
    // matters, namely a new trust gate nobody can see.
    let sources =
      System.IO.Directory.EnumerateFiles(
        System.IO.Path.Combine(PackageSurface.findRepoRoot (), "backend", "src"),
        "*.fs",
        System.IO.SearchOption.AllDirectories
      )
      |> Seq.map System.IO.File.ReadAllText
      |> String.concat "\n"

    let called =
      System.Text.RegularExpressions.Regex.Matches(
        sources,
        @"requireBundledCaller\s+\w+\s+\w+\s+""(\w+)"""
      )
      |> Seq.map (fun m -> m.Groups[1].Value)
      |> Set.ofSeq

    Expect.equal
      called
      LibExecution.PermissionCheck.firstPartyOnly
      "every builtin that checks caller trust is listed, and nothing else is"
  }

let aCollidingPlatformIsSkippedNotFatal =
  test "a platform claiming a name something else provides is skipped, not fatal" {
    // The bug this pins: an external platform whose builtin collided with a linked one raised at
    // startup, which bricked the CLI. The install could only be undone by a command, and the
    // command no longer started, so recovering meant hand-editing a file in the policy directory.
    //
    // A LINKED collision is still fatal, and should be: that is a build mistake, decided before
    // anybody ran anything. An INSTALLED one arrives afterwards, from somebody else's manifest.
    let core = Platforms.Sets.sealedCompute ()
    let claimed = core.platforms |> List.head
    match claimed with
    | None -> failtest "the compute floor ships no platforms"
    | Some claimed ->
      let stolen =
        { claimed with
            name = "Impostor"
            version = 0
            requires = [ "Core" ] }
      match PlatformSet.claimsTaken core.platforms stolen with
      | [] -> failtest "claiming every name of an existing platform should collide"
      | taken ->
        // Names WHAT collided and WHO has it, because "there was a collision" is not something a
        // person can act on.
        match List.head taken with
        | None -> failtest "a non-empty list had no head"
        | Some(key, owner) ->
          Expect.stringContains key "fn " "the kind and the name"
          Expect.equal owner claimed.name "and who already provides it"
  }

let aSpawnedPlatformIsConfinedToWhatItDeclared =
  testTask "a platform that never asked for the network does not get one" {
    // The one thing a separate process buys that nothing else can. The gate checks a manifest's
    // effects before every call; this makes them CONFINE the process, so a platform whose
    // executable decides to phone home cannot, whatever its own code says.
    //
    // The fixture declares one custom effect and no network, then tries to open a TCP connection.
    let handle =
      LibDB.PlatformSpawn.handleFor
        "EchoPlatform"
        (echoPlatformPath ())
        (Set.singleton describedEffect)
        echoTypes
    try
      let confinement = LibDB.PlatformSpawn.confinement handle
      if not (confinement.StartsWith "no network") then
        // Skipped rather than failed, and the reason is printed. This machine cannot confine a
        // process without privileges, which is a fact about the machine and not about the code.
        // Failing here would make the suite red on macOS for something that is working correctly.
        print $"skipped: {confinement}"
      else
        let! reached = callSpawned handle "echoReach" RT.DUnit
        match reached with
        | Ok(RT.DString "reached the network") ->
          failtest "the platform reached the network it never declared"
        | Ok(RT.DString errno) ->
          // ENETUNREACH, specifically. A timeout or a DNS failure would pass a weaker assertion
          // while proving nothing, since a machine with no network at all gives those too.
          Expect.equal errno "101" "the connection failed because there is no network to use"
        | other -> failtest $"unexpected: {other}"
    finally
      LibDB.PlatformSpawn.stop handle
  }

let aSpawnedPlatformBuildsAnEnum =
  testTask "a platform in another process returns a Result, not just a primitive" {
    // The reason the startup handshake exists. An enum on the wire carries the type's CONTENT
    // HASH, and a plugin has no way to know one: that is the same reason a manifest names types
    // symbolically rather than carrying hashes. The host resolves the names and hands the answers
    // over once, before the first call.
    //
    // Without it an external platform can only return primitives, which rules out anything
    // answering with a `Result`, which is most of what a platform would want to answer with.
    let handle =
      LibDB.PlatformSpawn.handleFor
        "EchoPlatform"
        (echoPlatformPath ())
        (Set.singleton describedEffect)
        echoTypes
    try
      let! answered = callSpawned handle "echoResult" (RT.DString "typed")
      match answered with
      | Ok(RT.DEnum(source, _, _, "Ok", [ RT.DString "TYPED" ])) ->
        Expect.equal source (LibExecution.Dval.resultType ()) "built with the hash it was handed"
      | other -> failtest $"unexpected: {other}"
    finally
      LibDB.PlatformSpawn.stop handle
  }

let aSpawnedPlatformCarriesBytes =
  testTask "bytes cross to another process and back" {
    // The case the at-rest `Dval` encoding cannot express, and rightly: at rest, bytes have to be
    // addressable, so an ephemeral blob has nowhere to point. A frame is not at rest, so the bytes
    // travel beside it in a table and the payload holds a reference.
    //
    // Load-bearing rather than a curiosity: every `HttpClient` builtin returns freshly fetched
    // bytes, so a platform that cannot carry a blob cannot be `HttpClient`.
    let handle =
      LibDB.PlatformSpawn.handleFor
        "EchoPlatform"
        (echoPlatformPath ())
        (Set.singleton describedEffect)
        echoTypes
    try
      let sent = System.Text.Encoding.UTF8.GetBytes "bytes over a pipe"
      let! answered = callSpawned handle "echoBytes" (LibExecution.Blob.newEphemeral sent)
      match answered with
      | Ok(RT.DBlob(RT.Ephemeral got)) ->
        Expect.equal
          (System.Text.Encoding.UTF8.GetString got.bytes)
          "BYTES OVER A PIPE"
          "the far side read the bytes and sent its own back"
      // Ephemeral rather than persistent on the way back, and that is the point: nothing was
      // written to a store, and the receiving runtime owns what it was handed.
      | Ok(RT.DBlob(RT.Persistent(hash, _))) ->
        failtest $"came back as a reference to {hash} rather than as bytes"
      | other -> failtest $"unexpected: {other}"
    finally
      LibDB.PlatformSpawn.stop handle
  }

let aSpawnedPlatformIsPermissionChecked =
  testTask "a spawned platform's declared capability is enforced before it is called" {
    // The gate runs on what the platform DECLARED, before any bytes cross, so an ungranted
    // capability never reaches the process at all.
    let handle =
      LibDB.PlatformSpawn.handleFor
        "EchoPlatform"
        (echoPlatformPath ())
        (Set.singleton describedEffect)
        echoTypes
    try
      let core = Platforms.Sets.sealedCompute ()
      let set = PlatformSet.make (core.platforms @ [ spawnedPlatform handle ]) []
      let pmRT =
        PT2RT.PackageManager.toRT
          set.builtins.values
          LibExecution.ProgramTypes.PackageManager.empty
      let everythingElse =
        Permission.Policy.create (Effects.all |> List.map Permission.Rule.Effect) []
      let state =
        Exe.createState
          set.builtins
          pmRT
          Exe.noTracing
          RT.consoleReporter
          RT.consoleNotifier
          { dbs = Map.empty }
        |> Exe.setInstancePolicy everythingElse
      let fnName = RT.FQFnName.FQFnName.Builtin(RT.FQFnName.builtin "echoCounter" 0)
      let! denied = Exe.executeFunction state fnName [] (NEList.singleton RT.DUnit)
      match denied with
      | Error _ -> ()
      | Ok other -> failtest $"ran without its capability granted: {other}"
    finally
      LibDB.PlatformSpawn.stop handle
  }

let aCrashedPlatformIsAnErrorNotAHang =
  testTask "a platform that exits without answering is an error, not a hang" {
    // The spike found this and its harness did not handle it: a crashed plugin is a CLOSED PIPE
    // rather than any response. Left alone that is a CLI that never returns.
    let handle =
      LibDB.PlatformSpawn.handleFor
        "EchoPlatform"
        (echoPlatformPath ())
        (Set.singleton describedEffect)
        echoTypes
    try
      let! crashed = callSpawned handle "echoCrash" RT.DUnit
      match crashed with
      | Error _ -> ()
      | Ok other -> failtest $"a crashed platform answered: {other}"

      // And the handle recovers: the dead process is dropped, so the next call starts a fresh one
      // rather than talking into a pipe nobody holds.
      let! after = callSpawned handle "echoCounter" RT.DUnit
      match after with
      | Ok(RT.DInt64 1L) -> ()
      | other -> failtest $"did not recover after a crash: {other}"
    finally
      LibDB.PlatformSpawn.stop handle
  }

// ── installing an external platform ───────────────────────────────────────────

let private withPolicyDir (f : unit -> System.Threading.Tasks.Task<unit>) =
  task {
    let dir =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-install-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
    let restore = LibExecution.HostSecurity.policyDirectoryForTesting dir
    try
      do! f ()
    finally
      restore.Dispose()
      System.IO.Directory.Delete(dir, true)
  }

/// A manifest for the Python fixture, with its real artifact hash for this machine.
let private echoManifestText () =
  let bytes = System.IO.File.ReadAllBytes(echoPlatformPath ())
  let hash = LibExecution.Blob.sha256Hex bytes
  let text =
    $"""DARK-PLATFORM-MANIFEST 1
owner acme
name EchoPlatform
version 0
description a platform in another process
requires Core
store no
artifact test-rid {hash}

fn echoCounter 0
param unit Unit
returns Int64
effect acme/serial

fn echoShout 0
param text String
returns String
effect acme/serial
"""
  (text, hash, bytes)

let installingAnExternalPlatformMakesItReconstructable =
  testTask "installing records a manifest hash, and the platform rebuilds from it alone" {
    do!
      withPolicyDir (fun () ->
        task {
          let (text, artifactHash, artifactBytes) = echoManifestText ()
          // The artifact has to be in the cache before the platform can be rebuilt, the same way a
          // fetch would have put it there.
          match LibDB.PlatformArtifacts.materialize artifactHash artifactBytes with
          | Error e -> failtest e
          | Ok _ -> ()

          let! (installed :
                 Result<string * External.Manifest, External.Rejection>) =
            LibDB.InstalledPlatforms.install
              TestUtils.TestUtils.pmPT
              nothingProvidesIt
              text
            |> Ply.toTask
          match installed with
          | Error r -> failtest $"install: {r.problems}"
          | Ok(manifestHash, manifest) ->
            Expect.equal manifest.name "EchoPlatform" "what was installed"
            Expect.equal
              (LibDB.InstalledPlatforms.get () |> Map.tryFind "EchoPlatform")
              (Some manifestHash)
              "recorded against its manifest hash"

          // Rebuilt from the record alone: name plus hash is enough, because the manifest is in
          // the cache under that hash and the artifact hashes are inside the manifest.
          let! (built, skipped) =
            LibDB.InstalledPlatforms.platforms TestUtils.TestUtils.pmPT "test-rid" |> Ply.toTask
          Expect.isEmpty skipped "nothing skipped"
          match built with
          | [ platform ] ->
            Expect.equal platform.name "EchoPlatform" "rebuilt"
            Expect.equal (Platform.fnCount platform) 2 "with its builtins"
            Expect.isTrue
              (Set.contains describedEffect (Platform.effectSurface platform))
              "and its declared capability"
          | other -> failtest $"expected one platform, got {List.length other}"
        })
  }

let anInstallForAnotherMachineIsSkippedNotFatal =
  testTask "a platform that does not build for this machine is skipped with a reason" {
    // One broken or foreign install should not stop an instance starting, and should be visible
    // rather than silent. Both halves matter.
    do!
      withPolicyDir (fun () ->
        task {
          let (text, artifactHash, artifactBytes) = echoManifestText ()
          match LibDB.PlatformArtifacts.materialize artifactHash artifactBytes with
          | Error e -> failtest e
          | Ok _ -> ()
          let! (_ : Result<string * External.Manifest, External.Rejection>) =
            LibDB.InstalledPlatforms.install
              TestUtils.TestUtils.pmPT
              nothingProvidesIt
              text
            |> Ply.toTask

          let! (built, skipped) =
            LibDB.InstalledPlatforms.platforms TestUtils.TestUtils.pmPT "some-other-rid"
            |> Ply.toTask
          Expect.isEmpty built "nothing built for a target it does not ship"
          match skipped with
          | [ (name, why) ] ->
            Expect.equal name "EchoPlatform" "named"
            Expect.stringContains why "some-other-rid" "with the target in the reason"
          | other -> failtest $"expected one skip, got {other}"
        })
  }

let aMissingArtifactIsSkippedNotFatal =
  testTask "an install whose manifest was never cached is skipped, not fatal" {
    do!
      withPolicyDir (fun () ->
        task {
          // Recorded by hand, pointing at a manifest hash nothing ever cached: the shape an
          // interrupted install or a hand-edited file would leave behind.
          LibDB.InstalledPlatforms.add "Ghost" (String.replicate 64 "a")
          let! (built, skipped) =
            LibDB.InstalledPlatforms.platforms TestUtils.TestUtils.pmPT "test-rid" |> Ply.toTask
          Expect.isEmpty built "nothing built"
          match skipped with
          | [ (name, why) ] ->
            Expect.equal name "Ghost" "named"
            Expect.stringContains why "cache" "and says what is missing"
          | other -> failtest $"expected one skip, got {other}"
        })
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
      aDescribedBuiltinResolvesByName
      aGoodManifestBecomesAPlatform
      aManifestNamesEveryProblemAtOnce
      aManifestRefusesWhatCannotCross
      aManifestRefusesADuplicateBuiltin
      namedTypesRoundTrip
      namedTypeParseRefusesNonsense
      namedTypeResolvesAgainstTheConsumersStore
      aTextManifestParsesAndRoundTrips
      aTextManifestCollectsEveryProblem
      aTextManifestNeedsItsHeader
      aWrittenManifestResolvesToAPlatform
      aWrittenManifestReportsUnresolvableNames
      manifestLocationsAreFullyQualified
      aManifestResolvesAgainstTheRealStore
      aManifestNamingAMissingTypeSaysSo
      aManifestAddressesItsExecutablesByHash
      aManifestRefusesABadArtifactLine
      testSequenced anArtifactIsCachedUnderItsOwnHash
      testSequenced anArtifactThatLiesIsRefusedBeforeTheWrite
      testSequenced aTamperedArtifactFailsVerification
      testSequenced anArtifactHashCannotBeAPath
      aManifestInTheStoreResolves
      aManifestMustBeALiteral
      aMissingManifestSaysSo
      aSpawnedPlatformAnswers
      theSandboxFollowsTheDeclaration
      theFirstPartyListMatchesTheSource
      aCollidingPlatformIsSkippedNotFatal
      aSpawnedPlatformIsConfinedToWhatItDeclared
      aSpawnedPlatformBuildsAnEnum
      aSpawnedPlatformCarriesBytes
      aSpawnedPlatformIsPermissionChecked
      aCrashedPlatformIsAnErrorNotAHang
      testSequenced theShippedFetchManifestStillResolves
      testSequenced installingAnExternalPlatformMakesItReconstructable
      testSequenced anInstallForAnotherMachineIsSkippedNotFatal
      testSequenced aMissingArtifactIsSkippedNotFatal
      testSequenced anArtifactIsFetchedOnceAndReusedAfter
      testSequenced anUnavailableArtifactSaysWhatItMeans
      testSequenced aLyingSourceIsRefused ]
