/// Where each platform's builtins are wrapped, on the Dark side.
///
/// A platform owns primitives; the Dark code that names them lives in `packages/`. Nothing today
/// checks that those two agree, so a `Host` builtin can be wrapped inside `Darklang.Stdlib` and the
/// stdlib quietly depends on the operating system with nothing saying so. This prints the mapping so
/// a person can see it.
///
/// A report AND a test, now. `Tests.Platform.builtinsAreWrappedInTheirPlatformsHome` enforces the
/// same question against a declared home per platform plus a pinned list of known strays; this
/// prints the whole picture, which is what you want while deciding what to move. The test tells you
/// when something new escaped; the report tells you what the shape looks like.
///
/// Dev tooling: it reads every `.dark` file in the repo, which no shipped binary does. The corpus,
/// the comment stripping and the reference regex are `TestUtils.PackageSurface`, shared with
/// `Builtin.Tests` and `Platform.Tests`. Shared rather than copied: a copy that forgets to strip
/// whole-line comments reports a builtin named in a doc comment beside its wrapper as a stray in
/// another area.
module LocalExec.PlatformReport

open Prelude

open LibExecution.Platform

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite


module PackageSurface = TestUtils.PackageSurface

let private packageFiles () = PackageSurface.packageFiles.Value
let private area = PackageSurface.area
let private referencesBuiltin = PackageSurface.referencesBuiltin


/// How many wrappers an area must hold before it counts as that platform's home rather than as a
/// place one primitive escaped to. Two, because one is unambiguous and two is still a person's
/// mistake rather than a namespace.
///
/// **"Home" here is INFERRED from where most wrappers are, and it can invert.** `Instance` is the
/// case: five of its seven wrappers are in `darklang/stdlib`, so this reports stdlib as its home and
/// `darklang/cli` as the outlier, when the finding is the other way round -- instance
/// administration should not be reachable from the standard library at all. A majority in the wrong
/// place looks identical to a majority in the right one from here.
///
/// `Tests.Platform.platformHomes` carries the DECLARED home and disagrees with this report about
/// `Instance` on purpose. When they disagree, the declaration is the intent and this is the
/// measurement; the gap between them is the finding.
let private outlierThreshold = 2


/// For each platform: which package areas wrap its builtins, and which builtins escaped to an area
/// that holds almost none of them.
///
/// The outlier list is the useful half. Knowing a platform is wrapped mostly under
/// `darklang/stdlib/cli` says the layering is working; knowing which ONE `Host` primitive is wrapped
/// in `darklang/prettyPrinter` says where it is not.
let report (set : PlatformSet) : List<string> =
  let files = packageFiles ()
  set.platforms
  |> List.sortBy _.name
  |> List.collect (fun p ->
    let names =
      p.builtins.fns.Values |> Seq.map (fun fn -> fn.name.name) |> List.ofSeq
    // (area, builtin) for every place a builtin of this platform is named.
    let placements =
      names
      |> List.collect (fun name ->
        files
        |> List.filter (fun (_, contents) -> referencesBuiltin contents name)
        |> List.map (fun (path, _) -> (area path, name))
        |> List.distinct)
    // Operator-dispatched builtins (`+`, `==`, unary `-`) have no textual `Builtin.x` anywhere by
    // construction, so listing them as unwrapped is noise. Asked of the lowering table rather than
    // of a list kept here, which is the only version that cannot go stale.
    let unwrapped =
      let wrapped = placements |> List.map snd |> Set.ofList
      names
      |> List.filter (fun n ->
        not (Set.contains n wrapped) && not (PT.InfixFnName.isOperatorDispatched n))
      |> List.sort
    // Prelude's `List.groupBy` answers a `Map`, not F#'s list of pairs.
    let byArea =
      placements
      |> List.groupBy fst
      |> Map.map (fun pairs -> pairs |> List.map snd |> List.distinct |> List.sort)
      |> Map.toList
      |> List.sortByDescending (snd >> List.length)

    // A platform with only a handful of builtins has no "home" to be an outlier from, so calling
    // its one wrapper an outlier is an artifact of the threshold rather than a finding.
    let home, outliers =
      if List.length names <= outlierThreshold * 2 then
        (byArea, [])
      else
        byArea |> List.partition (fun (_, ns) -> List.length ns > outlierThreshold)

    let header = $"{Platform.coordinate p}  ({List.length names} fns)"
    let homeLine =
      if List.isEmpty home then
        []
      else
        [ "    home: "
          + (home
             |> List.map (fun (a, ns) -> $"{a} ({List.length ns})")
             |> String.concat ", ") ]
    let outlierLines =
      outliers
      |> List.map (fun (a, ns) ->
        let shown = String.concat ", " ns
        $"    outlier: {a}  <- {shown}")
    let unwrappedLine =
      if List.isEmpty unwrapped then
        []
      else
        let shown = unwrapped |> List.truncate 8 |> String.concat ", "
        let extra =
          if List.length unwrapped > 8 then
            $", +{List.length unwrapped - 8} more"
          else
            ""
        [ $"    no Dark wrapper ({List.length unwrapped}): {shown}{extra}" ]
    header :: (homeLine @ outlierLines @ unwrappedLine))


// ── what composing the platform set costs ─────────────────────────────────────

/// Bytes this thread has allocated so far.
///
/// `GetAllocatedBytesForCurrentThread` rather than `GetTotalAllocatedBytes`: the second sees every
/// thread and the numbers here are small enough that a background finalizer would swamp them.
let private allocNow () : int64 = System.GC.GetAllocatedBytesForCurrentThread()


/// Run `f` twice and report the SECOND run's allocation.
///
/// The first pays for JIT and for whatever static initializers the call reaches, which is real cost
/// on a cold CLI invocation but is not what this is trying to attribute. Attributing it would make
/// every platform look expensive in catalog order rather than by size.
let private allocOf (f : unit -> 'a) : int64 =
  f () |> ignore<'a>
  let before = allocNow ()
  f () |> ignore<'a>
  allocNow () - before


/// What building the platform set costs, per platform and in total.
///
/// Every CLI invocation pays this before it does anything, so it is fixed cost in the most literal
/// sense: the same bytes whether you asked for `dark status` or a two-minute script. The per-fn
/// column is what makes it comparable across platforms of very different sizes.
let costReport () : List<string> =
  let pm = LibExecution.ProgramTypes.PackageManager.empty
  let entries =
    [ "Core", (fun () -> Builtins.Pure.Builtin.builtins ())
      "Clock", (fun () -> Builtins.Time.Builtin.builtins ())
      "Random", (fun () -> Builtins.Random.Builtin.builtins ())
      // The four platforms in `Builtins.Cli` share one assembly, and the cost this measures is
      // the assembly's.
      "Terminal+Files+Process+Posix", (fun () -> Builtins.Cli.Builtin.builtins ())
      "HttpClient", (fun () -> Builtins.Http.Client.Builtin.builtins ())
      "HttpServer", (fun () -> Builtins.Http.Server.Builtin.builtins ())
      "Lang", (fun () -> Builtins.Language.Builtin.builtins ())
      "Store", (fun () -> Builtins.Store.Builtin.builtins pm)
      "Instance+Seed+Policy", (fun () -> Builtins.Admin.Builtin.builtins ())
      "Db+Traces+Accounts+Sqlite", (fun () -> Builtins.Data.Builtin.builtins ())
      "Darklang", (fun () -> Builtins.CliHost.Builtin.builtins ()) ]

  let measured =
    entries
    |> List.map (fun (name, build) ->
      let bytes = allocOf build
      let count = (build ()).fns.Count
      (name, bytes, count))
    |> List.sortByDescending (fun (_, bytes, _) -> bytes)

  let wholeSet = allocOf (fun () -> Platforms.Sets.everythingFor pm)
  let sumOfParts = measured |> List.sumBy (fun (_, b, _) -> b)

  let rows =
    measured
    |> List.map (fun (name, bytes, count) ->
      let perFn = if count = 0 then 0L else bytes / int64 count
      $"    {name, -12} {bytes, 10} bytes  {count, 4} fns  {perFn, 6} B/fn")

  [ "What building the platform set allocates. Every CLI invocation pays this before it does"
    "anything, so it is fixed cost in the most literal sense: the same bytes whether you asked"
    "for `dark status` or a two-minute script."
    ""
    "Per platform: what ONE fresh `builtins ()` call allocates." ]
  @ rows
  @ [ ""
      $"    all of them together  {sumOfParts, 10} bytes"
      ""
      // Not "overhead": the two numbers do not nest, and reading them as if they did says the
      // combine is free, which is the opposite of true.
      $"    everythingFor ()      {wholeSet, 10} bytes"
      ""
      "The second number is SMALLER, and the reason is worth knowing: each platform record is a"
      "module-level `let`, so its `builtins ()` runs once per process in a static initializer, not"
      "once per `catalogFor` call. `everythingFor` therefore re-pays only `Store` (which takes the"
      "package manager, so it is a function) plus the combine. A process pays the per-platform"
      "column once and the `everythingFor` column once." ]


// ── which builtins nothing actually reaches ───────────────────────────────────

/// Every builtin a compiled package function names.
///
/// Read off the RT instructions rather than the source text, so it cannot be fooled by a name in a
/// comment or a string, and it sees exactly what the interpreter would dispatch on. A builtin
/// reaches the instruction stream as a `LoadVal` of a `DApplicable(AppNamedFn ...)`; nothing else in
/// the instruction set names one.
let private builtinsNamedBy (fn : RT.PackageFn.PackageFn) : Set<string> =
  fn.body.instructions
  |> List.choose (fun instr ->
    match instr with
    | RT.LoadVal(_, RT.DApplicable(RT.AppNamedFn named)) ->
      match named.name with
      | RT.FQFnName.Builtin b -> Some b.name
      | RT.FQFnName.Package _ -> None
    | _ -> None)
  |> Set.ofList


type private LiveFn =
  {
    hash : string
    location : string
    /// How many distinct package items depend on this one.
    callers : int
    builtins : Set<string>
  }


/// Every listed fn on main, with what it names and who names it.
let private liveFns () : Ply<List<LiveFn>> =
  uply {
    let! rows =
      Sql.query
        """
        SELECT item_hash, owner, modules, name
        FROM locations
        WHERE item_type = 'fn' AND unlisted_at IS NULL
        """
      |> Sql.executeAsync (fun read ->
        let modules = read.string "modules"
        let owner = read.string "owner"
        let name = read.string "name"
        let location =
          if modules = "" then $"{owner}.{name}" else $"{owner}.{modules}.{name}"
        (read.string "item_hash", location))

    let! callerRows =
      Sql.query
        """
        SELECT depends_on_hash, COUNT(DISTINCT item_hash) AS n
        FROM package_dependencies
        GROUP BY depends_on_hash
        """
      |> Sql.executeAsync (fun read -> (read.string "depends_on_hash", read.int "n"))
    let callers = Map.ofList callerRows

    let result = ResizeArray<LiveFn>()
    for (hash, location) in rows do
      let! fn = LibDB.RuntimeTypes.Fn.get (RT.Hash hash)
      match fn with
      | None -> ()
      | Some fn ->
        let named = builtinsNamedBy fn
        if not (Set.isEmpty named) then
          result.Add
            { hash = hash
              location = location
              callers = Map.get hash callers |> Option.defaultValue 0
              builtins = named }
    return List.ofSeq result
  }


/// Builtins whose only Dark wrappers have no callers, and builtins nothing names at all.
///
/// The second kind `Builtin.Tests` already fails on. The first it cannot see: a builtin with exactly
/// one wrapper passes "every builtin has a Dark caller" even when nothing in the corpus calls that
/// wrapper. That is dead weight carried in every build and every serialized package, and it is the
/// shape a builtin takes on its way out of the language without anyone noticing.
///
/// "No callers" is read from `package_dependencies`, so it means no other PACKAGE item depends on
/// it. A wrapper reached only from a CLI entry point, a test file or a perf workload therefore looks
/// dead here and is not; those are named as such rather than filtered, since the list is short
/// enough to read and guessing which is which is exactly what a report should not do.
let unusedReport () : Ply<List<string>> =
  uply {
    let set = Platforms.Sets.everything ()
    let! live = liveFns ()

    let wrappersOf (builtinName : string) : List<LiveFn> =
      live |> List.filter (fun f -> Set.contains builtinName f.builtins)

    // A wrapper under `Darklang.Stdlib` is EXPORTED SURFACE. Nothing in this repo calling
    // `Stdlib.Int8.fromUInt16` says nothing about whether it should exist: it is there for user
    // code, and user code is not in this repo. A wrapper anywhere else is internal plumbing, and
    // internal plumbing with no caller is the thing worth looking at.
    let isExportedSurface (f : LiveFn) : bool =
      f.location.StartsWith "Darklang.Stdlib."

    /// Is this wrapper called from somewhere the caller graph cannot see?
    ///
    /// `package_dependencies` records package-to-package edges only, so a wrapper reached from a
    /// perf workload, a test file or a CLI script has no edge and looks dead. Both entries this
    /// report first produced were exactly that: `InterpreterStats.allocatedBytes` is called by
    /// `scripts/perf/workloads/costs.dark`.
    ///
    /// Textual, and deliberately generous: it looks for `Module.name` anywhere in the repo's Dark,
    /// which the definition itself (`let name ...`) does not match. A false positive here only
    /// SHORTENS the list, so the failure mode is missing a dead builtin rather than proposing to
    /// delete a live one. That is the right direction for a report that suggests deletions.
    let reachedFromOutsidePackages (f : LiveFn) : bool =
      match f.location.Split('.') |> Array.toList |> List.rev with
      | name :: modul :: _ ->
        System.Text.RegularExpressions.Regex.IsMatch(
          PackageSurface.repoDarkText.Value,
          $@"\b{System.Text.RegularExpressions.Regex.Escape modul}\.{System.Text.RegularExpressions.Regex.Escape name}\b"
        )
      | _ -> false

    let mutable exportedCount = 0
    let internalDead = ResizeArray<string>()
    let scriptOnly = ResizeArray<string>()

    for p in set.platforms |> List.sortBy _.name do
      for fn in p.builtins.fns.Values |> Seq.sortBy (fun f -> f.name.name) do
        let n = fn.name.name
        if
          not (PT.InfixFnName.isOperatorDispatched n)
          && not (Set.contains n PackageSurface.languageIdioms)
        then
          match wrappersOf n with
          // `Builtin.Tests` already fails on a builtin nothing names at all.
          | [] -> ()
          | ws when ws |> List.forall (fun w -> w.callers = 0) ->
            if ws |> List.forall isExportedSurface then
              exportedCount <- exportedCount + 1
            else
              let where =
                ws |> List.map _.location |> List.sort |> String.concat ", "
              if ws |> List.exists reachedFromOutsidePackages then
                scriptOnly.Add $"    {p.name}#{n}  <- {where}"
              else
                internalDead.Add $"    {p.name}#{n}  <- {where}"
          | _ -> ()

    return
      [ "Builtins whose only Dark wrapper has no caller in this repo." ]
      @ [ ""
          "INTERNAL wrappers with no caller. These are the ones worth looking at: nothing outside"
          "this repo calls them either, so a dead one is dead everywhere." ]
      @ (if internalDead.Count = 0 then [ "    (none)" ] else List.ofSeq internalDead)
      @ [ "" ]
      @ (if scriptOnly.Count = 0 then
           []
         else
           [ "Reached only from a perf workload, test or script, which the caller graph cannot see."
             "Not dead." ]
           @ List.ofSeq scriptOnly
           @ [ "" ])
      @ [ $"EXPORTED surface with no caller here: {exportedCount} builtins wrapped under"
          "`Darklang.Stdlib` that nothing in this repo uses. Expected, and NOT a deletion list: the"
          "standard library exists for programs that are not in this repo. Worth the count as a"
          "shape-of-the-library number, not as a to-do."
          ""
          $"    scanned {List.length live} package fns that name at least one builtin" ]
  }


/// The same measurement one level down, inside `Core`.
///
/// `Core` is the large majority of the builtins and most of the startup cost, so "make Core
/// cheaper" is the only version of "make startup cheaper" that matters. This says which of its
/// libraries the cost is actually in, and the B/fn column says whether a library is expensive
/// because it is big or because each of its builtins is.
let coreCostReport () : List<string> =
  let entries : List<string * (unit -> LibExecution.RuntimeTypes.Builtins)> =
    [ "Base64", (fun () -> Builtins.Pure.Libs.Base64.builtins ())
      "Char", (fun () -> Builtins.Pure.Libs.Char.builtins ())
      "Blob", (fun () -> Builtins.Pure.Libs.Blob.builtins ())
      "Bool", (fun () -> Builtins.Pure.Libs.Bool.builtins ())
      "AltJson", (fun () -> Builtins.Pure.Libs.AltJson.builtins ())
      "Crypto", (fun () -> Builtins.Pure.Libs.Crypto.builtins ())
      "Int8", (fun () -> Builtins.Pure.Libs.Int8.builtins ())
      "Math", (fun () -> Builtins.Pure.Libs.Math.builtins ())
      "Float", (fun () -> Builtins.Pure.Libs.Float.builtins ())
      "List", (fun () -> Builtins.Pure.Libs.List.builtins ())
      "Int64", (fun () -> Builtins.Pure.Libs.Int64.builtins ())
      "Int16", (fun () -> Builtins.Pure.Libs.Int16.builtins ())
      "Int", (fun () -> Builtins.Pure.Libs.Int.builtins ())
      "X509", (fun () -> Builtins.Pure.Libs.X509.builtins ())
      "Json", (fun () -> Builtins.Pure.Libs.Json.builtins ())
      "Dict", (fun () -> Builtins.Pure.Libs.Dict.builtins ())
      "UInt128", (fun () -> Builtins.Pure.Libs.UInt128.builtins ())
      "UInt64", (fun () -> Builtins.Pure.Libs.UInt64.builtins ())
      "Int128", (fun () -> Builtins.Pure.Libs.Int128.builtins ())
      "Uuid", (fun () -> Builtins.Pure.Libs.Uuid.builtins ())
      "UInt16", (fun () -> Builtins.Pure.Libs.UInt16.builtins ())
      "Stream", (fun () -> Builtins.Pure.Libs.Stream.builtins ())
      "Int32", (fun () -> Builtins.Pure.Libs.Int32.builtins ())
      "UInt32", (fun () -> Builtins.Pure.Libs.UInt32.builtins ())
      "UInt8", (fun () -> Builtins.Pure.Libs.UInt8.builtins ())
      "Regex", (fun () -> Builtins.Pure.Libs.Regex.builtins ())
      "String", (fun () -> Builtins.Pure.Libs.String.builtins ())
      "DateTime", (fun () -> Builtins.Pure.Libs.DateTime.builtins ())
      "NoModule", (fun () -> Builtins.Pure.Libs.NoModule.builtins ()) ]

  let measured =
    entries
    |> List.map (fun (name, build) ->
      let bytes = allocOf build
      let count = (build ()).fns.Count
      (name, bytes, count))
    |> List.sortByDescending (fun (_, bytes, _) -> bytes)

  let total = measured |> List.sumBy (fun (_, b, _) -> b)
  let totalFns = measured |> List.sumBy (fun (_, _, c) -> c)

  [ "What each of Core's libraries allocates when built:"; "" ]
  @ (measured
     |> List.map (fun (name, bytes, count) ->
       let perFn = if count = 0 then 0L else bytes / int64 count
       $"    {name, -12} {bytes, 8} bytes  {count, 4} fns  {perFn, 6} B/fn"))
  @ [ ""; $"    total        {total, 8} bytes  {totalFns, 4} fns" ]


// ── what a single BuiltInFn costs, by part ────────────────────────────────────

/// Where the ~1,000 bytes per builtin actually go.
///
/// Worth measuring before building anything on top of it: the case for lazy construction rests
/// entirely on that number, and if most of it were one avoidable thing (a description concatenated
/// at runtime, say) the cheap fix would beat the infrastructure. Each row builds N copies of a
/// `BuiltInFn` differing in one part, so the delta between rows is that part's cost.
///
/// Its own nested module because it needs `open LibExecution.RuntimeTypes` for the record labels,
/// and opening that at file scope would shadow names the rest of this file uses unqualified.
module private FnParts =
  open LibExecution.RuntimeTypes

  let private body : BuiltInFnSig =
    function
    | struct (_, _, _, [| _ |]) -> Ply DUnit
    | _ -> incorrectArgs ()

  let private baseFn () : BuiltInFn =
    { name = FQFnName.builtin "someName" 0
      typeParams = []
      parameters = [ BuiltInParam.make "b" TBool "" ]
      returnType = TBool
      description = "a fixed literal description of moderate length, as most of them are"
      fn = body
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }

  let private concatenatedDescription () : BuiltInFn =
    { baseFn () with
        description =
          "a fixed literal description of moderate length, " + "as most of them are" }

  let private threeParams () : BuiltInFn =
    { baseFn () with
        parameters =
          [ BuiltInParam.make "a" TBool ""
            BuiltInParam.make "b" (TList TString) ""
            BuiltInParam.make "c" TInt64 "" ]
        returnType = TypeReference.option TString }

  let private withEffects () : BuiltInFn =
    { baseFn () with callEffects = set [ LibExecution.Effects.Effect.FileRead ] }


  /// Build `n` of them and report bytes per one. `{ base with ... }` copies the record, so the
  /// variants measure the base cost PLUS the part, which is what the deltas want.
  let private perFn (build : unit -> BuiltInFn) : int64 =
    let n = 1000
    build () |> ignore<BuiltInFn>
    let before = System.GC.GetAllocatedBytesForCurrentThread()
    for _ in 1..n do
      build () |> ignore<BuiltInFn>
    (System.GC.GetAllocatedBytesForCurrentThread() - before) / int64 n

  let report () : List<string> =
    let rows =
      [ "record, 1 param, literal description", perFn baseFn
        "  the same, description built with +", perFn concatenatedDescription
        "  the same, 3 params and richer types", perFn threeParams
        "  the same, one declared effect", perFn withEffects ]
    [ "Allocation per constructed BuiltInFn, by what is in it:"
      "" ]
    @ (rows |> List.map (fun (label, bytes) -> $"    {label,-40} {bytes,6} bytes"))
    @ [ ""
        "The variants are `{ base with ... }`, so each is the base PLUS a record copy"
        "(about 128 bytes) PLUS the part. Net, on this machine: a description built with"
        "`+` costs ~128 bytes, a one-element effect Set ~112, and two extra parameters"
        "with richer types ~408. Parameters are the bulk, and they are data rather than"
        "waste: only not building them at all avoids the cost." ]


let fnPartsReport () : List<string> = FnParts.report ()


/// The minimum platform set a given package function needs in order to run.
///
/// This is the question the whole model is for, asked from the program's end instead of the
/// binary's. `dark platforms` says what a build contains; this says what a program requires, and
/// the difference between the two is what a smaller executable could drop.
///
/// It is also the install-time review surface the design asks for: before running someone else's
/// Dark, you can see the platforms it will need and therefore the effects it can reach, without
/// running it.
///
/// **What it cannot tell you.** The walk is static, over `CallGraph`, so a call through a
/// function-typed parameter is unknowable: the closure loader marks that `complete = false` and the
/// honest answer becomes "at least these". Reported rather than hidden, because a review surface
/// that quietly understates itself is worse than none.
module Needed =

  module Calls = LibExecution.CallGraph
  module PackagePermissions = LibDB.PackagePermissions

  /// `Darklang.Stdlib.List.map` -> the location to look up.
  let private locationOf (s : string) : Option<PT.PackageLocation> =
    match List.rev (s.Split('.') |> Array.toList) with
    | []
    | [ _ ] -> None
    | name :: revRest ->
      match List.rev revRest with
      | owner :: modules -> Some { owner = owner; modules = modules; name = name }
      | [] -> None

  let report (set : PlatformSet) (fnName : string) : List<string> =
    match locationOf fnName with
    | None ->
      [ $"'{fnName}' is not a package location; it needs at least an owner and a name" ]
    | Some location ->

    match (LibDB.PackageManager.pt.findFn location).Result with
    | None -> [ $"no package function named '{fnName}'" ]
    | Some pkg ->

    let closure =
      (PackagePermissions.loadClosure LibDB.ProgramTypes.Fn.get pkg |> Ply.toTask).Result

    // Every builtin named anywhere in the closure, and whether any member's analysis gave up.
    let builtins, complete =
      closure
      |> Map.toList
      |> List.fold
        (fun (names, complete) (_, (_, analysis : Calls.Analysis)) ->
          let found =
            analysis.names
            |> List.choose (function
              | PT.FQFnName.Builtin b -> Some b.name
              | PT.FQFnName.Package _ -> None)
          (Set.union names (Set.ofList found), complete && analysis.complete))
        (Set.empty, true)

    // Group them by the platform that declares them. A builtin no platform in this set declares is
    // its own answer: the program cannot run here at all.
    let byPlatform =
      builtins
      |> Set.toList
      |> List.map (fun name ->
        match PlatformSet.ownerOf name set with
        | Some p -> (p.name, name)
        | None -> ("(not in this build)", name))
      |> List.groupBy fst
      |> Map.map (fun pairs -> pairs |> List.map snd |> List.sort)

    let needed =
      set.platforms |> List.filter (fun p -> Map.containsKey p.name byPlatform)

    let effects =
      needed
      |> List.fold (fun acc p -> Set.union acc (Platform.effectSurface p)) Set.empty

    let platformLines =
      byPlatform
      |> Map.toList
      |> List.sortBy fst
      |> List.map (fun (platformName, names) ->
        let shown =
          match names with
          | [ one ] -> one
          | many ->
            let head = many |> List.truncate 4 |> String.concat ", "
            if List.length many > 4 then
              $"{head}, +{List.length many - 4} more"
            else
              head
        $"    {platformName,-12} {List.length names,3}  {shown}")

    let effectLine =
      match Set.toList effects with
      | [] -> "reaches nothing"
      | es -> es |> List.map LibExecution.Effects.name |> List.sort |> String.concat " "

    // `requiresStore` is about the BUILTINS: which of them need a package DB handle to work.
    // Deliberately not the same question as "can this run without a store", because resolving the
    // entry point by name reads the store no matter what the answer here is. Worded so nobody
    // reads "none" as "runs against nothing".
    let storeLine =
      match needed |> List.filter _.requiresStore with
      | [] -> "    No builtin here needs a store handle."
      | ps ->
        let names = ps |> List.map _.name |> List.sort |> String.concat ", "
        $"    Builtins needing a store handle: {names}"

    [ $"{fnName} needs:"
      "" ]
    @ platformLines
    @ [ ""
        $"    {List.length needed} platform(s), {Set.count builtins} builtins, "
        + $"across {Map.count closure} package fns."
        $"    Effect surface if you grant them whole: {effectLine}"
        storeLine ]
    @ (if complete then
         []
       else
         [ ""
           "    INCOMPLETE: some call in this closure goes through a function-typed parameter,"
           "    which no static walk can follow. Treat the list above as a lower bound." ])
    @ (if Map.containsKey "(not in this build)" byPlatform then
         [ ""
           "    Some builtins belong to no platform in this set, so this function cannot run"
           "    here at all." ]
       else
         [])


/// The numeric tower, and how much of it is replication.
///
/// A large fraction of every builtin we ship is one table copied per numeric type: `Int8`,
/// `Int16`, `Int32`, `Int64`, `Int128`, the five unsigned widths and `Int`, each with the same
/// operations. `Core` is not large because Dark needs many primitives, it is large because this is
/// written out longhand. This says so per operation, with current counts, so a plan to collapse it
/// can be argued from a list rather than from an impression.
///
/// Three verdicts:
///
/// - **TWIN**: a polymorphic builtin with the same operation name exists in `NoModule`. Nothing is
///   waiting on a language feature; the only question is whether the twelve-case match costs too
///   much on the hot path.
///
///   **What this check cannot see**, and it matters: it tests that the polymorphic builtin EXISTS,
///   not that it handles the type in question. The implementation is a closure, so nothing here can
///   look inside it. `power` is the case that shows the difference: there is no `int128Power` and
///   `NoModule.power` does not handle `DInt128`, so the two happen to agree, but a polymorphic
///   builtin that quietly dropped a type would keep being reported as a twin for it. Coverage was
///   checked by hand against every typed builtin on 2026-09-09 and had no gaps. Re-check it before
///   acting on this column, or the first thing the collapse deletes is the only implementation.
/// - **CONVERT**: a width conversion in either direction (`fromInt8`, `toUInt32`, `toFloat`). One
///   `convert` taking a type argument replaces all of them, with no perf or semantics question.
///   The suffix must NAME a numeric type, which is what keeps `toString` and `toBits` out.
/// - **PER-TYPE**: genuinely different per type, or at least no polymorphic form exists today.
///   Bitwise ops, `parse`, `toString`, `sqrt`, the shifts.
///
/// Derived from the live builtin set rather than from a file listing, so it stays true.
module Collapsible =

  /// Longest first: `int64Add` must match `int64` and not `int`.
  let private numericTypes =
    [ "uint128"; "uint64"; "uint32"; "uint16"; "uint8"
      "int128"; "int64"; "int32"; "int16"; "int8"
      "float"; "int" ]

  let private lowerFirst (s : string) : string =
    if s = "" then s else string (System.Char.ToLower s[0]) + s.Substring 1

  /// `int64Add` -> Some("int64", "add"). `listMap` -> None.
  let private split (name : string) : Option<string * string> =
    numericTypes
    |> List.tryPick (fun t ->
      if name.StartsWith(t, System.StringComparison.Ordinal) && name.Length > t.Length then
        let rest = name.Substring t.Length
        // The character after the type must start a new word, or `intParse` would read as
        // type `int` op `Parse` while `interpreterStatsGet` reads as type `int` op `erpreter...`.
        if System.Char.IsUpper rest[0] then Some(t, lowerFirst rest) else None
      else
        None)

  type private Verdict =
    | Twin
    | Convert
    | PerType

  let private verdictName =
    function
    | Twin -> "TWIN"
    | Convert -> "CONVERT"
    | PerType -> "PER-TYPE"

  let report (set : PlatformSet) : List<string> =
    let allNames =
      set.platforms
      |> List.collect (fun p -> p.builtins.fns.Keys |> Seq.map _.name |> List.ofSeq)
      |> Set.ofList

    // The polymorphic forms: a builtin whose whole name is an operation name, with no type prefix.
    let isPolymorphic (op : string) = Set.contains op allNames

    // A width conversion in either direction: `fromInt8`, `toUInt32`, `toFloat`. The suffix has to
    // NAME a numeric type, which is what keeps `toString` and `toBits` out of this bucket.
    let isConversion (op : string) : bool =
      let suffixNames (prefix : string) =
        op.StartsWith(prefix, System.StringComparison.Ordinal)
        && (let rest = op.Substring(prefix.Length).ToLower()
            List.contains rest numericTypes)
      suffixNames "from" || suffixNames "to"

    let verdictOf (op : string) : Verdict =
      if isConversion op then Convert
      elif isPolymorphic op then Twin
      else PerType

    let rows =
      allNames
      |> Set.toList
      |> List.choose (fun name ->
        split name |> Option.map (fun (typ, op) -> (op, typ, verdictOf op)))

    // Prelude's `List.groupBy` answers a `Map`, not F#'s list of pairs, and its `List.head`
    // answers an `Option`. The verdict comes from `verdictOf` rather than from peeking at an
    // entry, which sidesteps the second of those.
    let byOp =
      rows
      |> List.map (fun (op, typ, _) -> (op, typ))
      |> List.groupBy fst
      |> Map.map (fun pairs -> pairs |> List.map snd)

    let count v = rows |> List.filter (fun (_, _, x) -> x = v) |> List.length

    // Short labels, fixed order, so the matrix lines up under one header.
    let columnOrder =
      [ "int8"; "int16"; "int32"; "int64"; "int128"
        "uint8"; "uint16"; "uint32"; "uint64"; "uint128"
        "int"; "float" ]

    let columnHeader =
      let short =
        [ "i8"; "i16"; "i32"; "i64"; "128"; "u8"; "u16"; "u32"; "u64"; "u128"; "int"; "flt" ]
      let cells = short |> List.map (fun c -> $"{c,4}") |> String.concat ""
      $"""    {"operation",-22} {"n",3}  {"verdict",-9}{cells}"""

    let opLines =
      byOp
      |> Map.toList
      |> List.sortBy (fun (op, types) ->
        // Group the verdicts together, then most-replicated first inside each.
        let order =
          match verdictOf op with
          | Twin -> 0
          | Convert -> 1
          | PerType -> 2
        (order, -(List.length types), op))
      |> List.map (fun (op, types) ->
        // A presence matrix rather than a list of names: twelve type names on one line runs off
        // the side of a printed page, and the gaps are the interesting part anyway (`negate` and
        // `remainder` missing from the unsigned widths, `power` missing from the 128-bit ones).
        let has t = if List.contains t types then "x" else "."
        let cells = columnOrder |> List.map (fun t -> $"{has t,4}") |> String.concat ""
        $"    {op,-22} {List.length types,3}  {verdictName (verdictOf op),-9}{cells}")

    let byType =
      rows
      |> List.map (fun (_, typ, v) -> (typ, v))
      |> List.groupBy fst
      |> Map.map (fun pairs -> pairs |> List.map snd)

    let typeLines =
      byType
      |> Map.toList
      |> List.sortByDescending (fun (_, vs) -> List.length vs)
      |> List.map (fun (typ, vs) ->
        let n v = vs |> List.filter ((=) v) |> List.length
        $"    {typ,-10} {List.length vs,3} fns   twin {n Twin,3}   convert {n Convert,3}   per-type {n PerType,3}")

    [ $"The numeric tower: {List.length rows} builtins across {Map.count byType} types."
      ""
      "By operation. TWIN means a polymorphic builtin of the same name already exists and already"
      "dispatches over every numeric type; CONVERT means a width conversion that one `convert` with"
      "a type argument would replace; PER-TYPE means no polymorphic form exists today."
      ""
      columnHeader
      "" ]
    @ opLines
    @ [ ""
        "By type."
        "" ]
    @ typeLines
    @ [ ""
        $"    TWIN      {count Twin,4}   already covered by an existing polymorphic builtin"
        $"    CONVERT   {count Convert,4}   mechanical, no perf or semantics question"
        $"    PER-TYPE  {count PerType,4}   genuinely per type, or no polymorphic form yet"
        $"    ----------------"
        $"    total     {List.length rows,4}"
        ""
        "So the list is not read as a to-do:"
        ""
        "TWIN has two costs. The polymorphic `add` matches twelve cases where `int64Add` matches"
        "one, and arithmetic is the hottest path in the language; and `NoModule.add` carries a"
        "CLEANUP saying its SQL pushdown disagrees with runtime overflow semantics. Measure first."
        ""
        "CONVERT has neither cost, but it is not one builtin. 64 of them narrow and return"
        "`Option<T>`; 52 widen and return `T`. That needs `convert` AND `tryConvert`, with the"
        "existing Dark wrappers keeping their signatures and delegating to whichever is right."
        "116 builtins become 2, and no caller changes." ]


/// The layering audit: which package functions reach effects their MODULE has no business reaching.
///
/// `platforms wrappers` answers a textual question (where is this builtin named?) and finds direct
/// strays. This answers the transitive one, which is the one that matters: `Stdlib.List.map` naming
/// no builtin at all is uninteresting if something three calls down opens a socket.
///
/// The baseline is per top-level module and deliberately opinionated, because a baseline that
/// derives itself from what the code does can never be violated. `Stdlib` should be computation:
/// anything under it that reaches the host is a finding, not a fact. Where a module legitimately
/// reaches something, the baseline says so once, in one place, with a reason.
///
/// Slow. It loads a call-graph closure per function, and the corpus is thousands of them, so this is
/// a thing you run when you are asking the question rather than a thing the build does.
module Audit =

  module Calls = LibExecution.CallGraph
  module PackagePermissions = LibDB.PackagePermissions
  module Effects = LibExecution.Effects

  /// What each top-level Darklang module may reach without it being a finding.
  ///
  /// `Stdlib` is the interesting row and the reason the table exists. It is a standard library: a
  /// program calling into it should be able to assume computation. The entries it does have are the
  /// standard library's genuine host surface, and each one is a decision somebody made.
  let private baseline : Map<string, Set<Effects.Effect>> =
    Map.ofList
      [ // Files, processes, the terminal and the clock ARE the standard library's job in a language
        // with a CLI. What is not its job is the package store and the machine.
        "Stdlib",
        set
          [ Effects.Effect.FileRead
            Effects.Effect.FileWrite
            Effects.Effect.EnvRead
            Effects.Effect.EnvWrite
            Effects.Effect.Process
            Effects.Effect.Stdin
            Effects.Effect.Stdout
            Effects.Effect.Clock
            Effects.Effect.Random
            Effects.Effect.Http
            Effects.Effect.DbRead
            Effects.Effect.DbWrite ]

        // The language tools read the store to answer questions about code. They should not write
        // it, and they should not reach the machine.
        "LanguageTools", set [ Effects.Effect.PackageRead; Effects.Effect.Stdout ]

        // Source control writes code. That is the job.
        "SCM",
        set
          [ Effects.Effect.PackageRead
            Effects.Effect.PackageWrite
            Effects.Effect.DbRead
            Effects.Effect.Stdout
            Effects.Effect.Clock ]

        // Rendering. It reads the store because turning a hash back into a name is a lookup, and
        // that is the whole of its business with the outside world. A pretty printer that reaches
        // anything else is a finding by definition.
        "PrettyPrinter", set [ Effects.Effect.PackageRead ]

        // Traces are the subject; printing them is the output.
        "Tracing",
        set
          [ Effects.Effect.TraceRead
            Effects.Effect.TraceWrite
            Effects.Effect.PackageRead
            Effects.Effect.Stdout
            Effects.Effect.Clock ]

        // The language server speaks JSON-RPC over stdio and answers questions about code.
        "LanguageServerProtocol",
        set
          [ Effects.Effect.PackageRead
            Effects.Effect.Stdin
            Effects.Effect.Stdout ] ]

  /// One row: a function, and the effects it reaches beyond its module's baseline.
  ///
  /// `None` means there is no baseline for that module, which is NOT the same as nothing over it.
  /// Returning an empty set for an unlisted module would make `platforms audit
  /// Darklang.PrettyPrinter` answer "0 of 150 functions", reading as a clean bill of health for a
  /// namespace nobody has written a baseline for. Plausible output instead of an error is the
  /// failure mode these reports are most prone to.
  let private overBaseline
    (loadFn : PackagePermissions.LoadFn)
    (effectsOf : string -> Set<Effects.Effect>)
    (topModule : string)
    (hash : PT.Hash)
    : Ply<Option<Set<Effects.Effect>>> =
    uply {
      match Map.tryFind topModule baseline with
      | None -> return None
      | Some allowed ->
        let! closure = PackagePermissions.loadClosure loadFn hash
        let reached =
          closure
          |> Map.toList
          |> List.fold
            (fun acc (_, (_, analysis : Calls.Analysis)) ->
              analysis.names
              |> List.fold
                (fun acc name ->
                  match name with
                  | PT.FQFnName.Builtin b -> Set.union acc (effectsOf b.name)
                  | PT.FQFnName.Package _ -> acc)
                acc)
            Set.empty
        return Some(Set.difference reached allowed)
    }

  let report (set : PlatformSet) (modulePrefix : string) : Ply<List<string>> =
    uply {
      // Builtin name -> what THAT BUILTIN may do: its own `callEffects`, plus its platform's
      // `dynamicEffects` because those are effects its body may request and no static declaration
      // shows.
      //
      // Per builtin rather than per platform, and the difference matters. The platform's whole
      // surface is the right answer to "what does granting this cost", which is what `platforms
      // needed` reports. It is the wrong answer here: `Stdlib.Cli.Dir.current` calls one posix
      // builtin that reads a path, and attributing `Posix`'s surface to it reported 190-odd
      // standard library functions as reaching `native` when what they reach is the filesystem.
      let effectByBuiltin =
        set.platforms
        |> List.collect (fun p ->
          p.builtins.fns
          |> Seq.map (fun (KeyValue(k, fn)) ->
            (k.name, Set.union fn.callEffects p.dynamicEffects))
          |> List.ofSeq)
        |> Map.ofList

      let effectsOf (name : string) =
        Map.tryFind name effectByBuiltin |> Option.defaultValue Set.empty

      let! locations = LibDB.Queries.listedFnLocations modulePrefix

      let mutable findings = []
      let mutable audited = 0
      let mutable unbaselined = Set.empty
      for (location : PT.PackageLocation, hash) in locations do
        let topModule = List.tryHead location.modules |> Option.defaultValue ""
        match! overBaseline LibDB.ProgramTypes.Fn.get effectsOf topModule hash with
        | None -> unbaselined <- Set.add topModule unbaselined
        | Some over ->
          audited <- audited + 1
          if not (Set.isEmpty over) then
            let full =
              String.concat "." (location.owner :: (location.modules @ [ location.name ]))
            findings <- (full, over) :: findings

      let rendered =
        findings
        |> List.sortBy fst
        |> List.map (fun (name, over) ->
          let es =
            over |> Set.toList |> List.map Effects.name |> List.sort |> String.concat " "
          $"    {name,-58} {es}")

      let byEffect =
        findings
        |> List.collect (fun (_, over) -> over |> Set.toList |> List.map Effects.name)
        // Prelude's `List.groupBy` answers a `Map` keyed by the grouping value; the names ARE the
        // keys here, so the count is the length of each group. Written as a lambda because `id`
        // resolves to something else in this file's scope.
        |> List.groupBy (fun e -> e)
        |> Map.toList
        |> List.map (fun (e, occurrences) -> (e, List.length occurrences))
        |> List.sortByDescending snd
        |> List.map (fun (e, n) -> $"    {e,-16} {n,4} functions")

      return
        [ $"Functions under `{modulePrefix}` reaching effects their module's baseline does not allow."
          ""
          "The baseline is in `PlatformReport.Audit`, one row per top-level module, deliberately"
          "opinionated: a baseline derived from what the code does can never be violated. Effects are"
          "per BUILTIN (its own declaration plus its platform's dynamic ones), which is what the"
          "function really does. What GRANTING costs is a wider number, and `platforms needed` has it."
          "" ]
        @ (if List.isEmpty rendered then
             [ "    (nothing over baseline)" ]
           else
             rendered)
        @ [ ""; $"    {List.length findings} of {audited} audited functions, by effect:"; "" ]
        @ byEffect
        @ (if Set.isEmpty unbaselined then
             []
           else
             [ ""
               $"    NOT AUDITED: {List.length locations - audited} functions in modules with no"
               "    baseline row, so nothing above says anything about them. Add a row to"
               "    `PlatformReport.Audit.baseline`, or read this as unknown rather than clean:"
               "" ]
             @ (unbaselined |> Set.toList |> List.sort |> List.map (fun m -> $"      {m}")))
    }


/// Builtins that nothing in this repo can reach, transitively.
///
/// `platforms unused` asks a textual question: does any `.dark` file NAME this builtin. That finds
/// a builtin with no wrapper, which is a real thing to find, but it says nothing about whether the
/// wrapper is itself reachable. A wrapper called only by another unused wrapper counts as used.
///
/// This asks the transitive one. Take every listed package function as a root, walk the call graph,
/// union the builtins. Whatever is left in the platform set is reachable from NOTHING in the repo.
///
/// **It is a deletion CANDIDATE list, not a deletion list**, and the difference matters:
///
/// - The standard library exists for programs that are not in this repo. `Stdlib.Int8.bitwiseNot`
///   having no caller here says nothing about whether anyone needs it.
/// - The walk is static. A builtin only ever reached through a function-typed parameter is
///   invisible to it, and `Analysis.complete` says when that happened somewhere.
/// - Some builtins are called by the RUNTIME rather than by Dark: the pretty printers reach several
///   through `Execution.callStringPrinter`, and tests and perf workloads reach others.
///
/// What it is good for is the opposite question. A builtin nothing reaches AND nothing wraps is
/// dead with high confidence, and the two reports together say that where neither says it alone.
module Unreachable =

  module Calls = LibExecution.CallGraph
  module PackagePermissions = LibDB.PackagePermissions

  let report (set : PlatformSet) : Ply<List<string>> =
    uply {
      let! locations = LibDB.Queries.listedFnLocations "Darklang"

      let reached = System.Collections.Generic.HashSet<string>()
      let mutable incomplete = 0

      for (_, hash) in locations do
        let! closure = PackagePermissions.loadClosure LibDB.ProgramTypes.Fn.get hash
        for KeyValue(_, (_, analysis : Calls.Analysis)) in closure do
          if not analysis.complete then incomplete <- incomplete + 1
          for name in analysis.names do
            match name with
            | PT.FQFnName.Builtin b -> reached.Add b.name |> ignore<bool>
            | PT.FQFnName.Package _ -> ()

      // No operator exclusion here, deliberately. `CallGraph.analyze` records the builtin behind
      // an `EInfix`, so operator-dispatched builtins (`add`, `stringAppend`, `negate` ...) are
      // genuinely reached and this list is honest without help. If they ever reappear as
      // unreached, fix `analyze` rather than filtering them out here.
      let byPlatform =
        set.platforms
        |> List.map (fun p ->
          let unreachedHere =
            p.builtins.fns.Keys
            |> Seq.map _.name
            |> Seq.filter (reached.Contains >> not)
            |> Seq.sort
            |> List.ofSeq
          (p, unreachedHere))
        |> List.filter (fun (_, names) -> not (List.isEmpty names))
        |> List.sortByDescending (fun (_, names) -> List.length names)

      let total = byPlatform |> List.sumBy (fun (_, names) -> List.length names)
      let inSet = set.builtins.fns.Count

      // Two methods, one question. `PackageSurface.countReferences` greps the comment-stripped
      // `.dark` corpus for `Builtin.name`; the walk above follows the call graph. They should
      // agree, and where they do not, one of them is lying about something a test depends on:
      // `Tests.Platform.builtinsAreWrappedInTheirPlatformsHome` is built on the textual method.
      //
      // Expect the operators to disagree, and only them. `a + b` never spells `Builtin.add`, so
      // the text cannot see it while the graph now can (`analysisVersion` 4).
      let disagreements =
        set.builtins.fns.Keys
        |> Seq.map _.name
        |> Seq.choose (fun name ->
          let textual = PackageSurface.countReferences name > 0
          let structural = reached.Contains name
          if textual = structural then
            None
          elif structural then
            Some $"    {name,-32} call graph sees it, the text does not"
          else
            Some $"    {name,-32} the text sees it, the call graph does not")
        |> Seq.sort
        |> List.ofSeq

      let lines =
        byPlatform
        |> List.collect (fun (p, names) ->
          let shown = names |> String.concat ", "
          [ $"    {Platform.coordinate p}  {List.length names} of {Platform.fnCount p}"
            $"      {shown}"
            "" ])

      return
        [ $"Builtins no package function in this repo reaches: {total} of {inSet}."
          ""
          "A deletion CANDIDATE list. The standard library exists for programs that are not in this"
          "repo, so an unreached stdlib primitive is not evidence of anything on its own; pair it"
          "with `platforms unused`, which asks whether a builtin has a Dark wrapper at all. Both"
          "saying yes is a strong signal. Either alone is not."
          ""
          "An operator's builtin appearing here (`negate` for `-x`, `power` for `**`) means the"
          "OPERATOR is unused in this repo. It is reachable by anyone who types it, so it is the"
          "one entry on this list that is certainly not dead."
          "" ]
        @ lines
        @ [ ""
            $"Where the textual scan and the call graph disagree ({List.length disagreements}):"
            "" ]
        @ (if List.isEmpty disagreements then [ "    (they agree everywhere)" ] else disagreements)
        @ [ ""
            $"    Roots: {List.length locations} listed package functions."
            $"    {incomplete} closure members had an unresolvable call (a function-typed"
            "    parameter), so the reached set is a lower bound and this list an upper one." ]
    }
