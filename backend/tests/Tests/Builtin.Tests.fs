module Tests.Builtin

// Misc builtin tests that do not fit in LibExecution.tests.

open Expecto
open System.IO
open System.Text.RegularExpressions

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module PackageSurface = TestUtils.PackageSurface

open TestUtils.TestUtils


/// Every builtin a running `darklang` has, plus `LibTest`.
///
/// One entry, not two. `localBuiltIns` is the whole platform catalog, `Darklang` (CliHost)
/// included, so appending CliHost beside it would count those builtins twice, which the
/// duplicate-name check below says out loud.
///
/// `LibTest` has to be in here even though it ships in nothing: `packages/darklang/test` calls
/// `testRaiseException` and `testRuntimeError`, so a set without it reports those as builtins that
/// package code calls and that do not exist.
let private allBuiltinSets () : List<RT.Builtins> =
  [ localBuiltIns PT.PackageManager.empty ]


let oldFunctionsAreDeprecated =
  let builtinToString (name : RT.FQFnName.Builtin) = $"{name.name}_v{name.version}"

  testTask "old functions are deprecated" {
    let mutable counts = Map.empty

    let fns = allBuiltinSets () |> List.collect (fun b -> b.fns.Values |> List.ofSeq)

    fns
    |> List.iter (fun fn ->
      let key = builtinToString fn.name

      if fn.deprecated = RT.NotDeprecated then
        counts <-
          Map.update
            key
            (fun count -> count |> Option.defaultValue 0 |> (+) 1 |> Some)
            counts

      ())

    Map.iter
      (fun name count ->
        Expect.equal count 1 $"{name} has more than one undeprecated function")
      counts
  }


/// The name of every builtin a running `darklang` can call -- fns and values
/// alike, since `Builtin.x` is how Dark reaches both.
let private allBuiltinNames () : List<string> =
  allBuiltinSets ()
  |> List.collect (fun builtins ->
    let fnNames = builtins.fns.Values |> Seq.map (fun fn -> fn.name.name)
    let valueNames = builtins.values.Values |> Seq.map (fun v -> v.name.name)
    Seq.append fnNames valueNames |> List.ofSeq)
  |> List.distinct


// -- Builtin access in package matter --
//
// Walk every .dark under packages/ and count textual references to
// `Builtin.<name>` (or `Builtin.<name>_v<digits>`) for every registered
// builtin. Anything with >1 textual reference must appear in the allowlist
// below.
//
// A builtin should have one package wrapper, and callers should go through
// it. The allowlist names the cases where direct multi-use is intentional.
//
// Infix-dispatched builtins (`+`, `==`, etc.) reach the runtime through operator syntax, so they
// have no textual `Builtin.X` reference. `PT.InfixFnName.isOperatorDispatched` answers that from
// the lowering table itself, so there is no second copy of the list here to keep in step.

let private languageIdioms = PackageSurface.languageIdioms

/// `convert` and `tryConvert` are the two builtins that replaced 116 typed width conversions
/// (`Int8.fromInt64`, `UInt32.toFloat` and the rest). Every one of those wrappers still exists with
/// its old name and signature and delegates to one of these, so there are many call sites by
/// construction and the one-wrapper rule does not apply: the wrappers ARE the per-type surface, and
/// these two are the primitive underneath them. Wrapping the primitive again would just add one
/// more name for the same thing.
let private multiUseAllowlist : Set<string> = Set.ofList [ "convert"; "tryConvert" ]


let private findRepoRoot = PackageSurface.findRepoRoot
let private packagesText = PackageSurface.packagesText
let private repoDarkText = PackageSurface.repoDarkText
let private countReferencesIn = PackageSurface.countReferencesIn
let private countReferences = PackageSurface.countReferences


let builtinAccessInPackageMatter =
  testTask "builtin access in package matter" {
    let offenders =
      allBuiltinNames ()
      |> Seq.choose (fun name ->
        if Set.contains name multiUseAllowlist then
          None
        elif Set.contains name languageIdioms then
          None
        elif PT.InfixFnName.isOperatorDispatched name then
          None
        else
          let count = countReferences name
          if count <= 1 then None else Some(name, count))
      |> List.ofSeq

    if not (List.isEmpty offenders) then
      let lines =
        offenders
        |> List.sortBy fst
        |> List.map (fun (name, count) -> $"  {name}: {count} refs")
        |> String.concat "\n"
      Expect.isTrue
        false
        ("Some builtins are referenced from more than one place in packages/:\n"
         + lines
         + "\n\nWrap the builtin in one Dark package fn -- a Stdlib or Cli helper that names it, types it "
         + "and documents it -- and route the callers through that. `multiUseAllowlist` is for the cases "
         + "where a wrapper is the wrong answer, and it is down to one entry; a new one needs its reason "
         + "written next to it.")
  }


// -- Unused builtins --
//
// The mirror of the test above: a builtin nothing calls is dead F# we keep
// compiling, serializing and documenting. Every registered builtin should be
// reachable from Dark somewhere in the repo -- packages/, test files, perf
// workloads or sample scripts.

/// Builtins with no Dark caller anywhere, kept deliberately.
/// Each one needs a reason; without one, delete the builtin instead.
let private unusedAllowlist : Set<string> =
  Set.ofList
    [ // Test-harness escape hatch for the cases that expect an exception to
      // reach the reporter. The harness still reads the count it sets; the
      // testfile cases that set it are currently commented out.
      "testSetExpectedExceptionCount" ]


let everyBuiltinIsReferenced =
  testTask "every builtin is referenced from Dark" {
    let unused =
      allBuiltinNames ()
      |> Seq.filter (fun name ->
        not (Set.contains name unusedAllowlist)
        && not (PT.InfixFnName.isOperatorDispatched name)
        && countReferencesIn repoDarkText.Value name = 0)
      |> List.ofSeq

    if not (List.isEmpty unused) then
      let lines =
        unused
        |> List.sort
        |> List.map (fun name -> $"  {name}")
        |> String.concat "\n"
      Expect.isTrue
        false
        ("Some builtins have no Dark caller anywhere in the repo:\n"
         + lines
         + "\n\nDelete the builtin, or wire it up (a package wrapper, a test file, a perf workload). "
         + "Add to `unusedAllowlist` only with a reason -- an uncalled builtin is dead weight in every "
         + "build and every serialized package.")
  }


/// A description written across several source lines has to read as one sentence, since most
/// things that show it (the workbench signature pane, `dark help`, LSP hover) have one line to
/// show it on.
let descriptionsAreJoined =
  let builtins =
    allBuiltinSets () |> List.collect (fun b -> b.fns.Values |> List.ofSeq)

  testList
    "descriptions"
    [ test "no builtin description carries source indentation" {
        let ragged =
          builtins
          |> List.filter (fun fn -> Regex.IsMatch(fn.description, @"\n[ \t]"))
          |> List.map (fun fn -> string fn.name)

        Expect.isEmpty
          ragged
          ("These descriptions keep the indentation of the F# literal they were written in, which "
           + "renders as a gap mid-sentence:\n"
           + String.concat "\n" ragged)
      }
      test "a wrapped description reads as one sentence" {
        // `add` is written across four indented source lines.
        let add =
          builtins
          |> List.tryFind (fun fn -> fn.name.name = "add" && fn.name.version = 0)

        match add with
        | None -> failtest "no `add` builtin"
        | Some fn ->
          Expect.stringContains
            fn.description
            "integer overflow wraps around"
            "the line break should have become a single space"
      } ]


/// `Builtin.<name>` spellings that are NOT a builtin call. Dark has its own `Builtin`
/// modules and cases; the lookbehind below drops the qualified ones, and these bare
/// ones can only be told apart by the file they sit in.
let private notActuallyBuiltins : Set<string> =
  Set.ofList
    [ "tokenize" // semanticTokens.dark has its own `Builtin` module
      "toPT" // same, in writtenTypesToProgramTypes.dark
      "fullForReference" // FQValueName.Builtin
      "Json" // `Builtin.Json.*`: a module under the builtin namespace, not a builtin
      "X" ] // `Builtin.X` as prose, inside the for-ai docs


/// Every builtin that package code names has to exist. A `Builtin.x` naming nothing
/// builds fine and throws the moment someone reaches it.
///
/// Textual, because a name resolving only when it RUNS is the hole being covered --
/// there is no resolution step to hook.
let everyBuiltinPackagesCallExists =
  testTask "every builtin that package code calls exists" {
    let names (b : RT.Builtins) =
      Set.union
        (b.fns.Values |> Seq.map (fun fn -> fn.name.name) |> Set.ofSeq)
        (b.values.Values |> Seq.map (fun v -> v.name.name) |> Set.ofSeq)

    let defined = allBuiltinSets () |> List.map names |> Set.unionMany

    let regex =
      Regex(
        @"(?<![a-zA-Z0-9_.])Builtin\.([a-zA-Z][a-zA-Z0-9_]*)",
        RegexOptions.Compiled
      )

    let missing =
      regex.Matches(packagesText.Value)
      |> Seq.map (fun m -> m.Groups[1].Value)
      |> Set.ofSeq
      |> Set.filter (fun name ->
        not (Set.contains name defined)
        && not (Set.contains name notActuallyBuiltins))

    if not (Set.isEmpty missing) then
      let listed = missing |> Set.toList |> List.sort |> String.concat ", "
      Expect.isTrue
        false
        ($"package code calls builtins that don't exist: {listed}\n\n"
         + "A missing builtin resolves lazily, so nothing else in this suite will tell "
         + "you. Either it was deleted and its callers need repointing at the Dark "
         + "replacement, or it was renamed.")
  }


/// Every CLI command's `help` returns the help TEXT, not an AppState.
///
/// `executeCommandHelp` appends the alias line to whatever `help` returns, so one that
/// prints internally and returns `state` makes `dark <cmd> --help` throw. A Dark record
/// field is not checked against the function stored in it, so nothing else catches it.
let everyCliHelpReturnsText =
  testTask "every CLI command's `help` returns String" {
    let root = Path.Combine(findRepoRoot (), "packages", "darklang", "cli")
    Expect.isTrue (Directory.Exists root) $"the CLI package directory exists: {root}"

    let sourceLines =
      Directory.EnumerateFiles(root, "*.dark", SearchOption.AllDirectories)
      |> Seq.collect (fun path ->
        File.ReadAllLines path
        |> Array.toSeq
        |> Seq.mapi (fun i line -> (path.Replace('\\', '/'), i + 1, line.Trim())))
      |> List.ofSeq

    let report (matches : string -> bool) : List<string> =
      sourceLines
      |> List.filter (fun (_, _, t) -> matches t)
      |> List.map (fun (path, n, t) -> $"  {path}:{n}  {t}")
      |> List.sort

    // `help`, and also `helpShow`/`helpReview`: the registry takes any of them for the
    // `help` slot, so the contract is the same for all of them. Keyed on the AppState
    // parameter, which is what makes it a registry help rather than a local helper
    // that happens to start with the same word.
    let offenders =
      report (fun t ->
        t.StartsWith "let help"
        && t.Contains "AppState)"
        && not (t.EndsWith ": String ="))

    // The other half of the same contract: a caller that RETURNS `help state` where an
    // AppState is expected.
    let badCallers =
      report (fun t ->
        Regex.IsMatch(t, @"^(\| .*-> )?help[A-Za-z]* _?state$")
        && not (t.Contains "printLine"))

    if not (List.isEmpty badCallers) then
      Expect.isTrue
        false
        ("These call sites return a `help` result where an AppState is expected:\n"
         + String.concat "\n" badCallers
         + "\n\n`help` returns the TEXT. A caller that wants to show it and carry on "
         + "writes `Stdlib.printLine (help state)` and then `state`.")

    if not (List.isEmpty offenders) then
      Expect.isTrue
        false
        ("These `help` functions do not return the help text:\n"
         + String.concat "\n" offenders
         + "\n\nBuild the lines and `|> Stdlib.String.join \"\\n\"`.")
  }


// Scoped effects must check their concrete target. OS operations use Host;
// store operations use PermissionCheck.


/// The two assemblies carved out of the former `Builtins.Matter`: the package store, and the
/// user database plus the raw SQLite floor. Both, because a scoped store effect can be declared in
/// either half.
let private storeBuiltinsRoots () =
  let builtins = Path.Combine(findRepoRoot (), "backend", "src", "Builtins")
  [ Path.Combine(builtins, "Builtins.Store")
    Path.Combine(builtins, "Builtins.Data") ]

let private storeScopedEffectRegex =
  Regex(@"\bEffect\.(FileRead|FileWrite|DbRead|DbWrite)\b", RegexOptions.Compiled)

let storeScopedEffectsRequireTargetChecks =
  test "store scoped effects require target checks" {
    let root = findRepoRoot ()
    let nameRegex = Regex(@"^""(?<name>[^""]+)""", RegexOptions.Compiled)
    let missing =
      storeBuiltinsRoots ()
      |> List.collect (fun dir ->
        Directory.GetFiles(dir, "*.fs", SearchOption.AllDirectories) |> List.ofArray)
      |> Seq.collect (fun file ->
        File
          .ReadAllText(file)
          .Split([| "{ name = fn " |], System.StringSplitOptions.None)
        |> Seq.skip 1
        |> Seq.choose (fun block ->
          let nameMatch = nameRegex.Match block
          if nameMatch.Success && storeScopedEffectRegex.IsMatch block then
            Some(file, nameMatch.Groups["name"].Value, block)
          else
            None))
      |> Seq.choose (fun (file, name, body) ->
        if body.Contains "PermissionCheck.require" then
          None
        else
          Some(Path.GetRelativePath(root, file), name))
      |> Seq.sortBy snd
      |> List.ofSeq

    if not (List.isEmpty missing) then
      let lines =
        missing
        |> List.map (fun (file, name) -> $"  {name} ({file})")
        |> String.concat "\n"
      Expect.isTrue
        false
        ("Scoped-effect store builtins missing a concrete PermissionCheck.require* call:\n"
         + lines
         + "\n\nCheck the actual path/table immediately before the store operation.")
  }

// Keep this canary so a stale regex cannot match nothing silently.
let scopedEffectInventoryRegexIsLive =
  test "scoped-effect inventory regex matches source" {
    let matches =
      storeBuiltinsRoots ()
      |> List.collect (fun dir ->
        Directory.GetFiles(dir, "*.fs", SearchOption.AllDirectories) |> List.ofArray)
      |> Seq.filter (fun file ->
        storeScopedEffectRegex.IsMatch(File.ReadAllText file))
      |> Seq.length
    Expect.isGreaterThan
      matches
      0
      "no builtin source matched the scoped-effect pattern; the inventory regex is stale"
  }


// Keep the handwritten Dark effect table aligned with Effects.all.
let darkEffectTableMatchesRuntime =
  test "Dark effect table matches runtime effects" {
    let source =
      File.ReadAllText(
        Path.Combine(
          findRepoRoot (),
          "packages",
          "darklang",
          "languageTools",
          "permissions.dark"
        )
      )
    let block =
      let start = source.IndexOf "val effectCases ="
      Expect.isGreaterThan start -1 "permissions.dark should define effectCases"
      let stop = source.IndexOf("]", start)
      source.Substring(start, stop - start)
    // Each row must match the runtime enum case and rule name.
    let darkRows =
      Regex.Matches(block, "Effect\\.([A-Za-z]+), \"([A-Za-z]+)\", \"([a-z-]+)\"\\)")
      |> Seq.map (fun m -> m.Groups[1].Value, m.Groups[2].Value, m.Groups[3].Value)
      |> List.ofSeq
    let runtimeRows =
      LibExecution.Effects.all
      |> List.map (fun effect ->
        $"%A{effect}", $"%A{effect}", LibExecution.Effects.name effect)
    Expect.equal darkRows runtimeRows "Dark effectCases must match Effects.all"
  }


let tests =
  testList
    "builtin"
    [ darkEffectTableMatchesRuntime
      oldFunctionsAreDeprecated
      builtinAccessInPackageMatter
      everyBuiltinIsReferenced
      descriptionsAreJoined
      everyBuiltinPackagesCallExists
      everyCliHelpReturnsText
      storeScopedEffectsRequireTargetChecks
      scopedEffectInventoryRegexIsLive ]
