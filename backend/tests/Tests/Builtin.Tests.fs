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

open TestUtils.TestUtils


/// Every builtin library a running `darklang` has. `localBuiltIns` is the set
/// the tests execute with, which leaves out CliHost -- `dark eval`, script
/// running, the CLI's own entry points -- so the checks below would otherwise
/// ignore that whole library.
let private allBuiltinSets () : List<RT.Builtins> =
  [ localBuiltIns PT.PackageManager.empty; Builtins.CliHost.Builtin.builtins () ]


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
// Walk every .dark under packages/ and count code references to
// `Builtin.<name>` (or `Builtin.<name>_v<digits>`) for every registered
// builtin. Anything with >1 code reference must appear in the allowlist
// below.
//
// A builtin should have one package wrapper, and callers should go through
// it. The allowlist names the cases where direct multi-use is intentional.
//
// Infix-dispatched builtins (`+`, `==`, etc.) are dispatched through
// operator syntax, so they have no textual `Builtin.X` references.

/// Builtins that are language IDIOM rather than library calls, so "one wrapper, everyone through it"
/// does not apply to them. Distinct from the allowlist below, which is for builtins that could be
/// wrapped and deliberately are not.
let private languageIdioms : Set<string> =
  Set.ofList
    [ // `unwrap` reads as syntax and appears in 60-odd places. A generic Dark wrapper typechecks
      // (`let unwrap (value: 'optOrRes) : 'a` works for both Option and Result) and buys nothing: it
      // has no shape to type and nothing to document that the name does not say, and it puts itself
      // at the bottom of every unwrap failure's call stack, one frame below the code that had the
      // None. That frame is the reason, and it is a reason about error messages, not about layering.
      "unwrap" ]

/// Builtins called via infix operators rather than `Builtin.X` syntax.
/// Source: LibExecution/ProgramTypesToRuntimeTypes.fs InfixFnName.toFnName
/// for binary ops; LibParser/Parser.fs lowers the unary `-x`, `~x` and `!x`
/// prefixes to Builtin.negate / bitwiseNot / boolNot.
let private infixDispatched : Set<string> =
  Set.ofList
    [ // Polymorphic numeric operators
      "add"
      "subtract"
      "multiply"
      "divide"
      "modulo"
      "power"
      // Bitwise operators
      "bitwiseAnd"
      "bitwiseOr"
      "bitwiseXor"
      "bitwiseNot"
      "shiftLeft"
      "shiftRight"
      "greaterThan"
      "greaterThanOrEqualTo"
      "lessThan"
      "lessThanOrEqualTo"
      "negate"
      "equals"
      "notEquals" ]


/// Builtins intentionally referenced from more than one place in `packages/`.
///
/// EMPTY, and worth keeping that way: when a builtin picks up a second caller, wrap it.
/// Before adding an entry, check whether a wrapper already exists and the new caller
/// simply has not been pointed at it. "A wrapper would just name the thing it already
/// is" is not a reason -- that is what a wrapper is.
let private multiUseAllowlist : Set<string> = Set.empty


/// The repo root: the first directory at or above CWD holding `packages/darklang/`.
let private findRepoRoot () : string =
  let rec walk (dir : string) : string option =
    if System.String.IsNullOrEmpty dir then
      None
    else
      let candidate = Path.Combine(dir, "packages", "darklang")
      if Directory.Exists candidate then
        Some dir
      else
        walk (Path.GetDirectoryName dir)

  match walk (Directory.GetCurrentDirectory()) with
  | Some d -> d
  | None ->
    Exception.raiseInternal
      "Couldn't find packages/ walking up from CWD"
      [ "cwd", Directory.GetCurrentDirectory() ]


/// Read builtin references from code tokens, ignoring comments and literal text.
/// Package tests contain expected error strings that name builtins. Counting
/// those strings as callers would falsely report duplicate wrappers when tests
/// move into packages/.
/// Interpolated strings contain executable expressions, so scan those too using
/// the same brace scanner as the parser (including nested strings and comments).
let rec private builtinReferences (source : string) : List<string> =
  let tokens =
    match LibParser.Lexer.tokenize source with
    | Ok(tokens, _) -> List.toArray tokens
    | Error message -> failtest $"Cannot scan builtin references: {message}"

  let found = ResizeArray<string>()
  let scanInterpolation (text : string) =
    let raw = text.StartsWith "$\"\"\""
    let mutable index = if raw then 4 else 2
    let limit = text.Length - (if raw then 3 else 1)
    while index < limit do
      if not raw && text[index] = '\\' then
        index <- index + 2
      elif index + 1 < limit && text[index] = '{' && text[index + 1] = '{' then
        index <- index + 2
      elif text[index] = '{' then
        let close = LibParser.Lexer.findInterpExprClose text limit (index + 1)
        if close < 0 then
          failtest "Unclosed expression while scanning builtin references"
        found.AddRange(
          builtinReferences (text.Substring(index + 1, close - index - 1))
        )
        index <- close + 1
      else
        index <- index + 1

  let tokenAt i = tokens[i].token
  for i in 0 .. tokens.Length - 1 do
    match tokenAt i with
    | LibParser.Tokenizer.TInterpString -> scanInterpolation tokens[i].text
    | LibParser.Tokenizer.TIdent "Builtin" when
      i + 2 < tokens.Length && (i = 0 || tokenAt (i - 1) <> LibParser.Tokenizer.TDot)
      ->
      match tokenAt (i + 1), tokenAt (i + 2) with
      | LibParser.Tokenizer.TDot, LibParser.Tokenizer.TIdent name -> found.Add name
      | _ -> ()
    | _ -> ()
  List.ofSeq found


/// Scan each file independently so unfinished literals cannot consume another
/// file's code. Build output holds copies of source and is excluded.
let private builtinReferencesUnder (root : string) : List<string> =
  Directory.EnumerateFiles(root, "*.dark", SearchOption.AllDirectories)
  |> Seq.filter (fun path ->
    let sep = Path.DirectorySeparatorChar
    not (path.Contains $"{sep}Build{sep}"))
  |> Seq.collect (fun path ->
    try
      builtinReferences (File.ReadAllText path)
    with e ->
      failtest $"{path}: {e.Message}")
  |> List.ofSeq


let private packagesReferences : Lazy<List<string>> =
  lazy (builtinReferencesUnder (Path.Combine(findRepoRoot (), "packages")))


/// Include legacy tests, perf workloads and scripts when checking for dead code.
let private repoReferences : Lazy<List<string>> =
  lazy (builtinReferencesUnder (findRepoRoot ()))


/// `Builtin.foo_v0` counts towards `foo`, and towards a builtin actually named
/// `foo_v0` if one exists, preserving the versioned-reference check.
let private referenceCounts (references : List<string>) : Map<string, int> =
  let versionSuffix = Regex(@"_v[0-9]+$")
  let mutable counts = Map.empty
  let bump (name : string) =
    counts <-
      Map.add name (1 + (counts |> Map.tryFind name |> Option.defaultValue 0)) counts

  for name in references do
    bump name
    let stripped = versionSuffix.Replace(name, "")
    if stripped <> name then bump stripped
  counts

let private packagesRefCounts : Lazy<Map<string, int>> =
  lazy (referenceCounts packagesReferences.Value)

let private repoRefCounts : Lazy<Map<string, int>> =
  lazy (referenceCounts repoReferences.Value)

let private countReferences (builtinName : string) : int =
  packagesRefCounts.Value |> Map.tryFind builtinName |> Option.defaultValue 0


/// Every name excluded as infix-dispatched is still a registered builtin.
///
/// `infixDispatched` is an exclusion list, and an exclusion nobody revisits is how
/// a sweep quietly stops covering the thing it was written for: rename or retire a
/// builtin and its name sits here forever, excusing nothing and hiding nothing,
/// while the sweep it was carved out of no longer has a reason to skip anything.
/// `notSweepable` in `CliSurface.Tests.fs` has had this check for the same reason;
/// this is the second list learning it.
let everyInfixExclusionIsReal =
  test "every infix-dispatched exclusion is still a builtin" {
    let registered = allBuiltinNames () |> Set.ofList
    Expect.isGreaterThan (Set.count registered) 100 "the builtin sets were read"

    let stale = Set.difference infixDispatched registered

    if not (Set.isEmpty stale) then
      Tests.failtestf
        "excluded as infix-dispatched but not registered as a builtin: %s"
        (stale |> Set.toList |> List.sort |> String.concat ", ")
  }
let builtinReferenceScanning =
  testList
    "builtin reference scanning"
    [ for name, source, expected in
        [ "comments",
          "// Builtin.fake\nBuiltin.printLine (* Builtin.fake (* nested *) *) () // Builtin.fake",
          [ "printLine" ]
          "literal text",
          "\"Builtin.fake \\\" // still a string\"\n\"\"\"Builtin.fake\"\"\"\nBuiltin.printLine ()",
          [ "printLine" ]
          "interpolation",
          "$\"Builtin.fake {{Builtin.fake}} {Builtin.int64Add 1L 2L}\"",
          [ "int64Add" ]
          "raw interpolation",
          "$\"\"\"Builtin.fake {{Builtin.fake}} {Builtin.printLine \"Builtin.fake\"}\"\"\"",
          [ "printLine" ]
          "nested interpolation",
          "$\"outer {$\"inner {Builtin.printLine \"ok\"}\"}\"",
          [ "printLine" ]
          "qualified Dark names",
          "Darklang.Example.Builtin.fake NotBuiltin.fake Builtin.real_v2",
          [ "real_v2" ]
          "function values and whitespace",
          "let f = Builtin . int64Add\nBuiltin.int64Add 1L 2L",
          [ "int64Add"; "int64Add" ] ] do
        test name {
          Expect.equal
            (builtinReferences source)
            expected
            "only code references count"
        }
      test "versioned calls retain both counts" {
        Expect.equal
          (builtinReferences "Builtin.example_v2 () Builtin.example ()"
           |> referenceCounts)
          (Map.ofList [ "example", 2; "example_v2", 1 ])
          "version suffixes contribute to the builtin's base name"
      } ]


let builtinAccessInPackageMatter =
  testTask "builtin access in package matter" {
    let offenders =
      allBuiltinNames ()
      |> Seq.choose (fun name ->
        if Set.contains name multiUseAllowlist then
          None
        elif Set.contains name languageIdioms then
          None
        elif Set.contains name infixDispatched then
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
         + "where a wrapper is the wrong answer, and it is empty; a new one needs its reason "
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
      "testSetExpectedExceptionCount"
      // Scheduler test fixtures: their callers are the Dark programs embedded as strings in
      // `Scheduler.Tests.fs`, which this scan of `.dark` files cannot see. A testfile cannot
      // use them: a gate blocks until F# releases it.
      "testGateWait"
      "testTrace"
      "testRead"
      "testSlowStream"
      "testFailingRead" ]


let everyBuiltinIsReferenced =
  testTask "every builtin is referenced from Dark" {
    let unused =
      allBuiltinNames ()
      |> Seq.filter (fun name ->
        not (Set.contains name unusedAllowlist)
        && not (Set.contains name infixDispatched)
        && not (Map.containsKey name repoRefCounts.Value))
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


/// Bare `Builtin` names that refer to Dark modules or cases, not F# builtins.
/// Fully qualified occurrences have already been excluded by the token scan.
let private notActuallyBuiltins : Set<string> =
  Set.ofList
    [ "tokenize" // semanticTokens.dark has its own `Builtin` module
      "toPT" // same, in writtenTypesToProgramTypes.dark
      "fullForReference" // FQValueName.Builtin
      "Json" ] // `Builtin.Json.*`: a module under the builtin namespace


/// Every builtin that package code names has to exist. A `Builtin.x` naming nothing
/// builds fine and throws the moment someone reaches it.
///
/// Scan syntax without executing it: name resolution at runtime is too late.
let everyBuiltinPackagesCallExists =
  testTask "every builtin that package code calls exists" {
    let names (b : RT.Builtins) =
      Set.union
        (b.fns.Values |> Seq.map (fun fn -> fn.name.name) |> Set.ofSeq)
        (b.values.Values |> Seq.map (fun v -> v.name.name) |> Set.ofSeq)

    let defined = allBuiltinSets () |> List.map names |> Set.unionMany

    let missing =
      packagesReferences.Value
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


let private storeBuiltinsRoot () =
  Path.Combine(findRepoRoot (), "backend", "src", "Builtins", "Builtins.Matter")

let private storeScopedEffectRegex =
  Regex(@"\bEffect\.(FileRead|FileWrite|DbRead|DbWrite)\b", RegexOptions.Compiled)

let storeScopedEffectsRequireTargetChecks =
  test "store scoped effects require target checks" {
    let root = findRepoRoot ()
    let nameRegex = Regex(@"^""(?<name>[^""]+)""", RegexOptions.Compiled)
    let missing =
      Directory.GetFiles(storeBuiltinsRoot (), "*.fs", SearchOption.AllDirectories)
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
      Directory.GetFiles(storeBuiltinsRoot (), "*.fs", SearchOption.AllDirectories)
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
      everyInfixExclusionIsReal
      builtinReferenceScanning
      everyBuiltinIsReferenced
      descriptionsAreJoined
      everyBuiltinPackagesCallExists
      everyCliHelpReturnsText
      storeScopedEffectsRequireTargetChecks
      scopedEffectInventoryRegexIsLive ]
