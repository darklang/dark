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
// Walk every .dark under packages/ and count textual references to
// `Builtin.<name>` (or `Builtin.<name>_v<digits>`) for every registered
// builtin. Anything with >1 textual reference must appear in the allowlist
// below.
//
// A builtin should have one package wrapper, and callers should go through
// it. The allowlist names the cases where direct multi-use is intentional.
//
// Infix-dispatched builtins (`+`, `==`, etc.) are dispatched through
// operator syntax, so they have no textual `Builtin.X` references.

/// Builtins called via infix operators rather than `Builtin.X` syntax.
/// Source: LibExecution/ProgramTypesToRuntimeTypes.fs InfixFnName.toFnName
/// for binary ops; LibParser/Parser.fs lowers unary `-x` to Builtin.negate.
let private infixDispatched : Set<string> =
  Set.ofList
    [ // Polymorphic numeric operators
      "add"
      "subtract"
      "multiply"
      "divide"
      "modulo"
      "power"
      "greaterThan"
      "greaterThanOrEqualTo"
      "lessThan"
      "lessThanOrEqualTo"
      "negate"
      "stringAppend"
      "equals"
      "notEquals" ]


/// Builtins intentionally referenced from more than one place in `packages/`.
/// Everything else routes through a single Dark wrapper; when a builtin picks
/// up a second caller, wrap it rather than adding it here. An entry needs a
/// comment saying why a wrapper is the wrong answer for that one.
let private multiUseAllowlist : Set<string> =
  Set.ofList
    [ // The unwrap idiom, in 60-odd places across packages/. A generic Dark
      // wrapper does work -- `let unwrap (value: 'optOrRes) : 'a` typechecks
      // for both Option and Result -- but it costs a package call at every
      // unwrap and puts itself at the bottom of every unwrap failure's call
      // stack, one frame below the code that actually had the None.
      "unwrap" ]


/// Find the repo root by walking up from CWD until we hit one with
/// packages/darklang/.
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


/// Read every .dark file under <root>, minus whole-line comments, as one string.
/// Build output is skipped: it holds copies of files we've already read.
let private darkTextUnder (root : string) : string =
  Directory.EnumerateFiles(root, "*.dark", SearchOption.AllDirectories)
  |> Seq.filter (fun path ->
    let sep = Path.DirectorySeparatorChar
    not (path.Contains $"{sep}Build{sep}"))
  |> Seq.map File.ReadAllText
  |> String.concat "\n"
  |> String.splitOnNewline
  |> List.filter (fun line -> not ((line.TrimStart()).StartsWith "//"))
  |> String.concat "\n"


/// Concatenate every .dark file under packages/ into one string, minus whole-line
/// comments. Cached.
///
/// The comments go because the count below is textual: naming a builtin in a doc
/// comment, which is a reasonable thing to do next to the one fn that wraps it,
/// otherwise reads as a second caller and fails this test. Only lines that are
/// entirely a comment are dropped, so a `//` inside a string literal can't swallow
/// real code after it on the same line.
let private packagesText : Lazy<string> =
  lazy (darkTextUnder (Path.Combine(findRepoRoot (), "packages")))


/// Every .dark file in the repo, minus whole-line comments. Wider than
/// `packagesText`: it also covers test files, perf workloads and sample
/// scripts, which is the difference between "shipped once" and "dead".
let private repoDarkText : Lazy<string> = lazy (darkTextUnder (findRepoRoot ()))


/// Count textual references to `Builtin.<name>` (or `Builtin.<name>_v<n>`)
/// across packages/. The `(?![a-zA-Z0-9_])` lookahead prevents matching
/// `Builtin.dictGet` against the prefix of `Builtin.dictGetItem`.
let private countReferencesIn (corpus : string) (builtinName : string) : int =
  let escaped = Regex.Escape builtinName
  let pattern = $@"Builtin\.{escaped}(?:_v[0-9]+)?(?![a-zA-Z0-9_])"
  let regex = Regex(pattern, RegexOptions.Compiled)
  regex.Matches(corpus).Count

let private countReferences (builtinName : string) : int =
  countReferencesIn packagesText.Value builtinName


let builtinAccessInPackageMatter =
  testTask "builtin access in package matter" {
    let offenders =
      allBuiltinNames ()
      |> Seq.choose (fun name ->
        if Set.contains name multiUseAllowlist then
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
        && not (Set.contains name infixDispatched)
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


let tests =
  testList
    "builtin"
    [ oldFunctionsAreDeprecated
      builtinAccessInPackageMatter
      everyBuiltinIsReferenced
      descriptionsAreJoined
      everyBuiltinPackagesCallExists
      everyCliHelpReturnsText ]
