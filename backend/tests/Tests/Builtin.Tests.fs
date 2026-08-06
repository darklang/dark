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
      "unwrap"

      // `Stdlib.String.contains` and `Stdlib.String.indexOf` both call the
      // builtin directly. `contains` stays direct because the SQL compiler
      // maps the builtin to SQLite INSTR; the Option-returning wrapper is
      // not queryable.
      "stringIndexOf"

      // Structured parse diagnostics used by CLI package creation,
      // CLI-script parsing, and LSP diagnostics.
      "parserParseDiagnostics"

      // CLI / IO surface called by many CLI commands.
      "debug"
      // This instance's local store path -- called directly by several local CLI commands
      // (config/ops/doctor/deprecate). Its old single wrapper lived in the
      // now-deleted sync layer.
      "localDbPath"
      // Local key-value config get/set, called directly by many CLI commands (config, branch
      // switch/current-branch, doctor, connect). Like localDbPath, no single wrapper
      // -- a local primitive.
      "configGet"
      "configSet"
      // Build hash (this binary's version) -- shown by CLI version output and stamped
      // into the sync wire bundle. Read from a couple of places directly.
      "getBuildHash"
      // Stage ops on a branch (effective=0 + tag + per-name bases): used by
      // branch-bundle import (branch-transfer) AND by review-import/review-pull
      // (op-level approval stages incoming ops on a review branch). Both
      // legitimately land ops on a branch for later merge.
      "scmImportBranchOps"
      "directoryCurrent"
      "directoryList"
      "environmentGet"
      "fileAppendText"
      "fileDelete"
      "fileExists"
      "fileIsDirectory"
      "fileRead"
      "fileWrite"
      "getCurrentExecutablePath"
      "print"
      "printLine"
      "stdinReadAll"
      "stdinReadLine"
      "timeSleep"
      "toRepr"
      "unwrap"

      // Posix wrappers (file descriptor primitives).
      "posixFdClose"
      "posixFdWrite"
      "posixReadlink"
      "posixUname"

      // Package manager browsing used by CLI, LSP, and agent code.
      "dbListAll"
      "depsGetDependents"
      "getAllBuiltinFns"
      "pmFindFn"
      "pmFindType"
      "pmFindValue"
      "pmGetFn"
      "pmGetLocationsByFn"
      "pmGetLocationsByType"
      "pmGetLocationsByValue"
      "pmGetType"
      "pmGetValue"
      "pmScriptsGet"
      "pmScriptsList"
      "pmScriptsUpdate"
      "pmSearch"

      // HTTP server entry called by the `dark serve` wrapper.
      "httpServerServe"

      // Streams (CLI / agent / scripts use them directly).
      "streamClose"
      "streamFilter"
      "streamMap"
      "streamNext"
      "streamToBlob"
      "streamToList"
      "streamUnfold"

      // Trace surface read by CLI commands and LSP.
      "tracesFind"
      "tracesHotspots"
      "tracesList"
      "tracesStatsByHandler"

      // Parser entry point used by CLI syntax highlighting, package display,
      // LSP, and CLI-script parsing.
      "parserParseToWrittenTypes"

      // Sync conflict-review surface: the recorded-divergence log, read by the
      // `dark conflicts` review UI and by the `dark sync` pull nudge (which
      // counts unreviewed conflicts so last-writer-wins is never silent).
      "conflictsList"

      // Script/expression evaluation, reached from the CLI's own runners, the workbench REPL and the
      // agent tools. Each is a genuine entry point rather than a wrapper someone forgot to route through.
      "cliEvaluateExpression"
      "cliParseAndExecuteScript"

      // The rest of the trace surface, read by the `dark traces` commands and by the workbench's traces
      // view. Same reason as `tracesFind` and friends above.
      "tracesClear"
      "tracesClearBefore"
      "tracesDelete"
      "tracesEnabled"
      "tracesGetInput"
      "tracesListByFn"
      "tracesPruneKeep"
      "tracesResolveID"
      "tracesView"

      // Misc.
      "interpreterStatsReset" ]


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


/// Every `.dark` line under packages/ that isn't a comment. Cached.
///
/// Comments have to go before looking for calls, or a docstring explaining that a
/// builtin was DELETED reads as a call to it.
let private packageCodeLines : Lazy<List<string>> =
  lazy
    (let root = Path.Combine(findRepoRoot (), "packages")
     Directory.EnumerateFiles(root, "*.dark", SearchOption.AllDirectories)
     |> Seq.collect File.ReadAllLines
     |> Seq.filter (fun line -> not ((line.TrimStart()).StartsWith "//"))
     |> List.ofSeq)


/// `Builtin.<name>` references that are NOT a builtin call.
///
/// Dark has its own `Builtin` cases and modules, so the text `Builtin.foo` is ambiguous. The
/// `FQValueName.Builtin.` ones are excluded by the lookbehind in the regex; these
/// three are bare and can only be told apart by knowing the file.
let private notActuallyBuiltins : Set<string> =
  Set.ofList
    [ "tokenize" // semanticTokens.dark has its own `Builtin` module
      "toPT" // same, in writtenTypesToProgramTypes.dark
      "fullForReference" // FQValueName.Builtin
      "Json" // `Builtin.Json.*`: a module under the builtin namespace, not a builtin
      "X" ] // `Builtin.X` as prose, inside the for-ai docs


/// Builtins that package code calls and that do not exist.
///
/// Empty, and worth keeping empty: a name here is a feature that throws the moment anyone uses it, so
/// prefer fixing the caller over adding one.
///
/// `testRaiseException` and `testRuntimeError` are deliberately NOT here. They come from `LibTest`, which
/// the test harness loads and the CLI does not, so the one file calling them only runs where they exist.
let private knownMissingOnMainToo : Set<string> = Set.empty


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

/// Every builtin that package code calls actually exists.
///
/// This is the counterpart to the test above, and it exists because we shipped the
/// bug it catches. A missing builtin resolves LAZILY: `reload-packages` prints Done,
/// the whole suite passes, and the error arrives only when a person calls the
/// function. This branch deleted four builtins that had live callers and broke the
/// workbench's item pane and the LSP's file provider, with a green build the whole
/// time.
///
/// Textual rather than resolved, because there is no resolution step to hook: the
/// point is that nothing resolves these until they run.
let everyBuiltinPackagesCallExists =
  testTask "every builtin that package code calls exists" {
    // The test builtin set does NOT include CliHost, so the CLI's own builtins would
    // read as missing. Adding it here rather than allowlisting them: they exist, and
    // a test that calls a real builtin an exception is a test that will one day hide
    // a real one.
    let names (b : RT.Builtins) =
      Set.union
        (b.fns.Values |> Seq.map (fun fn -> fn.name.name) |> Set.ofSeq)
        (b.values.Values |> Seq.map (fun v -> v.name.name) |> Set.ofSeq)

    let defined =
      Set.union
        (names (localBuiltIns PT.PackageManager.empty))
        (names (Builtins.CliHost.Builtin.builtins ()))

    // Lookbehind drops `FQValueName.Builtin.foo` and friends, where `Builtin` is a
    // Dark module rather than the builtin namespace.
    let regex =
      Regex(
        @"(?<![a-zA-Z0-9_.])Builtin\.([a-zA-Z][a-zA-Z0-9_]*)",
        RegexOptions.Compiled
      )

    let referenced =
      packageCodeLines.Value
      |> List.collect (fun line ->
        regex.Matches(line) |> Seq.map (fun m -> m.Groups[1].Value) |> List.ofSeq)
      |> Set.ofList

    let missing =
      referenced
      |> Set.filter (fun name ->
        not (Set.contains name defined)
        && not (Set.contains name notActuallyBuiltins)
        && not (Set.contains name knownMissingOnMainToo))

    if not (Set.isEmpty missing) then
      let names = missing |> Set.toList |> List.sort |> String.concat ", "
      Expect.isTrue
        false
        ($"package code calls builtins that don't exist: {names}\n\n"
         + "A missing builtin resolves lazily, so nothing else in this suite will tell you. Either the "
         + "builtin was deleted and its callers need repointing at the Dark replacement, or it was renamed. "
         + "If it is genuinely missing on main too, add it to `knownMissingOnMainToo` with the reason.")
  }


/// Every CLI command's `help` returns the help TEXT, not an AppState.
///
/// `Registry.CommandHandler.help` is declared `AppState -> String`, and `executeCommandHelp` appends the
/// alias line to whatever it returns. Ten commands printed internally and returned `state` instead, so
/// `dark <cmd> --help` died in `Builtin.stringAppend` for 17 commands, the SCM ones among them. Nothing
/// caught it: a Dark record field is not checked against the function stored in it, and the failure is a
/// runtime type error in a command nobody runs in a test.
///
/// Textual, because there is no resolution step to hook -- the same reason the two builtin tests above
/// are textual.
let everyCliHelpReturnsText =
  testTask "every CLI command's `help` returns String" {
    let root = System.IO.Path.Combine("..", "packages", "darklang", "cli")

    let offenders =
      System.IO.Directory.GetFiles(
        root,
        "*.dark",
        System.IO.SearchOption.AllDirectories
      )
      |> Array.collect (fun path ->
        System.IO.File.ReadAllLines path
        |> Array.mapi (fun i line -> (i + 1, line))
        |> Array.filter (fun (_, line) ->
          let t = line.Trim()
          // `help`, and also `helpShow`/`helpReview`: the registry takes any of them for the
          // `help` slot, so the contract is the same for all of them. Keyed on the AppState
          // parameter, which is what makes it a registry help rather than a local helper that
          // happens to start with the same word.
          t.StartsWith "let help"
          && t.Contains "AppState)"
          && not (t.EndsWith ": String ="))
        |> Array.map (fun (n, line) ->
          $"  {path.Replace('\\', '/')}:{n}  {line.Trim()}"))
      |> List.ofArray

    // The other half of the same contract: a caller that RETURNS `help state` was fine when help
    // returned an AppState and is a type error now. I broke six of these converting the first half,
    // and the sweep that caught it was luck rather than design.
    let badCallers =
      System.IO.Directory.GetFiles(
        root,
        "*.dark",
        System.IO.SearchOption.AllDirectories
      )
      |> Array.collect (fun path ->
        System.IO.File.ReadAllLines path
        |> Array.mapi (fun i line -> (i + 1, line))
        |> Array.filter (fun (_, line) ->
          let t = line.Trim()
          let isBareHelpCall =
            System.Text.RegularExpressions.Regex.IsMatch(
              t,
              @"^(\| .*-> )?help[A-Za-z]* _?state$"
            )
          isBareHelpCall && not (t.Contains "printLine"))
        |> Array.map (fun (n, line) ->
          $"  {path.Replace('\\', '/')}:{n}  {line.Trim()}"))
      |> List.ofArray

    if not (List.isEmpty badCallers) then
      Expect.isTrue
        false
        ("These call sites return a `help` result where an AppState is expected:\n"
         + String.concat "\n" (List.sort badCallers)
         + "\n\n`help` returns the TEXT. A caller that wants to show it and carry on writes "
         + "`Stdlib.printLine (help state)` and then `state`.")

    if not (List.isEmpty offenders) then
      Expect.isTrue
        false
        ("These `help` functions do not return the help text:\n"
         + String.concat "\n" (List.sort offenders)
         + "\n\n`Registry.CommandHandler.help` is `AppState -> String`. Returning `state` and printing "
         + "inside makes `dark <cmd> --help` throw in `Builtin.stringAppend`, because the registry "
         + "appends the alias line to the result. Build the lines and "
         + "`|> Stdlib.String.join \"\\n\"`.")
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
