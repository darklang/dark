module Tests.Builtin

// Misc tests of Builtin (both LibCloud and LibExecution) that could not be
// tested via LibExecution.tests

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


let oldFunctionsAreDeprecated =
  let builtinToString (name : RT.FQFnName.Builtin) = $"{name.name}_v{name.version}"

  testTask "old functions are deprecated" {
    let mutable counts = Map.empty

    let fns = (localBuiltIns PT.PackageManager.empty).fns |> Map.values

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


// -- Builtin access in package matter --
//
// Walks every .dark under packages/ and counts textual references to
// `Builtin.<name>` (or `Builtin.<name>_v<digits>`) for every registered
// builtin fn. Anything with >1 textual reference must appear in the
// hardcoded allowlist below; otherwise the test fails.
//
// The intent: a builtin should normally have exactly one wrapper
// package fn. Multi-use is a smell — usually it means we forgot to
// route through a wrapper. The allowlist names the cases where
// multi-use is intentional (helpers called from many CLI commands,
// reflection fns the LSP/CLI both pull through, etc.).
//
// Infix-dispatched builtins (`+`, `==`, etc.) are dispatched through
// operator syntax, so they show up as 0 textual `Builtin.X` references
// but are still load-bearing. They go in `infixDispatched`.

/// Builtins called via infix operators rather than `Builtin.X` syntax.
/// Source: LibExecution/ProgramTypesToRuntimeTypes.fs InfixFnName.toFnName
/// for binary ops; LibParser/FSharpToWrittenTypes.fs op_UnaryNegation
/// for `-x`.
let private infixDispatched : Set<string> =
  Set.ofList
    [ "int64Add"
      "int64Subtract"
      "int64Multiply"
      "int64Mod"
      "int64Power"
      "int64Negate"
      "int64GreaterThan"
      "int64GreaterThanOrEqualTo"
      "int64LessThan"
      "int64LessThanOrEqualTo"
      "floatDivide"
      "stringAppend"
      "equals"
      "notEquals" ]


/// Builtins that are intentionally referenced from more than one
/// place in `packages/`. Add a comment per group naming why it's
/// multi-use; if a name shouldn't be here, route the second caller
/// through a wrapper and remove it. Keep alphabetical.
///
/// TODO continue routing direct `Builtin.X` callers through stdlib
/// wrappers in batches and shrink this list. Phase 0 (delete unused),
/// Phase 1 (int conversions/ops), Phase 2 (json + blob + string codecs)
/// landed; remaining batches: Phase 3 CLI/IO surface (`unwrap`, `print*`,
/// `file*`, `stdinRead*`, `directoryCurrent`, `environmentGet`, `debug`,
/// `toRepr`, `timeSleep`, `getCurrentExecutablePath`,
/// `getAllBuiltinFns`); Phase 4 Posix; Phase 5 PM browse + traces;
/// Phase 6 Streams; Phase 7 misc (`httpServerServe`,
/// `interpreterStatsReset`, `cliEvaluateExpression`, `dbListAll`,
/// `depsGetDependents`, `pmScripts*`). For each callsite: either
/// route via a sibling stdlib wrapper, or document why it must stay
/// direct (e.g. `stringIndexOf` is direct in `String.contains` because
/// the `SqlCallback2` SQL-compile path breaks if wrapped in
/// match-on-Option).
let private multiUseAllowlist : Set<string> =
  Set.ofList
    [ // `Stdlib.String.contains` and `Stdlib.String.indexOf` both call the
      // builtin directly. `contains` *must* — the query compiler routes
      // the builtin via SqlCallback2 → SQLite INSTR; routing through the
      // Option-returning wrapper breaks SQL queryability and trips
      // `Stdlib.DB.queryAll` callers (cloud/db.dark line 815+).
      "stringIndexOf"

      // CLI / IO surface — direct calls from many CLI commands.
      "debug"
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

      // Package manager browsing — used by CLI/LSP/agent.
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

      // HTTP server entry — bypassed via `dark serve` wrapper.
      "httpServerServe"

      // Streams (CLI / agent / scripts use them directly).
      "streamClose"
      "streamFilter"
      "streamMap"
      "streamNext"
      "streamToBlob"
      "streamToList"
      "streamUnfold"

      // Trace surface — CLI commands + LSP both read.
      "tracesFind"
      "tracesHotspots"
      "tracesList"
      "tracesStatsByHandler"

      // Misc.
      "interpreterStatsReset" ]


/// Find packages/ by walking up from CWD until we hit one with darklang/.
let private findPackagesDir () : string =
  let rec walk (dir : string) : string option =
    if System.String.IsNullOrEmpty dir then
      None
    else
      let candidate = Path.Combine(dir, "packages", "darklang")
      if Directory.Exists candidate then
        Some(Path.Combine(dir, "packages"))
      else
        walk (Path.GetDirectoryName dir)

  match walk (Directory.GetCurrentDirectory()) with
  | Some d -> d
  | None ->
    Exception.raiseInternal
      "Couldn't find packages/ walking up from CWD"
      [ "cwd", Directory.GetCurrentDirectory() ]


/// Concatenate every .dark file under packages/ into one string. Cached.
let private packagesText : Lazy<string> =
  lazy
    (let root = findPackagesDir ()
     Directory.EnumerateFiles(root, "*.dark", SearchOption.AllDirectories)
     |> Seq.map File.ReadAllText
     |> String.concat "\n")


/// Count textual references to `Builtin.<name>` (or `Builtin.<name>_v<n>`)
/// across packages/. The `(?![a-zA-Z0-9_])` lookahead prevents matching
/// `Builtin.dictGet` against the prefix of `Builtin.dictGetItem`.
let private countReferences (builtinName : string) : int =
  let escaped = Regex.Escape builtinName
  let pattern = $@"Builtin\.{escaped}(?:_v[0-9]+)?(?![a-zA-Z0-9_])"
  let regex = Regex(pattern, RegexOptions.Compiled)
  regex.Matches(packagesText.Value).Count


let builtinAccessInPackageMatter =
  testTask "builtin access in package matter" {
    let fns = (localBuiltIns PT.PackageManager.empty).fns |> Map.values

    let offenders =
      fns
      |> Seq.choose (fun fn ->
        let name = fn.name.name
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
        |> List.map (fun (name, count) ->
          $"  {name}: {count} refs (expected ≤1, or add to multiUseAllowlist)")
        |> String.concat "\n"
      Expect.isTrue
        false
        $"Builtins referenced from >1 place must be in the allowlist:\n{lines}"
  }


let tests =
  testList "builtin" [ oldFunctionsAreDeprecated; builtinAccessInPackageMatter ]
