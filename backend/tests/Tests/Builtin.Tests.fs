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
module PTParser = LibExecution.ProgramTypesParser
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
/// Source: LibExecution/ProgramTypesToRuntimeTypes.fs InfixFnName.toFnName.
let private infixDispatched : Set<string> =
  Set.ofList
    [ "int64Add"
      "int64Subtract"
      "int64Multiply"
      "int64Mod"
      "int64Power"
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
/// Initial allowlist: harvested from a first run of the test against
/// the current package corpus. These are *not* a curated set —
/// they're the existing multi-use callsites. Promote individual
/// entries to wrappers (and remove from this list) over time.
let private multiUseAllowlist : Set<string> =
  Set.ofList
    [ // Blob/string/json codecs called from many places.
      "blobFromBytes"
      "blobFromString"
      "blobToString"
      "jsonParse"
      "jsonSerialize"
      "stringFromBlobWithReplacement"
      "stringIndexOf"
      "stringJoin"
      "stringToBlob"

      // Int conversions / ops — wrapper-stamping these would explode.
      "int128LessThan"
      "int128Negate"
      "int128ToString"
      "int16LessThan"
      "int16Mod"
      "int16Negate"
      "int16ToString"
      "int32Add"
      "int32GreaterThan"
      "int32LessThan"
      "int32Mod"
      "int32Negate"
      "int32ToString"
      "int64ToFloat"
      "int64ToString"
      "int8LessThan"
      "int8Mod"
      "int8Negate"
      "int8ToString"
      "uint128ToString"
      "uint16ToString"
      "uint32ToString"
      "uint64Add"
      "uint64GreaterThan"
      "uint64LessThan"
      "uint64ToString"
      "uint8ToString"

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
  testList
    "builtin"
    [ oldFunctionsAreDeprecated; builtinAccessInPackageMatter ]
