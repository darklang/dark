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


let oldFunctionsAreDeprecated =
  let builtinToString (name : RT.FQFnName.Builtin) = $"{name.name}_v{name.version}"

  testTask "old functions are deprecated" {
    let mutable counts = Map.empty

    let fns = (localBuiltIns PT.PackageManager.empty).fns.Values |> List.ofSeq

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
// Walk every .dark under packages/ and count textual references to
// `Builtin.<name>` (or `Builtin.<name>_v<digits>`) for every registered
// builtin fn. Anything with >1 textual reference must appear in the
// allowlist below.
//
// A builtin should normally have one package wrapper. The allowlist names
// the cases where direct multi-use is intentional.
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
/// Add a short comment for each group. Keep alphabetical within each group.
///
/// TODO continue routing direct `Builtin.X` callers through stdlib
/// wrappers in batches and shrink this list. Finished: delete unused,
/// int conversions/ops, json/blob/string codecs. Remaining: CLI/IO,
/// Posix, package-manager browsing, traces, streams, and misc runtime
/// entry points. Route each caller through a wrapper unless direct builtin
/// access is required.
let private multiUseAllowlist : Set<string> =
  Set.ofList
    [ // `Stdlib.String.contains` and `Stdlib.String.indexOf` both call the
      // builtin directly. `contains` stays direct because the SQL compiler
      // maps the builtin to SQLite INSTR; the Option-returning wrapper is
      // not queryable.
      "stringIndexOf"

      // Structured parse diagnostics used by CLI package creation,
      // CLI-script parsing, and LSP diagnostics.
      "parserParseDiagnostics"

      // CLI / IO surface called by many CLI commands.
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


/// Concatenate every .dark file under packages/ into one string, minus whole-line
/// comments. Cached.
///
/// The comments go because the count below is textual: naming a builtin in a doc
/// comment, which is a reasonable thing to do next to the one fn that wraps it,
/// otherwise reads as a second caller and fails this test. Only lines that are
/// entirely a comment are dropped, so a `//` inside a string literal can't swallow
/// real code after it on the same line.
let private packagesText : Lazy<string> =
  lazy
    (let root = findPackagesDir ()
     Directory.EnumerateFiles(root, "*.dark", SearchOption.AllDirectories)
     |> Seq.map File.ReadAllText
     |> String.concat "\n"
     |> String.splitOnNewline
     |> List.filter (fun line -> not ((line.TrimStart()).StartsWith "//"))
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
    let fns = (localBuiltIns PT.PackageManager.empty).fns.Values |> List.ofSeq

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
        |> List.map (fun (name, count) -> $"  {name}: {count} refs")
        |> String.concat "\n"
      Expect.isTrue
        false
        ("Some builtins are referenced from more than one place in packages/:\n"
         + lines
         + "\n\nPrefer wrapping the builtin in a single Dark package fn (e.g. a Stdlib/Cli helper) and routing "
         + "all callers through it, so the builtin is referenced once. Only add to `multiUseAllowlist` as a "
         + "last resort, when direct builtin access from several places is genuinely required (e.g. the SQL "
         + "compiler needs the raw builtin). The goal is to shrink that list, not grow it.")
  }


let tests =
  testList "builtin" [ oldFunctionsAreDeprecated; builtinAccessInPackageMatter ]
