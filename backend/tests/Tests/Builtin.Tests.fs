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


// -- Accessibility invariant --
//
// Each builtin declares an Accessibility:
//   FromLocation <pkg-fn>: only that wrapper may call it.
//   Any: open — F# host code, debug, or many callers intentionally.
//
// The test below textually scans packages/ for `Builtin.<name>` and
// `Builtin.<name>_v<digits>` references. FromLocation builtins must
// have exactly 1 textual reference. Infix-dispatched builtins
// (`+`, `-`, `==`, etc.) are mapped through operator syntax, not
// `Builtin.X`, so they're allow-listed.

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


let allBuiltinsHaveAccessibilitySet =
  testTask "all builtins have an Accessibility set" {
    let fns = (localBuiltIns PT.PackageManager.empty).fns |> Map.values
    let count = Seq.length fns
    Expect.isGreaterThan count 0 "should have at least one builtin"
    // Field is non-nullable — if it weren't set, the build would fail.
    // This documents that the registry is populated and matchable.
    fns
    |> Seq.iter (fun fn ->
      match fn.accessibility with
      | RT.FromLocation _
      | RT.Any -> ())
  }


let fromLocationBuiltinsAreSinglyReferenced =
  testTask "FromLocation builtins are referenced from exactly 1 wrapper" {
    let fns = (localBuiltIns PT.PackageManager.empty).fns |> Map.values

    let offenders =
      fns
      |> Seq.choose (fun fn ->
        match fn.accessibility with
        | RT.Any -> None
        | RT.FromLocation _ ->
          if Set.contains fn.name.name infixDispatched then
            None // infix-dispatched: counts as ≥1 implicitly
          else
            let count = countReferences fn.name.name
            if count = 1 then None else Some(fn.name.name, count))
      |> List.ofSeq

    if not (List.isEmpty offenders) then
      let lines =
        offenders
        |> List.map (fun (name, count) -> $"  {name}: {count} refs (expected 1)")
        |> String.concat "\n"
      Expect.isTrue
        false
        $"FromLocation builtins must have exactly 1 wrapper:\n{lines}"
  }


let anyBuiltinsWithSingleWrapperReport =
  // Diagnostic, not a fail. Builtins marked Any with exactly 1 textual
  // reference are tightening candidates: the FromLocation invariant
  // would already hold for them. Prints the names directly so they
  // can be scanned without a separate audit script.
  testTask "report Any builtins that look like tightening candidates" {
    let fns = (localBuiltIns PT.PackageManager.empty).fns |> Map.values

    let candidates =
      fns
      |> Seq.choose (fun fn ->
        match fn.accessibility with
        | RT.FromLocation _ -> None
        | RT.Any ->
          if Set.contains fn.name.name infixDispatched then None
          else if countReferences fn.name.name = 1 then Some fn.name.name
          else None)
      |> Seq.toList

    if not (List.isEmpty candidates) then
      let preview = candidates |> List.truncate 10 |> String.concat ", "
      let suffix =
        if List.length candidates > 10 then
          $", … ({List.length candidates - 10} more)"
        else
          ""
      print
        $"[builtin/accessibility] {List.length candidates} `Any` builtins have exactly 1 wrapper — tightening candidates: {preview}{suffix}"
  }


let tests =
  testList
    "builtin"
    [ oldFunctionsAreDeprecated
      allBuiltinsHaveAccessibilitySet
      fromLocationBuiltinsAreSinglyReferenced
      anyBuiltinsWithSingleWrapperReport ]
