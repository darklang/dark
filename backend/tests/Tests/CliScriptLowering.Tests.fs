/// Tests for how a CLI script's own declarations are lowered and identified.
///
/// `dark run` / `dark eval` parse a script, lower its declarations to PT, and
/// graft them into the package manager keyed by content hash. Two declarations
/// that hash the same collapse into one, and calls to either reach whichever
/// survived, so the hashes these tests assert on are what decides whether a
/// script's functions call each other correctly.
module Tests.CliScriptLowering

open Expecto
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Cli = Builtins.CliHost.Libs.Cli
module CliScript = Builtins.CliHost.Utils.CliScript

open TestUtils.TestUtils


let private parse (code : string) : Task<CliScript.PTCliScriptModule> =
  task {
    let! state = executionStateFor pmPT false Map.empty
    let! result = Cli.parseCliScript state "Tests" "script" code |> Ply.toTask
    match result with
    | Ok m -> return m
    | Error diags -> return failtest $"Parse failed: %A{diags}"
  }

let private hashes (items : List<PT.Hash>) : List<string> =
  items |> List.map (fun (PT.Hash h) -> h)

/// The hash a function's first parameter is typed against, for checking that a
/// declaration is wired to the type it names.
let private firstParamTypeHash (fn : PT.PackageFn.PackageFn) : string =
  match fn.parameters.head.typ with
  | PT.TCustomType({ resolved = Ok { name = PT.FQTypeName.Package(PT.Hash h) } }, _) ->
    h
  | other -> failtest $"Expected a resolved package type, got %A{other}"


/// A parameter type declared in the same script is unresolved on the first
/// lowering pass, and an unresolved reference serialises without its name. So
/// these two functions are byte-identical at that point, and hashing them there
/// gives one hash for both: the graft keeps one function and `takesA` starts
/// calling `takesB`'s body. Hashing after resolution is what keeps them apart.
let private testUnresolvedRefsDoNotCollide =
  testTask "declarations differing only in an unresolved ref stay distinct" {
    let! (m : CliScript.PTCliScriptModule) =
      parse
        "type TA = { a: String }\n\
         type TB = { b: Int }\n\
         let takesA (r: TA) : Int = 7\n\
         let takesB (r: TB) : Int = 7\n\
         0"

    Expect.hasLength m.types 2 "both types lowered"
    Expect.hasLength m.fns 2 "both fns lowered"

    let fnHashes = m.fns |> List.map (fun f -> f.hash) |> hashes
    Expect.isTrue
      (List.distinct fnHashes |> List.length = 2)
      "the two fns have distinct hashes"

    // Each fn is typed against the type it actually named.
    let typeHashes = m.types |> List.map (fun t -> t.hash) |> hashes |> Set.ofList
    let paramHashes = m.fns |> List.map firstParamTypeHash |> Set.ofList
    Expect.equal paramHashes typeHashes "each fn points at a different script type"
  }

/// The other direction. Declarations that really are identical SHOULD share a
/// hash: content addressing means a name is not part of what a thing is, so
/// writing the same function twice under two names defines it once.
let private testIdenticalDeclarationsShareAHash =
  testTask "structurally identical declarations share one hash" {
    let! (m : CliScript.PTCliScriptModule) =
      parse
        "let alpha (x: Int) : Int = x + 1\n\
         let beta (x: Int) : Int = x + 1\n\
         0"

    Expect.hasLength m.fns 2 "both fns lowered"
    let fnHashes = m.fns |> List.map (fun f -> f.hash) |> hashes
    Expect.isTrue
      (List.distinct fnHashes |> List.length = 1)
      "identical fns collapse to one hash"
  }

/// Hashing after resolution means the reference graph can contain cycles, so the
/// hashes have to be computed per strongly-connected component rather than in
/// plain dependency order.
let private testMutuallyRecursiveDeclarations =
  testTask "mutually recursive fns hash as a batch" {
    let! (m : CliScript.PTCliScriptModule) =
      parse
        "let isEven (n: Int) : Bool = if n == 0 then true else isOdd (n - 1)\n\
         let isOdd (n: Int) : Bool = if n == 0 then false else isEven (n - 1)\n\
         0"

    Expect.hasLength m.fns 2 "both fns lowered"
    let fnHashes = m.fns |> List.map (fun f -> f.hash) |> hashes
    Expect.isTrue
      (List.distinct fnHashes |> List.length = 2)
      "the two fns have distinct hashes"
    Expect.isFalse
      (fnHashes |> List.contains "")
      "neither fn kept the empty placeholder"
  }

/// Content addressing has to reach across the script/package boundary too: a
/// script type with the same shape as a package type IS that type. Identifying
/// script declarations by location instead of content would have cost this.
let private testScriptTypeUnifiesWithPackageType =
  testTask "a script type matching a package type shares its hash" {
    let! (m : CliScript.PTCliScriptModule) = parse "type MyErr = | BadFormat\n0"
    let! (reference : CliScript.PTCliScriptModule) =
      parse
        "let f (e: Darklang.Stdlib.Int.ParseError) : Int = 1\n\
         0"

    match m.types, reference.fns with
    | [ scriptType ], [ fn ] ->
      let (PT.Hash scriptHash) = scriptType.hash
      Expect.equal
        scriptHash
        (firstParamTypeHash fn)
        "script-declared type hashes to the package type it duplicates"
    | _ -> failtest "expected one script type and one reference fn"
  }


/// A runtime error carries content hashes and is rendered by the CLI after the
/// executor that raised it is gone, so the pretty-printer turns a hash back into
/// a name by asking the package manager where that hash is bound. A script's
/// declarations are never in the store, so that lookup used to miss and every
/// such name printed as a 64-character hash.
///
/// Lowering now registers them in `EphemeralPackages`, which `PackageManager.pt`
/// consults ahead of the store. This asserts the lookup, not the rendered
/// string: the string needs CLI dispatch, which lives in `CliTraces.Tests.fs`.
let private testDeclarationsAreNameableAfterLowering =
  testTask "lowered declarations can be resolved back to their names" {
    let! (m : CliScript.PTCliScriptModule) =
      parse "type Celsius = { degrees: Int }\n0"

    match m.types with
    | [ celsius ] ->
      let! locations =
        LibDB.PackageManager.pt.getTypeLocations celsius.hash |> Ply.toTask
      let names = locations |> List.map (fun (l : PT.PackageLocation) -> l.name)
      Expect.contains names "Celsius" "the script's type is reachable by hash"
    | _ -> failtest "expected exactly one script type"
  }


/// The registry is a fallback, never an override.
///
/// Hashes are content addressed, so a script's private name for some shape is
/// also a name for every stored declaration of that shape. `pickLocation` breaks
/// ties by shortest path, and a script's path is one segment, so consulting the
/// registry first would let `type MyErr = | BadFormat` in a throwaway script
/// rename `Stdlib.Int.ParseError` for the rest of the process.
let private testRegistryDoesNotDisplaceStoredNames =
  testTask "a script's name does not displace the store's" {
    let! (m : CliScript.PTCliScriptModule) = parse "type MyErr = | BadFormat\n0"

    match m.types with
    | [ myErr ] ->
      let! locations =
        LibDB.PackageManager.pt.getTypeLocations myErr.hash |> Ply.toTask
      let names = locations |> List.map (fun (l : PT.PackageLocation) -> l.name)
      // Same shape as the stdlib `ParseError`s, so the store names this hash.
      Expect.contains names "ParseError" "the stored name is still there"
      Expect.isFalse
        (List.contains "MyErr" names)
        "the script's name does not join the stored ones"
    | _ -> failtest "expected exactly one script type"
  }


/// Each script expression is awaited as it runs. Awaiting only the last one drops an
/// earlier failure silently -- it hid a broken perf-workload row.
let private testMiddleStatementErrorStopsTheScript =
  testTask "an error in a middle statement ends the script" {
    let code =
      "let boom (xs: List<Int>) : Unit =\n"
      + "  match xs with\n"
      + "  | [] -> ()\n"
      + "boom [ 1, 2 ]\n"
      + "0\n"

    let! mod' = parse code
    let! state = executionStateFor pmPT false Map.empty
    let! result =
      // `execute` takes no branch here: the execution state already carries it.
      Cli.execute state mod' [] Map.empty (Cli.RunScript("t", code)) |> Ply.toTask

    match result with
    | Ok dval -> failtest $"expected the failure to surface, got %A{dval}"
    | Error _ -> ()
  }


/// Script and eval lowering both produce a list of top-level expressions. Every
/// expression before the last must be Unit, just as in a function's body.
let private testTopLevelSequencing =
  [ "script", false; "eval", true ]
  |> List.map (fun (mode, isEval) ->
    let run code =
      task {
        let! state = executionStateFor pmPT false Map.empty
        let! parsed =
          (if isEval then
             Cli.parseCliExpr state code
           else
             Cli.parseCliScript state "Tests" "sequence" code)
          |> Ply.toTask
        let mod' =
          match parsed with
          | Ok mod' -> mod'
          | Error diags -> failtest $"Parse failed: %A{diags}"
        let source =
          if isEval then Cli.EvalExpression code else Cli.RunScript("t", code)
        let! result = Cli.execute state mod' [] Map.empty source |> Ply.toTask
        return result, state.test.sideEffectCount
      }

    let rejects label code expectedEffects =
      testTask label {
        let! result, effects = run code
        match result with
        | Error(error, _) ->
          Expect.equal
            error
            (RT.RuntimeError.Statement(
              RT.RuntimeError.Statements.FirstExpressionMustBeUnit(
                LibExecution.ValueType.unit,
                LibExecution.ValueType.int64,
                RT.DInt64 1L
              )
            ))
            "a non-Unit intermediate uses the ordinary statement error"
        | Ok value -> failtest $"expected a statement error, got %A{value}"
        Expect.equal effects expectedEffects "nothing after the error executes"
      }

    let accepts label code expected =
      testTask label {
        let! result, _ = run code
        Expect.equal
          result
          (Ok expected)
          "the final expression determines the result"
      }

    testList
      mode
      [ rejects "newline sequence rejects a non-Unit intermediate" "1L\n\"done\"" 0
        rejects
          "a non-Unit middle expression stops later effects"
          "Builtin.testIncrementSideEffectCounter ()\n1L\nBuiltin.testIncrementSideEffectCounter \"done\""
          1
        accepts "Unit intermediate" "()\n\"done\"" (RT.DString "done")
        accepts "explicit discard" "let _ = 1L\n\"done\"" (RT.DString "done")
        accepts "non-Unit final expression" "1L" (RT.DInt64 1L)
        accepts "Unit final expression" "()\n()" RT.DUnit ])
  |> testList "top-level sequencing"


/// Script values use the same interpreter as expressions, and are initialized once.
let private scriptValueExecution =
  let accepts name code expected expectedEffects =
    testTask name {
      let! state = executionStateFor pmPT false Map.empty
      let! script = parse code
      let! result =
        Cli.execute state script [] Map.empty (Cli.RunScript("values", code))
        |> Ply.toTask
      Expect.equal result (Ok expected) "computed values reach their callers"
      Expect.equal state.test.sideEffectCount expectedEffects "initializers run once"
    }

  testList
    "script value execution"
    [ accepts "arithmetic" "val n = 1L + 2L\nn + 1L" (RT.DInt64 4L) 0
      accepts
        "lambda and value alias"
        "val inc = fun x -> x + 1L\nval alias = inc\nalias 2L"
        (RT.DInt64 3L)
        0
      accepts
        "interpolation"
        "val text = $\"hello {\"world\"}\"\ntext"
        (RT.DString "hello world")
        0
      accepts
        "forward dependency through a function is evaluated once"
        "val first = later ()\nlet later () : Int64 = second\nval second = Builtin.testIncrementSideEffectCounter 2L\nfirst + second"
        (RT.DInt64 4L)
        1
      accepts
        "submodule values"
        "module Local =\n  val n = 1L + 2L\nLocal.n + 1L"
        (RT.DInt64 4L)
        0
      accepts
        "unused values are initialized"
        "val unused = Builtin.testIncrementSideEffectCounter 2L\n0L"
        (RT.DInt64 0L)
        1
      testTask "a failed initializer stops later effects" {
        let code =
          "val bad = 1L / 0L\nval later = Builtin.testIncrementSideEffectCounter 2L\nBuiltin.testIncrementSideEffectCounter 3L"
        let! state = executionStateFor pmPT false Map.empty
        let! script = parse code
        let! result =
          Cli.execute state script [] Map.empty (Cli.RunScript("values", code))
          |> Ply.toTask
        Expect.isError result "the initializer's error reaches the caller"
        Expect.equal state.test.sideEffectCount 0 "execution stops at the error"
      }
      testTask "cyclic initializers fail without substituting Unit" {
        let code = "val first = second\nval second = first\nfirst"
        let! state = executionStateFor pmPT false Map.empty
        let! script = parse code
        let! result =
          Cli.execute state script [] Map.empty (Cli.RunScript("values", code))
          |> Ply.toTask
        match result with
        | Error(RT.RuntimeError.ValueNotFound _, _) -> ()
        | other -> failtest $"Expected an unavailable cyclic value, got %A{other}"
      }
      testTask "initializers inherit script permissions" {
        let code = "val now = Builtin.timeNowMs ()\nnow"
        let! state = executionStateFor pmPT false Map.empty
        let! script = parse code
        let restricted =
          LibExecution.Execution.restrictRun
            LibExecution.Permissions.Policy.denyAll
            state
        let! result =
          Cli.execute restricted script [] Map.empty (Cli.RunScript("values", code))
          |> Ply.toTask
        Expect.isError result "a value initializer cannot bypass the run policy"
        Expect.isTrue
          (state.deniedRequests
           |> Seq.exists (fun denial ->
             denial.layer = LibExecution.Permissions.Layer.Run))
          "the failure comes from the inherited run policy"
      } ]

let tests =
  testList
    "CliScriptLowering"
    [ scriptValueExecution
      testUnresolvedRefsDoNotCollide
      testIdenticalDeclarationsShareAHash
      testMutuallyRecursiveDeclarations
      testScriptTypeUnifiesWithPackageType
      testDeclarationsAreNameableAfterLowering
      testRegistryDoesNotDisplaceStoredNames
      testMiddleStatementErrorStopsTheScript
      testTopLevelSequencing ]
