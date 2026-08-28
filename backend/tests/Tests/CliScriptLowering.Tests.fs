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
        LibDB.PackageManager.pt.getTypeLocations PT.mainBranchId celsius.hash
        |> Ply.toTask
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
        LibDB.PackageManager.pt.getTypeLocations PT.mainBranchId myErr.hash
        |> Ply.toTask
      let names = locations |> List.map (fun (l : PT.PackageLocation) -> l.name)
      // Same shape as the stdlib `ParseError`s, so the store names this hash.
      Expect.contains names "ParseError" "the stored name is still there"
      Expect.isFalse
        (List.contains "MyErr" names)
        "the script's name does not join the stored ones"
    | _ -> failtest "expected exactly one script type"
  }


/// A script's expressions used to be started as a list, with only the last one awaited, so an error
/// in any earlier one was dropped: the statement ran, its failure vanished, and the script carried on
/// and reported the last expression's success. That is silent, and it hid a broken row in a perf
/// workload for long enough to be worth a test.
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
      Cli.execute state PT.mainBranchId mod' [] Map.empty (Cli.RunScript("t", code))
      |> Ply.toTask

    match result with
    | Ok dval -> failtest $"expected the failure to surface, got %A{dval}"
    | Error _ -> ()
  }


let tests =
  testList
    "CliScriptLowering"
    [ testUnresolvedRefsDoNotCollide
      testIdenticalDeclarationsShareAHash
      testMutuallyRecursiveDeclarations
      testScriptTypeUnifiesWithPackageType
      testDeclarationsAreNameableAfterLowering
      testRegistryDoesNotDisplaceStoredNames
      testMiddleStatementErrorStopsTheScript ]
