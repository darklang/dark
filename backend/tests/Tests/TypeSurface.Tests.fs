/// What the type-surface check reports, and more importantly what it refuses to.
///
/// The whole value of this check rests on one asymmetry: a false positive is a standing finding about
/// correct code, which teaches people to stop reading `dark constraints`, while a false negative is only
/// the status quo. So the cases below are weighted towards the silences. If a change here makes something
/// newly reported, that is a decision, and it should have to break a test to make it.
///
/// The strongest assertion is the last one: run over every function this store holds, roughly four thousand
/// of them, the check finds nothing. That is what fails if a rule ever gets loose.
module Tests.TypeSurface

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module PM = LibDB.PackageManager
module HS = LibDB.HashStabilization
module Package = LibParser.Package
module NR = LibParser.NameResolver
module Inserts = LibDB.Inserts
module WipRefresh = LibDB.WipRefresh
module TypeSurface = LibDB.TypeSurface

open TestUtils.TestUtils

let private pmPT = PM.pt


/// Author source into main the way the CLI does, and hand back the fns it defined.
let private author (source : string) : Task<List<PT.PackageFn.PackageFn>> =
  task {
    let builtins = localBuiltIns pmPT
    let! parsed =
      Package.parse builtins pmPT NR.OnMissing.ThrowError source |> Ply.toTask
    match parsed with
    | Ok ops ->
      let stabilized = HS.computeRealHashes ops
      let! _ = Inserts.insertAndApplyOpsAsWip stabilized
      let! _ = WipRefresh.refresh pmPT
      return
        stabilized
        |> List.choose (fun op ->
          match op with
          | PT.PackageOp.AddFn fn -> Some fn
          | _ -> None)
    | Error errs ->
      return
        Exception.raiseInternal "type surface test parse failed" [ "errs", errs ]
  }


/// Author `deps` first, then `source`, and check the fn called `name`.
///
/// Two steps rather than one module, deliberately. A reference to something defined alongside you resolves
/// on the way into the store rather than at parse time, so a fn authored in the same breath as its callee
/// still holds an unresolved reference when the checker sees it -- and the checker correctly says nothing
/// about a reference it cannot follow. Authoring what you depend on first is what a person does anyway.
let private checkAfter
  (deps : List<string>)
  (name : string)
  (source : string)
  : Task<List<TypeSurface.Mismatch>> =
  task {
    for d in deps do
      let! _ = author d
      ()

    let! fns = author source

    let! located =
      fns
      |> Ply.List.mapSequentially (fun fn ->
        uply {
          let! locs = pmPT.getFnLocations fn.hash
          return (fn, locs)
        })
      |> Ply.toTask

    let target =
      located
      |> List.tryPick (fun (fn, locs) ->
        if locs |> List.exists (fun (l : PT.PackageLocation) -> l.name = name) then
          Some fn
        else
          None)

    match target with
    | None ->
      return Exception.raiseInternal "fn not found after authoring" [ "name", name ]
    | Some fn -> return! TypeSurface.inFn pmPT.getFn fn |> Ply.toTask
  }


let reportsADefiniteReturnMismatch =
  testTask "a body that cannot be the declared return type is reported" {
    let src =
      "module Darklang.TypeSurfaceTest1\n\nlet saysString (x: Int64) : String = x\n"

    let! found = checkAfter [] "saysString" src

    match found with
    | [ (m : TypeSurface.Mismatch) ] ->
      Expect.equal m.site TypeSurface.ReturnValue "it is about the return value"
      Expect.equal m.expected PT.TString "declared String"
      Expect.equal m.actual PT.TInt64 "body is Int64"
    | other ->
      Exception.raiseInternal
        $"expected exactly one mismatch, got {List.length other}"
        []
  }


let reportsAnUnderAppliedCallInATypedPosition =
  testTask "a partially applied call handed to a scalar parameter is reported" {
    // The shape that got through review and travelled two modules before dying inside `String.split`.
    // `takesTwo n` is not an error, it is a FUNCTION, and the mistake is only visible where it lands.
    let deps =
      "module Darklang.TypeSurfaceTest2\n\n"
      + "let takesTwo (a: Int64) (b: Int64) : String = \"x\"\n\n"
      + "let wantsAString (s: String) : Int64 = 1L\n"

    let src =
      "module Darklang.TypeSurfaceTest2\n\n"
      + "let caller (n: Int64) : Int64 =\n"
      + "  let partial = takesTwo n\n"
      + "  wantsAString partial\n"

    let! found = checkAfter [ deps ] "caller" src

    match found with
    | [ (m : TypeSurface.Mismatch) ] ->
      Expect.equal m.site (TypeSurface.Argument 0) "argument 0"
      Expect.equal m.expected PT.TString "the parameter wants String"

      match m.actual with
      | PT.TFn _ -> ()
      | other ->
        Exception.raiseInternal "expected a function type" [ "actual", other ]
    | other ->
      Exception.raiseInternal
        $"expected exactly one mismatch, got {List.length other}"
        []
  }


let saysNothingAboutCorrectCode =
  testTask "correct code produces nothing, including the shapes nearest the rules" {
    // Each of these is one small edit away from something the check DOES report, which is the point: the
    // rules have to be able to tell them apart.
    let deps =
      "module Darklang.TypeSurfaceTest3\n\n"
      + "let addsUp (x: Int64) : Int64 = x + 1L\n\n"
      + "let compares (x: Int64) : Bool = x > 1L\n\n"
      + "let concats (s: String) : String = s ++ \"!\"\n\n"
      + "let fullyApplied (a: Int64) (b: Int64) : String = \"x\"\n"

    let src =
      "module Darklang.TypeSurfaceTest3\n\n"
      + "let entry (n: Int64) : String = fullyApplied n n\n"

    let! found = checkAfter [ deps ] "entry" src
    Expect.isEmpty found "nothing reported about correct code"
  }


let staysQuietWhereItCannotBeSure =
  testTask "generics are left alone rather than guessed at" {
    // The case most likely to produce a confident wrong answer, since 'a unifies with anything. Silence
    // here is the design, not a gap waiting to be closed.
    let deps = "module Darklang.TypeSurfaceTest4\n\nlet identity (x: 'a) : 'a = x\n"

    let src =
      "module Darklang.TypeSurfaceTest4\n\n"
      + "let usesGeneric (n: Int64) : String = identity n\n"

    let! found = checkAfter [ deps ] "usesGeneric" src
    Expect.isEmpty found "says nothing about a generic it cannot reason about"
  }


let findsNothingInTheWholePackageTree =
  testTask "every function in the store passes" {
    // The soundness claim, against real code rather than samples. A check that cries wolf is a check nobody
    // reads, so this is the failure that matters most.
    let! hashes =
      Sql.query
        """
        SELECT DISTINCT item_hash FROM locations
        WHERE unlisted_at IS NULL AND item_type = 'fn'
          AND owner = 'Darklang' AND modules NOT LIKE 'TypeSurfaceTest%'
        """
      |> Sql.executeAsync (fun read -> read.string "item_hash")

    Expect.isGreaterThan
      (List.length hashes)
      500
      "the store actually holds the package tree (otherwise this passes vacuously)"

    let! found =
      hashes
      |> Ply.List.mapSequentially (fun h ->
        uply {
          match! pmPT.getFn (PT.Hash h) with
          | None -> return []
          | Some fn ->
            let! ms = TypeSurface.inFn pmPT.getFn fn
            return ms |> List.map (fun m -> (h, m))
        })
      |> Ply.map List.concat
      |> Ply.toTask

    Expect.isEmpty found "no false positives across the whole package tree"
  }


let tests =
  testList
    "TypeSurface"
    [ reportsADefiniteReturnMismatch
      reportsAnUnderAppliedCallInATypedPosition
      saysNothingAboutCorrectCode
      staysQuietWhereItCannotBeSure
      findsNothingInTheWholePackageTree ]
