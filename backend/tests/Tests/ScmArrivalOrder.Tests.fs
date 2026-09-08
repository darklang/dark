/// Two machines holding the same ops must agree, whatever order the ops arrived in.
///
/// Ocean's round two, findings 2 and D. Both projections settled on whichever op landed LAST rather
/// than whichever was SAID last, which is only the same thing on the machine where they were made.
/// Driven at the fold rather than through the CLI, because the whole point is arrival order, and
/// `insertAndApplyOpsWith` takes the stamp function that lets a test choose it.
module Tests.ScmArrivalOrder

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude

open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module Inserts = LibDB.Inserts
module Queries = LibDB.Queries


/// Apply these (op, stamp) pairs in the order given, as though they had arrived that way.
let private applyInOrder (pairs : List<PT.PackageOp * string>) : Task<unit> =
  task {
    let stamps =
      pairs
      |> List.map (fun (op, ts) ->
        (LibSerialization.Hashing.Hashing.computeOpRowId op, ts))
      |> Map.ofList

    let! _ =
      Inserts.insertAndApplyOpsWith
        (fun opId ->
          Map.tryFind opId stamps |> Option.defaultValue "2026-01-01T00:00:00.000Z")
        (fun _ -> None)
        "op"
        (pairs |> List.map fst)

    return ()
  }

let private aFn (n : int64) : PT.PackageFn.PackageFn =
  testPackageFn [] (NEList.singleton "x") PT.TInt64 (PT.EInt64(gid (), n))


let private deprecationsSettleByWhenTheyWereSaid =
  testList
    "deprecations settle by when they were said"
    [ testTask "the newer statement wins, whichever arrived last" {
        let fn = aFn 1L
        let target = PT.Reference.PackageFn fn.hash

        // Said harmful at t2, said fine again at... no: said fine at t1, harmful at t2. The harmful
        // one is newer, so it is the answer either way round.
        let harmful =
          PT.PackageOp.Deprecate(target, PT.DeprecationKind.Harmful, "no")
        let fine = PT.PackageOp.Undeprecate target

        do!
          applyInOrder
            [ (PT.PackageOp.AddFn fn, "2026-01-01T00:00:00.000Z")
              (fine, "2026-01-01T00:00:01.000Z")
              (harmful, "2026-01-01T00:00:02.000Z") ]

        let! inOrder = Queries.getCurrentDeprecation fn.hash PT.ItemKind.Fn
        Expect.isSome inOrder "said harmful last, so it is harmful"

        // The same two ops, reaching a second store the other way round.
        let fn2 = aFn 2L
        let target2 = PT.Reference.PackageFn fn2.hash
        let harmful2 =
          PT.PackageOp.Deprecate(target2, PT.DeprecationKind.Harmful, "no")
        let fine2 = PT.PackageOp.Undeprecate target2

        do!
          applyInOrder
            [ (PT.PackageOp.AddFn fn2, "2026-01-01T00:00:00.000Z")
              (harmful2, "2026-01-01T00:00:02.000Z")
              (fine2, "2026-01-01T00:00:01.000Z") ]

        let! reordered = Queries.getCurrentDeprecation fn2.hash PT.ItemKind.Fn
        Expect.isSome
          reordered
          "and it is still harmful when the older op arrives second"
      } ]


let tests = testList "ScmArrivalOrder" [ deprecationsSettleByWhenTheyWereSaid ]
