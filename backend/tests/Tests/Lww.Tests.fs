/// The last-writer-wins rule, pinned on the F# side.
///
/// The rule exists in two languages because two different things ask it: the op-fold decides which binding
/// survives (`LibDB.Lww`, called from `PackageOpPlayback`), and conflict recording decides which side to
/// name as the winner (`SCM.Conflicts.incomingWins`, in Dark). If those disagree, a recorded conflict names
/// a winner the fold did not pick, and two instances converge on different content with nothing to say so.
///
/// The Dark copy is pinned by the same table, case for case, in
/// `backend/testfiles/execution/scm/lww.dark`. **Change one, change both, and the tables must
/// match.**
module Tests.Lww

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open TestUtils.TestUtils

module Lww = LibDB.Lww
module PT = LibExecution.ProgramTypes
module Inserts = LibDB.Inserts
module Queries = LibDB.Queries


/// (newTs, newHash, curTs, curHash, incoming wins?) -- the same rows as the Dark testfile.
let private cases : List<string * string * string * string * bool> =
  [ // A later stamp wins, whatever the hashes are.
    ("2026-01-02T00:00:00.000Z", "aaaa", "2026-01-01T00:00:00.000Z", "ffff", true)
    ("2026-01-01T00:00:00.000Z", "ffff", "2026-01-02T00:00:00.000Z", "aaaa", false)

    // An exact tie goes to the HIGHER hash. Portable, so every instance picks the same side.
    ("2026-01-01T00:00:00.000Z", "ffff", "2026-01-01T00:00:00.000Z", "aaaa", true)
    ("2026-01-01T00:00:00.000Z", "aaaa", "2026-01-01T00:00:00.000Z", "ffff", false)

    // A live binding with no stamp cannot defend itself.
    ("2026-01-01T00:00:00.000Z", "aaaa", "", "ffff", true)

    // Stamps are `yyyy-MM-ddTHH:mm:ss.fffZ`, so lexical comparison is already chronological: no parsing,
    // and the millisecond field is what separates two edits in the same second.
    ("2026-01-01T00:00:00.002Z", "aaaa", "2026-01-01T00:00:00.001Z", "ffff", true)
    ("2026-01-01T00:00:59.999Z", "aaaa", "2026-01-01T00:01:00.000Z", "aaaa", false)

    // An INCOMING binding with no stamp loses to anything that has one: the one asymmetry, and the row
    // `conflicts.dark` documents that neither declared table had.
    ("", "ffff", "2026-01-01T00:00:00.000Z", "aaaa", false)

    // The same binding again, stamped the same: nothing to win, so the incoming side loses. The
    // fold keeps what it has and its earliest stamp, and recording never sees this at all, since
    // equal hashes are not a conflict.
    ("2026-01-01T00:00:00.000Z", "aaaa", "2026-01-01T00:00:00.000Z", "aaaa", false) ]


let private agreesWithTheTable =
  test "the LWW rule matches its pinned table" {
    cases
    |> List.iter (fun (newTs, newHash, curTs, curHash, expected) ->
      Expect.equal
        (Lww.incomingWins newTs newHash curTs curHash)
        expected
        $"incoming ({newTs}, {newHash}) vs live ({curTs}, {curHash})")
  }


/// `isStale` is what the fold calls; the table is written from the recording side. Pinning both to the
/// same rows is what stops one drifting while the other keeps the table green. Note that it asserts
/// against the table's own expectation, not against `incomingWins`: comparing the two functions to
/// each other would pass for any pair that agreed, including a pair that agreed on the wrong answer.
let private isStaleAgreesWithTheTable =
  test "isStale answers the table's rows from the fold's side" {
    cases
    |> List.iter (fun (newTs, newHash, curTs, curHash, incomingWins) ->
      if curTs <> "" then
        Expect.equal
          (Lww.isStale newTs newHash curTs curHash)
          (not incomingWins)
          $"fold side of ({newTs}, {newHash}) vs ({curTs}, {curHash})")
  }


/// Whatever the rule is, it has to be a TOTAL order on (stamp, hash) or instances can cycle: A beats B,
/// B beats C, C beats A, and a three-way sync never settles. Checked over every pair in the table.
let private theRuleIsAntisymmetric =
  test "no two distinct bindings both win against each other" {
    // Both columns, so a binding that only ever sits on the live side (`("", "ffff")`) enters the pairs.
    let stamped =
      cases
      |> List.collect (fun (nt, nh, ct, ch, _) -> [ (nt, nh); (ct, ch) ])
      |> List.distinct

    for (tsA, hA) in stamped do
      for (tsB, hB) in stamped do
        if (tsA, hA) <> (tsB, hB) then
          let aBeatsB = Lww.incomingWins tsA hA tsB hB
          let bBeatsA = Lww.incomingWins tsB hB tsA hA
          Expect.notEqual
            aBeatsB
            bBeatsA
            $"({tsA}, {hA}) and ({tsB}, {hB}) must not both win, nor both lose"
  }


/// The Unbind/SetName pair has no hash to tie-break on, so the rule is: ties go to the BINDING.
/// Without one rule used by both sides, the SECOND arrival won a tie and two stores that saw the
/// pair in different orders diverged forever. Both fold sites (`applyUnbind`, and `applySetNameFrom`'s
/// unboundAfter check) route through `unbindBeatsBinding`, so this table pins both.
let private unbindTies =
  test "an unbind beats a binding only with a strictly later stamp" {
    Expect.isTrue
      (Lww.unbindBeatsBinding "2026-01-02T00:00:00.000Z" "2026-01-01T00:00:00.000Z")
      "strictly later unbind wins"
    Expect.isFalse
      (Lww.unbindBeatsBinding "2026-01-01T00:00:00.000Z" "2026-01-01T00:00:00.000Z")
      "an exact tie goes to the binding"
    Expect.isFalse
      (Lww.unbindBeatsBinding "2026-01-01T00:00:00.000Z" "2026-01-02T00:00:00.000Z")
      "an earlier unbind loses"
  }


// ---------------------
// Arrival order
// ---------------------
//
// The rule above decides WHICH statement wins. This decides that the answer does not depend on
// which one showed up first, which is the same rule seen from the fold: two machines holding the
// same ops must agree whatever order they arrived in. Driven at the fold rather than through the
// CLI, because arrival order is the whole point and `insertAndApplyOpsWith` takes the stamp
// function that lets a test choose it.



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

let tests =
  testList
    "Lww"
    [ agreesWithTheTable
      isStaleAgreesWithTheTable
      theRuleIsAntisymmetric
      unbindTies
      deprecationsSettleByWhenTheyWereSaid ]
