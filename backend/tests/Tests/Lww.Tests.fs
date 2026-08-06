/// The last-writer-wins rule, pinned on the F# side.
///
/// The rule exists in two languages because two different things ask it: the op-fold decides which binding
/// survives (`LibDB.Lww`, called from `PackageOpPlayback`), and conflict recording decides which side to
/// name as the winner (`SCM.Conflicts.incomingWins`, in Dark). If those disagree, a recorded conflict names
/// a winner the fold did not pick, and two instances converge on different content with nothing to say so.
///
/// The Dark copy is pinned by the same table, case for case, in
/// `backend/testfiles/execution/scm/lww.dark`. **Change one, change both, and the tables must match.**
module Tests.Lww

open Expecto

open Prelude
open TestUtils.TestUtils

module Lww = LibDB.Lww


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
    ("2026-01-01T00:00:59.999Z", "aaaa", "2026-01-01T00:01:00.000Z", "aaaa", false) ]


let private agreesWithTheTable =
  test "the LWW rule matches its pinned table" {
    cases
    |> List.iter (fun (newTs, newHash, curTs, curHash, expected) ->
      Expect.equal
        (Lww.incomingWins newTs newHash curTs curHash)
        expected
        $"incoming ({newTs}, {newHash}) vs live ({curTs}, {curHash})")
  }


/// `isStale` and `incomingWins` are the same question asked from opposite sides, so outside the
/// no-stamp case they must never agree. Stated as a test because the fold calls one and conflict
/// recording calls the other, and an inverted sign in either is invisible until two machines diverge.
let private theTwoDirectionsAreOpposites =
  test
    "isStale and incomingWins are exact opposites when the live binding has a stamp" {
    cases
    |> List.iter (fun (newTs, newHash, curTs, curHash, _) ->
      if curTs <> "" then
        Expect.notEqual
          (Lww.isStale newTs newHash curTs curHash)
          (Lww.incomingWins newTs newHash curTs curHash)
          $"({newTs}, {newHash}) vs ({curTs}, {curHash})")
  }


/// Whatever the rule is, it has to be a TOTAL order on (stamp, hash) or instances can cycle: A beats B,
/// B beats C, C beats A, and a three-way sync never settles. Checked over every pair in the table.
let private theRuleIsAntisymmetric =
  test "no two distinct bindings both win against each other" {
    let stamped =
      cases |> List.map (fun (ts, h, _, _, _) -> (ts, h)) |> List.distinct

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


let tests =
  testList
    "Lww"
    [ agreesWithTheTable; theTwoDirectionsAreOpposites; theRuleIsAntisymmetric ]
