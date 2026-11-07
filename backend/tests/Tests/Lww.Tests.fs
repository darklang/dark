/// The last-writer-wins rule, pinned.
///
/// One rule, `LibExecution.Lww`, asked by everything that has to agree: the op-fold decides which binding
/// survives, conflict detection (`LibDB.ScmDetect`) names which side a recorded conflict keeps, and Dark
/// asks through `SCM.Conflicts.beats`. If the fold and the recorder disagreed, a recorded conflict would
/// name a winner the fold did not pick.
///
/// `packages/darklang/tests/scm/lww.dark` states the same table from the Dark side, case for case.
module Tests.Lww

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open TestUtils.TestUtils

module Lww = LibExecution.Lww
module PT = LibExecution.ProgramTypes
module Inserts = LibDB.Inserts
module Queries = LibDB.Queries
module RT = LibExecution.RuntimeTypes


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


/// The rule as Dark reaches it (`SCM.Conflicts.beats`, through `scmLwwBeats`), driven over the SAME
/// rows: what this pins is that the Dark name reaches the one rule with its arguments the right way
/// round.
///
/// `Darklang.` in full because this file is parsed with owner `Tests`, and the error goes IN the
/// failure message because a name-resolution failure names the exact missing module and that is
/// the whole diagnostic.
let private darkAgreesWithTheSameTable =
  testTask "the Dark name answers the same table, row for row" {
    for (newTs, newHash, curTs, curHash, expected) in cases do
      let code =
        $"Darklang.SCM.Conflicts.beats(\"{newTs}\", \"{newHash}\", \"{curTs}\", \"{curHash}\")"

      match! evalDarkExpr code with
      | Ok(RT.DBool actual) ->
        Expect.equal
          actual
          expected
          $"Dark: incoming ({newTs}, {newHash}) vs live ({curTs}, {curHash})"
      | Ok other -> failtest $"expected a Bool from the Dark rule, got: {other}"
      | Error(rte, _) -> failtest $"the Dark call raised: {rte}"
  }


/// A recorded conflict names the winner the fold picks: detection asks the same rule, row for row.
let private detectionNamesTheFoldsWinner =
  test "a detected conflict names the side the rule picks" {
    let binding
      (ts : string)
      (hash : string)
      (previous : Option<string>)
      : LibDB.ScmDetect.Binding =
      { LibDB.ScmDetect.Binding.owner = "Zz"
        modules = "Lww"
        name = "n"
        itemType = "fn"
        hash = hash
        originTs = ts
        author = ""
        previous = previous }
    for (newTs, newHash, curTs, curHash, expected) in cases do
      if newHash <> curHash then
        // No base and both created from nothing: an independent creation, whoever wins.
        let found =
          LibDB.ScmDetect.detect
            "local"
            "incoming"
            [ binding curTs curHash None ]
            [ binding newTs newHash None ]
            []
        match found with
        | [ c ] ->
          Expect.equal
            c.autoResolvedTo
            (if expected then newHash else curHash)
            $"detect: incoming ({newTs}, {newHash}) vs live ({curTs}, {curHash})"
        | other -> failtest $"expected one conflict, got {List.length other}"
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
      Inserts.authorWith
        PT.BranchId.Main
        "op"
        (fun opId ->
          Map.tryFind opId stamps |> Option.defaultValue "2026-01-01T00:00:00.000Z")
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
          PT.PackageOp.Deprecate(target, PT.DeprecationKind.Harmful, "no", None)
        let fine = PT.PackageOp.Undeprecate(target, None)

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
          PT.PackageOp.Deprecate(target2, PT.DeprecationKind.Harmful, "no", None)
        let fine2 = PT.PackageOp.Undeprecate(target2, None)

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

/// Two impls of one trait for one type are chosen by the same rule, over the stamp of the op that
/// added each (`LibExecution.Traits.select`). A pair that carries no stamps at all is the one case
/// that has no winner: it is not a tie, it is an absence of provenance, and the call reports both.
let private winnerAmongRivals =
  test "the newest rival wins, and unstamped rivals have no winner" {
    let a = ("a", "2026-01-01T00:00:00.000Z", "aaaa")
    let b = ("b", "2026-01-02T00:00:00.000Z", "bbbb")
    Expect.equal (Lww.winnerOf [ a; b ]) (Some "b") "the later stamp wins"
    Expect.equal (Lww.winnerOf [ b; a ]) (Some "b") "whatever order they come in"

    let tieLow = ("low", "2026-01-01T00:00:00.000Z", "aaaa")
    let tieHigh = ("high", "2026-01-01T00:00:00.000Z", "ffff")
    Expect.equal
      (Lww.winnerOf [ tieLow; tieHigh ])
      (Some "high")
      "an exact tie goes to the higher hash, as everywhere else"

    Expect.equal
      (Lww.winnerOf [ ("unstamped", "", "aaaa"); b ])
      (Some "b")
      "one with no stamp loses to one with a stamp"

    Expect.equal
      (Lww.winnerOf [ ("x", "", "aaaa"); ("y", "", "ffff") ])
      None
      "nothing orders two unstamped rivals, so neither wins"
  }

// ── ids minted from content ─────────────────────────────────────────────────────────────────────
//
// A conflict id and a commit id are content-derived, so the same divergence or the same commit gets
// the same id on every machine and across versions. The vectors below were minted by the Dark code
// these replaced; an id minted differently would split one conflict into two, or one commit's
// history in half.

let private conflictIdsAreStable =
  test
    "conflict ids: sorted, location-bound, eight hex, and the ones already recorded" {
    let id = LibDB.ScmDetect.conflictId
    Expect.equal
      (id "Zz" "W" "f" [ "aaa"; "bbb" ])
      (id "Zz" "W" "f" [ "bbb"; "aaa" ])
      "the hashes are sorted, so which side was local does not matter"
    Expect.notEqual
      (id "Zz" "W" "f" [ "aaa"; "bbb" ])
      (id "Zz" "W" "g" [ "aaa"; "bbb" ])
      "a different name is a different conflict"
    Expect.equal
      (String.length (id "Zz" "W" "f" [ "aaa"; "bbb" ]))
      8
      "short enough to type"
    Expect.equal
      (id
        "Golden"
        "A"
        "f"
        [ "dcb1afb730ebbf3fdf46b2f63babfd38b7499671bced4cc2112d0ee592247050"
          "cc338361ec7eba6361de1042d216bd9627b7db72a6d0c53d087389150ae0ef8e" ])
      "fddbaf92"
      "an id the Dark detector recorded is minted the same here"
  }

/// A name stored in another Unicode form (here `e` + a combining acute, not the composed `é`) gets the
/// id the Dark detector gave it. Dark built the material with `+`, which NFC-normalises, so the oracle is
/// that same expression evaluated in Dark over a name decoded from raw UTF-8 (a literal would already be
/// normalised by the parser).
let private conflictIdsNormaliseLikeDark =
  testTask "conflict ids for a non-NFC name match what Dark minted" {
    let decomposed = "cafe\u0301"
    Expect.isFalse
      (decomposed.IsNormalized System.Text.NormalizationForm.FormC)
      "the name under test really is not NFC"
    let code =
      "let name = (Stdlib.Utf8.decode((Stdlib.Base16.decode(\"63616665cc81\") |> Builtin.unwrap)) |> Builtin.unwrap)\n"
      + "let material = $\"Zz/W/{name}|\" + (Stdlib.String.join(Stdlib.List.sort([ \"bbb\", \"aaa\" ]), \",\"))\n"
      + "Stdlib.String.toLowercase(Stdlib.String.first(Stdlib.Base16.encode(Stdlib.Crypto.sha256(Stdlib.Utf8.encode(material))), 8))"
    match! evalDarkExpr code with
    | Ok(RT.DString fromDark) ->
      Expect.equal
        (LibDB.ScmDetect.conflictId "Zz" "W" decomposed [ "aaa"; "bbb" ])
        fromDark
        "F# mints the id Dark did"
      Expect.equal
        (LibDB.ScmDetect.conflictId "Zz" "W" decomposed [ "aaa"; "bbb" ])
        (LibDB.ScmDetect.conflictId "Zz" "W" "caf\u00e9" [ "aaa"; "bbb" ])
        "and the two spellings of one name are one conflict"
    | Ok other -> failtest $"expected a String from Dark, got: {other}"
    | Error(rte, _) -> failtest $"the Dark expression raised: {rte}"
  }

let private commitIdsAreStable =
  test
    "commit ids: every input counts, the order is the caller's, and old ids re-derive" {
    let ts = "2026-01-01T00:00:00.000Z"
    let h = LibDB.ScmActions.mintCommitHash
    Expect.equal
      (h "m" ts [ "aabb"; "ccdd" ] "0011")
      (h "m" ts [ "aabb"; "ccdd" ] "0011")
      "same in, same out"
    Expect.equal (String.length (h "m" ts [ "aabb" ] "0011")) 16 "sixteen hex"
    for other in
      [ h "m" ts [ "aabb" ] "0022"
        h "n" ts [ "aabb" ] "0011"
        h "m" "2026-01-01T00:00:00.001Z" [ "aabb" ] "0011"
        h "m" ts [ "aabb"; "ccdd" ] "0011" ] do
      Expect.notEqual
        other
        (h "m" ts [ "aabb" ] "0011")
        "each input moved gives another id"
    Expect.notEqual
      (h "m" ts [ "aabb"; "ccdd" ] "0011")
      (h "m" ts [ "ccdd"; "aabb" ] "0011")
      "not sorted here: callers sort, so a sort that moved in here would show"
    Expect.equal
      (h
        "golden base"
        "2026-11-04T20:10:35.255Z"
        [ "05688f70-c0f0-12cc-f1c2-035b658b6942"
          "37d93c5f-8987-c288-4460-1b79149105a4"
          "528bf3bb-f0c5-dd8d-9ed3-35920bb5ade1"
          "5987b23d-c8b6-95c0-436e-5d04ab133acc"
          "a710664a-8dc4-8cb0-9345-a3564d19cd77"
          "c7b9ab6f-9c0b-d3f9-5680-663e89d8d3ef" ]
        "1a9c847662c364f5")
      "c3a61814e3828ab0"
      "a commit the Dark code minted re-derives to the same id"
  }

let private candidatesReadAsDarkWroteThem =
  test "recorded candidates: the JSON Dark wrote, byte for byte, and back" {
    let stored =
      """[{"author":"main","hash":"baa7","originTs":"2026-11-04T20:10:57.068Z","removed":false,"side":"local","text":""},{"author":"gb1 <&'>","hash":"d52c","originTs":"2026-11-04T20:10:52.126Z","removed":true,"side":"incoming","text":"Café \"x\""}]"""
    let parsed = LibDB.ScmDetect.parseCandidates stored
    Expect.equal (List.length parsed) 2 "both candidates read"
    Expect.equal
      (LibDB.ScmDetect.candidatesJson parsed)
      stored
      "and write back exactly as stored"
    Expect.equal
      (LibDB.ScmDetect.parseCandidates "not json")
      []
      "unparseable reads as none"
  }


let tests =
  testList
    "Lww"
    [ agreesWithTheTable
      darkAgreesWithTheSameTable
      detectionNamesTheFoldsWinner
      conflictIdsAreStable
      conflictIdsNormaliseLikeDark
      commitIdsAreStable
      candidatesReadAsDarkWroteThem
      winnerAmongRivals
      isStaleAgreesWithTheTable
      theRuleIsAntisymmetric
      unbindTies
      deprecationsSettleByWhenTheyWereSaid ]
