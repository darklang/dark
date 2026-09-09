/// Which propagation-policy row covers a location, pinned on the F# side.
///
/// The rule exists in two languages because two different things ask it: the cascade asks per dependent
/// while it rewrites ASTs (`LibDB.Propagation.candidateKeys`), and `dark propagate policy` asks in order
/// to tell a person what is in force (`SCM.Propagation.candidateKeys`, in Dark). If those disagree, the
/// report names a policy the cascade did not apply -- a pin you can see and cannot rely on.
///
/// The Dark copy is pinned by the same table, case for case, in the `CandidateOrder` module of
/// `backend/testfiles/execution/scm/propagation-policy.dark`. **Change one, change both, and the tables
/// must match.** That file also covers what Dark does with the keys once it has them (overrides,
/// scoping, the codec); this covers only the half that has to agree across the boundary.
module Tests.PropagationPolicy

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module Propagation = LibDB.Propagation


let private loc (modules : List<string>) (name : string) : PT.PackageLocation =
  { owner = "Zz"; modules = modules; name = name }


/// (modules, name, the keys that could cover it, most specific first) -- the same rows as the Dark
/// testfile.
let private cases : List<List<string> * string * List<string * string>> =
  [ // The item, then its own module, then each parent outward, then owner-wide.
    ([ "A"; "B"; "C" ],
     "f",
     [ ("A.B.C", "f"); ("A.B.C", ""); ("A.B", ""); ("A", ""); ("", "") ])

    ([ "A" ], "f", [ ("A", "f"); ("A", ""); ("", "") ])

    // A top-level item still gets the owner-wide fallback, and nothing else.
    ([], "f", [ ("", "f"); ("", "") ])

    // A MODULE target (name = "") asks the same question with an empty name, so its first candidate is
    // its own module row rather than an item row that cannot exist. The repeat is not a bug: the item
    // key and the innermost module key coincide, and de-duplicating them would cost a pass over the
    // list to change nothing, since the first hit wins either way.
    ([ "A"; "B" ], "", [ ("A.B", ""); ("A.B", ""); ("A", ""); ("", "") ]) ]


let private agreesWithTheTable =
  test "policy candidate keys match their pinned table" {
    cases
    |> List.iter (fun (modules, name, expected) ->
      let where = (String.concat "." modules) + "." + name

      Expect.equal
        (Propagation.candidateKeys (loc modules name))
        expected
        $"candidates for {where}")
  }


/// The ordering IS the rule: a caller takes the first hit, so a list that is not most-specific-first
/// silently resolves a module-wide pin ahead of the item's own `follow`.
let private mostSpecificComesFirst =
  test "an item's own key precedes every module key that could cover it" {
    let keys = Propagation.candidateKeys (loc [ "A"; "B"; "C" ] "f")
    Expect.equal (List.head keys) (Some(("A.B.C", "f"))) "the item itself is first"

    let moduleKeys = List.tail keys |> List.map fst
    Expect.equal
      moduleKeys
      (List.sortByDescending String.length moduleKeys)
      "module keys run innermost outward"
  }


let tests =
  testList "PropagationPolicy" [ agreesWithTheTable; mostSpecificComesFirst ]
