module Tests.Propagation

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude

open TestUtils.TestUtils
open TestUtils.PTShortcuts

module PT = LibExecution.ProgramTypes
module Branches = LibPackageManager.Branches
module Inserts = LibPackageManager.Inserts
module Queries = LibPackageManager.Queries
module Propagation = LibPackageManager.Propagation


// ── Helpers ──────────────────────────────────────────────────────────────

let private loc (name : string) : PT.PackageLocation =
  { owner = "Test"; modules = [ "Prop" ]; name = name }

let private makeFn (body : PT.Expr) : PT.PackageFn.PackageFn =
  testPackageFn [] (NEList.singleton "x") PT.TInt64 body

let private callFn (fnId : System.Guid) : PT.Expr =
  eApply (ePackageFn fnId) [] [ eVar "x" ]

let private addFnAt
  (branchId : PT.BranchId)
  (location : PT.PackageLocation)
  (fn : PT.PackageFn.PackageFn)
  : Task<unit> =
  task {
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        branchId
        [ PT.PackageOp.AddFn fn; PT.PackageOp.SetFnName(fn.id, location) ]
    ()
  }

let private setupBranch (name : string) : Task<PT.BranchId> =
  task {
    let! branch = Branches.create name PT.mainBranchId
    return branch.id
  }

let private discardAndDeleteBranch (branchId : PT.BranchId) : Task<unit> =
  task {
    let! _ = Inserts.discardWipOps branchId
    let! _ = Branches.delete branchId
    ()
  }

let private propagateOrFail
  (branchId : PT.BranchId)
  (location : PT.PackageLocation)
  (fromUUIDs : List<uuid>)
  (toUUID : uuid)
  : Task<Propagation.PropagationResult * List<PT.PackageOp>> =
  task {
    let! result =
      Propagation.propagate branchId location PT.ItemKind.Fn fromUUIDs toUUID
    match result with
    | Ok(Some(r, ops)) -> return (r, ops)
    | Ok None -> return failtest "expected dependents, got None"
    | Error e -> return failtest $"propagate failed: {e}"
  }


// ── Tests ────────────────────────────────────────────────────────────────

let testPropagateOps =
  testTask "propagate: produces AddFn + SetFnName + PropagateUpdate" {
    let! branchId = setupBranch "test-prop-ops"

    // let base (x: Int64) : Int64 = x
    let baseFnV1 = makeFn (eVar "x")
    do! addFnAt branchId (loc "base") baseFnV1

    // let caller (x: Int64) : Int64 = base x
    let callerFn = makeFn (callFn baseFnV1.id)
    do! addFnAt branchId (loc "caller") callerFn

    // let base (x: Int64) : Int64 = x + 1
    let baseFnV2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "base") baseFnV2

    let! (_propResult, propOps) =
      propagateOrFail branchId (loc "base") [ baseFnV1.id ] baseFnV2.id

    match propOps with
    | [ PT.PackageOp.AddFn newCallerFn
        PT.PackageOp.SetFnName(nameId, nameLoc)
        PT.PackageOp.PropagateUpdate(_propId,
                                     srcLoc,
                                     _srcKind,
                                     fromUUIDs,
                                     toUUID,
                                     reps) ] ->

      Expect.notEqual baseFnV2.id baseFnV1.id "base v2 gets a fresh uuid"
      Expect.equal
        srcLoc
        (loc "base")
        "the propagation was triggered by a change to base"
      Expect.equal fromUUIDs [ baseFnV1.id ] "the old versions list contains base v1"
      Expect.equal toUUID baseFnV2.id "the new version is base v2"

      Expect.notEqual newCallerFn.id callerFn.id "new caller gets a fresh uuid"
      Expect.equal nameId newCallerFn.id "the name is assigned to the new caller"
      Expect.equal
        nameLoc
        (loc "caller")
        "the new caller has the same location as the old caller"

      let repoint : PT.PropagateRepoint = List.head reps |> Option.get
      Expect.equal
        repoint.location
        (loc "caller")
        "the repoint targets the caller location"
      Expect.equal repoint.fromUUID callerFn.id "the old caller was replaced"
      Expect.equal repoint.toUUID newCallerFn.id "the new caller took its place"

    | _ -> failtest $"unexpected ops: {propOps}"

    do! discardAndDeleteBranch branchId
  }


let testTransitiveOps =
  testTask "transitive: A→B→C, propagating A repoints B and C" {
    let! branchId = setupBranch "test-transitive-ops"

    // let chainA (x: Int64) : Int64 = x
    let fnA = makeFn (eVar "x")
    do! addFnAt branchId (loc "chainA") fnA

    // let chainB (x: Int64) : Int64 = chainA x
    let fnB = makeFn (callFn fnA.id)
    do! addFnAt branchId (loc "chainB") fnB

    // let chainC (x: Int64) : Int64 = chainB x
    let fnC = makeFn (callFn fnB.id)
    do! addFnAt branchId (loc "chainC") fnC

    // let chainA (x: Int64) : Int64 = x + 1
    let fnA2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "chainA") fnA2

    let! (_propResult, propOps) =
      propagateOrFail branchId (loc "chainA") [ fnA.id ] fnA2.id

    // 5 ops: AddFn(B') + SetFnName(B') + AddFn(C') + SetFnName(C') + PropagateUpdate
    Expect.hasLength propOps 5 "5 ops"

    match propOps |> List.last |> Option.get with
    | PT.PackageOp.PropagateUpdate(_, _, _, _, _, reps) ->
      let fromUUIDs =
        reps |> List.map (fun (r : PT.PropagateRepoint) -> r.fromUUID) |> Set.ofList
      Expect.contains fromUUIDs fnB.id "B repointed"
      Expect.contains fromUUIDs fnC.id "C repointed"
    | op -> failtest $"expected PropagateUpdate, got {op}"

    do! discardAndDeleteBranch branchId
  }


let testNoDependents =
  testTask "no dependents: propagate returns None" {
    let! branchId = setupBranch "test-no-deps"

    // let lonely (x: Int64) : Int64 = x
    let v1 = makeFn (eVar "x")
    do! addFnAt branchId (loc "lonely") v1

    // let lonely (x: Int64) : Int64 = x + 1
    let v2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "lonely") v2

    let! result =
      Propagation.propagate branchId (loc "lonely") PT.ItemKind.Fn [ v1.id ] v2.id

    match result with
    | Ok None -> ()
    | _ -> failtest "expected Ok None"

    do! discardAndDeleteBranch branchId
  }


let testCallersOnDifferentVersions =
  testTask "callers on different versions all get repointed" {
    let! branchId = setupBranch "test-multi-ver"

    // mvBase v1, with caller1 referencing it
    // let mvBase (x: Int64) : Int64 = x
    let baseV1 = makeFn (eVar "x")
    do! addFnAt branchId (loc "mvBase") baseV1

    // let mvC1 (x: Int64) : Int64 = mvBase x
    let caller1 = makeFn (callFn baseV1.id)
    do! addFnAt branchId (loc "mvC1") caller1

    // mvBase v2, with caller2 referencing it
    // let mvBase (x: Int64) : Int64 = x + 1
    let baseV2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "mvBase") baseV2

    // let mvC2 (x: Int64) : Int64 = mvBase x
    let caller2 = makeFn (callFn baseV2.id)
    do! addFnAt branchId (loc "mvC2") caller2

    // mvBase v3 — propagation should rewrite both caller1 and caller2
    // let mvBase (x: Int64) : Int64 = x + 2
    let baseV3 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 2L))
    do! addFnAt branchId (loc "mvBase") baseV3

    let! branchChain = Branches.getBranchChain branchId
    let! allUUIDs =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "mvBase" "fn"
    let prevUUIDs = allUUIDs |> List.filter (fun id -> id <> baseV3.id)

    let! (_propResult, propOps) =
      propagateOrFail branchId (loc "mvBase") prevUUIDs baseV3.id

    // both callers get repointed, even though they referenced different versions
    match propOps |> List.last |> Option.get with
    | PT.PackageOp.PropagateUpdate(_, _, _, fromSourceUUIDs, _, reps) ->
      // Verify fromSourceUUIDs stores ALL previous UUIDs
      let fromSourceSet = Set.ofList fromSourceUUIDs
      Expect.contains fromSourceSet baseV1.id "fromSourceUUIDs should contain v1"
      Expect.contains fromSourceSet baseV2.id "fromSourceUUIDs should contain v2"
      Expect.hasLength fromSourceUUIDs 2 "exactly two previous UUIDs stored"

      let repointFromUUIDs =
        reps |> List.map (fun (r : PT.PropagateRepoint) -> r.fromUUID) |> Set.ofList
      Expect.contains
        repointFromUUIDs
        caller1.id
        "caller1 (referencing v1) was repointed"
      Expect.contains
        repointFromUUIDs
        caller2.id
        "caller2 (referencing v2) was repointed"
    | op -> failtest $"expected PropagateUpdate, got {op}"

    do! discardAndDeleteBranch branchId
  }


let private makeType
  (definition : PT.TypeDeclaration.Definition)
  : PT.PackageType.PackageType =
  { id = System.Guid.NewGuid()
    declaration = { typeParams = []; definition = definition }
    description = ""
    deprecated = PT.NotDeprecated }

let private makeValue (body : PT.Expr) : PT.PackageValue.PackageValue =
  { id = System.Guid.NewGuid()
    body = body
    description = ""
    deprecated = PT.NotDeprecated }

let private addTypeAt
  (branchId : PT.BranchId)
  (location : PT.PackageLocation)
  (typ : PT.PackageType.PackageType)
  : Task<unit> =
  task {
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        branchId
        [ PT.PackageOp.AddType typ; PT.PackageOp.SetTypeName(typ.id, location) ]
    ()
  }

let private addValueAt
  (branchId : PT.BranchId)
  (location : PT.PackageLocation)
  (value : PT.PackageValue.PackageValue)
  : Task<unit> =
  task {
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        branchId
        [ PT.PackageOp.AddValue value
          PT.PackageOp.SetValueName(value.id, location) ]
    ()
  }


let private findFn (branchId : PT.BranchId) (location : PT.PackageLocation) =
  task {
    let! chain = Branches.getBranchChain branchId
    let! result = LibPackageManager.ProgramTypes.Fn.find chain location
    return result
  }

let private findType (branchId : PT.BranchId) (location : PT.PackageLocation) =
  task {
    let! chain = Branches.getBranchChain branchId
    let! result = LibPackageManager.ProgramTypes.Type.find chain location
    return result
  }

let private findValue (branchId : PT.BranchId) (location : PT.PackageLocation) =
  task {
    let! chain = Branches.getBranchChain branchId
    let! result = LibPackageManager.ProgramTypes.Value.find chain location
    return result
  }

let private commitAll (branchId : PT.BranchId) (msg : string) =
  task {
    let! result = Inserts.commitWipOps branchId msg
    match result with
    | Ok commitId -> return commitId
    | Error e -> return failtest $"commit failed: {e}"
  }




let testMutualRecursion =
  testTask "mutual recursion: A↔B, updating A creates new versions of both" {
    let! branchId = setupBranch "test-mutual-rec"

    // A calls B
    // B calls A
    // We'll set them up step by step.

    // First create A with a simple body (we'll update it after B exists)
    let fnA1 = makeFn (eVar "x")
    do! addFnAt branchId (loc "mrA") fnA1

    // Create B that calls A
    let fnB1 = makeFn (callFn fnA1.id)
    do! addFnAt branchId (loc "mrB") fnB1

    // Now update A to call B (creating the mutual recursion)
    let fnA2 = makeFn (callFn fnB1.id)
    do! addFnAt branchId (loc "mrA") fnA2

    // Now update A again — propagation should detect that:
    // 1. B depends on old A (fnA2 or fnA1)
    // 2. The new A (fnA3) depends on B (which is being replaced)
    // 3. So A also needs a new UUID (mutual recursion handling)
    let fnA3 =
      makeFn (
        eInfix
          (PT.InfixFnCall PT.ArithmeticPlus)
          (eApply (ePackageFn fnB1.id) [] [ eVar "x" ])
          (eInt64 1L)
      )
    do! addFnAt branchId (loc "mrA") fnA3

    let! branchChain = Branches.getBranchChain branchId
    let! allUUIDs = Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "mrA" "fn"
    let prevUUIDs = allUUIDs |> List.filter (fun id -> id <> fnA3.id)

    let! result =
      Propagation.propagate branchId (loc "mrA") PT.ItemKind.Fn prevUUIDs fnA3.id

    match result with
    | Ok(Some((propResult : Propagation.PropagationResult), propOps)) ->
      // Should have repoints for both A (source, due to mutual recursion) and B
      let repointLocations =
        propResult.repoints
        |> List.map (fun (r : PT.PropagateRepoint) -> r.location)
        |> Set.ofList

      Expect.contains repointLocations (loc "mrB") "B should be repointed"
      Expect.contains
        repointLocations
        (loc "mrA")
        "A should be repointed (mutual recursion)"

      // The final source UUID should be different from fnA3 (new version created)
      let sourceRepoint =
        propResult.repoints
        |> List.find (fun (r : PT.PropagateRepoint) -> r.location = loc "mrA")
        |> Option.get
      Expect.notEqual
        sourceRepoint.toUUID
        fnA3.id
        "source gets a new UUID for mutual recursion"

      // The ops should include AddFn + SetFnName for both B' and A'
      let addFnCount =
        propOps
        |> List.filter (fun op ->
          match op with
          | PT.PackageOp.AddFn _ -> true
          | _ -> false)
        |> List.length
      Expect.equal addFnCount 2 "two AddFn ops (B' and A')"
    | Ok None -> failtest "expected dependents for mutual recursion"
    | Error e -> failtest $"propagation failed: {e}"

    do! discardAndDeleteBranch branchId
  }


let testBranchIsolation =
  testTask "branch isolation: propagation on branch A doesn't affect branch B" {
    let! branchA = setupBranch "test-iso-a"
    let! branchB = setupBranch "test-iso-b"

    // Create fnBase on branch A
    let fnBase = makeFn (eVar "x")
    do! addFnAt branchA (loc "isoBase") fnBase

    // Create callerA on branch A (depends on fnBase)
    let callerA = makeFn (callFn fnBase.id)
    do! addFnAt branchA (loc "isoCallerA") callerA

    // Create callerB on branch B that also references fnBase.id
    // This simulates a cross-branch reference — callerB's location is on branchB
    let callerB = makeFn (callFn fnBase.id)
    do! addFnAt branchB (loc "isoCallerB") callerB

    // Update fnBase on branch A
    let fnBase2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchA (loc "isoBase") fnBase2

    // Propagate on branch A — should only find callerA, not callerB
    let! result =
      Propagation.propagate
        branchA
        (loc "isoBase")
        PT.ItemKind.Fn
        [ fnBase.id ]
        fnBase2.id

    match result with
    | Ok(Some((propResult : Propagation.PropagationResult), _propOps)) ->
      let repointLocs =
        propResult.repoints
        |> List.map (fun (r : PT.PropagateRepoint) -> r.location)
        |> Set.ofList

      Expect.contains repointLocs (loc "isoCallerA") "callerA should be repointed"
      Expect.isFalse
        (Set.contains (loc "isoCallerB") repointLocs)
        "callerB on branch B should NOT be repointed"
    | Ok None -> failtest "expected dependents on branch A"
    | Error e -> failtest $"propagation failed: {e}"

    // Verify callerB on branch B still points to the original function
    let! callerBAfter = findFn branchB (loc "isoCallerB")
    Expect.equal
      callerBAfter
      (Some callerB.id)
      "callerB should be unaffected by branch A propagation"

    do! discardAndDeleteBranch branchA
    do! discardAndDeleteBranch branchB
  }


let testRevertPropagationOp =
  testTask "RevertPropagation op: atomically reverts repoints and restores source" {
    let! branchId = setupBranch "test-revert-prop-op"

    // Create base and caller, then commit
    let fnBase = makeFn (eVar "x")
    do! addFnAt branchId (loc "rpBase") fnBase

    let callerV1 = makeFn (callFn fnBase.id)
    do! addFnAt branchId (loc "rpCaller") callerV1

    let! _ = commitAll branchId "initial base + caller"

    // Record committed caller UUID
    let! committedCaller = findFn branchId (loc "rpCaller")
    let committedCallerId = committedCaller |> Option.get

    // Update base → propagate → caller gets repointed
    let fnBase2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "rpBase") fnBase2

    let! ((propResult : Propagation.PropagationResult), propOps) =
      propagateOrFail branchId (loc "rpBase") [ fnBase.id ] fnBase2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps

    // Verify caller was repointed away from committed version
    let! callerAfterProp = findFn branchId (loc "rpCaller")
    let callerV2Id = callerAfterProp |> Option.get
    Expect.notEqual callerV2Id committedCallerId "caller should be repointed to v2"

    // Now create and apply a RevertPropagation op
    let revertId = System.Guid.NewGuid()

    let revertOp =
      PT.PackageOp.RevertPropagation(
        revertId,
        [ propResult.propagationId ],
        loc "rpBase",
        PT.ItemKind.Fn,
        fnBase.id, // restore to committed base v1
        propResult.repoints // revert all repoints
      )

    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp ]

    // Verify caller is restored to committed version
    let! callerAfterRevert = findFn branchId (loc "rpCaller")
    Expect.equal
      callerAfterRevert
      (Some committedCallerId)
      "caller should be restored to committed v1"

    // Verify base is restored to committed version
    let! baseAfterRevert = findFn branchId (loc "rpBase")
    Expect.equal
      baseAfterRevert
      (Some fnBase.id)
      "base should be restored to committed v1"

    do! discardAndDeleteBranch branchId
  }


let testRevertPropagationMultipleUpdates =
  testTask
    "RevertPropagation: multiple updates produce single active location per path" {
    let! branchId = setupBranch "test-revert-multi-update"

    // Create base and caller, then commit
    let fnBase = makeFn (eVar "x")
    do! addFnAt branchId (loc "rmBase") fnBase

    let callerV1 = makeFn (callFn fnBase.id)
    do! addFnAt branchId (loc "rmCaller") callerV1

    let! _ = commitAll branchId "initial base + caller"

    let! committedCaller = findFn branchId (loc "rmCaller")
    let committedCallerId = committedCaller |> Option.get

    // Update base twice, propagating each time → caller goes b1→b2→b3
    let fnBase2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "rmBase") fnBase2

    let! branchChain = Branches.getBranchChain branchId
    let! allUUIDs1 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "rmBase" "fn"
    let prevUUIDs1 = allUUIDs1 |> List.filter (fun id -> id <> fnBase2.id)
    let! ((propResult1 : Propagation.PropagationResult), propOps1) =
      propagateOrFail branchId (loc "rmBase") prevUUIDs1 fnBase2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps1

    let! callerAfterProp1 = findFn branchId (loc "rmCaller")
    let _callerV2Id = callerAfterProp1 |> Option.get

    let fnBase3 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 2L))
    do! addFnAt branchId (loc "rmBase") fnBase3

    let! allUUIDs2 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "rmBase" "fn"
    let prevUUIDs2 = allUUIDs2 |> List.filter (fun id -> id <> fnBase3.id)
    let! ((propResult2 : Propagation.PropagationResult), propOps2) =
      propagateOrFail branchId (loc "rmBase") prevUUIDs2 fnBase3.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps2

    let! callerAfterProp2 = findFn branchId (loc "rmCaller")
    let callerV3Id = callerAfterProp2 |> Option.get
    Expect.notEqual
      callerV3Id
      committedCallerId
      "caller should be at v3 after two propagations"

    // Collect all repoints from both propagations for the caller location
    let allRepoints = propResult1.repoints @ propResult2.repoints
    let callerRepoints =
      allRepoints
      |> List.filter (fun (r : PT.PropagateRepoint) -> r.location = loc "rmCaller")

    // Consolidate: chain [{b1→b2}, {b2→b3}] → [{b1→b3}]
    let consolidated =
      match callerRepoints with
      | [] -> []
      | _ ->
        let first = List.head callerRepoints |> Option.get
        let last = List.last callerRepoints |> Option.get
        [ { first with PT.PropagateRepoint.toUUID = last.toUUID } ]

    // Create RevertPropagation with consolidated repoints
    let revertId = System.Guid.NewGuid()
    let revertOp =
      PT.PackageOp.RevertPropagation(
        revertId,
        [ propResult1.propagationId; propResult2.propagationId ],
        loc "rmBase",
        PT.ItemKind.Fn,
        fnBase.id,
        consolidated
      )

    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp ]

    // Verify caller is restored to committed version (exactly one active location)
    let! callerAfterRevert = findFn branchId (loc "rmCaller")
    Expect.equal
      callerAfterRevert
      (Some committedCallerId)
      "caller should be restored to committed v1 (single active location)"

    // Verify base is restored to committed version
    let! baseAfterRevert = findFn branchId (loc "rmBase")
    Expect.equal
      baseAfterRevert
      (Some fnBase.id)
      "base should be restored to committed v1"

    do! discardAndDeleteBranch branchId
  }


let testIncrementalUndoRestoresWipLocation =
  testTask
    "incremental undo: RevertPropagation can restore a WIP (non-committed) location" {
    let! branchId = setupBranch "test-incr-wip-restore"

    // Create base and caller, commit
    let fnBase = makeFn (eVar "x")
    do! addFnAt branchId (loc "iwBase") fnBase

    let callerV1 = makeFn (callFn fnBase.id)
    do! addFnAt branchId (loc "iwCaller") callerV1

    let! _ = commitAll branchId "initial"

    // Update base to v2 → propagate → caller becomes callerV2
    let fnBaseV2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "iwBase") fnBaseV2

    let! branchChain = Branches.getBranchChain branchId
    let! allUUIDs1 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "iwBase" "fn"
    let prevUUIDs1 = allUUIDs1 |> List.filter (fun id -> id <> fnBaseV2.id)
    let! ((_propResult1 : Propagation.PropagationResult), propOps1) =
      propagateOrFail branchId (loc "iwBase") prevUUIDs1 fnBaseV2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps1

    let! callerAfterV2 = findFn branchId (loc "iwCaller")
    let callerV2Id = callerAfterV2 |> Option.get

    // Update base to v3 → propagate → caller becomes callerV3
    let fnBaseV3 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 2L))
    do! addFnAt branchId (loc "iwBase") fnBaseV3

    let! allUUIDs2 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "iwBase" "fn"
    let prevUUIDs2 = allUUIDs2 |> List.filter (fun id -> id <> fnBaseV3.id)
    let! ((propResult2 : Propagation.PropagationResult), propOps2) =
      propagateOrFail branchId (loc "iwBase") prevUUIDs2 fnBaseV3.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps2

    // Now undo v3 → v2 (restoring a WIP location, not committed)
    let callerRepoint2 =
      propResult2.repoints
      |> List.find (fun (r : PT.PropagateRepoint) -> r.location = loc "iwCaller")
      |> Option.get

    let revertOp =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult2.propagationId ],
        loc "iwBase",
        PT.ItemKind.Fn,
        fnBaseV2.id, // restore to WIP v2, not committed
        [ callerRepoint2 ]
      )

    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp ]

    // Base should be restored to v2
    let! baseAfterRevert = findFn branchId (loc "iwBase")
    Expect.equal
      baseAfterRevert
      (Some fnBaseV2.id)
      "base should be restored to WIP v2"

    // Caller should be restored to callerV2
    let! callerAfterRevert = findFn branchId (loc "iwCaller")
    Expect.equal
      callerAfterRevert
      (Some callerV2Id)
      "caller should be restored to WIP callerV2"

    do! discardAndDeleteBranch branchId
  }


let testIncrementalUndoFullWalkthrough =
  testTask "incremental undo: two updates, two undos step back through each version" {
    let! branchId = setupBranch "test-incr-full"

    // Create base and caller, commit
    let fnBase = makeFn (eVar "x")
    do! addFnAt branchId (loc "ifBase") fnBase

    let callerV1 = makeFn (callFn fnBase.id)
    do! addFnAt branchId (loc "ifCaller") callerV1

    let! _ = commitAll branchId "initial"

    let! committedCaller = findFn branchId (loc "ifCaller")
    let committedCallerId = committedCaller |> Option.get

    // Update base to v2 → propagate
    let fnBaseV2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "ifBase") fnBaseV2

    let! branchChain = Branches.getBranchChain branchId
    let! allUUIDs1 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "ifBase" "fn"
    let prevUUIDs1 = allUUIDs1 |> List.filter (fun id -> id <> fnBaseV2.id)
    let! ((propResult1 : Propagation.PropagationResult), propOps1) =
      propagateOrFail branchId (loc "ifBase") prevUUIDs1 fnBaseV2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps1

    let! callerAfterV2 = findFn branchId (loc "ifCaller")
    let callerV2Id = callerAfterV2 |> Option.get

    // Update base to v3 → propagate
    let fnBaseV3 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 2L))
    do! addFnAt branchId (loc "ifBase") fnBaseV3

    let! allUUIDs2 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "ifBase" "fn"
    let prevUUIDs2 = allUUIDs2 |> List.filter (fun id -> id <> fnBaseV3.id)
    let! ((propResult2 : Propagation.PropagationResult), propOps2) =
      propagateOrFail branchId (loc "ifBase") prevUUIDs2 fnBaseV3.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps2

    // --- First undo: v3 → v2 ---
    let callerRepoint2 =
      propResult2.repoints
      |> List.find (fun (r : PT.PropagateRepoint) -> r.location = loc "ifCaller")
      |> Option.get

    let revertOp1 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult2.propagationId ],
        loc "ifBase",
        PT.ItemKind.Fn,
        fnBaseV2.id,
        [ callerRepoint2 ]
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp1 ]

    let! baseAfterUndo1 = findFn branchId (loc "ifBase")
    Expect.equal baseAfterUndo1 (Some fnBaseV2.id) "after first undo: base at v2"

    let! callerAfterUndo1 = findFn branchId (loc "ifCaller")
    Expect.equal
      callerAfterUndo1
      (Some callerV2Id)
      "after first undo: caller at callerV2"

    // --- Second undo: v2 → committed ---
    let callerRepoint1 =
      propResult1.repoints
      |> List.find (fun (r : PT.PropagateRepoint) -> r.location = loc "ifCaller")
      |> Option.get

    let revertOp2 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult1.propagationId ],
        loc "ifBase",
        PT.ItemKind.Fn,
        fnBase.id, // committed UUID
        [ callerRepoint1 ]
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp2 ]

    let! baseAfterUndo2 = findFn branchId (loc "ifBase")
    Expect.equal
      baseAfterUndo2
      (Some fnBase.id)
      "after second undo: base at committed"

    let! callerAfterUndo2 = findFn branchId (loc "ifCaller")
    Expect.equal
      callerAfterUndo2
      (Some committedCallerId)
      "after second undo: caller at committed"

    do! discardAndDeleteBranch branchId
  }


let testUndoThenRedo =
  testTask "undo-then-redo: v2→v3→undo→v4→undo goes to v2, skipping v3" {
    let! branchId = setupBranch "test-undo-redo"

    // Create base, commit
    let fnBase = makeFn (eVar "x")
    do! addFnAt branchId (loc "urBase") fnBase

    let! _ = commitAll branchId "initial"

    // Update to v2
    let fnBaseV2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 2L))
    do! addFnAt branchId (loc "urBase") fnBaseV2

    // Update to v3
    let fnBaseV3 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 3L))
    do! addFnAt branchId (loc "urBase") fnBaseV3

    // Undo v3 → v2
    let revertOp1 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [],
        loc "urBase",
        PT.ItemKind.Fn,
        fnBaseV2.id,
        []
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp1 ]

    let! baseAfterUndo1 = findFn branchId (loc "urBase")
    Expect.equal baseAfterUndo1 (Some fnBaseV2.id) "after undo v3: base at v2"

    // Update to v4
    let fnBaseV4 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 4L))
    do! addFnAt branchId (loc "urBase") fnBaseV4

    let! baseAtV4 = findFn branchId (loc "urBase")
    Expect.equal baseAtV4 (Some fnBaseV4.id) "after update: base at v4"

    // Undo v4 → v2 (should skip v3 because we already reverted past it)
    let revertOp2 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [],
        loc "urBase",
        PT.ItemKind.Fn,
        fnBaseV2.id,
        []
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp2 ]

    let! baseAfterUndo2 = findFn branchId (loc "urBase")
    Expect.equal
      baseAfterUndo2
      (Some fnBaseV2.id)
      "after undo v4: base at v2 (not v3)"

    // Undo v2 → committed
    let revertOp3 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [],
        loc "urBase",
        PT.ItemKind.Fn,
        fnBase.id,
        []
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp3 ]

    let! baseAfterUndo3 = findFn branchId (loc "urBase")
    Expect.equal baseAfterUndo3 (Some fnBase.id) "after undo v2: base at committed"

    do! discardAndDeleteBranch branchId
  }


let testMultiCycleUndoRedo =
  testTask
    "multi-cycle: undo→update→undo→update→undo exercises complex version stacks" {
    let! branchId = setupBranch "test-multi-cycle"

    // Create base and caller, commit
    let fnBase = makeFn (eVar "x")
    do! addFnAt branchId (loc "mcBase") fnBase

    let callerV1 = makeFn (callFn fnBase.id)
    do! addFnAt branchId (loc "mcCaller") callerV1

    let! _ = commitAll branchId "initial"

    let! committedCaller = findFn branchId (loc "mcCaller")
    let committedCallerId = committedCaller |> Option.get

    let! branchChain = Branches.getBranchChain branchId

    // --- Update base to v2 → propagate ---
    let fnBaseV2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 2L))
    do! addFnAt branchId (loc "mcBase") fnBaseV2

    let! allUUIDs1 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "mcBase" "fn"
    let prevUUIDs1 = allUUIDs1 |> List.filter (fun id -> id <> fnBaseV2.id)
    let! ((propResult1 : Propagation.PropagationResult), propOps1) =
      propagateOrFail branchId (loc "mcBase") prevUUIDs1 fnBaseV2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps1

    let! callerAfterV2 = findFn branchId (loc "mcCaller")
    let _callerV2Id = callerAfterV2 |> Option.get

    // --- Undo v2 → committed ---
    let callerRepoint1 =
      propResult1.repoints
      |> List.find (fun (r : PT.PropagateRepoint) -> r.location = loc "mcCaller")
      |> Option.get

    let revertOp1 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult1.propagationId ],
        loc "mcBase",
        PT.ItemKind.Fn,
        fnBase.id,
        [ callerRepoint1 ]
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp1 ]

    let! baseAfterUndo1 = findFn branchId (loc "mcBase")
    Expect.equal baseAfterUndo1 (Some fnBase.id) "cycle 1 undo: base at committed"
    let! callerAfterUndo1 = findFn branchId (loc "mcCaller")
    Expect.equal
      callerAfterUndo1
      (Some committedCallerId)
      "cycle 1 undo: caller at committed"

    // --- Update base to v3 → propagate ---
    let fnBaseV3 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 3L))
    do! addFnAt branchId (loc "mcBase") fnBaseV3

    let! allUUIDs2 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "mcBase" "fn"
    let prevUUIDs2 = allUUIDs2 |> List.filter (fun id -> id <> fnBaseV3.id)
    let! ((propResult2 : Propagation.PropagationResult), propOps2) =
      propagateOrFail branchId (loc "mcBase") prevUUIDs2 fnBaseV3.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps2

    let! callerAfterV3 = findFn branchId (loc "mcCaller")
    let callerV3Id = callerAfterV3 |> Option.get
    Expect.notEqual callerV3Id committedCallerId "cycle 2 update: caller repointed"

    // --- Undo v3 → committed ---
    let callerRepoint2 =
      propResult2.repoints
      |> List.find (fun (r : PT.PropagateRepoint) -> r.location = loc "mcCaller")
      |> Option.get

    let revertOp2 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult2.propagationId ],
        loc "mcBase",
        PT.ItemKind.Fn,
        fnBase.id,
        [ callerRepoint2 ]
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp2 ]

    let! baseAfterUndo2 = findFn branchId (loc "mcBase")
    Expect.equal baseAfterUndo2 (Some fnBase.id) "cycle 2 undo: base at committed"
    let! callerAfterUndo2 = findFn branchId (loc "mcCaller")
    Expect.equal
      callerAfterUndo2
      (Some committedCallerId)
      "cycle 2 undo: caller at committed"

    // --- Update base to v4 → propagate ---
    let fnBaseV4 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 4L))
    do! addFnAt branchId (loc "mcBase") fnBaseV4

    let! allUUIDs3 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "mcBase" "fn"
    let prevUUIDs3 = allUUIDs3 |> List.filter (fun id -> id <> fnBaseV4.id)
    let! ((_propResult3 : Propagation.PropagationResult), propOps3) =
      propagateOrFail branchId (loc "mcBase") prevUUIDs3 fnBaseV4.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps3

    let! baseAfterV4 = findFn branchId (loc "mcBase")
    Expect.equal baseAfterV4 (Some fnBaseV4.id) "cycle 3 update: base at v4"
    let! callerAfterV4 = findFn branchId (loc "mcCaller")
    Expect.notEqual
      (callerAfterV4 |> Option.get)
      committedCallerId
      "cycle 3 update: caller repointed again"

    do! discardAndDeleteBranch branchId
  }


let testUndoThenRedoWithDependents =
  testTask
    "undo-then-redo with dependents: v2→v3→undo→v4→undo restores callers correctly" {
    let! branchId = setupBranch "test-undo-redo-deps"

    // Create base and caller, commit
    let fnBase = makeFn (eVar "x")
    do! addFnAt branchId (loc "urdBase") fnBase

    let callerV1 = makeFn (callFn fnBase.id)
    do! addFnAt branchId (loc "urdCaller") callerV1

    let! _ = commitAll branchId "initial"

    let! committedCaller = findFn branchId (loc "urdCaller")
    let committedCallerId = committedCaller |> Option.get

    let! branchChain = Branches.getBranchChain branchId

    // --- Update base to v2 → propagate ---
    let fnBaseV2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 2L))
    do! addFnAt branchId (loc "urdBase") fnBaseV2

    let! allUUIDs1 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "urdBase" "fn"
    let prevUUIDs1 = allUUIDs1 |> List.filter (fun id -> id <> fnBaseV2.id)
    let! ((propResult1 : Propagation.PropagationResult), propOps1) =
      propagateOrFail branchId (loc "urdBase") prevUUIDs1 fnBaseV2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps1

    let! callerAfterV2 = findFn branchId (loc "urdCaller")
    let callerV2Id = callerAfterV2 |> Option.get

    // --- Update base to v3 → propagate ---
    let fnBaseV3 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 3L))
    do! addFnAt branchId (loc "urdBase") fnBaseV3

    let! allUUIDs2 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "urdBase" "fn"
    let prevUUIDs2 = allUUIDs2 |> List.filter (fun id -> id <> fnBaseV3.id)
    let! ((propResult2 : Propagation.PropagationResult), propOps2) =
      propagateOrFail branchId (loc "urdBase") prevUUIDs2 fnBaseV3.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps2

    // --- Undo v3 → v2 ---
    let callerRepoint2 =
      propResult2.repoints
      |> List.find (fun (r : PT.PropagateRepoint) -> r.location = loc "urdCaller")
      |> Option.get

    let revertOp1 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult2.propagationId ],
        loc "urdBase",
        PT.ItemKind.Fn,
        fnBaseV2.id,
        [ callerRepoint2 ]
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp1 ]

    let! baseAfterUndo1 = findFn branchId (loc "urdBase")
    Expect.equal baseAfterUndo1 (Some fnBaseV2.id) "after undo v3: base at v2"
    let! callerAfterUndo1 = findFn branchId (loc "urdCaller")
    Expect.equal
      callerAfterUndo1
      (Some callerV2Id)
      "after undo v3: caller at callerV2"

    // --- Update base to v4 → propagate ---
    let fnBaseV4 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 4L))
    do! addFnAt branchId (loc "urdBase") fnBaseV4

    let! allUUIDs3 =
      Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "urdBase" "fn"
    let prevUUIDs3 = allUUIDs3 |> List.filter (fun id -> id <> fnBaseV4.id)
    let! ((propResult3 : Propagation.PropagationResult), propOps3) =
      propagateOrFail branchId (loc "urdBase") prevUUIDs3 fnBaseV4.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps3

    let! callerAfterV4 = findFn branchId (loc "urdCaller")
    let callerV4Id = callerAfterV4 |> Option.get
    Expect.notEqual callerV4Id callerV2Id "after update v4: caller repointed"

    // --- Undo v4 → v2 (skipping v3) ---
    let callerRepoint3 =
      propResult3.repoints
      |> List.find (fun (r : PT.PropagateRepoint) -> r.location = loc "urdCaller")
      |> Option.get

    let revertOp2 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult3.propagationId ],
        loc "urdBase",
        PT.ItemKind.Fn,
        fnBaseV2.id, // restore to v2, not v3
        [ callerRepoint3 ]
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp2 ]

    let! baseAfterUndo2 = findFn branchId (loc "urdBase")
    Expect.equal
      baseAfterUndo2
      (Some fnBaseV2.id)
      "after undo v4: base at v2 (not v3)"
    let! callerAfterUndo2 = findFn branchId (loc "urdCaller")
    Expect.equal
      callerAfterUndo2
      (Some callerV2Id)
      "after undo v4: caller at callerV2 (not v3)"

    // --- Undo v2 → committed ---
    let callerRepoint1 =
      propResult1.repoints
      |> List.find (fun (r : PT.PropagateRepoint) -> r.location = loc "urdCaller")
      |> Option.get

    let revertOp3 =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult1.propagationId ],
        loc "urdBase",
        PT.ItemKind.Fn,
        fnBase.id,
        [ callerRepoint1 ]
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp3 ]

    let! baseAfterUndo3 = findFn branchId (loc "urdBase")
    Expect.equal baseAfterUndo3 (Some fnBase.id) "after undo v2: base at committed"
    let! callerAfterUndo3 = findFn branchId (loc "urdCaller")
    Expect.equal
      callerAfterUndo3
      (Some committedCallerId)
      "after undo v2: caller at committed"

    do! discardAndDeleteBranch branchId
  }


let testTypePropagation =
  testTask "type propagation: updating a type repoints dependent functions" {
    let! branchId = setupBranch "test-type-prop"

    // Create an enum type: | Active | Inactive
    let typeV1 =
      makeType (
        PT.TypeDeclaration.Enum(
          NEList.ofList
            { name = "Active"; fields = []; description = "" }
            [ { name = "Inactive"; fields = []; description = "" } ]
        )
      )
    do! addTypeAt branchId (loc "myStatus") typeV1

    // Create a fn that references the type via EEnum
    let fnBody = eEnum (PT.FQTypeName.fqPackage typeV1.id) [] "Active" []
    let callerFn = makeFn fnBody
    do! addFnAt branchId (loc "makeStatus") callerFn

    // Update the type: | Active | Inactive | Pending
    let typeV2 =
      makeType (
        PT.TypeDeclaration.Enum(
          NEList.ofList
            { name = "Active"; fields = []; description = "" }
            [ { name = "Inactive"; fields = []; description = "" }
              { name = "Pending"; fields = []; description = "" } ]
        )
      )
    do! addTypeAt branchId (loc "myStatus") typeV2

    // Propagate — callerFn should be repointed
    let! result =
      Propagation.propagate
        branchId
        (loc "myStatus")
        PT.ItemKind.Type
        [ typeV1.id ]
        typeV2.id

    match result with
    | Ok(Some((propResult : Propagation.PropagationResult), propOps)) ->
      let repointLocs =
        propResult.repoints
        |> List.map (fun (r : PT.PropagateRepoint) -> r.location)
        |> Set.ofList
      Expect.contains repointLocs (loc "makeStatus") "callerFn should be repointed"

      // Verify the ops contain AddFn + SetFnName for the new caller
      let addFnCount =
        propOps
        |> List.filter (fun op ->
          match op with
          | PT.PackageOp.AddFn _ -> true
          | _ -> false)
        |> List.length
      Expect.isGreaterThanOrEqual addFnCount 1 "at least one AddFn op"
    | Ok None -> failtest "expected dependents for type propagation"
    | Error e -> failtest $"propagation failed: {e}"

    do! discardAndDeleteBranch branchId
  }


let testValuePropagation =
  testTask "value propagation: updating a value repoints dependent functions" {
    let! branchId = setupBranch "test-value-prop"

    // Create a value: 42L
    let valueV1 = makeValue (eInt64 42L)
    do! addValueAt branchId (loc "myConst") valueV1

    // Create a fn that uses the value
    let callerFn = makeFn (ePackageValue valueV1.id)
    do! addFnAt branchId (loc "useConst") callerFn

    // Update value: 99L
    let valueV2 = makeValue (eInt64 99L)
    do! addValueAt branchId (loc "myConst") valueV2

    // Propagate — callerFn should be repointed
    let! result =
      Propagation.propagate
        branchId
        (loc "myConst")
        PT.ItemKind.Value
        [ valueV1.id ]
        valueV2.id

    match result with
    | Ok(Some((propResult : Propagation.PropagationResult), _propOps)) ->
      let repointLocs =
        propResult.repoints
        |> List.map (fun (r : PT.PropagateRepoint) -> r.location)
        |> Set.ofList
      Expect.contains repointLocs (loc "useConst") "callerFn should be repointed"
    | Ok None -> failtest "expected dependents for value propagation"
    | Error e -> failtest $"propagation failed: {e}"

    do! discardAndDeleteBranch branchId
  }


let testDiamondUndoPropagation =
  testTask "diamond undo: A→B, A→C, D=B+C, update A, propagate, undo restores all" {
    let! branchId = setupBranch "test-diamond-undo"

    // Create A (base)
    let fnA = makeFn (eVar "x")
    do! addFnAt branchId (loc "diaA") fnA

    // Create B (depends on A)
    let fnB = makeFn (callFn fnA.id)
    do! addFnAt branchId (loc "diaB") fnB

    // Create C (depends on A)
    let fnC = makeFn (callFn fnA.id)
    do! addFnAt branchId (loc "diaC") fnC

    // Create D (depends on B and C)
    let fnD =
      makeFn (
        eInfix
          (PT.InfixFnCall PT.ArithmeticPlus)
          (eApply (ePackageFn fnB.id) [] [ eVar "x" ])
          (eApply (ePackageFn fnC.id) [] [ eVar "x" ])
      )
    do! addFnAt branchId (loc "diaD") fnD

    // Commit
    let! _ = commitAll branchId "initial diamond"

    let! committedB = findFn branchId (loc "diaB")
    let committedBId = committedB |> Option.get
    let! committedC = findFn branchId (loc "diaC")
    let committedCId = committedC |> Option.get
    let! committedD = findFn branchId (loc "diaD")
    let committedDId = committedD |> Option.get

    // Update A
    let fnA2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "diaA") fnA2

    // Propagate — should repoint B, C, and D
    let! ((propResult : Propagation.PropagationResult), propOps) =
      propagateOrFail branchId (loc "diaA") [ fnA.id ] fnA2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps

    // Verify all changed
    let! wipB = findFn branchId (loc "diaB")
    Expect.notEqual wipB (Some committedBId) "B should have WIP version"
    let! wipC = findFn branchId (loc "diaC")
    Expect.notEqual wipC (Some committedCId) "C should have WIP version"
    let! wipD = findFn branchId (loc "diaD")
    Expect.notEqual wipD (Some committedDId) "D should have WIP version"

    // Undo: revert all repoints and restore source
    let revertId = System.Guid.NewGuid()
    let revertOp =
      PT.PackageOp.RevertPropagation(
        revertId,
        [ propResult.propagationId ],
        loc "diaA",
        PT.ItemKind.Fn,
        fnA.id,
        propResult.repoints
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp ]

    // Verify all restored to committed versions
    let! restoredA = findFn branchId (loc "diaA")
    Expect.equal restoredA (Some fnA.id) "A should be at committed"
    let! restoredB = findFn branchId (loc "diaB")
    Expect.equal restoredB (Some committedBId) "B should be at committed"
    let! restoredC = findFn branchId (loc "diaC")
    Expect.equal restoredC (Some committedCId) "C should be at committed"
    let! restoredD = findFn branchId (loc "diaD")
    Expect.equal restoredD (Some committedDId) "D should be at committed"

    do! discardAndDeleteBranch branchId
  }


let testDeepTransitiveChain =
  testTask "deep transitive: A→B→C→D→E, update A repoints all 4 dependents" {
    let! branchId = setupBranch "test-deep-chain"

    // Build a 5-deep chain: A → B → C → D → E
    let fnA = makeFn (eVar "x")
    do! addFnAt branchId (loc "deepA") fnA

    let fnB = makeFn (callFn fnA.id)
    do! addFnAt branchId (loc "deepB") fnB

    let fnC = makeFn (callFn fnB.id)
    do! addFnAt branchId (loc "deepC") fnC

    let fnD = makeFn (callFn fnC.id)
    do! addFnAt branchId (loc "deepD") fnD

    let fnE = makeFn (callFn fnD.id)
    do! addFnAt branchId (loc "deepE") fnE

    // Update A
    let fnA2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "deepA") fnA2

    let! (_propResult, propOps) =
      propagateOrFail branchId (loc "deepA") [ fnA.id ] fnA2.id

    // Should have 8 ops: 4x (AddFn + SetFnName) + 1 PropagateUpdate = 9
    // Actually: AddFn(B') + SetFnName(B') + AddFn(C') + SetFnName(C') + ... + PropagateUpdate
    Expect.hasLength
      propOps
      9
      "9 ops: 4 pairs of (AddFn+SetFnName) + PropagateUpdate"

    match propOps |> List.last |> Option.get with
    | PT.PackageOp.PropagateUpdate(_, _, _, _, _, reps) ->
      Expect.hasLength reps 4 "4 repoints (B, C, D, E)"
      let fromUUIDs =
        reps |> List.map (fun (r : PT.PropagateRepoint) -> r.fromUUID) |> Set.ofList
      Expect.contains fromUUIDs fnB.id "B repointed"
      Expect.contains fromUUIDs fnC.id "C repointed"
      Expect.contains fromUUIDs fnD.id "D repointed"
      Expect.contains fromUUIDs fnE.id "E repointed"
    | op -> failtest $"expected PropagateUpdate, got {op}"

    do! discardAndDeleteBranch branchId
  }


let testDiscardAfterPropagate =
  testTask "discard after propagate: restores committed versions" {
    let! branchId = setupBranch "test-discard-prop"

    // Create base and caller, commit
    let fnBase = makeFn (eVar "x")
    do! addFnAt branchId (loc "discBase") fnBase

    let callerV1 = makeFn (callFn fnBase.id)
    do! addFnAt branchId (loc "discCaller") callerV1

    let! _ = commitAll branchId "initial"

    let! committedBase = findFn branchId (loc "discBase")
    let committedBaseId = committedBase |> Option.get
    let! committedCaller = findFn branchId (loc "discCaller")
    let committedCallerId = committedCaller |> Option.get

    // Update base → propagate → caller gets repointed
    let fnBase2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "discBase") fnBase2

    let! (_propResult, propOps) =
      propagateOrFail branchId (loc "discBase") [ fnBase.id ] fnBase2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps

    // Verify both changed
    let! wipBase = findFn branchId (loc "discBase")
    Expect.notEqual wipBase (Some committedBaseId) "base should have WIP version"
    let! wipCaller = findFn branchId (loc "discCaller")
    Expect.notEqual
      wipCaller
      (Some committedCallerId)
      "caller should have WIP version"

    // Discard all WIP
    let! discardResult = Inserts.discardWipOps branchId
    Expect.isOk discardResult "discard should succeed"

    // Verify both restored to committed versions
    let! restoredBase = findFn branchId (loc "discBase")
    Expect.equal
      restoredBase
      (Some committedBaseId)
      "base should be at committed after discard"
    let! restoredCaller = findFn branchId (loc "discCaller")
    Expect.equal
      restoredCaller
      (Some committedCallerId)
      "caller should be at committed after discard"

    do! discardAndDeleteBranch branchId
  }


let testRenameFn =
  testTask "rename fn: standalone SetFnName moves location" {
    let! branchId = setupBranch "test-rename-fn"

    let fn = makeFn (eVar "x")
    do! addFnAt branchId (loc "rnFnOld") fn

    // Standalone rename — no AddFn, just SetFnName
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        branchId
        [ PT.PackageOp.SetFnName(fn.id, loc "rnFnNew") ]

    let! atNew = findFn branchId (loc "rnFnNew")
    Expect.equal atNew (Some fn.id) "fn should be at new location"

    let! atOld = findFn branchId (loc "rnFnOld")
    Expect.equal atOld None "fn should not be at old location"

    do! discardAndDeleteBranch branchId
  }


let testRenameType =
  testTask "rename type: standalone SetTypeName moves location" {
    let! branchId = setupBranch "test-rename-type"

    let typ =
      makeType (
        PT.TypeDeclaration.Enum(
          NEList.ofList
            { name = "A"; fields = []; description = "" }
            [ { name = "B"; fields = []; description = "" } ]
        )
      )
    do! addTypeAt branchId (loc "rnTypeOld") typ

    // Standalone rename
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        branchId
        [ PT.PackageOp.SetTypeName(typ.id, loc "rnTypeNew") ]

    let! atNew = findType branchId (loc "rnTypeNew")
    Expect.equal atNew (Some typ.id) "type should be at new location"

    let! atOld = findType branchId (loc "rnTypeOld")
    Expect.equal atOld None "type should not be at old location"

    do! discardAndDeleteBranch branchId
  }


let testRenameValue =
  testTask "rename value: standalone SetValueName moves location" {
    let! branchId = setupBranch "test-rename-value"

    let value = makeValue (eInt64 42L)
    do! addValueAt branchId (loc "rnValOld") value

    // Standalone rename
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        branchId
        [ PT.PackageOp.SetValueName(value.id, loc "rnValNew") ]

    let! atNew = findValue branchId (loc "rnValNew")
    Expect.equal atNew (Some value.id) "value should be at new location"

    let! atOld = findValue branchId (loc "rnValOld")
    Expect.equal atOld None "value should not be at old location"

    do! discardAndDeleteBranch branchId
  }


let testRenameFnWithDependent =
  testTask "rename fn with dependent: dependents survive rename (UUID unchanged)" {
    let! branchId = setupBranch "test-rename-fn-dep"

    // Create fn A at renBase
    let fnA = makeFn (eVar "x")
    do! addFnAt branchId (loc "renBase") fnA

    // Create fn B that calls A
    let fnB = makeFn (callFn fnA.id)
    do! addFnAt branchId (loc "renCaller") fnB

    // Rename A to renBase2 via standalone SetFnName
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        branchId
        [ PT.PackageOp.SetFnName(fnA.id, loc "renBase2") ]

    // A is at new location
    let! atNew = findFn branchId (loc "renBase2")
    Expect.equal atNew (Some fnA.id) "A should be at renBase2"

    let! atOld = findFn branchId (loc "renBase")
    Expect.equal atOld None "A should not be at renBase"

    // B is still at renCaller with same UUID (references by UUID, not name)
    let! callerResult = findFn branchId (loc "renCaller")
    Expect.equal
      callerResult
      (Some fnB.id)
      "B should still be at renCaller with same UUID"

    do! discardAndDeleteBranch branchId
  }


let testRenameThenUpdateWithPropagation =
  testTask "rename then propagate: propagation works after rename" {
    let! branchId = setupBranch "test-rename-then-prop"

    // Create fn A and fn B (B calls A). Commit.
    let fnA = makeFn (eVar "x")
    do! addFnAt branchId (loc "rtpBase") fnA

    let fnB = makeFn (callFn fnA.id)
    do! addFnAt branchId (loc "rtpCaller") fnB

    let! _ = commitAll branchId "initial"

    // Rename A to rtpBase2
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        branchId
        [ PT.PackageOp.SetFnName(fnA.id, loc "rtpBase2") ]

    // Update A at new location (new UUID A2)
    let fnA2 =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "rtpBase2") fnA2

    // Propagate from old A.id to new A2.id
    let! (_propResult, propOps) =
      propagateOrFail branchId (loc "rtpBase2") [ fnA.id ] fnA2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps

    // B should be repointed (new UUID)
    let! callerResult = findFn branchId (loc "rtpCaller")
    let callerNewId = callerResult |> Option.get
    Expect.notEqual callerNewId fnB.id "B should be repointed after propagation"

    do! discardAndDeleteBranch branchId
  }


let testRenameToOccupiedLocation =
  testTask "rename to occupied location: displaces existing item" {
    let! branchId = setupBranch "test-rename-displace"

    // Create fn A at rdSlot1 and fn B at rdSlot2
    let fnA = makeFn (eVar "x")
    do! addFnAt branchId (loc "rdSlot1") fnA

    let fnB =
      makeFn (eInfix (PT.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1L))
    do! addFnAt branchId (loc "rdSlot2") fnB

    // Rename A to rdSlot2 (displacing B)
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        branchId
        [ PT.PackageOp.SetFnName(fnA.id, loc "rdSlot2") ]

    // A should be at rdSlot2
    let! atSlot2 = findFn branchId (loc "rdSlot2")
    Expect.equal atSlot2 (Some fnA.id) "A should be at rdSlot2"

    // rdSlot1 should be empty (A moved away)
    let! atSlot1 = findFn branchId (loc "rdSlot1")
    Expect.equal atSlot1 None "rdSlot1 should be empty"

    do! discardAndDeleteBranch branchId
  }


let testSelfRecursionPropagation =
  testTask "self-recursion: update recursive fn repoints caller, no infinite loop" {
    let! branchId = setupBranch "test-self-rec"

    // Create F1 (simple body)
    let fnF1 = makeFn (eVar "x")
    do! addFnAt branchId (loc "selfRec") fnF1

    // Create caller C1 that calls F1
    let fnC1 = makeFn (callFn fnF1.id)
    do! addFnAt branchId (loc "selfRecCaller") fnC1

    // Create F2 that calls F1 (simulates self-reference — at parse time,
    // "selfRec" resolves to F1.id, the current UUID at that location)
    let fnF2 = makeFn (callFn fnF1.id)
    do! addFnAt branchId (loc "selfRec") fnF2

    // Propagate — should repoint C1 only, no infinite loop
    let! result =
      Propagation.propagate
        branchId
        (loc "selfRec")
        PT.ItemKind.Fn
        [ fnF1.id ]
        fnF2.id

    match result with
    | Ok(Some((propResult : Propagation.PropagationResult), _propOps)) ->
      let repointLocs =
        propResult.repoints
        |> List.map (fun (r : PT.PropagateRepoint) -> r.location)
        |> Set.ofList

      Expect.contains repointLocs (loc "selfRecCaller") "caller should be repointed"
      Expect.isFalse
        (Set.contains (loc "selfRec") repointLocs)
        "source should NOT be repointed (self-ref is not mutual recursion)"
    | Ok None -> failtest "expected dependents"
    | Error e -> failtest $"propagation failed: {e}"

    do! discardAndDeleteBranch branchId
  }


let testSelfRecursionUndo =
  testTask "self-recursion undo: undo after propagating recursive fn" {
    let! branchId = setupBranch "test-self-rec-undo"

    // Create F1 and caller, commit
    let fnF1 = makeFn (eVar "x")
    do! addFnAt branchId (loc "srBase") fnF1

    let callerV1 = makeFn (callFn fnF1.id)
    do! addFnAt branchId (loc "srCaller") callerV1

    let! _ = commitAll branchId "initial"

    let! committedCaller = findFn branchId (loc "srCaller")
    let committedCallerId = committedCaller |> Option.get

    // Update F1 to F2 (self-referencing) → propagate
    let fnF2 = makeFn (callFn fnF1.id)
    do! addFnAt branchId (loc "srBase") fnF2

    let! ((propResult : Propagation.PropagationResult), propOps) =
      propagateOrFail branchId (loc "srBase") [ fnF1.id ] fnF2.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps

    // Verify caller was repointed
    let! callerAfterProp = findFn branchId (loc "srCaller")
    Expect.notEqual
      callerAfterProp
      (Some committedCallerId)
      "caller should be repointed"

    // Undo via RevertPropagation
    let revertOp =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult.propagationId ],
        loc "srBase",
        PT.ItemKind.Fn,
        fnF1.id,
        propResult.repoints
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp ]

    // Verify caller restored
    let! callerAfterUndo = findFn branchId (loc "srCaller")
    Expect.equal callerAfterUndo (Some committedCallerId) "caller should be restored"

    // Verify source restored
    let! baseAfterUndo = findFn branchId (loc "srBase")
    Expect.equal baseAfterUndo (Some fnF1.id) "base should be restored"

    do! discardAndDeleteBranch branchId
  }


let testMutualRecursionUndo =
  testTask "mutual recursion undo: undo after propagating A↔B restores both" {
    let! branchId = setupBranch "test-mutual-rec-undo"

    // Create A1 (simple), B1 (calls A1)
    let fnA1 = makeFn (eVar "x")
    do! addFnAt branchId (loc "muA") fnA1

    let fnB1 = makeFn (callFn fnA1.id)
    do! addFnAt branchId (loc "muB") fnB1

    // Update A to call B (creating mutual recursion A↔B)
    let fnA2 = makeFn (callFn fnB1.id)
    do! addFnAt branchId (loc "muA") fnA2

    // Commit
    let! _ = commitAll branchId "mutual recursion setup"

    let! committedB = findFn branchId (loc "muB")
    let committedBId = committedB |> Option.get
    let! committedA = findFn branchId (loc "muA")
    let committedAId = committedA |> Option.get

    // Update A to A3
    let fnA3 =
      makeFn (
        eInfix
          (PT.InfixFnCall PT.ArithmeticPlus)
          (eApply (ePackageFn fnB1.id) [] [ eVar "x" ])
          (eInt64 1L)
      )
    do! addFnAt branchId (loc "muA") fnA3

    let! branchChain = Branches.getBranchChain branchId
    let! allUUIDs = Queries.getAllPreviousUUIDs branchChain "Test" "Prop" "muA" "fn"
    let prevUUIDs = allUUIDs |> List.filter (fun id -> id <> fnA3.id)

    let! ((propResult : Propagation.PropagationResult), propOps) =
      propagateOrFail branchId (loc "muA") prevUUIDs fnA3.id
    let! _ = Inserts.insertAndApplyOpsAsWip branchId propOps

    // Verify both A and B changed (mutual recursion → both repointed)
    let! aAfterProp = findFn branchId (loc "muA")
    Expect.notEqual aAfterProp (Some committedAId) "A should have new UUID"
    let! bAfterProp = findFn branchId (loc "muB")
    Expect.notEqual bAfterProp (Some committedBId) "B should have new UUID"

    // Undo via RevertPropagation
    let revertOp =
      PT.PackageOp.RevertPropagation(
        System.Guid.NewGuid(),
        [ propResult.propagationId ],
        loc "muA",
        PT.ItemKind.Fn,
        committedAId,
        propResult.repoints
      )
    let! _ = Inserts.insertAndApplyOpsAsWip branchId [ revertOp ]

    // Verify both restored to committed versions
    let! aAfterUndo = findFn branchId (loc "muA")
    Expect.equal aAfterUndo (Some committedAId) "A should be restored to committed"

    let! bAfterUndo = findFn branchId (loc "muB")
    Expect.equal bAfterUndo (Some committedBId) "B should be restored to committed"

    do! discardAndDeleteBranch branchId
  }


let tests =
  testList
    "Propagation"
    [ testPropagateOps
      testTransitiveOps
      testNoDependents
      testCallersOnDifferentVersions
      testMutualRecursion
      testBranchIsolation
      testRevertPropagationOp
      testRevertPropagationMultipleUpdates
      testIncrementalUndoRestoresWipLocation
      testIncrementalUndoFullWalkthrough
      testUndoThenRedo
      testMultiCycleUndoRedo
      testUndoThenRedoWithDependents
      testTypePropagation
      testValuePropagation
      testDiamondUndoPropagation
      testDeepTransitiveChain
      testDiscardAfterPropagate
      testRenameFn
      testRenameType
      testRenameValue
      testRenameFnWithDependent
      testRenameThenUpdateWithPropagation
      testRenameToOccupiedLocation
      testSelfRecursionPropagation
      testSelfRecursionUndo
      testMutualRecursionUndo ]
