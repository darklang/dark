/// Remember which names each caller uses, for each location and content version.
/// Two callers can share the same code while following different names.
/// A branch reads its own changes, and what its ancestors committed, over committed main.
module LibDB.ReferenceNames

open System.Threading.Tasks
open FSharp.Control.Tasks
open Fumble
open Prelude
open LibExecution.ProgramTypes
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module AT = AstTransformer
module BS = LibSerialization.Binary.Serialization

type Names = List<PT.Reference * Option<PT.PackageLocation>>
type State = Map<PT.PackageLocation * PT.Reference, Names>

/// Apply reference-name changes here, using the original operations.
/// If two renames affect different calls in the same caller, keep both changes.
let private apply (state : State) (op : PT.PackageOp) : State =
  match op with
  | PT.PackageOp.Rename(oldLocation, newLocation, renamed, references, _) ->
    references
    |> List.fold
      (fun state snapshot ->
        // Change only references to the old name. Another rename may already
        // have updated other references in this caller; keep those changes.
        let moving = snapshot.location = newLocation && snapshot.target = renamed
        let previous =
          // Bring the declaration's reference names with it. The destination
          // may still have reference names from an item that used to live there.
          if moving then
            Map.tryFind (oldLocation, snapshot.target) state
          else
            Map.tryFind (snapshot.location, snapshot.target) state
        let names =
          match previous with
          | None -> snapshot.names
          | Some names ->
            if List.map fst names <> List.map fst snapshot.names then
              Exception.raiseInternal "Rename snapshot reference mismatch" []
            List.map2
              (fun (reference, current) (_, authored) ->
                reference,
                if
                  reference.kind = renamed.kind
                  && current = Some oldLocation
                  && authored = Some newLocation
                then
                  Some newLocation
                else
                  current)
              names
              snapshot.names
        Map.add (snapshot.location, snapshot.target) names state)
      state
  | PT.PackageOp.Decision(_, location, _, PT.DecisionKind.Restore(_, target, names))
  | PT.PackageOp.SetReferenceNames(location, target, names, _) ->
    Map.add (location, target) names state
  | _ -> state

/// Apply an operation to saved reference names using the same fold as branch reads.
/// Dark's undo history uses this to keep independent renames together.
let applySnapshots (snapshots : List<PT.ReferenceNamesSnapshot>) op =
  let state =
    snapshots
    |> List.map (fun snapshot ->
      (snapshot.location, snapshot.target), snapshot.names)
    |> Map.ofList
  apply state op
  |> Map.toList
  |> List.map (fun ((location, target), names) ->
    { PT.ReferenceNamesSnapshot.location = location
      target = target
      names = names
      restating = None })

// Reuse the result while the ordered operation IDs stay the same.
// Reading the IDs also catches SQL writes and commits made by another process.
let private mainStates =
  System.Collections.Concurrent.ConcurrentDictionary<bool, System.Guid list * State>()
do Caching.register (fun () -> mainStates.Clear())

let private loadAt (branch : PT.BranchId) (before : Option<string>) : Task<State> =
  task {
    let cutoff =
      before |> Option.toList |> List.map (fun stamp -> "before", Sql.string stamp)
    let metadata = "p.op_kind IN ('rename', 'reference-names', 'decision')"
    // Main's ops: its draft too when main is the reader, its committed work otherwise.
    let mainPredicate =
      let visible =
        OpHistory.visibility (if branch.IsMain then "1 = 1" else "0 = 1") before
      // `+` keeps SQLite on the op-kind index: main's ops are nearly the whole log.
      $"{metadata} AND +p.branch_id = @main AND {visible}"
    let parameters = ("main", Sql.string (string PT.BranchId.Main)) :: cutoff
    let! ids =
      Sql.query
        $"SELECT p.id FROM package_ops p WHERE {mainPredicate} ORDER BY p.origin_ts, p.id"
      |> Sql.parameters parameters
      |> Sql.executeAsync (fun r -> r.uuid "id")
    let! mainState =
      match mainStates.TryGetValue branch.IsMain with
      | true, (previous, state) when Option.isNone before && previous = ids ->
        Task.FromResult state
      | _ ->
        task {
          let! rows =
            Sql.query
              $"SELECT p.id, p.op_blob FROM package_ops p WHERE {mainPredicate}
                ORDER BY p.origin_ts, p.id"
            |> Sql.parameters parameters
            |> Sql.executeAsync (fun r ->
              r.uuid "id",
              BS.PT.PackageOp.tryDeserialize (r.uuid "id") (r.bytes "op_blob"))
          let state = rows |> List.choose snd |> List.fold apply Map.empty
          if Option.isNone before then
            mainStates[branch.IsMain] <- (List.map fst rows, state)
          return state
        }
    // A branch's chain over main: its own ops, and what its ancestors below main committed.
    let! overlay =
      if branch.IsMain then
        Task.FromResult []
      else
        let below =
          Layers.chainOf branch
          |> List.filter (fun b -> not b.IsMain)
          |> List.map string
        let visible = OpHistory.visibility "p.branch_id = @start" before
        Sql.query
          $"SELECT p.id, p.op_blob FROM package_ops p
            WHERE {metadata} AND p.branch_id IN (SELECT value FROM json_each(@chain))
              AND {visible}
            ORDER BY p.origin_ts, p.id"
        |> Sql.parameters (
          [ "chain", Sql.string (jsonStrings below)
            "start", Sql.string (string branch) ]
          @ cutoff
        )
        |> Sql.executeAsync (fun r ->
          BS.PT.PackageOp.tryDeserialize (r.uuid "id") (r.bytes "op_blob"))
        |> Task.map (List.choose identity)
    return List.fold apply mainState overlay
  }

let load branch = loadAt branch None

let private patch visit (state : State) location target item =
  match Map.tryFind (location, target) state with
  | None -> item
  | Some names ->
    let mutable remaining = names
    let rename reference _ =
      match reference, remaining with
      | None, _ -> None
      | Some reference, (expected, location) :: rest when expected = reference ->
        remaining <- rest
        location
      | _ ->
        Exception.raiseInternal
          "Reference names do not match declaration content"
          [ "location", location; "target", target ]
    let result = visit { AT.emptyMapping with referenceName = Some rename } item
    if not (List.isEmpty remaining) then
      Exception.raiseInternal
        "Reference names outlast declaration content"
        [ "location", location; "target", target ]
    result

let patchType state loc (t : PT.PackageType.PackageType) =
  patch AT.transformType state loc (PT.PackageType t.hash) t
let patchFn state loc (f : PT.PackageFn.PackageFn) =
  patch AT.transformFn state loc (PT.PackageFn f.hash) f
let patchValue state loc (v : PT.PackageValue.PackageValue) =
  patch AT.transformValue state loc (PT.PackageValue v.hash) v
let patchTrait state loc (t : PT.Trait.Trait) =
  patch AT.transformTrait state loc (PT.PackageTrait t.hash) t
let patchImpl state loc (i : PT.TraitImpl.TraitImpl) =
  patch AT.transformImpl state loc (PT.PackageTraitImpl i.hash) i
let patchTest state loc (t : PT.PackageTest.PackageTest) =
  patch AT.transformTest state loc (PT.PackageTest t.hash) t

let patchResults
  (state : State)
  (r : PT.Search.SearchResults)
  : PT.Search.SearchResults =
  let patch f (i : PT.LocatedItem<'a>) =
    { i with entity = f state i.location i.entity }
  if Map.isEmpty state then
    r
  else
    { r with
        types = List.map (patch patchType) r.types
        fns = List.map (patch patchFn) r.fns
        values = List.map (patch patchValue) r.values
        traits = List.map (patch patchTrait) r.traits
        impls = List.map (patch patchImpl) r.impls
        tests = List.map (patch patchTest) r.tests }

let private storedNames = ReferenceNamesContent.storedNames

/// Use the caller's saved reference names when it has them.
/// Check that they still describe the same references, in the same order.
let internal effectiveNames (state : State) location target original =
  match Map.tryFind (location, target) state with
  | None -> original
  | Some names ->
    if List.map fst names <> List.map fst original then
      Exception.raiseInternal
        "Reference names do not match declaration content"
        [ "location", location; "target", target ]
    names

/// Save each declaration's reference names before the store keeps one body per hash.
/// Authoring, propagation and draft refresh all need this before writing content.
let captureForStorage
  (branch : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageOp>> =
  task {
    let captureDeclaration = ReferenceNamesContent.declaration
    let captured = ops |> List.map (fun op -> op, captureDeclaration op)
    let declarations =
      captured
      |> List.pairwise
      |> List.choose (fun ((declaration, names), (binding, _)) ->
        match PT.tryDeclarationBinding (declaration, binding), names with
        | Some(location, target), Some(_, names) -> Some(location, target, names)
        | _ -> None)
    if List.isEmpty declarations then
      return []
    else
      let! state = load branch
      // The first Add is the blob that insertion keeps, even when several names share it.
      let! canonical =
        captured
        |> List.choose snd
        |> List.distinctBy fst
        |> Ply.List.mapSequentially (fun (target, names) ->
          uply {
            let! stored = storedNames target
            return (target, Option.defaultValue names stored)
          })
        |> Ply.map Map.ofList
        |> Ply.toTask
      let! edits =
        declarations
        |> Ply.List.mapSequentially (fun (location, target, names) ->
          uply {
            match Map.tryFind (location, target) state with
            | Some existing when existing = names -> return []
            | None when Map.tryFind target canonical = Some names -> return []
            | existing ->
              // Keep the original names before the first reference-only edit.
              // A new alias has no earlier version at this location.
              let! keepOriginal =
                if Option.isNone existing then
                  uply {
                    let find =
                      match target.kind with
                      | PT.ItemKind.Type -> LibDB.ProgramTypes.Type.find
                      | PT.ItemKind.Fn -> LibDB.ProgramTypes.Fn.find
                      | PT.ItemKind.Value -> LibDB.ProgramTypes.Value.find
                      | PT.ItemKind.Trait -> LibDB.ProgramTypes.Trait.find
                      | PT.ItemKind.TraitImpl -> LibDB.ProgramTypes.TraitImpl.find
                      | PT.ItemKind.Test -> LibDB.ProgramTypes.Test.find
                    let! found = find branch location
                    let current =
                      found
                      |> Option.map (fun h ->
                        PT.Reference.fromHashAndKind (h, target.kind))
                    if current = Some target then
                      return
                        Map.tryFind target canonical
                        |> Option.map (fun original ->
                          PT.PackageOp.SetReferenceNames(
                            location,
                            target,
                            original,
                            Some(OriginTs.next ())
                          ))
                    else
                      return None
                  }
                else
                  Ply None
              let edit =
                PT.PackageOp.SetReferenceNames(
                  location,
                  target,
                  names,
                  Some(OriginTs.next ())
                )
              return Option.toList keepOriginal @ [ edit ]
          })
        |> Ply.toTask
      return List.concat edits
  }


/// Read the saved reference-name changes for these content versions.
/// Dark combines them with the original names when building snapshots.
let overrides branch (targets : List<PT.Reference>) before =
  task {
    let! state = loadAt branch before
    let wanted = Set.ofList targets
    return
      state
      |> Map.toList
      |> List.choose (fun ((location, target), names) ->
        if Set.contains target wanted then
          Some
            { PT.ReferenceNamesSnapshot.location = location
              target = target
              names = names
              restating = None }
        else
          None)
  }
