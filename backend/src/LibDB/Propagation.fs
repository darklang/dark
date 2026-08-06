/// Propagates package item updates to all dependents by creating new versions
/// with updated Hash references.
module LibDB.Propagation

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
open LibSerialization.Hashing

module PMQueries = Queries
module PMTypes = ProgramTypes
module PackageLocation = LibDB.PackageLocation
module AT = LibDB.AstTransformer
module HS = LibDB.HashStabilization


/// What a propagation moved. Reported back so the caller can say so; the state
/// change itself is the accompanying Add + SetName ops, which is all it ever was.
type PropagationResult = { repoints : List<PT.PropagateRepoint> }


/// Does an explicit `pin` cover this location? Resolved most-specific-first: the
/// item, then its module, then each parent module, then owner-wide. Mirrors
/// `Darklang.SCM.Propagation.explicitPolicyFor`.
///
/// Only explicit rows are consulted and the FIRST hit wins whatever it says, so an
/// item marked `follow` inside a module marked `pin` still follows. No row anywhere
/// means follow, which is why an explicit `follow` and silence look identical to the
/// cascade and differ only as an override.
let private isPinned
  (pins : Set<string * string * string>)
  (follows : Set<string * string * string>)
  (loc : PT.PackageLocation)
  : bool =
  let modulesOf (ms : List<string>) = String.concat "." ms

  // innermost module outward: "A.B.C", "A.B", "A", ""
  let moduleChain =
    [ for i in List.length loc.modules .. -1 .. 0 ->
        modulesOf (List.truncate i loc.modules) ]

  let candidates =
    (modulesOf loc.modules, loc.name)
    :: (moduleChain |> List.map (fun m -> (m, "")))

  candidates
  |> List.tryPick (fun (m, n) ->
    let k = (loc.owner, m, n)
    if Set.contains k pins then Some true
    elif Set.contains k follows then Some false
    else None)
  |> Option.defaultValue false


/// Every item that transitively depends on the targets, filtered by FQN at each level so same-hash content
/// at other locations does not enter the cascade.
///
/// <param branchBindings> is the branch's view of where things live, merged OVER main. A branch's items
/// have no `locations` row, so without it a branch-authored dependent never repoints, and a name the branch
/// rebound would resolve to main's version and walk the cascade off the branch.
///
/// Reports the full candidate set. WHICH of them repoint is the user's choice at commit time, not a rule
/// inferred here from ownership or module.
let private discoverDependents
  (pins : Set<string * string * string>)
  (follows : Set<string * string * string>)
  (branchBindings : Map<string, List<PT.ItemKind * PT.PackageLocation>>)
  (sourceLocations : List<PT.PackageLocation>)
  (sourceItemKind : PT.ItemKind)
  (fromSourceHashes : List<Hash>)
  : Task<List<PMQueries.LocationDependent>> =
  task {
    let key (target : PMQueries.LocationTarget) =
      (target.itemKind.toString (), PackageLocation.toFQN target.location)

    let dependentTarget
      (d : PMQueries.LocationDependent)
      : PMQueries.LocationTarget =
      { itemKind = d.itemKind; location = d.itemLocation; hashes = [ d.itemHash ] }

    let rec loop
      (pending : List<PMQueries.LocationTarget>)
      (processed : Set<string * string>)
      (dependentsByLevelRev : List<List<PMQueries.LocationDependent>>)
      =
      task {
        let toProcess =
          pending
          |> List.filter (fun target -> not (Set.contains (key target) processed))

        match toProcess with
        | [] -> return List.concat (List.rev dependentsByLevelRev)
        | _ ->
          let newProcessed =
            toProcess
            |> List.fold (fun acc target -> Set.add (key target) acc) processed

          let! hashes = PMQueries.getDependentHashesByTargets toProcess
          let! mainLocations = PMQueries.getLiveLocationsForHashes hashes

          // The UNION of where the branch says a hash lives and where main says it
          // does, not one or the other. One hash is live at several names whenever
          // two items have identical content -- which is routine, since
          // content-addressing makes `x + 1L` in two modules literally the same item
          // -- and every one of those names is a dependent that has to repoint.
          //
          // Taking the branch's answer INSTEAD of main's dropped main's names on a
          // branch, so a dependent that exists only in main never repointed there;
          // taking main's when the branch had nothing did the reverse. On a branch a
          // repoint of a main name is a branch-local one (it lands as a branch op),
          // which is exactly what a branch should do with a name it changed the
          // meaning of.
          let batchDependents =
            hashes
            |> List.collect (fun h ->
              let fromBranch =
                Map.tryFind h branchBindings |> Option.defaultValue []
              let fromMain = Map.tryFind h mainLocations |> Option.defaultValue []

              // Branch first so it wins the dedup below for a location both describe.
              let resolved = (fromBranch @ fromMain) |> List.distinctBy snd
              resolved
              |> List.map (fun (kind, loc) ->
                ({ itemHash = Hash h; itemKind = kind; itemLocation = loc }
                : PMQueries.LocationDependent)))

          let unseen =
            batchDependents
            |> List.filter (fun d ->
              not (Set.contains (key (dependentTarget d)) newProcessed))
            // A pinned dependent doesn't repoint -- and because `newPending` comes
            // from this list, the cascade also stops THERE rather than stepping over
            // it. That's the right shape: a pin means this item keeps calling the
            // old version, so nothing above it sees a change either.
            |> List.filter (fun d -> not (isPinned pins follows d.itemLocation))
            |> List.distinctBy (fun d -> key (dependentTarget d))

          let newPending = unseen |> List.map dependentTarget

          return! loop newPending newProcessed (unseen :: dependentsByLevelRev)
      }

    return!
      loop
        (sourceLocations
         |> List.map (fun sourceLocation ->
           { itemKind = sourceItemKind
             location = sourceLocation
             hashes = fromSourceHashes }))
        Set.empty
        []
  }


/// One affected item, fetched from the PM, ready to feed into SCC.
type private Affected =
  | AffectedType of
    fqn : string *
    item : PT.PackageType.PackageType *
    currentHash : Hash *
    location : PT.PackageLocation
  | AffectedFn of
    fqn : string *
    item : PT.PackageFn.PackageFn *
    currentHash : Hash *
    location : PT.PackageLocation
  | AffectedValue of
    fqn : string *
    item : PT.PackageValue.PackageValue *
    currentHash : Hash *
    location : PT.PackageLocation

let private affectedFqn =
  function
  | AffectedType(fqn, _, _, _) -> fqn
  | AffectedFn(fqn, _, _, _) -> fqn
  | AffectedValue(fqn, _, _, _) -> fqn

let private affectedCurrentHash =
  function
  | AffectedType(_, _, currentHash, _) -> currentHash
  | AffectedFn(_, _, currentHash, _) -> currentHash
  | AffectedValue(_, _, currentHash, _) -> currentHash


/// Resolve an item's authoritative hash from its location. The caller's
/// `toSourceHash` is sometimes a parser-time placeholder (e.g. empty on a
/// fresh `val ... = ...` from the CLI); the location row is the source of
/// truth post-WipRefresh.
let private resolveCurrentHash
  (branch : PT.BranchId)
  (loc : PT.PackageLocation)
  (kind : PT.ItemKind)
  (fallback : Hash)
  : Task<Hash> =
  task {
    // The BRANCH's binding first, then main's. A branch's `SetName`s never fold into
    // `locations`, so main's row for a branch-edited item is the pre-branch version,
    // and for an item the branch introduced there is no row at all.
    //
    // Read from the branch's delta ops rather than a branch package manager because
    // `PackageManager` compiles after this file. Last binding wins: the ops arrive
    // oldest-first, same as the overlay's own rule, and `Resolve` counts as a
    // binding for the same reason `SCM.PackageOps.bindingFromOp` says it does -- a
    // rebind is a `Resolve`, and missing it would resolve to a superseded version.
    let! fromBranch =
      if branch.IsMain then
        Task.FromResult None
      else
        task {
          let! ops = Branches.chainOverlayOps branch

          return
            ops
            |> List.fold
              (fun acc op ->
                match op with
                | PT.PackageOp.SetName(l, target, _) when l = loc ->
                  Some target.hash
                | PT.PackageOp.Decision(_, l, _, PT.DecisionKind.Override target) when
                  l = loc
                  ->
                  Some target.hash
                | _ -> acc)
              None
        }

    match fromBranch with
    | Some h -> return h
    | None ->
      let find =
        match kind with
        | PT.ItemKind.Type -> PMTypes.Type.find loc
        | PT.ItemKind.Fn -> PMTypes.Fn.find loc
        | PT.ItemKind.Value -> PMTypes.Value.find loc
      let! resolved = Ply.toTask find
      return resolved |> Option.defaultValue fallback
  }


/// Fetch one item from the PM by hash + kind, tagged with FQN/location.
let private fetchAffected
  (fqn : string)
  (loc : PT.PackageLocation)
  (kind : PT.ItemKind)
  (hash : Hash)
  : Task<Result<Affected, string>> =
  task {
    match kind with
    | PT.ItemKind.Type ->
      let! item = Ply.toTask (PMTypes.Type.get hash)
      match item with
      | Some t -> return Ok(AffectedType(fqn, t, hash, loc))
      | None -> return Error $"Type at {hash} not found"
    | PT.ItemKind.Fn ->
      let! item = Ply.toTask (PMTypes.Fn.get hash)
      match item with
      | Some f -> return Ok(AffectedFn(fqn, f, hash, loc))
      | None -> return Error $"Fn at {hash} not found"
    | PT.ItemKind.Value ->
      let! item = Ply.toTask (PMTypes.Value.get hash)
      match item with
      | Some v -> return Ok(AffectedValue(fqn, v, hash, loc))
      | None -> return Error $"Value at {hash} not found"
  }


let private fetchAffectedDependents
  (dependents : List<PMQueries.LocationDependent>)
  : Task<Result<List<Affected>, string>> =
  let rec loop remaining acc =
    task {
      match remaining with
      | [] -> return Ok(List.rev acc)
      | (dep : PMQueries.LocationDependent) :: rest ->
        let depFqn = PackageLocation.toFQN dep.itemLocation
        match! fetchAffected depFqn dep.itemLocation dep.itemKind dep.itemHash with
        | Ok a -> return! loop rest (a :: acc)
        | Error e -> return Error e
    }
  loop dependents []


/// Build the SCC input from affected items. `seedMapping` covers
/// out-of-batch substitutions such as source refs when source is not
/// included in the SCC batch.
let private stabilizationFromAffected
  (seedMapping : AT.HashMapping)
  (affected : List<Affected>)
  : HS.Stabilization =
  let mutable types = Map.empty
  let mutable fns = Map.empty
  let mutable values = Map.empty
  for a in affected do
    match a with
    | AffectedType(fqn, t, h, loc) -> types <- Map.add fqn (t, h, loc) types
    | AffectedFn(fqn, f, h, loc) -> fns <- Map.add fqn (f, h, loc) fns
    | AffectedValue(fqn, v, h, loc) -> values <- Map.add fqn (v, h, loc) values
  HS.stabilize seedMapping { types = types; fns = fns; values = values }


/// Apply the SCC stabilization to one affected item: transform body, stamp
/// final hash, emit Add+SetName ops, plus a repoint when the hash actually
/// changed.
///
/// An item that stabilizes to the hash it already has emits NOTHING. It is reached whenever
/// propagation runs a second time over an edit that already propagated -- `commit` does exactly
/// that -- and the op it would otherwise author is `SetName(loc, X, previous = X)`: a name rebound
/// to what it already holds. That op is not merely noise. It is a second naming of the same name in
/// one draft, so `Draft.collapse` keeps it and drops the FIRST one, which is the binding the fold
/// actually recorded -- and dropping that relists the pre-edit version. Committing a propagated
/// edit reverted its callers.
let private applyStabilization
  (s : HS.Stabilization)
  (a : Affected)
  : List<PT.PackageOp> * Option<PT.PropagateRepoint> =
  let fqn = affectedFqn a
  let newHash = Map.findUnsafe fqn s.fqnHashes
  // Past the guard below, `newHash` differs from `currentHash` by construction, so every
  // item that gets here repoints.
  let mkRepoint loc currentHash newRef =
    Some { location = loc; fromRef = newRef currentHash; toRef = newRef newHash }
  if newHash = affectedCurrentHash a then
    [], None
  else
    match a with
    | AffectedType(_, t, currentHash, loc) ->
      let transformed = { AT.transformType s.mapping t with hash = newHash }
      let ops =
        [ PT.PackageOp.AddType transformed
          // Propagation KNOWS what it replaced -- it is the same `currentHash` the repoint records as
          // `fromRef`. A caller that followed its dependency descends from the version it was on, which is
          // the fact that stops it looking like an independent creation to the other machine.
          PT.PackageOp.SetName(loc, PT.PackageType newHash, Some currentHash) ]
      ops, mkRepoint loc currentHash PT.PackageType
    | AffectedFn(_, f, currentHash, loc) ->
      let transformed = { AT.transformFn s.mapping f with hash = newHash }
      let ops =
        [ PT.PackageOp.AddFn transformed
          // Propagation KNOWS what it replaced -- it is the same `currentHash` the repoint records as
          // `fromRef`. A caller that followed its dependency descends from the version it was on, which is
          // the fact that stops it looking like an independent creation to the other machine.
          PT.PackageOp.SetName(loc, PT.PackageFn newHash, Some currentHash) ]
      ops, mkRepoint loc currentHash PT.PackageFn
    | AffectedValue(_, v, currentHash, loc) ->
      let transformed = { AT.transformValue s.mapping v with hash = newHash }
      let ops =
        [ PT.PackageOp.AddValue transformed
          // Propagation KNOWS what it replaced -- it is the same `currentHash` the repoint records as
          // `fromRef`. A caller that followed its dependency descends from the version it was on, which is
          // the fact that stops it looking like an independent creation to the other machine.
          PT.PackageOp.SetName(loc, PT.PackageValue newHash, Some currentHash) ]
      ops, mkRepoint loc currentHash PT.PackageValue


let private buildSeedMapping
  (sourceLocations : List<PT.PackageLocation>)
  (sourceLocation : PT.PackageLocation)
  (fromSourceHashes : List<Hash>)
  (resolvedSourceHash : Hash)
  : AT.HashMapping =
  { byLocation =
      sourceLocations
      |> List.fold (fun m loc -> Map.add loc resolvedSourceHash m) Map.empty
    byHash =
      fromSourceHashes
      |> List.fold (fun m h -> Map.add h resolvedSourceHash m) Map.empty
    byLocationRename =
      sourceLocations
      |> List.filter (fun loc -> loc <> sourceLocation)
      |> List.fold (fun m loc -> Map.add loc sourceLocation m) Map.empty }


/// Compute new hashes for source + all transitive dependents using SCC-aware
/// hashing, then emit Add+SetName ops and PropagateRepoints.
///
/// SCC hashing is required for mutually-recursive package items; location
/// data lets stale refs be matched without relying only on old hashes.
let private createAllItems
  (branch : PT.BranchId)
  (fromSourceHashes : List<Hash>)
  (toSourceHash : Hash)
  (sourceLocation : PT.PackageLocation)
  (sourceLocations : List<PT.PackageLocation>)
  (sourceItemKind : PT.ItemKind)
  (dependents : List<PMQueries.LocationDependent>)
  : Task<Result<List<PT.PropagateRepoint> * List<PT.PackageOp> * Hash, string>> =
  task {
    if List.isEmpty dependents then
      return Ok([], [], toSourceHash)
    else
      let sourceFqn = PackageLocation.toFQN sourceLocation

      // The source's CURRENT hash beats whatever the caller passed, because the CLI sometimes passes a
      // parser-time placeholder.
      //
      // The placeholder can be EMPTY, and an empty source hash seeds the substitution mapping with
      // "rewrite every reference to this item to nothing" -- dependents get bodies pointing at no hash
      // and fail at runtime with `Function  couldn't be found`, an empty name because there is no hash
      // to name. Resolving through the branch's own package manager gives the branch's binding where it
      // has one and main's where it doesn't, which is what "current" means on a branch.
      let! resolvedSourceHash =
        resolveCurrentHash branch sourceLocation sourceItemKind toSourceHash

      match! fetchAffectedDependents dependents with
      | Error e -> return Error e
      | Ok dependentsAffected ->
        // Only include source when mutual recursion makes it part of the
        // affected SCC; otherwise keep the user's already-computed source hash.
        let dependentHashes =
          dependents |> List.map (fun d -> d.itemHash) |> Set.ofList
        let! sourceDeps = PMQueries.getDependencies resolvedSourceHash
        // CLEANUP: this cycle check is still hash-based. It can
        // conservatively include the source when a dependency hash collides
        // with an affected item at another FQN. Make forward dependency
        // lookup location-aware too, then compare by (location, kind).
        let sourceInCycle =
          sourceDeps
          |> List.exists (fun dep -> Set.contains dep.itemHash dependentHashes)

        let! affectedAndSource =
          if sourceInCycle then
            task {
              match!
                fetchAffected
                  sourceFqn
                  sourceLocation
                  sourceItemKind
                  resolvedSourceHash
              with
              | Error e -> return Error e
              | Ok s -> return Ok(dependentsAffected @ [ s ], true)
            }
          else
            task { return Ok(dependentsAffected, false) }

        match affectedAndSource with
        | Error e -> return Error e
        | Ok(affected, sourceIncluded) ->
          // Seed both lookup paths: location for resolver-tagged refs, hash
          // for legacy/null-location refs. Hashing and AST rewriting must see
          // the same substitutions.
          let seedMapping =
            buildSeedMapping
              sourceLocations
              sourceLocation
              fromSourceHashes
              resolvedSourceHash

          let stabilization = stabilizationFromAffected seedMapping affected

          let finalSourceHash =
            if sourceIncluded then
              Map.findUnsafe sourceFqn stabilization.fqnHashes
            else
              resolvedSourceHash

          // Source emits ops only if the SCC changed its hash.
          let toEmit =
            if sourceIncluded && finalSourceHash = resolvedSourceHash then
              affected |> List.filter (fun a -> affectedFqn a <> sourceFqn)
            else
              affected

          let perItem = toEmit |> List.map (applyStabilization stabilization)
          let ops = perItem |> List.collect fst
          let repoints = perItem |> List.choose snd

          return Ok(repoints, ops, finalSourceHash)
  }


/// Propagates an update to all dependents (including transitive).
/// Returns None if no dependents, or Some(result, ops) if propagation occurred.
/// Entry point for the entire propagation process. Called after a package item is updated.
/// <param branch> is the branch this runs on; main is an ordinary id here.
let propagate
  (branch : PT.BranchId)
  (sourceLocation : PT.PackageLocation)
  (sourceItemKind : PT.ItemKind)
  (fromSourceHashes : List<Hash>)
  (toSourceHash : Hash)
  : Task<Result<Option<PropagationResult * List<PT.PackageOp>>, string>> =
  task {
    let! previousSourceLocations =
      PMQueries.getUnlistedLocationsForRefs sourceItemKind fromSourceHashes
    let sourceLocations =
      (sourceLocation :: previousSourceLocations) |> List.distinct

    // On a branch, resolve dependents through the branch's own bindings first.
    let! branchBindings =
      if branch.IsMain then
        Task.FromResult Map.empty
      else
        Branches.chainBindingsByHash branch

    // The user's explicit choices about what follows what. Loaded once per cascade
    // rather than per dependent: the table only ever holds things a person
    // deliberately said, so it stays small. Scoped to where the cascade is running.
    // On a branch that's the branch's own choices layered over main's; on main it's
    // main's alone, so another branch's experiment can't reach it.
    // Main is an id like any other here: its policy rows are stored under its id, and the inheritance
    // clause compares real ids. Sending "" for main matches no row, which reads as "no pins" rather
    // than as an error.
    let! pins = PMQueries.getPropagationPins branch
    let! follows = PMQueries.getPropagationFollows branch

    let! dependents =
      discoverDependents
        pins
        follows
        branchBindings
        sourceLocations
        sourceItemKind
        fromSourceHashes

    match dependents with
    | [] -> return Ok None
    | _ ->
      let! result =
        createAllItems
          branch
          fromSourceHashes
          toSourceHash
          sourceLocation
          sourceLocations
          sourceItemKind
          dependents

      match result with
      | Error err -> return Error err
      | Ok(repoints, ops, _finalToSourceHash) ->
        // No marker op. The Add + SetName ops ARE the propagation; a PropagateUpdate
        // op recorded that one happened, which the fold then ignored. Grouping now
        // comes from the commit, and "this version lost" from a recorded conflict.
        return Ok(Some({ repoints = repoints }, ops))
  }
