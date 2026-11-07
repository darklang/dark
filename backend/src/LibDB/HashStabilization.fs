/// SCC-aware content-addressed hash recomputation for batches of package
/// items, including cyclic references.
module LibDB.HashStabilization

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module DE = LibDB.DependencyExtractor
module AT = LibDB.AstTransformer
module PackageLocation = LibDB.PackageLocation
open LibSerialization.Hashing


// --------------------------------------------------------------------------
// Core SCC-aware stabilization
// --------------------------------------------------------------------------

/// A batch of items grouped by kind, keyed by FQN and paired with current
/// hash plus location. Location lets stale references be matched even after
/// the referenced item's hash has changed.
type Input =
  { types : Map<string, PT.PackageType.PackageType * Hash * PT.PackageLocation>
    fns : Map<string, PT.PackageFn.PackageFn * Hash * PT.PackageLocation>
    values : Map<string, PT.PackageValue.PackageValue * Hash * PT.PackageLocation>
    traits : Map<string, PT.Trait.Trait * Hash * PT.PackageLocation>
    impls : Map<string, PT.TraitImpl.TraitImpl * Hash * PT.PackageLocation> }

let emptyInput : Input =
  { types = Map.empty
    fns = Map.empty
    values = Map.empty
    traits = Map.empty
    impls = Map.empty }


/// Result of `stabilize`. Callers apply `mapping` to item ASTs and stamp
/// `fqnHashes[fqn]` onto transformed items.
type Stabilization =
  {
    /// FQN → final SCC-stabilized hash. Use for SetName ops and to set the
    /// `hash` field on transformed items.
    fqnHashes : Map<string, Hash>
    /// Substitution map for `AstTransformer.transform*`. Location-keyed
    /// is the authoritative path; the hash-keyed fallback is for NRs
    /// without location (legacy / builtins) and for caller-injected
    /// substitutions like `fromSourceHashes → newSourceHash`.
    mapping : AT.HashMapping
  }


/// `(fqn, currentHash, location)` of every item in the input.
let private items (input : Input) : List<string * Hash * PT.PackageLocation> =
  let entries (m : Map<string, 'item * Hash * PT.PackageLocation>) =
    m |> Map.toList |> List.map (fun (fqn, (_, h, loc)) -> (fqn, h, loc))
  entries input.types
  @ entries input.fns
  @ entries input.values
  @ entries input.traits
  @ entries input.impls


/// Extract deps for one item by FQN.
let private depsOfItem (input : Input) (fqn : string) : List<DE.Dependency> =
  match Map.tryFind fqn input.types with
  | Some(t, _, _) -> DE.extractFromType t
  | None ->
    match Map.tryFind fqn input.fns with
    | Some(f, _, _) -> DE.extractFromFn f
    | None ->
      match Map.tryFind fqn input.values with
      | Some(v, _, _) -> DE.extractFromValue v
      | None ->
        match Map.tryFind fqn input.traits with
        | Some(t, _, _) -> DE.extractFromTrait t
        | None ->
          match Map.tryFind fqn input.impls with
          | Some(i, _, _) -> DE.extractFromImpl i
          | None -> []


/// Project input into the shape `Hashing.computeHashesWithSCCs` expects.
let private toHashingMaps (input : Input) =
  let types =
    input.types
    |> Map.toSeq
    |> Seq.map (fun (fqn, (t, h, loc)) -> (fqn, (t, h, Some loc)))
    |> Map.ofSeq
  let fns =
    input.fns
    |> Map.toSeq
    |> Seq.map (fun (fqn, (f, h, loc)) -> (fqn, (f, h, Some loc)))
    |> Map.ofSeq
  let values =
    input.values
    |> Map.toSeq
    |> Seq.map (fun (fqn, (v, h, loc)) -> (fqn, (v, h, Some loc)))
    |> Map.ofSeq
  let traits =
    input.traits
    |> Map.toSeq
    |> Seq.map (fun (fqn, (t, h, loc)) -> (fqn, (t, h, Some loc)))
    |> Map.ofSeq
  let impls =
    input.impls
    |> Map.toSeq
    |> Seq.map (fun (fqn, (i, h, loc)) -> (fqn, (i, h, Some loc)))
    |> Map.ofSeq
  types, fns, values, traits, impls


/// Run SCC-aware hashing over an interconnected batch.
///
/// `seedMapping` injects substitutions for refs to items outside the batch.
/// Use location for resolver-tagged refs and hash for legacy/null-location
/// refs.
let stabilize (seedMapping : AT.HashMapping) (input : Input) : Stabilization =
  let allFqns =
    [ yield! Map.keys input.types
      yield! Map.keys input.fns
      yield! Map.keys input.values
      yield! Map.keys input.traits
      yield! Map.keys input.impls ]
    |> Set.ofList

  // Prefer location edges; they keep stale refs connected after hash changes.
  let locationToFqn : Map<PT.PackageLocation, string> =
    items input |> List.fold (fun m (fqn, _, loc) -> Map.add loc fqn m) Map.empty

  // Fallback for deps without location.
  let hashToFqns : Map<Hash, List<string>> =
    items input
    |> List.fold
      (fun m (fqn, h, _) ->
        let existing = Map.tryFind h m |> Option.defaultValue []
        Map.add h (fqn :: existing) m)
      Map.empty

  let resolveDep (dep : DE.Dependency) : List<string> =
    match dep.location with
    | Some loc ->
      match Map.tryFind loc locationToFqn with
      | Some fqn -> [ fqn ]
      | None -> []
    | None -> hashToFqns |> Map.tryFind dep.hash |> Option.defaultValue []

  let getDeps (fqn : string) : List<string> =
    depsOfItem input fqn
    |> List.collect resolveDep
    |> List.filter (fun fqn -> Set.contains fqn allFqns)

  let typeMap, fnMap, valueMap, traitMap, implMap = toHashingMaps input

  let seed : Canonical.Substitution =
    { byLocation = seedMapping.byLocation; byHash = seedMapping.byHash }

  let fqnHashes =
    Hashing.computeHashesWithSCCs
      seed
      typeMap
      fnMap
      valueMap
      traitMap
      implMap
      getDeps

  // Extend the caller seed with every batched item's final hash.
  let byLocation : Map<PT.PackageLocation, Hash> =
    items input
    |> List.fold
      (fun m (fqn, _, loc) ->
        match Map.tryFind fqn fqnHashes with
        | Some newHash -> Map.add loc newHash m
        | None -> m)
      seedMapping.byLocation

  let byHash : Map<Hash, Hash> =
    items input
    |> List.fold
      (fun m (fqn, currentHash, _) ->
        match Map.tryFind fqn fqnHashes with
        | Some newHash -> Map.add currentHash newHash m
        | None -> m)
      seedMapping.byHash

  { fqnHashes = fqnHashes
    mapping =
      { byLocation = byLocation
        byHash = byHash
        referenceName = seedMapping.referenceName
        resolvedImpls = seedMapping.resolvedImpls
        boundImpls = seedMapping.boundImpls } }


// --------------------------------------------------------------------------
// Op-list wrapper used by load-from-disk + WIP refresh
// --------------------------------------------------------------------------

/// Replace SetName placeholder hashes with the hashes from the previous iteration.
/// After parsing, SetName ops have placeholder Reference hashes. But the items'
/// AST references point to the PREVIOUS iteration's computed hashes (from the PM).
/// This function remaps SetName hashes to match, so the dependency graph in
/// computeRealHashes is correctly connected.
let remapSetNames
  (newOps : List<PT.PackageOp>)
  (prevOps : List<PT.PackageOp>)
  : List<PT.PackageOp> =
  // Build (kind, location) → hash mapping from previous ops
  let prevHashes =
    prevOps
    |> List.choose (function
      | PT.PackageOp.SetName(loc, target, _) ->
        Some((target.kind.toString (), loc), target.hash)
      | _ -> None)
    |> Map.ofList

  newOps
  |> List.map (function
    | PT.PackageOp.SetName(loc, target, previous) ->
      let hash =
        prevHashes
        |> Map.tryFind (target.kind.toString (), loc)
        |> Option.defaultValue (Hash "")
      // Only the TARGET is being restated here; what this binding replaced is unchanged by rehashing it.
      PT.PackageOp.SetName(
        loc,
        PT.Reference.fromHashAndKind (hash, target.kind),
        previous
      )
    | other -> other)


/// Post-process parsed ops to compute real hashes using SCC-aware hashing.
/// Expects SetName hashes to match the Hash references in items' ASTs
/// (either placeholder IDs on first pass, or previous hashes via remapSetNames).
/// Assumes each (FQN, kind) appears in at most one Add+SetName pair —
/// `WipRefresh` calls `compactWipOps` beforehand; load-from-disk paths
/// produce single pairs by construction.
let computeRealHashes (ops : List<PT.PackageOp>) : List<PT.PackageOp> =
  // Walk the op list pairwise to collect items keyed by FQN, paired with
  // their SetName hashes and locations. Add* is always followed by SetName.
  //
  let input =
    List.pairwise ops
    |> List.fold
      (fun (acc : Input) (declaration, binding) ->
        match PT.tryDeclarationBinding (declaration, binding) with
        | None -> acc
        | Some(loc, target) ->
          let key = PackageLocation.toFQN loc
          let hash = target.hash
          match declaration with
          | PT.PackageOp.AddType item ->
            { acc with types = Map.add key (item, hash, loc) acc.types }
          | PT.PackageOp.AddFn item ->
            { acc with fns = Map.add key (item, hash, loc) acc.fns }
          | PT.PackageOp.AddValue item ->
            { acc with values = Map.add key (item, hash, loc) acc.values }
          | PT.PackageOp.AddTrait item ->
            { acc with traits = Map.add key (item, hash, loc) acc.traits }
          | PT.PackageOp.AddTraitImpl item ->
            { acc with impls = Map.add key (item, hash, loc) acc.impls }
          | _ -> acc)
      emptyInput

  let s = stabilize AT.emptyMapping input

  // Walk ops again, replacing SetName hashes with computed hashes and
  // transforming Add* bodies via the substitution. Setting `hash` on the
  // Add* item means `applyAdd*` skips its own normal-mode recompute.
  let hashFor (loc : PT.PackageLocation) (fallback : Hash) : Hash =
    Map.tryFind (PackageLocation.toFQN loc) s.fqnHashes
    |> Option.defaultValue fallback

  let rec processOps (remaining : List<PT.PackageOp>) (acc : List<PT.PackageOp>) =
    match remaining with
    | PT.PackageOp.AddType t :: PT.PackageOp.SetName(loc,
                                                     PT.PackageType oldHash,
                                                     previous) :: rest ->
      let newHash = hashFor loc oldHash
      let transformed = { AT.transformType s.mapping t with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetName(loc, PT.PackageType newHash, previous)
         :: PT.PackageOp.AddType transformed
         :: acc)
    | PT.PackageOp.AddFn f :: PT.PackageOp.SetName(loc,
                                                   PT.PackageFn oldHash,
                                                   previous) :: rest ->
      let newHash = hashFor loc oldHash
      let transformed = { AT.transformFn s.mapping f with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetName(loc, PT.PackageFn newHash, previous)
         :: PT.PackageOp.AddFn transformed
         :: acc)
    | PT.PackageOp.AddValue v :: PT.PackageOp.SetName(loc,
                                                      PT.PackageValue oldHash,
                                                      previous) :: rest ->
      let newHash = hashFor loc oldHash
      let transformed = { AT.transformValue s.mapping v with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetName(loc, PT.PackageValue newHash, previous)
         :: PT.PackageOp.AddValue transformed
         :: acc)
    | PT.PackageOp.AddTrait t :: PT.PackageOp.SetName(loc,
                                                      PT.PackageTrait oldHash,
                                                      previous) :: rest ->
      let newHash = hashFor loc oldHash
      let transformed = { AT.transformTrait s.mapping t with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetName(loc, PT.PackageTrait newHash, previous)
         :: PT.PackageOp.AddTrait transformed
         :: acc)
    | PT.PackageOp.AddTraitImpl i :: PT.PackageOp.SetName(loc,
                                                          PT.PackageTraitImpl oldHash,
                                                          previous) :: rest ->
      let newHash = hashFor loc oldHash
      let transformed = { AT.transformImpl s.mapping i with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetName(loc, PT.PackageTraitImpl newHash, previous)
         :: PT.PackageOp.AddTraitImpl transformed
         :: acc)
    // Nothing can refer to a test, so a test is never part of a cycle and needs
    // no group hashing. Hash each one on its own, once `s.mapping` has everything
    // it refers to at its final hash.
    | PT.PackageOp.AddTest test :: PT.PackageOp.SetName(loc,
                                                        PT.PackageTest _,
                                                        previous) :: rest ->
      let transformed = AT.transformTest s.mapping test
      let newHash = Hashing.computeTestHash Hashing.Normal transformed
      processOps
        rest
        (PT.PackageOp.SetName(loc, PT.PackageTest newHash, previous)
         :: PT.PackageOp.AddTest { transformed with hash = newHash }
         :: acc)
    | op :: rest -> processOps rest (op :: acc)
    | [] -> List.rev acc

  processOps ops []


/// Extract all hashes from SetName ops (for convergence checking).
let extractAllHashes (ops : List<PT.PackageOp>) : List<Hash> =
  ops
  |> List.choose (function
    | PT.PackageOp.SetName(_, target, _) -> Some target.hash
    | _ -> None)
