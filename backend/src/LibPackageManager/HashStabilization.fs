/// SCC-aware content-addressed hash recomputation for batches of package
/// items, including cyclic references.
module LibPackageManager.HashStabilization

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module DE = LibPackageManager.DependencyExtractor
module AT = LibPackageManager.AstTransformer
module PackageLocation = LibPackageManager.PackageLocation
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
    values : Map<string, PT.PackageValue.PackageValue * Hash * PT.PackageLocation> }


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


/// Iterate `(fqn, currentHash, location)` over every item in the input.
let private iterItems
  (input : Input)
  (f : string -> Hash -> PT.PackageLocation -> unit)
  : unit =
  input.types |> Map.iter (fun fqn (_, h, loc) -> f fqn h loc)
  input.fns |> Map.iter (fun fqn (_, h, loc) -> f fqn h loc)
  input.values |> Map.iter (fun fqn (_, h, loc) -> f fqn h loc)


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
  types, fns, values


/// Run SCC-aware hashing over an interconnected batch.
///
/// `seedMapping` injects substitutions for refs to items outside the batch.
/// Use location for resolver-tagged refs and hash for legacy/null-location
/// refs.
let stabilize (seedMapping : AT.HashMapping) (input : Input) : Stabilization =
  let allFqns =
    [ yield! Map.keys input.types
      yield! Map.keys input.fns
      yield! Map.keys input.values ]
    |> Set.ofList

  // Prefer location edges; they keep stale refs connected after hash changes.
  let locationToFqn : Map<PT.PackageLocation, string> =
    let mutable m = Map.empty
    iterItems input (fun fqn _ loc -> m <- Map.add loc fqn m)
    m

  // Fallback for deps without location.
  let hashToFqns : Map<Hash, List<string>> =
    let mutable m = Map.empty
    iterItems input (fun fqn h _ ->
      let existing = Map.tryFind h m |> Option.defaultValue []
      m <- Map.add h (fqn :: existing) m)
    m

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

  let typeMap, fnMap, valueMap = toHashingMaps input

  let seed : Canonical.Substitution =
    { byLocation = seedMapping.byLocation; byHash = seedMapping.byHash }

  let fqnHashes = Hashing.computeHashesWithSCCs seed typeMap fnMap valueMap getDeps

  // Extend the caller seed with every batched item's final hash.
  let byLocation : Map<PT.PackageLocation, Hash> =
    let mutable m = seedMapping.byLocation
    iterItems input (fun fqn _ loc ->
      match Map.tryFind fqn fqnHashes with
      | Some newHash -> m <- Map.add loc newHash m
      | None -> ())
    m

  let byHash : Map<Hash, Hash> =
    let mutable m = seedMapping.byHash
    iterItems input (fun fqn currentHash _ ->
      match Map.tryFind fqn fqnHashes with
      | Some newHash -> m <- Map.add currentHash newHash m
      | None -> ())
    m

  { fqnHashes = fqnHashes
    mapping =
      { byLocation = byLocation
        byHash = byHash
        byLocationRename = seedMapping.byLocationRename } }


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
      | PT.PackageOp.SetName(loc, target) ->
        Some((target.kind.toString (), loc), target.hash)
      | _ -> None)
    |> Map.ofList

  newOps
  |> List.map (function
    | PT.PackageOp.SetName(loc, target) ->
      let hash =
        prevHashes
        |> Map.tryFind (target.kind.toString (), loc)
        |> Option.defaultValue (Hash "")
      PT.PackageOp.SetName(loc, PT.Reference.fromHashAndKind (hash, target.kind))
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
  let mutable typeMap = Map.empty
  let mutable fnMap = Map.empty
  let mutable valueMap = Map.empty

  let mutable pendingType : Option<PT.PackageType.PackageType> = None
  let mutable pendingFn : Option<PT.PackageFn.PackageFn> = None
  let mutable pendingValue : Option<PT.PackageValue.PackageValue> = None

  for op in ops do
    match op with
    | PT.PackageOp.AddType t -> pendingType <- Some t
    | PT.PackageOp.SetName(loc, PT.PackageType hash) ->
      match pendingType with
      | Some t ->
        typeMap <- Map.add (PackageLocation.toFQN loc) (t, hash, loc) typeMap
        pendingType <- None
      | None -> ()
    | PT.PackageOp.AddFn f -> pendingFn <- Some f
    | PT.PackageOp.SetName(loc, PT.PackageFn hash) ->
      match pendingFn with
      | Some f ->
        fnMap <- Map.add (PackageLocation.toFQN loc) (f, hash, loc) fnMap
        pendingFn <- None
      | None -> ()
    | PT.PackageOp.AddValue v -> pendingValue <- Some v
    | PT.PackageOp.SetName(loc, PT.PackageValue hash) ->
      match pendingValue with
      | Some v ->
        valueMap <- Map.add (PackageLocation.toFQN loc) (v, hash, loc) valueMap
        pendingValue <- None
      | None -> ()
    | _ -> ()

  let s =
    stabilize AT.emptyMapping { types = typeMap; fns = fnMap; values = valueMap }

  // Walk ops again, replacing SetName hashes with computed hashes and
  // transforming Add* bodies via the substitution. Setting `hash` on the
  // Add* item means `applyAdd*` skips its own normal-mode recompute.
  let hashFor (loc : PT.PackageLocation) (fallback : Hash) : Hash =
    Map.tryFind (PackageLocation.toFQN loc) s.fqnHashes
    |> Option.defaultValue fallback

  let rec processOps (remaining : List<PT.PackageOp>) (acc : List<PT.PackageOp>) =
    match remaining with
    | PT.PackageOp.AddType t :: PT.PackageOp.SetName(loc, PT.PackageType oldHash) :: rest ->
      let newHash = hashFor loc oldHash
      let transformed = { AT.transformType s.mapping t with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetName(loc, PT.PackageType newHash)
         :: PT.PackageOp.AddType transformed
         :: acc)
    | PT.PackageOp.AddFn f :: PT.PackageOp.SetName(loc, PT.PackageFn oldHash) :: rest ->
      let newHash = hashFor loc oldHash
      let transformed = { AT.transformFn s.mapping f with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetName(loc, PT.PackageFn newHash)
         :: PT.PackageOp.AddFn transformed
         :: acc)
    | PT.PackageOp.AddValue v :: PT.PackageOp.SetName(loc, PT.PackageValue oldHash) :: rest ->
      let newHash = hashFor loc oldHash
      let transformed = { AT.transformValue s.mapping v with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetName(loc, PT.PackageValue newHash)
         :: PT.PackageOp.AddValue transformed
         :: acc)
    | op :: rest -> processOps rest (op :: acc)
    | [] -> List.rev acc

  processOps ops []


/// Extract all hashes from SetName ops (for convergence checking).
let extractAllHashes (ops : List<PT.PackageOp>) : List<Hash> =
  ops
  |> List.choose (function
    | PT.PackageOp.SetName(_, target) -> Some target.hash
    | _ -> None)
