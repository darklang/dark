module LibPackageManager.HashStabilization

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module DE = LibPackageManager.DependencyExtractor
open LibSerialization.Hashing
module AT = LibPackageManager.AstTransformer


/// Replace Set*Name placeholder IDs with the hashes from the previous iteration.
/// After parsing, Set*Name ops have placeholder IDs (PackageRefs IDs for types/fns,
/// name hashes for values). But the items' AST references point to the PREVIOUS
/// iteration's computed hashes (from the PM). This function remaps Set*Name IDs
/// to match, so the dependency graph in computeRealHashes is correctly connected.
let remapSetNames
  (newOps : List<PT.PackageOp>)
  (prevOps : List<PT.PackageOp>)
  : List<PT.PackageOp> =
  // Build location → hash mapping from previous ops
  let prevHashes =
    prevOps
    |> List.choose (function
      | PT.PackageOp.SetTypeName(h, loc) -> Some(("type", loc), h)
      | PT.PackageOp.SetFnName(h, loc) -> Some(("fn", loc), h)
      | PT.PackageOp.SetValueName(h, loc) -> Some(("value", loc), h)
      | _ -> None)
    |> Map.ofList

  newOps
  |> List.map (function
    | PT.PackageOp.SetTypeName(_, loc) ->
      let hash =
        prevHashes |> Map.tryFind ("type", loc) |> Option.defaultValue (Hash "")
      PT.PackageOp.SetTypeName(hash, loc)
    | PT.PackageOp.SetFnName(_, loc) ->
      let hash =
        prevHashes |> Map.tryFind ("fn", loc) |> Option.defaultValue (Hash "")
      PT.PackageOp.SetFnName(hash, loc)
    | PT.PackageOp.SetValueName(_, loc) ->
      let hash =
        prevHashes |> Map.tryFind ("value", loc) |> Option.defaultValue (Hash "")
      PT.PackageOp.SetValueName(hash, loc)
    | other -> other)


/// Post-process parsed ops to compute real hashes using SCC-aware hashing.
/// Expects Set*Name IDs to match the Hash references in items' ASTs
/// (either placeholder IDs on first pass, or previous hashes via remapSetNames).
let computeRealHashes (ops : List<PT.PackageOp>) : List<PT.PackageOp> =
  // Extract items paired with their Set*Name IDs and FQNs
  let mutable pendingType : Option<PT.PackageType.PackageType> = None
  let mutable pendingFn : Option<PT.PackageFn.PackageFn> = None
  let mutable pendingValue : Option<PT.PackageValue.PackageValue> = None

  let types = ResizeArray<Hash * PT.PackageType.PackageType * string>()
  let fns = ResizeArray<Hash * PT.PackageFn.PackageFn * string>()
  let values = ResizeArray<Hash * PT.PackageValue.PackageValue * string>()

  for op in ops do
    match op with
    | PT.PackageOp.AddType t -> pendingType <- Some t
    | PT.PackageOp.SetTypeName(hash, loc) ->
      match pendingType with
      | Some t ->
        types.Add((hash, t, PackageLocation.toFQN loc))
        pendingType <- None
      | None -> ()
    | PT.PackageOp.AddFn f -> pendingFn <- Some f
    | PT.PackageOp.SetFnName(hash, loc) ->
      match pendingFn with
      | Some f ->
        fns.Add((hash, f, PackageLocation.toFQN loc))
        pendingFn <- None
      | None -> ()
    | PT.PackageOp.AddValue v -> pendingValue <- Some v
    | PT.PackageOp.SetValueName(hash, loc) ->
      match pendingValue with
      | Some v ->
        values.Add((hash, v, PackageLocation.toFQN loc))
        pendingValue <- None
      | None -> ()
    | _ -> ()

  // Build maps keyed by FQN to avoid collisions when multiple items
  // share the same Hash (e.g. type aliases with unresolved refs).
  // Value tuple: (item, oldHash)
  let typeMap =
    types |> Seq.map (fun (hash, t, fqn) -> (fqn, (t, hash))) |> Map.ofSeq
  let fnMap = fns |> Seq.map (fun (hash, f, fqn) -> (fqn, (f, hash))) |> Map.ofSeq
  let valueMap =
    values |> Seq.map (fun (hash, v, fqn) -> (fqn, (v, hash))) |> Map.ofSeq

  // Reverse lookup: Hash → FQNs for converting AST deps to FQN deps
  let hashToFqns : Map<Hash, List<string>> =
    let mutable result = Map.empty<Hash, List<string>>
    for (h, _, fqn) in types do
      let existing = Map.tryFind h result |> Option.defaultValue []
      result <- Map.add h (fqn :: existing) result
    for (h, _, fqn) in fns do
      let existing = Map.tryFind h result |> Option.defaultValue []
      result <- Map.add h (fqn :: existing) result
    for (h, _, fqn) in values do
      let existing = Map.tryFind h result |> Option.defaultValue []
      result <- Map.add h (fqn :: existing) result
    result

  // Build dependency graph using FQNs
  let allFqns =
    [ yield! Map.keys typeMap; yield! Map.keys fnMap; yield! Map.keys valueMap ]
    |> Set.ofList

  let getDeps (fqn : string) : List<string> =
    let deps =
      match Map.tryFind fqn typeMap with
      | Some(t, _) -> DE.extractFromType t
      | None ->
        match Map.tryFind fqn fnMap with
        | Some(f, _) -> DE.extractFromFn f
        | None ->
          match Map.tryFind fqn valueMap with
          | Some(v, _) -> DE.extractFromValue v
          | None -> []
    // Map Hash deps to FQNs, filtering to items in this batch
    deps
    |> List.collect (fun hash ->
      hashToFqns |> Map.tryFind hash |> Option.defaultValue [])
    |> List.filter (fun dep -> Set.contains dep allFqns)

  // Compute hashes with SCC awareness (keyed by FQN)
  let fqnHashMap = Hashing.computeHashesWithSCCs typeMap fnMap valueMap getDeps

  // Build old Hash → new Hash mapping for AT.transform*
  let oldToNewHash : Map<Hash, Hash> =
    let mutable result = Map.empty<Hash, Hash>
    let addMappings (items : ResizeArray<Hash * _ * string>) =
      for (oldHash, _, fqn) in items do
        match Map.tryFind fqn fqnHashMap with
        | Some newHash -> result <- Map.add oldHash newHash result
        | None -> ()
    addMappings types
    addMappings fns
    addMappings values
    result

  // Update ops: replace Set*Name IDs with computed hashes,
  // and transform Add* content ASTs to use new hashes.
  // Also set the hash field on Add* items so applyAdd* uses the correct
  // (SCC-aware) hash rather than recomputing in Normal mode.
  //
  // Process ops pairwise: Add* is always followed by Set*Name.
  // We compute FQN from the location to look up the real hash in fqnHashMap,
  // then set it on both the Add* item and the Set*Name op.
  let rec processOps (remaining : List<PT.PackageOp>) (acc : List<PT.PackageOp>) =
    match remaining with
    | PT.PackageOp.AddType t :: PT.PackageOp.SetTypeName(oldHash, loc) :: rest ->
      let newHash =
        Map.tryFind (PackageLocation.toFQN loc) fqnHashMap
        |> Option.defaultValue oldHash
      let transformed = { AT.transformType oldToNewHash t with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetTypeName(newHash, loc)
         :: PT.PackageOp.AddType transformed
         :: acc)
    | PT.PackageOp.AddFn f :: PT.PackageOp.SetFnName(oldHash, loc) :: rest ->
      let newHash =
        Map.tryFind (PackageLocation.toFQN loc) fqnHashMap
        |> Option.defaultValue oldHash
      let transformed = { AT.transformFn oldToNewHash f with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetFnName(newHash, loc)
         :: PT.PackageOp.AddFn transformed
         :: acc)
    | PT.PackageOp.AddValue v :: PT.PackageOp.SetValueName(oldHash, loc) :: rest ->
      let newHash =
        Map.tryFind (PackageLocation.toFQN loc) fqnHashMap
        |> Option.defaultValue oldHash
      let transformed = { AT.transformValue oldToNewHash v with hash = newHash }
      processOps
        rest
        (PT.PackageOp.SetValueName(newHash, loc)
         :: PT.PackageOp.AddValue transformed
         :: acc)
    | op :: rest -> processOps rest (op :: acc)
    | [] -> List.rev acc

  processOps ops []


/// Extract all hashes from Set*Name ops (for convergence checking).
let extractAllHashes (ops : List<PT.PackageOp>) : List<Hash> =
  ops
  |> List.choose (function
    | PT.PackageOp.SetTypeName(hash, _) -> Some hash
    | PT.PackageOp.SetFnName(hash, _) -> Some hash
    | PT.PackageOp.SetValueName(hash, _) -> Some hash
    | _ -> None)
