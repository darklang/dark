/// Content-addressable hashing for package items.
///
/// Computes SHA-256 hashes of canonical serialized forms, with SCC-aware
/// batch hashing for mutually-recursive definitions (Tarjan's algorithm).
///
/// TODO the name of a fn's argument shouldn't relate to the hash.
/// To to that end, need to adjust PT and such to model `| EArg of pos: Int` or something
namespace LibSerialization.Hashing

open System.IO
open System.Security.Cryptography
open Prelude
open LibExecution.ProgramTypes
module PT = LibExecution.ProgramTypes

module Common = LibSerialization.Binary.Serializers.Common
module PTC = LibSerialization.Binary.Serializers.PT.Common


[<AutoOpen>]
module Hashing =

  type HashRefMode = Canonical.HashRefMode
  let Normal = Canonical.Normal


  // =====================
  // Hash computation helpers
  // =====================

  let private fromSHA256Bytes (bytes : byte array) : Hash =
    Hash(System.Convert.ToHexString(bytes).ToLowerInvariant())

  /// Serialize to bytes using a writer function, then SHA-256 hash to Hash
  let private hashWithWriter (writerFn : BinaryWriter -> unit) : Hash =
    use ms = new MemoryStream()
    use w = new BinaryWriter(ms)
    writerFn w
    let bytes = ms.ToArray()
    SHA256.HashData(bytes) |> fromSHA256Bytes


  // =====================
  // Hash computation functions
  // =====================

  /// Hash a PackageType (skip id, description, deprecated)
  let computeTypeHash (mode : HashRefMode) (t : PT.PackageType.PackageType) : Hash =
    hashWithWriter (fun w -> Canonical.writeType mode w t)


  /// Hash a PackageFn (skip id, description, deprecated, param descriptions)
  let computeFnHash (mode : HashRefMode) (fn : PT.PackageFn.PackageFn) : Hash =
    hashWithWriter (fun w -> Canonical.writeFn mode w fn)


  /// Hash a PackageValue (skip id, description, deprecated)
  let computeValueHash
    (mode : HashRefMode)
    (v : PT.PackageValue.PackageValue)
    : Hash =
    hashWithWriter (fun w -> Canonical.writeValue mode w v)


  /// Hash a PackageOp (reuse existing PackageOp.write — ops have no metadata to skip)
  let computeOpHash (op : PT.PackageOp) : Hash =
    hashWithWriter (fun w ->
      LibSerialization.Binary.Serializers.PT.PackageOp.write w op)


  /// Hash a BranchOp (reuse existing BranchOp.write — ops have no metadata to skip)
  let computeBranchOpHash (op : PT.BranchOp) : Hash =
    hashWithWriter (fun w ->
      LibSerialization.Binary.Serializers.PT.BranchOp.write w op)


  /// Hash a commit: hash(parentHash + sorted(opHashes))
  let computeCommitHash (parentHash : Hash option) (opHashes : List<Hash>) : Hash =
    hashWithWriter (fun w ->
      Common.Option.write w PTC.Hash.write parentHash
      let sorted = opHashes |> List.map Hash.toHexString |> List.sort
      Common.List.write w Common.String.write sorted)


  // =====================
  // Tarjan's SCC algorithm
  // =====================

  /// Find all strongly-connected components in a dependency graph.
  /// Returns list of SCCs, each an NEList of node IDs.
  let findSCCs
    (nodes : List<'id>)
    (edges : 'id -> List<'id>)
    : List<NEList<'id>> when 'id : equality and 'id : comparison =

    let mutable index = 0
    let mutable stack : List<'id> = []
    let mutable indices = Map.empty<'id, int>
    let mutable lowlinks = Map.empty<'id, int>
    let mutable onStack = Set.empty<'id>
    let mutable result : List<NEList<'id>> = []

    let nodeSet = Set.ofList nodes

    let rec strongConnect (v : 'id) =
      indices <- Map.add v index indices
      lowlinks <- Map.add v index lowlinks
      index <- index + 1
      stack <- v :: stack
      onStack <- Set.add v onStack

      for w in edges v do
        if Set.contains w nodeSet then
          if not (Map.containsKey w indices) then
            strongConnect w
            lowlinks <-
              Map.add
                v
                (min (Map.findUnsafe v lowlinks) (Map.findUnsafe w lowlinks))
                lowlinks
          elif Set.contains w onStack then
            lowlinks <-
              Map.add
                v
                (min (Map.findUnsafe v lowlinks) (Map.findUnsafe w indices))
                lowlinks

      if Map.findUnsafe v lowlinks = Map.findUnsafe v indices then
        let mutable scc = []
        let mutable keepPopping = true
        while keepPopping do
          match stack with
          | w :: rest ->
            stack <- rest
            onStack <- Set.remove w onStack
            scc <- w :: scc
            if w = v then keepPopping <- false
          | [] -> keepPopping <- false

        match scc with
        | head :: tail -> result <- { head = head; tail = tail } :: result
        | [] -> ()

    for node in nodes do
      if not (Map.containsKey node indices) then strongConnect node

    result |> List.rev


  // =====================
  // SCC batch hashing
  // =====================

  type private ItemInfo =
    | TypeItem of PT.PackageType.PackageType * string * Hash
    | FnItem of PT.PackageFn.PackageFn * string * Hash
    | ValueItem of PT.PackageValue.PackageValue * string * Hash

  let private getItemFQN (item : ItemInfo) : string =
    match item with
    | TypeItem(_, fqn, _) -> fqn
    | FnItem(_, fqn, _) -> fqn
    | ValueItem(_, fqn, _) -> fqn

  let private getItemOldHash (item : ItemInfo) : Hash =
    match item with
    | TypeItem(_, _, h) -> h
    | FnItem(_, _, h) -> h
    | ValueItem(_, _, h) -> h

  let private computeItemHash (mode : HashRefMode) (item : ItemInfo) : Hash =
    match item with
    | TypeItem(t, _, _) -> computeTypeHash mode t
    | FnItem(fn, _, _) -> computeFnHash mode fn
    | ValueItem(v, _, _) -> computeValueHash mode v

  let private serializeItemBytes
    (mode : Canonical.HashRefMode)
    (item : ItemInfo)
    : byte array =
    use ms = new MemoryStream()
    use w = new BinaryWriter(ms)
    match item with
    | TypeItem(t, _, _) -> Canonical.writeType mode w t
    | FnItem(fn, _, _) -> Canonical.writeFn mode w fn
    | ValueItem(v, _, _) -> Canonical.writeValue mode w v
    ms.ToArray()


  /// Given a set of package items (keyed by FQN) and their dependencies, compute
  /// hashes handling SCCs via batch hashing with name-ref substitution.
  /// Maps are keyed by FQN (string) to avoid collisions when multiple items share
  /// the same Hash (e.g. type aliases with unresolved refs on first parse).
  /// The Hash in each tuple is the item's current/old hash.
  let computeHashesWithSCCs
    (types : Map<string, PT.PackageType.PackageType * Hash>)
    (fns : Map<string, PT.PackageFn.PackageFn * Hash>)
    (values : Map<string, PT.PackageValue.PackageValue * Hash>)
    (getDeps : string -> List<string>)
    : Map<string, Hash> =

    // Build unified item map (keyed by FQN)
    let items =
      Map.fold
        (fun acc fqn (t, oldHash) -> Map.add fqn (TypeItem(t, fqn, oldHash)) acc)
        Map.empty
        types
      |> Map.fold (fun acc fqn (fn, oldHash) ->
        Map.add fqn (FnItem(fn, fqn, oldHash)) acc)
      <| fns
      |> Map.fold (fun acc fqn (v, oldHash) ->
        Map.add fqn (ValueItem(v, fqn, oldHash)) acc)
      <| values

    let allIds = items |> Map.keys |> Seq.toList

    // Detect SCCs (operating on FQN strings as node IDs)
    let sccs = findSCCs allIds getDeps

    // FQN → finalHash result map
    let mutable hashMap = Map.empty<string, Hash>
    // oldHash → finalHash for the canonical serializer's resolvedDeps
    let mutable resolvedDepsMap = Map.empty<Hash, Hash>

    for scc in sccs do
      let sccIds = scc.head :: scc.tail

      // A size-1 SCC without a self-loop is a true singleton — hash normally.
      // A size-1 SCC WITH a self-loop (e.g. recursive type Expr referencing itself)
      // must use SccNameRef mode, otherwise the hash depends on the previous
      // iteration's hash and never converges.
      let hasSelfLoop =
        List.length sccIds = 1 && getDeps scc.head |> List.contains scc.head

      if List.length sccIds = 1 && not hasSelfLoop then
        // Single-item SCC without self-reference: hash with finalized deps
        let fqn = scc.head
        let item = Map.findUnsafe fqn items

        let mode : Canonical.HashRefMode =
          { resolvedDeps = resolvedDepsMap; sccNames = Map.empty }

        let hash = computeItemHash mode item
        hashMap <- Map.add fqn hash hashMap
        resolvedDepsMap <- Map.add (getItemOldHash item) hash resolvedDepsMap
      else
        // Multi-item SCC: batch hash with FQN substitution + finalized deps
        // sccNames maps old Hash → FQN for cycle-breaking in canonical serializer
        let sccNameMap =
          sccIds
          |> List.map (fun fqn ->
            let item = Map.findUnsafe fqn items
            getItemOldHash item, fqn)
          |> Map.ofList

        let mode : Canonical.HashRefMode =
          { resolvedDeps = resolvedDepsMap; sccNames = sccNameMap }

        // Sort by FQN for determinism
        let sortedItems =
          sccIds
          |> List.map (fun fqn -> (fqn, Map.findUnsafe fqn items))
          |> List.sortBy (fun (_, item) -> getItemFQN item)

        // Concatenate canonical bytes of all items in the SCC
        let groupBytes =
          sortedItems
          |> List.map (fun (_, item) -> serializeItemBytes mode item)
          |> Array.concat

        let groupHash = SHA256.HashData(groupBytes) |> fromSHA256Bytes

        // Each item's hash = hash(groupHash + FQN)
        for (fqn, item) in sortedItems do
          let itemFQN = getItemFQN item
          let itemHash =
            hashWithWriter (fun w ->
              PTC.Hash.write w groupHash
              Common.String.write w itemFQN)
          hashMap <- Map.add fqn itemHash hashMap
          resolvedDepsMap <- Map.add (getItemOldHash item) itemHash resolvedDepsMap

    hashMap
