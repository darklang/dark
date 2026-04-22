module LibPackageManager.Queries

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


/// Get recent package ops from the database
let getRecentOps (limit : int64) : Task<List<PT.PackageOp>> =
  task {
    return!
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        ORDER BY created_at DESC
        LIMIT @limit
        """
      |> Sql.parameters [ "limit", Sql.int64 limit ]
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"
        BS.PT.PackageOp.deserialize opId opBlob)
  }


/// Get all package ops created since the specified datetime
let getAllOpsSince
  (since : LibExecution.DarkDateTime.T)
  : Task<List<PT.PackageOp * bool>> =
  task {
    let sinceStr = LibExecution.DarkDateTime.toIsoString since

    return!
      Sql.query
        """
        SELECT id, op_blob, commit_hash
        FROM package_ops
        WHERE datetime(created_at) > datetime(@since)
        ORDER BY created_at ASC
        """
      |> Sql.parameters [ "since", Sql.string sinceStr ]
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"
        let isWip = (read.uuidOrNone "commit_hash").IsNone
        let op = BS.PT.PackageOp.deserialize opId opBlob
        (op, isWip))
  }


/// A dependency relationship between package items.
/// For dependents: itemHash is the item that has the dependency.
/// For dependencies: itemHash is what the item depends on.
type PackageDep = { itemHash : Hash; itemKind : PT.ItemKind }


/// Get items that depend on the given Hash (reverse dependencies / "what uses this?")
let getDependents
  (branchChain : List<PT.BranchId>)
  (dependsOnHash : Hash)
  : Task<List<PackageDep>> =
  task {
    if List.isEmpty branchChain then
      return []
    else
      let branchParams = branchChain |> List.mapi (fun i id -> $"b_{i}", Sql.uuid id)

      let branchInClause =
        branchChain |> List.mapi (fun i _ -> $"@b_{i}") |> String.concat ", "

      let (Hash dependsOnHash) = dependsOnHash

      return!
        Sql.query
          $"""
          SELECT DISTINCT pd.item_hash, l.item_type
          FROM package_dependencies pd
          INNER JOIN locations l ON pd.item_hash = l.item_hash
          WHERE pd.depends_on_hash = @depends_on_hash
            AND l.unlisted_at IS NULL
            AND l.branch_id IN ({branchInClause})
          ORDER BY pd.item_hash
          """
        |> Sql.parameters (
          [ "depends_on_hash", Sql.string dependsOnHash ] @ branchParams
        )
        |> Sql.executeAsync (fun read ->
          { itemHash = Hash(read.string "item_hash")
            itemKind = read.string "item_type" |> PT.ItemKind.fromString })
  }


/// Get Hashes that the given item depends on (forward dependencies / "what does this use?" / uses)
let getDependencies
  (branchChain : List<PT.BranchId>)
  (itemHash : Hash)
  : Task<List<PackageDep>> =
  task {
    if List.isEmpty branchChain then
      return []
    else
      let branchParams = branchChain |> List.mapi (fun i id -> $"b_{i}", Sql.uuid id)

      let branchInClause =
        branchChain |> List.mapi (fun i _ -> $"@b_{i}") |> String.concat ", "

      let (Hash itemHashStr) = itemHash

      return!
        Sql.query
          $"""
          SELECT DISTINCT pd.depends_on_hash, l.item_type
          FROM package_dependencies pd
          INNER JOIN locations l ON pd.depends_on_hash = l.item_hash
          WHERE pd.item_hash = @item_hash
            AND l.unlisted_at IS NULL
            AND l.branch_id IN ({branchInClause})
          ORDER BY pd.depends_on_hash
          """
        |> Sql.parameters ([ "item_hash", Sql.string itemHashStr ] @ branchParams)
        |> Sql.executeAsync (fun read ->
          { itemHash = Hash(read.string "depends_on_hash")
            itemKind = read.string "item_type" |> PT.ItemKind.fromString })
  }


/// Batch result including the dependency target that was queried
type BatchDependent =
  { dependsOnHash : Hash // The item that was queried (what is being depended on)
    itemHash : Hash // The item that has the dependency
    itemKind : PT.ItemKind }

/// Batch lookup of dependents for a chunk of dependency IDs
let private getDependentsBatchChunk
  (branchChain : List<PT.BranchId>)
  (dependsOnHashes : List<Hash>)
  : Task<List<BatchDependent>> =
  task {
    if List.isEmpty dependsOnHashes || List.isEmpty branchChain then
      return []
    else
      // Build parameterized IN clauses
      let depParams =
        dependsOnHashes
        |> List.mapi (fun i (Hash idStr) -> $"dep_{i}", Sql.string idStr)

      let inClause =
        dependsOnHashes |> List.mapi (fun i _ -> $"@dep_{i}") |> String.concat ", "

      let branchParams = branchChain |> List.mapi (fun i id -> $"b_{i}", Sql.uuid id)

      let branchInClause =
        branchChain |> List.mapi (fun i _ -> $"@b_{i}") |> String.concat ", "

      let sql =
        $"""
          SELECT DISTINCT pd.depends_on_hash, pd.item_hash, l.item_type
          FROM package_dependencies pd
          INNER JOIN locations l ON pd.item_hash = l.item_hash
          WHERE pd.depends_on_hash IN ({inClause})
            AND l.unlisted_at IS NULL
            AND l.branch_id IN ({branchInClause})
          ORDER BY pd.depends_on_hash, pd.item_hash
          """

      return!
        Sql.query sql
        |> Sql.parameters (depParams @ branchParams)
        |> Sql.executeAsync (fun read ->
          { dependsOnHash = Hash(read.string "depends_on_hash")
            itemHash = Hash(read.string "item_hash")
            itemKind = read.string "item_type" |> PT.ItemKind.fromString })
  }


/// Batch lookup of dependents for multiple dependency IDs.
/// Chunks requests to avoid SQLite expression tree depth limits.
let getDependentsBatch
  (branchChain : List<PT.BranchId>)
  (dependsOnHashes : List<Hash>)
  : Task<List<BatchDependent>> =
  task {
    if List.isEmpty dependsOnHashes then
      return []
    else
      // SQLite has a limit on expression tree depth (~1000)
      // Chunk into batches of 100 to stay well under the limit
      let chunks = dependsOnHashes |> List.chunkBySize 100

      let! results =
        chunks |> List.map (getDependentsBatchChunk branchChain) |> Task.flatten

      return results |> List.concat
  }


// ===========================================
// SCM Queries (branch-scoped)
// ===========================================

/// Get all WIP ops on a branch (commit_hash IS NULL)
let getWipOps (branchId : PT.BranchId) : Task<List<PT.PackageOp>> =
  task {
    return!
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        WHERE branch_id = @branch_id AND commit_hash IS NULL
        ORDER BY created_at ASC
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"
        BS.PT.PackageOp.deserialize opId opBlob)
  }


/// Summary of WIP changes
type WipSummary =
  { types : int64
    values : int64
    fns : int64
    renames : int64
    deprecations : int64
    total : int64 }


/// Get summary of WIP ops by type on a branch
let getWipSummary (branchId : PT.BranchId) : Task<WipSummary> =
  task {
    let! ops = getWipOps branchId

    let types =
      ops
      |> List.filter (function
        | PT.PackageOp.AddType _ -> true
        | _ -> false)
      |> List.length
      |> int64

    let values =
      ops
      |> List.filter (function
        | PT.PackageOp.AddValue _ -> true
        | _ -> false)
      |> List.length
      |> int64

    let fns =
      ops
      |> List.filter (function
        | PT.PackageOp.AddFn _ -> true
        | _ -> false)
      |> List.length
      |> int64

    let renames =
      ops
      |> List.filter (function
        | PT.PackageOp.SetName _ -> true
        | _ -> false)
      |> List.length
      |> int64

    // Deprecate and Undeprecate both count here — they're author intent changes.
    let deprecations =
      ops
      |> List.filter (function
        | PT.PackageOp.Deprecate _
        | PT.PackageOp.Undeprecate _ -> true
        | _ -> false)
      |> List.length
      |> int64

    let total = ops |> List.length |> int64

    return
      { types = types
        values = values
        fns = fns
        renames = renames
        deprecations = deprecations
        total = total }
  }


// CLEANUP: getWipItems, getWipOpCount, and getCommitCount exist as F# builtins
// purely for performance — they avoid sending large op lists to the Dark runtime.
// When Dark execution is fast enough, replace these with Dark implementations that
// use the existing getWipOps/getCommits builtins directly.

/// A WIP item on a branch (excludes auto-propagated ops)
type WipItem =
  { name : string; kind : string; modulePath : string; propagatedCount : int64 }

let private locationToModulePath (loc : PT.PackageLocation) : string =
  match loc.modules with
  | [] -> loc.owner
  | modules ->
    let modStr = String.concat "." modules
    $"{loc.owner}.{modStr}"

/// Get direct (non-propagated) WIP items on a branch.
/// Performs deserialization in F# and returns only the filtered results.
let getWipItems (branchId : PT.BranchId) : Task<List<WipItem>> =
  task {
    let! ops = getWipOps branchId

    // Collect propagated names from PropagateUpdate/RevertPropagation repoints
    let propagatedNames =
      ops
      |> List.collect (function
        | PT.PackageOp.PropagateUpdate(_, _, _, _, repoints) ->
          repoints |> List.map (fun rp -> PackageLocation.toFQN rp.location)
        | PT.PackageOp.RevertPropagation(_, _, _, _, repoints) ->
          repoints |> List.map (fun rp -> PackageLocation.toFQN rp.location)
        | _ -> [])
      |> Set.ofList

    // Count propagated repoints per source
    let propCounts : Map<string, int64> =
      ops
      |> List.choose (function
        | PT.PackageOp.PropagateUpdate(_, sloc, _, _, repoints) ->
          let name = PackageLocation.toFQN sloc
          let count = int64 repoints.Length
          Some(name, count)
        | _ -> None)
      |> List.fold
        (fun (acc : Map<string, int64>) (name, count) ->
          let existing = Map.tryFind name acc |> Option.defaultValue 0L
          Map.add name (existing + count) acc)
        Map.empty

    // Extract SetName ops, filter out propagated, deduplicate
    let kindDisplayName (k : PT.ItemKind) : string =
      match k with
      | PT.ItemKind.Type -> "Type"
      | PT.ItemKind.Fn -> "Fn"
      | PT.ItemKind.Value -> "Value"

    let items =
      ops
      |> List.choose (function
        | PT.PackageOp.SetName(loc, target) -> Some(kindDisplayName target.kind, loc)
        | _ -> None)
      |> List.map (fun (kind, loc) ->
        let name = PackageLocation.toFQN loc
        let modPath = locationToModulePath loc
        let pCount = Map.tryFind name propCounts |> Option.defaultValue 0L
        { name = name; kind = kind; modulePath = modPath; propagatedCount = pCount })
      |> List.filter (fun item -> not (Set.contains item.name propagatedNames))
      |> List.distinctBy (fun item -> item.name)

    return items
  }


/// Fast count of WIP ops on a branch (no deserialization)
let getWipOpCount (branchId : PT.BranchId) : Task<int64> =
  task {
    return!
      Sql.query
        """
        SELECT COUNT(*)
        FROM package_ops
        WHERE branch_id = @branch_id AND commit_hash IS NULL
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
      |> Sql.executeRowAsync (fun read -> read.int64 0)
  }


/// Fast count of commits on a branch (no deserialization)
let getCommitCount (branchId : PT.BranchId) : Task<int64> =
  task {
    return!
      Sql.query
        """
        SELECT COUNT(*)
        FROM commits
        WHERE branch_id = @branch_id
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
      |> Sql.executeRowAsync (fun read -> read.int64 0)
  }


/// Get commits on a branch ordered by date descending
let getCommits (branchId : PT.BranchId) (limit : int64) : Task<List<PT.Commit>> =
  task {
    return!
      Sql.query
        """
        SELECT c.hash, c.message, c.created_at,
               (SELECT COUNT(*) FROM package_ops WHERE commit_hash = c.hash) as op_count,
               c.account_id,
               a.name as committer_name,
               c.branch_id,
               b.name as branch_name
        FROM commits c
        JOIN branches b ON c.branch_id = b.id
        JOIN accounts_v0 a ON c.account_id = a.id
        WHERE c.branch_id = @branch_id
        ORDER BY c.created_at DESC
        LIMIT @limit
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branchId; "limit", Sql.int64 limit ]
      |> Sql.executeAsync (fun read ->
        { hash = Hash(read.string "hash")
          message = read.string "message"
          createdAt = read.instant "created_at"
          opCount = read.int64 "op_count"
          committerId = read.uuid "account_id"
          committerName = read.string "committer_name"
          branchId = read.uuid "branch_id"
          branchName = read.string "branch_name" })
  }


/// Get commits across the entire branch chain (current + ancestors), ordered by date descending.
let getCommitsForBranchChain
  (branchId : PT.BranchId)
  (limit : int64)
  : Task<List<PT.Commit>> =
  task {
    let! chain = Branches.getBranchChain branchId

    if List.isEmpty chain then
      return []
    else
      // Build parameterized IN clause for branch IDs
      let branchParams = chain |> List.mapi (fun i id -> $"bid_{i}", Sql.uuid id)
      let inClause =
        chain |> List.mapi (fun i _ -> $"@bid_{i}") |> String.concat ", "

      return!
        Sql.query
          $"""
          SELECT c.hash, c.message, c.created_at,
                 (SELECT COUNT(*) FROM package_ops WHERE commit_hash = c.hash) as op_count,
                 c.account_id,
                 a.name as committer_name,
                 c.branch_id,
                 b.name as branch_name
          FROM commits c
          JOIN branches b ON c.branch_id = b.id
          JOIN accounts_v0 a ON c.account_id = a.id
          WHERE c.branch_id IN ({inClause})
          ORDER BY c.created_at DESC
          LIMIT @limit
          """
        |> Sql.parameters (branchParams @ [ "limit", Sql.int64 limit ])
        |> Sql.executeAsync (fun read ->
          { hash = Hash(read.string "hash")
            message = read.string "message"
            createdAt = read.instant "created_at"
            opCount = read.int64 "op_count"
            committerId = read.uuid "account_id"
            committerName = read.string "committer_name"
            branchId = read.uuid "branch_id"
            branchName = read.string "branch_name" })
  }


/// Get ops for a specific commit
let getCommitOps (commitHash : Hash) : Task<List<PT.PackageOp>> =
  task {
    let (Hash commitHashStr) = commitHash
    return!
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        WHERE commit_hash = @commit_hash
        ORDER BY created_at ASC
        """
      |> Sql.parameters [ "commit_hash", Sql.string commitHashStr ]
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"
        BS.PT.PackageOp.deserialize opId opBlob)
  }


// ===========================================
// Propagation Queries
// ===========================================

/// Gets all Hashes that have ever been at a location across the branch chain.
/// Returns all distinct item_hashs (active or deprecated) at this location.
/// Callers should filter out the "current" hash to get only previous versions.
///
/// This is chain-aware so that the first update on a branch correctly finds the
/// parent's active version as a "previous" hash (enabling propagation).
let getAllPreviousHashes
  (branchChain : List<PT.BranchId>)
  (owner : string)
  (modules : string)
  (name : string)
  (itemType : string)
  : Task<List<Hash>> =
  task {
    if List.isEmpty branchChain then
      return []
    else
      let branchParams = branchChain |> List.mapi (fun i id -> $"b_{i}", Sql.uuid id)

      let branchInClause =
        branchChain |> List.mapi (fun i _ -> $"@b_{i}") |> String.concat ", "

      return!
        Sql.query
          $"""
          SELECT item_hash
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = @item_type
            AND branch_id IN ({branchInClause})
          GROUP BY item_hash
          ORDER BY MAX(CASE WHEN unlisted_at IS NULL THEN '9999-12-31' ELSE unlisted_at END) DESC
          """
        |> Sql.parameters (
          [ "owner", Sql.string owner
            "modules", Sql.string modules
            "name", Sql.string name
            "item_type", Sql.string itemType ]
          @ branchParams
        )
        |> Sql.executeAsync (fun read -> Hash(read.string "item_hash"))
  }


/// Current deprecation state for a single item on a branch chain.
/// None → not deprecated on this chain (or explicitly undeprecated by child).
/// Some (kind, message) → annotation from the latest non-superseded row.
let getCurrentDeprecation
  (branchChain : List<PT.BranchId>)
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  task {
    if List.isEmpty branchChain then
      return None
    else
      let (Hash itemHashStr) = itemHash
      let itemKindStr = itemKind.toString ()
      let branchParams = branchChain |> List.mapi (fun i id -> $"b_{i}", Sql.uuid id)
      let branchInClause =
        branchChain |> List.mapi (fun i _ -> $"@b_{i}") |> String.concat ", "

      let! row =
        Sql.query
          $"""
          SELECT state, annotation_blob
          FROM deprecations
          WHERE item_hash = @item_hash
            AND item_kind = @item_kind
            AND unlisted_at IS NULL
            AND branch_id IN ({branchInClause})
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters (
          [ "item_hash", Sql.string itemHashStr
            "item_kind", Sql.string itemKindStr ]
          @ branchParams
        )
        |> Sql.executeRowOptionAsync (fun read ->
          (read.string "state", read.bytesOrNone "annotation_blob"))

      match row with
      | Some("deprecated", Some blob) ->
        try
          use ms = new System.IO.MemoryStream(blob)
          use r = new System.IO.BinaryReader(ms)
          let kind =
            LibSerialization.Binary.Serializers.PT.PackageOp.DeprecationKind.read r
          let message = LibSerialization.Binary.Serializers.Common.String.read r
          return Some(kind, message)
        with _ ->
          return None
      | _ -> return None
  }


/// All currently-deprecated item hashes on a branch chain (any kind — Harmful,
/// Obsolete, or SupersededBy). Used to decorate list/tree/search output.
let getDeprecatedHashes (branchChain : List<PT.BranchId>) : Task<Set<Hash>> =
  task {
    if List.isEmpty branchChain then
      return Set.empty
    else
      let branchParams = branchChain |> List.mapi (fun i id -> $"b_{i}", Sql.uuid id)
      let branchInClause =
        branchChain |> List.mapi (fun i _ -> $"@b_{i}") |> String.concat ", "

      let! rows =
        Sql.query
          $"""
          SELECT DISTINCT item_hash
          FROM deprecations
          WHERE unlisted_at IS NULL
            AND state = 'deprecated'
            AND branch_id IN ({branchInClause})
          """
        |> Sql.parameters branchParams
        |> Sql.executeAsync (fun read -> Hash(read.string "item_hash"))

      return Set.ofList rows
  }


/// Deprecated hashes with no live direct caller — i.e. the set that `ls`/`tree`/
/// `search` should hide by default. A "live" caller is one whose hash is NOT
/// itself in the deprecated set. Direct only — we don't walk the dep graph
/// transitively; if A is live and calls B (deprecated) which calls C (deprecated),
/// C is hidden (its only caller is deprecated B), B is shown (A is live).
///
/// `package_dependencies` is global (keyed on hashes, not branches), so the
/// reverse-dep walk isn't branch-scoped — only the deprecation status is.
let getHiddenDeprecatedHashes (branchChain : List<PT.BranchId>) : Task<Set<Hash>> =
  task {
    let! deprecated = getDeprecatedHashes branchChain
    if Set.isEmpty deprecated then
      return Set.empty
    else
      let deprecatedStrs = deprecated |> Set.map (fun (Hash h) -> h)
      let hashList = deprecatedStrs |> Set.toList
      let hashParams = hashList |> List.mapi (fun i h -> $"h_{i}", Sql.string h)
      let hashInClause =
        hashList |> List.mapi (fun i _ -> $"@h_{i}") |> String.concat ", "

      // Find inbound refs for each deprecated hash, plus whether the caller
      // itself is deprecated. We just return (target, caller) pairs and let
      // F# decide; keeps the SQL simple.
      let! edges =
        Sql.query
          $"""
          SELECT depends_on_hash AS target, item_hash AS caller
          FROM package_dependencies
          WHERE depends_on_hash IN ({hashInClause})
          """
        |> Sql.parameters hashParams
        |> Sql.executeAsync (fun read ->
          (read.string "target", read.string "caller"))

      // Targets that have at least one live (non-deprecated) caller:
      let hasLiveCaller =
        edges
        |> List.filter (fun (_, caller) -> not (Set.contains caller deprecatedStrs))
        |> List.map fst
        |> Set.ofList

      // Hidden = deprecated AND no live caller.
      return
        deprecatedStrs
        |> Set.filter (fun h -> not (Set.contains h hasLiveCaller))
        |> Set.map Hash
  }


/// Load the set of package fn hashes currently marked `Harmful` on a
/// branch chain. Backs `PackageManager.isHarmful` via a per-branch cache,
/// which the interpreter consults before each package-fn call.
///
/// Logic mirrors `getAllPreviousHashes`:
/// - scope to branch chain
/// - latest non-superseded row wins (`unlisted_at IS NULL`)
/// - state = 'deprecated' with a Harmful annotation
let getHarmfulFnHashes (branchChain : List<PT.BranchId>) : Task<Set<Hash>> =
  task {
    if List.isEmpty branchChain then
      return Set.empty
    else
      let branchParams = branchChain |> List.mapi (fun i id -> $"b_{i}", Sql.uuid id)

      let branchInClause =
        branchChain |> List.mapi (fun i _ -> $"@b_{i}") |> String.concat ", "

      // Read latest non-superseded deprecation per (item_hash, item_kind)
      // filtered to fns. We read the annotation_blob and let F# decide if
      // it's Harmful; keeps the SQL schema simple.
      let! rows =
        Sql.query
          $"""
          SELECT item_hash, state, annotation_blob
          FROM deprecations
          WHERE item_kind = 'fn'
            AND unlisted_at IS NULL
            AND branch_id IN ({branchInClause})
          """
        |> Sql.parameters branchParams
        |> Sql.executeAsync (fun read ->
          (read.string "item_hash",
           read.string "state",
           read.bytesOrNone "annotation_blob"))

      let isHarmful (blob : byte array) : bool =
        try
          use ms = new System.IO.MemoryStream(blob)
          use r = new System.IO.BinaryReader(ms)
          let kind =
            LibSerialization.Binary.Serializers.PT.PackageOp.DeprecationKind.read r
          match kind with
          | PT.Harmful -> true
          | PT.SupersededBy _
          | PT.Obsolete -> false
        with _ ->
          // Malformed blob: fail closed (don't halt). Logging the issue
          // is left to the caller if they care.
          false

      let harmfulHashes =
        rows
        |> List.choose (fun (hashStr, state, blobOpt) ->
          match state, blobOpt with
          | "deprecated", Some blob when isHarmful blob -> Some(Hash hashStr)
          | _ -> None)

      return Set.ofList harmfulHashes
  }
