module LibDB.Queries

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


/// Deserialize an op_blob (as stored in package_ops) into a PackageOp. The one F# primitive
/// Dark needs to read STRUCTURED ops -- the binary format isn't Dark-decodable. The query
/// that selects the blobs lives in Dark (Stdlib.Sqlite); this is just the decode, so it
/// serves any op read. `id` is used only for error context, not the decode.
let deserializeOp (id : System.Guid) (opBlob : byte[]) : PT.PackageOp =
  BS.PT.PackageOp.deserialize id opBlob


/// A dependency relationship between package items.
/// For dependents: itemHash is the item that has the dependency.
/// For dependencies: itemHash is what the item depends on.
type PackageDep = { itemHash : Hash; itemKind : PT.ItemKind }


/// Get Hashes that the given item depends on (forward dependencies / "what does this
/// use?" / uses)
let getDependencies (itemHash : Hash) : Task<List<PackageDep>> =
  task {
    let (Hash itemHashStr) = itemHash

    return!
      Sql.query
        """
        SELECT DISTINCT pd.depends_on_hash, l.item_type
        FROM package_dependencies pd
        INNER JOIN locations l ON pd.depends_on_hash = l.item_hash
        WHERE pd.item_hash = @item_hash
          AND pd.depends_on_item_type = l.item_type
          AND l.unlisted_at IS NULL
        ORDER BY pd.depends_on_hash
        """
      |> Sql.parameters [ "item_hash", Sql.string itemHashStr ]
      |> Sql.executeAsync (fun read ->
        { itemHash = Hash(read.string "depends_on_hash")
          itemKind = read.string "item_type" |> PT.ItemKind.fromString })
  }


let getUnlistedLocationsForRefs
  (itemKind : PT.ItemKind)
  (hashes : List<Hash>)
  : Task<List<PT.PackageLocation>> =
  task {
    if List.isEmpty hashes then
      return []
    else
      let hashParams =
        hashes
        |> List.distinct
        |> List.mapi (fun i (Hash h) -> $"loc_ref_hash_{i}", Sql.string h)
      let hashInClause =
        hashParams
        |> List.mapi (fun i _ -> $"@loc_ref_hash_{i}")
        |> String.concat ", "

      return!
        Sql.query
          $"""
          SELECT DISTINCT owner, modules, name
          FROM locations
          WHERE item_hash IN ({hashInClause})
            AND item_type = @item_type
            AND unlisted_at IS NOT NULL
          """
        |> Sql.parameters (
          [ "item_type", Sql.string (itemKind.toString ()) ] @ hashParams
        )
        |> Sql.executeAsync (fun read ->
          let modulesStr = read.string "modules"
          { owner = read.string "owner"
            modules = modulesStr.Split('.') |> Array.toList
            name = read.string "name" })
  }


/// A dependent found via location-keyed lookup, paired with its own active location, so
/// propagation can drive the next cascade level without an extra hash -> location lookup.
type LocationDependent =
  { itemHash : Hash; itemKind : PT.ItemKind; itemLocation : PT.PackageLocation }

type LocationTarget =
  { itemKind : PT.ItemKind; location : PT.PackageLocation; hashes : List<Hash> }


/// Find items whose dep edges point at any of the given target package items.
///
/// Primary match: the edge's target kind + location equals one of the targets, which is what
/// keeps same-hash and same-location cross-kind cascades apart.
///
/// Fallback match: edges with NULL `depends_on_owner` match by `(item kind,
/// depends_on_hash)` instead -- there is no FQN to filter by, so a hash collision can still
/// produce a false positive there. Propagation passes prior hashes for this reason.
let private getDependentsByLocationsChunk
  (targets : List<LocationTarget>)
  : Task<List<string>> =
  task {
    if List.isEmpty targets then
      return []
    else
      let locParams =
        targets
        |> List.mapi (fun i target ->
          [ $"loc_kind_{i}", Sql.string (target.itemKind.toString ())
            $"loc_owner_{i}", Sql.string target.location.owner
            $"loc_modules_{i}",
            Sql.string (String.concat "." target.location.modules)
            $"loc_name_{i}", Sql.string target.location.name ])
        |> List.concat

      let locTuples =
        targets
        |> List.mapi (fun i _ ->
          $"(@loc_kind_{i}, @loc_owner_{i}, @loc_modules_{i}, @loc_name_{i})")
        |> String.concat ", "

      let hashParams =
        targets
        |> List.collect (fun target ->
          target.hashes |> List.map (fun hash -> target.itemKind, hash))
        |> List.distinct
        |> List.mapi (fun i (kind, Hash h) ->
          [ $"target_hash_kind_{i}", Sql.string (kind.toString ())
            $"target_hash_{i}", Sql.string h ])
        |> List.concat

      let hashFallbackClause =
        match hashParams with
        | [] ->
          $"""
          pd.depends_on_hash IN (
            SELECT tl.item_hash FROM visible_locations tl
            WHERE (tl.item_type, tl.owner, tl.modules, tl.name) IN ({locTuples})
              AND tl.unlisted_at IS NULL
          )
          """
        | _ ->
          let hashInClause =
            targets
            |> List.collect (fun target ->
              target.hashes |> List.map (fun hash -> target.itemKind, hash))
            |> List.distinct
            |> List.mapi (fun i _ -> $"(@target_hash_kind_{i}, @target_hash_{i})")
            |> String.concat ", "
          $"(pd.depends_on_item_type, pd.depends_on_hash) IN ({hashInClause})"

      // Return the dependent HASHES only. Resolving each to a location is the caller's
      // job: a branch's items deliberately have no rows in `locations` (that's the name
      // isolation), so joining here would make every branch-authored dependent invisible
      // to propagation. The caller merges the branch overlay over main and decides.
      let sql =
        $"""
          SELECT DISTINCT pd.item_hash
          FROM package_dependencies pd
          WHERE (
              (pd.depends_on_item_type,
               pd.depends_on_owner,
               pd.depends_on_modules,
               pd.depends_on_name)
                IN ({locTuples})
              OR (
                pd.depends_on_owner IS NULL
                AND {hashFallbackClause}
              )
            )
          -- By hash, not by name: this query deliberately does NOT join `locations` (see above), so there
          -- are no name columns to order by. Upstream orders the OTHER dependency query by name for
          -- readability, and that one does join.
          ORDER BY pd.item_hash
        """

      return!
        Sql.query sql
        |> Sql.parameters (locParams @ hashParams)
        |> Sql.executeAsync (fun read -> read.string "item_hash")
  }


/// Group (key, value) pairs into a list-valued map, preserving order within each key.
let private groupToMap
  (pairs : List<'k * 'v>)
  : Map<'k, List<'v>> when 'k : comparison =
  pairs
  |> List.fold
    (fun (m : Map<'k, List<'v>>) (k, v) ->
      let existing = Map.tryFind k m |> Option.defaultValue []
      Map.add k (existing @ [ v ]) m)
    Map.empty

/// Where each of <param hashes> lives in MAIN's projection, for the hashes still live there.
///
/// LIST-valued on purpose: content-addressing means one hash can be live at SEVERAL names
/// (every `(x: Int64): Int64 = x + 1L` in the store is literally the same item). Collapsing
/// to one location drops the other dependents from a propagation.
let getLiveLocationsForHashes
  (hashes : List<string>)
  : Task<Map<string, List<PT.ItemKind * PT.PackageLocation>>> =
  task {
    if List.isEmpty hashes then
      return Map.empty
    else
      let ps = hashes |> List.mapi (fun i h -> ($"h_{i}", Sql.string h))
      let inClause = hashes |> List.mapi (fun i _ -> $"@h_{i}") |> String.concat ", "

      let! rows =
        Sql.query
          $"""
          SELECT item_hash, item_type, owner, modules, name
          FROM locations
          WHERE item_hash IN ({inClause}) AND unlisted_at IS NULL
          """
        |> Sql.parameters ps
        |> Sql.executeAsync (fun read ->
          let modulesStr = read.string "modules"
          read.string "item_hash",
          (read.string "item_type" |> PT.ItemKind.fromString,
           { owner = read.string "owner"
             modules = modulesStr.Split('.') |> Array.toList
             name = read.string "name" }))

      return groupToMap rows
  }


/// Location-keyed batch lookup of dependents. Chunks the input list to
/// stay under SQLite's expression-tree depth limit.
let getDependentHashesByTargets
  (targets : List<LocationTarget>)
  : Task<List<string>> =
  task {
    if List.isEmpty targets then
      return []
    else
      let chunks = targets |> List.chunkBySize 100
      let! results = chunks |> List.map getDependentsByLocationsChunk |> Task.flatten
      return results |> List.concat |> List.distinct
  }


/// Every op NOT tagged to a branch, committed or not. Branch ops are branch-pending rather
/// than main WIP.
///
/// "WIP" does NOT mean "uncommitted": there is a `commit_hash` column and this deliberately
/// ignores it. `Draft.rebuild` re-inserts what this returns, so filtering to the draft here
/// would delete all of history and put back only the uncommitted part.
let getWipOps () : Task<List<PT.PackageOp>> =
  task {
    return!
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        -- Branch (op_branches-tagged) ops are effective=0 branch-pending state, NOT main WIP.
        -- Excluding them keeps main authoring's WIP-refresh from sweeping a branch's ops into
        -- main (re-inserting them effective=1 + folding). Branch isolation.
        WHERE id NOT IN (SELECT op_id FROM op_branches)
        ORDER BY created_at ASC
        """
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"
        BS.PT.PackageOp.deserialize opId opBlob)
  }


/// The COMMIT each main op was committed into, for ops that have one.
///
/// WipRefresh deletes and re-inserts the whole main log when a hash changes, so without
/// carrying this forward a refresh would un-commit the entire history.
let getWipOpCommits () : Task<Map<System.Guid, string>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT id, commit_hash
        FROM package_ops
        WHERE id NOT IN (SELECT op_id FROM op_branches)
          AND commit_hash IS NOT NULL
        """
      |> Sql.executeAsync (fun read -> (read.uuid "id", read.string "commit_hash"))
    return Map.ofList rows
  }


/// Map of WIP op id -> its current origin_ts, so WipRefresh can PRESERVE authoring stamps
/// across a discard+reinsert: an op that survives re-stabilization unchanged keeps its
/// original stamp. Without it a whole-store reinsert pushes `lastOriginTs` into the future
/// (the clock advances ~1ms per op), which makes the next genuine update look stale to LWW.
let getWipOpOriginTs () : Task<Map<System.Guid, string>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT id, origin_ts
        FROM package_ops
        WHERE id NOT IN (SELECT op_id FROM op_branches)
          AND origin_ts IS NOT NULL
        """
      |> Sql.executeAsync (fun read -> (read.uuid "id", read.string "origin_ts"))
    return Map.ofList rows
  }


/// The ops a commit committed, oldest first.
///
/// Ordered by `origin_ts` then rowid rather than `created_at`: `created_at` is local insert time, so a
/// synced commit's ops would come back in arrival order rather than the order they were authored in.
let getCommitOps (commitHash : Hash) : Task<List<PT.PackageOp>> =
  task {
    let (Hash commitHashStr) = commitHash
    return!
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        WHERE commit_hash = @commit_hash
        ORDER BY origin_ts ASC, rowid ASC
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

/// Gets all Hashes that have ever been at a location.
/// Returns all distinct item_hashs (active or deprecated) at this location.
/// Callers should filter out the "current" hash to get only previous versions.
let getAllPreviousHashes
  (owner : string)
  (modules : string)
  (name : string)
  (itemType : string)
  : Task<List<Hash>> =
  task {
    return!
      Sql.query
        """
        SELECT item_hash
        FROM locations
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND item_type = @item_type
        GROUP BY item_hash
        ORDER BY MAX(CASE WHEN unlisted_at IS NULL THEN '9999-12-31' ELSE unlisted_at END) DESC
        """
      |> Sql.parameters
        [ "owner", Sql.string owner
          "modules", Sql.string modules
          "name", Sql.string name
          "item_type", Sql.string itemType ]
      |> Sql.executeAsync (fun read -> Hash(read.string "item_hash"))
  }


/// Current deprecation state for a single item.
/// None -> not deprecated.
/// Some (kind, message) -> annotation from the latest non-superseded row.
let getCurrentDeprecation
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  task {
    let (Hash itemHashStr) = itemHash
    let itemKindStr = itemKind.toString ()

    let! row =
      Sql.query
        """
        SELECT state, annotation_blob
        FROM deprecations
        WHERE item_hash = @item_hash
          AND item_kind = @item_kind
          AND unlisted_at IS NULL
        ORDER BY created_at DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "item_hash", Sql.string itemHashStr; "item_kind", Sql.string itemKindStr ]
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


/// Deprecation info for `ls`/`tree`/`search`: the full deprecated-hash set plus the subset
/// hidden by default (deprecated AND with no live direct caller; "live" = not itself
/// deprecated).
///
/// Direct only, no transitive walk: if live A calls deprecated B which calls deprecated C,
/// C is hidden and B is shown.
type DeprecationSets = { allDeprecated : Set<Hash>; hidden : Set<Hash> }

let getDeprecationSets () : Task<DeprecationSets> =
  task {
    let! rows =
      Sql.query
        """
        SELECT DISTINCT item_hash
        FROM deprecations
        WHERE unlisted_at IS NULL
          AND state = 'deprecated'
        """
      |> Sql.executeAsync (fun read -> read.string "item_hash")

    let deprecatedStrs = Set.ofList rows
    if Set.isEmpty deprecatedStrs then
      return { allDeprecated = Set.empty; hidden = Set.empty }
    else
      let hashList = Set.toList deprecatedStrs
      let hashParams = hashList |> List.mapi (fun i h -> $"h_{i}", Sql.string h)
      let hashInClause =
        hashList |> List.mapi (fun i _ -> $"@h_{i}") |> String.concat ", "

      // (target, caller) pairs -- "target" is deprecated by construction.
      // Caller is live iff it's not itself in deprecatedStrs.
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

      let hasLiveCaller =
        edges
        |> List.filter (fun (_, caller) -> not (Set.contains caller deprecatedStrs))
        |> List.map fst
        |> Set.ofList

      let allDeprecated = deprecatedStrs |> Set.map Hash
      let hidden =
        deprecatedStrs
        |> Set.filter (fun h -> not (Set.contains h hasLiveCaller))
        |> Set.map Hash
      return { allDeprecated = allDeprecated; hidden = hidden }
  }


/// Load the set of package fn hashes currently marked `Harmful`. Backs
/// `PackageManager.isHarmful` via a cache, which the interpreter consults before each
/// package-fn call.
///
/// - latest non-superseded row wins (`unlisted_at IS NULL`)
/// - state = 'deprecated' with a Harmful annotation
let getHarmfulFnHashes () : Task<Set<Hash>> =
  task {
    // F# decides whether the annotation is Harmful, which keeps the SQL schema simple.
    let! rows =
      Sql.query
        """
        SELECT item_hash, state, annotation_blob
        FROM deprecations
        WHERE item_kind = 'fn'
          AND unlisted_at IS NULL
        """
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
        // A blob we cannot read means we cannot tell whether it says Harmful, and this answers "not
        // harmful", so the fn RUNS. That is failing open on a safety marking: chosen so one corrupt row
        // cannot brick a function, but it is a choice, and the opposite is defensible.
        false

    let harmfulHashes =
      rows
      |> List.choose (fun (hashStr, state, blobOpt) ->
        match state, blobOpt with
        | "deprecated", Some blob when isHarmful blob -> Some(Hash hashStr)
        | _ -> None)

    return Set.ofList harmfulHashes
  }


/// The explicit propagation choices of one kind that apply on <param branchId>: the
/// branch's own rows plus main's. A choice is (owner, modules, name) -> policy, where
/// `name = ""` covers a whole MODULE rather than one item.
///
/// MIRRORS `Darklang.SCM.Propagation`, which owns the same table and resolves it the same
/// most-specific-first way for display. Change both or neither, or `dark propagate` shows
/// one thing and the cascade does another.
///
/// Scoping matters in BOTH directions: without the filter main's cascade would honour a pin
/// made on an unrelated branch, and including main's rows is the inheritance half.
let private getPropagationPolicy
  (branchId : PT.BranchId)
  (policy : string)
  : Task<Set<string * string * string>> =
  task {
    // A main row is inherited ONLY where the branch has no row of its OWN for that key,
    // whatever policy that row names. Inheriting unconditionally puts the same key in both
    // the pin set and the follow set, and `isPinned` consults pins first, so main's `pin`
    // would beat the branch's explicit `follow`.
    let! rows =
      Sql.query
        "SELECT owner, modules, name FROM propagation_policy
         WHERE policy = @policy
           AND (branch_id = @branch
                OR (branch_id = @mainBranch
                    AND @branch <> @mainBranch
                    AND NOT EXISTS (
                      SELECT 1 FROM propagation_policy b
                      WHERE b.branch_id = @branch
                        AND b.owner = propagation_policy.owner
                        AND b.modules = propagation_policy.modules
                        AND b.name = propagation_policy.name)))"
      |> Sql.parameters
        [ "policy", Sql.string policy
          "branch", Sql.string (string branchId)
          "mainBranch", Sql.string (string PT.BranchId.Main) ]
      |> Sql.executeAsync (fun read ->
        (read.string "owner", read.string "modules", read.string "name"))
    return Set.ofList rows
  }

let getPropagationPins
  (branchId : PT.BranchId)
  : Task<Set<string * string * string>> =
  getPropagationPolicy branchId "pin"

/// Explicit `follow` rows. They matter only as OVERRIDES: an item marked follow
/// inside a module marked pin still follows. Without them the most-specific-first
/// walk would have nothing to stop at.
let getPropagationFollows
  (branchId : PT.BranchId)
  : Task<Set<string * string * string>> =
  getPropagationPolicy branchId "follow"
