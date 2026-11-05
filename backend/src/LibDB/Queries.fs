module LibDB.Queries

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization



/// Make sure new edits get timestamps after everything already saved.
let ensureAuthoringClockIsCurrent () : Task<unit> =
  task {
    let! latest =
      Sql.query
        "SELECT MAX(stamp) AS stamp FROM
         (SELECT MAX(origin_ts) AS stamp FROM package_ops
          UNION ALL SELECT MAX(origin_ts) AS stamp FROM commits)"
      |> Sql.executeRowAsync (fun row -> row.stringOrNone "stamp")
    latest |> Option.iter OriginTs.observe
  }


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
          AND l.live = 1
        ORDER BY pd.depends_on_hash
        """
      |> Sql.parameters [ "item_hash", Sql.string itemHashStr ]
      |> Sql.executeAsync (fun read ->
        { itemHash = Hash(read.string "depends_on_hash")
          itemKind = read.string "item_type" |> PT.ItemKind.fromString })
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
///
/// Both sides bind their targets as one JSON parameter and join it, which is what lets SQLite use the
/// location index and the hash index: a row-value `IN` over an `OR` of the two scanned every edge in
/// the store, on every level of every propagation.
let getDependentHashesByTargets
  (targets : List<LocationTarget>)
  : Task<List<string>> =
  if List.isEmpty targets then
    Task.FromResult []
  else
    let locations =
      targets
      |> List.map (fun t ->
        [ t.itemKind.toString ()
          t.location.owner
          String.concat "." t.location.modules
          t.location.name ])
      |> List.distinct
    let hashes =
      targets
      |> List.collect (fun t ->
        t.hashes |> List.map (fun (Hash h) -> [ t.itemKind.toString (); h ]))
      |> List.distinct
    // Hashes only, by design: a branch's dependents are resolved to names in the caller's view.
    Sql.query
      "SELECT pd.item_hash FROM json_each(@locations) j CROSS JOIN package_dependencies pd
         ON pd.depends_on_item_type = json_extract(j.value, '$[0]')
        AND pd.depends_on_owner = json_extract(j.value, '$[1]')
        AND pd.depends_on_modules = json_extract(j.value, '$[2]')
        AND pd.depends_on_name = json_extract(j.value, '$[3]')
       UNION
       SELECT pd.item_hash FROM json_each(@hashes) j CROSS JOIN package_dependencies pd
         ON pd.depends_on_hash = json_extract(j.value, '$[1]')
        AND pd.depends_on_item_type = json_extract(j.value, '$[0]')
       WHERE pd.depends_on_owner IS NULL
       ORDER BY 1"
    |> Sql.parameters
      [ "locations", Sql.string (jsonRows locations)
        "hashes", Sql.string (jsonRows hashes) ]
    |> Sql.executeAsync (fun read -> read.string "item_hash")


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

/// Where each of <param hashes> lives on <param branch>, for the hashes still live there.
///
/// LIST-valued on purpose: content-addressing means one hash can be live at SEVERAL names
/// (every `(x: Int64): Int64 = x + 1L` in the store is literally the same item). Collapsing
/// to one location drops the other dependents from a propagation.
let getLiveLocationsForHashes
  (branch : PT.BranchId)
  (hashes : List<string>)
  : Task<Map<string, List<PT.ItemKind * PT.PackageLocation>>> =
  task {
    if List.isEmpty hashes then
      return Map.empty
    else
      let ps = hashes |> List.mapi (fun i h -> ($"h_{i}", Sql.string h))
      let inClause = hashes |> List.mapi (fun i _ -> $"@h_{i}") |> String.concat ", "

      let! rows =
        Sql.query (
          Layers.view "@" Layers.locations
          + $"""
          SELECT item_hash, item_type, owner, modules, name
          FROM visible
          WHERE item_hash IN ({inClause})
          """
        )
        |> Sql.parameters (ps @ Layers.sqlParams branch true)
        |> Sql.executeAsync (fun read ->
          let modulesStr = read.string "modules"
          read.string "item_hash",
          (read.string "item_type" |> PT.ItemKind.fromString,
           { owner = read.string "owner"
             modules = PackageLocation.modulesOfColumn modulesStr
             name = read.string "name" }))

      return groupToMap rows
  }




/// Main's DRAFT: main's ops not yet committed. This is what
/// `WipRefresh` re-resolves and rewrites. It deliberately excludes committed ops, because the
/// hash-stabilization it feeds keys items by name and keeps ONE version per name. That is right for
/// a draft, whose newest edit is the one that counts, and it destroys history the moment committed
/// ops go through it: every earlier committed version of every name disappears from the log.
///
/// Also returns the id of every row read, decodable or not, which is what
/// `Inserts.rewriteDraftIfUnchanged` checks the draft against.
let getDraftOpsWithIds () : Task<List<System.Guid> * List<PT.PackageOp>> =
  task {
    let! rows =
      Sql.query
        $"SELECT id, op_blob FROM package_ops WHERE {Inserts.mainDraftWhere}
          ORDER BY rowid"
      |> Sql.executeAsync (fun read ->
        let id = read.uuid "id"
        (id, BS.PT.PackageOp.tryDeserialize id (read.bytes "op_blob")))
    return (List.map fst rows, List.choose snd rows)
  }

let getDraftOps () : Task<List<PT.PackageOp>> =
  task {
    let! (_, ops) = getDraftOpsWithIds ()
    return ops
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
        WHERE branch_id = @main
        """
      |> Sql.parameters [ "main", Sql.string (string PT.BranchId.Main) ]
      |> Sql.executeAsync (fun read -> (read.uuid "id", read.string "origin_ts"))
    return Map.ofList rows
  }


/// What deprecation stands for an item as <param branchId> sees it: None when it is not deprecated
/// there, else the kind and message of the newest statement.
let getCurrentDeprecationFor
  (branchId : PT.BranchId)
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  Deprecations.standing branchId itemHash itemKind

/// <fn getCurrentDeprecationFor> on main.
let getCurrentDeprecation
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  Deprecations.standing PT.BranchId.Main itemHash itemKind


/// Deprecation info for `ls`/`tree`/`search`: the full deprecated-hash set plus the subset
/// hidden by default (deprecated AND with no live direct caller; "live" = not itself
/// deprecated).
///
/// Direct only, no transitive walk: if live A calls deprecated B which calls deprecated C,
/// C is hidden and B is shown.
type DeprecationSets = { allDeprecated : Set<Hash>; hidden : Set<Hash> }

/// <param branchId>'s deprecation sets.
let getDeprecationSetsFor (branchId : PT.BranchId) : Task<DeprecationSets> =
  task {
    let! rows = Deprecations.deprecatedIn branchId
    let deprecatedStrs = rows |> List.map (fun (h, _, _) -> h) |> Set.ofList
    if Set.isEmpty deprecatedStrs then
      return { allDeprecated = Set.empty; hidden = Set.empty }
    else
      // (target, caller) pairs -- "target" is deprecated by construction.
      // Caller is live iff it's not itself in deprecatedStrs.
      let! edges =
        Sql.query
          """
          SELECT depends_on_hash AS target, item_hash AS caller
          FROM package_dependencies
          WHERE depends_on_hash IN (SELECT value FROM json_each(@hashes))
          """
        |> Sql.parameters
          [ "hashes", Sql.string (jsonStrings (Set.toList deprecatedStrs)) ]
        |> Sql.executeAsync (fun read ->
          (read.string "target", read.string "caller"))

      let hasLiveCaller =
        edges
        |> List.filter (fun (_, caller) -> not (Set.contains caller deprecatedStrs))
        |> List.map fst
        |> Set.ofList

      return
        { allDeprecated = deprecatedStrs |> Set.map Hash
          hidden =
            deprecatedStrs
            |> Set.filter (fun h -> not (Set.contains h hasLiveCaller))
            |> Set.map Hash }
  }

/// When each impl was added, by the stamp of its `AddTraitImpl` op. Selection orders two
/// impls of one trait for one type by it (`LibExecution.Lww`), so the same call picks the
/// same impl on every instance. A row written before the column answers "", and a pair of
/// unstamped rivals is reported rather than picked.
let getTraitImplStamps () : Task<Map<string, string>> =
  task {
    let! rows =
      Sql.query "SELECT hash, origin_ts FROM package_trait_impls"
      |> Sql.executeAsync (fun read -> (read.string "hash", read.string "origin_ts"))
    return Map.ofList rows
  }


/// The impls <param branchId> sees deprecated. A deprecated impl is not a dispatch candidate:
/// deprecating one of two rivals is how the ambiguity finding says to settle it. The candidate path
/// and the display path both read this, so `dark view` and dispatch agree on a branch.
let getDeprecatedTraitImplHashesFor (branchId : PT.BranchId) : Task<Set<string>> =
  task {
    let! rows = Deprecations.deprecatedIn branchId
    return
      rows
      |> List.choose (fun (h, kind, _) ->
        if kind = PT.ItemKind.TraitImpl.toString () then Some h else None)
      |> Set.ofList
  }


/// The fns marked Harmful, read synchronously.
///
/// Synchronous on purpose: `PackageManager.isHarmful` answers before every package call and
/// cannot wait. It used to block on the async form below with `Async.RunSynchronously`, which
/// never returns in a browser tab, where there is one thread and nothing else to run the wait.
/// Boot preloaded the cache to dodge that, but authoring invalidates every cache, so the first
/// package call after any `dark fn` in a tab hung there for good.
let getHarmfulFnHashesSync () : Set<Hash> =
  // F# decides whether the annotation is Harmful, which keeps the SQL schema simple.
  let rows =
    Sql.query (
      Layers.view "@" Layers.deprecations
      + " SELECT item_hash, state, annotation_blob FROM visible WHERE item_kind = 'fn'"
    )
    |> Sql.parameters (Layers.sqlParams PT.BranchId.Main true)
    |> Sql.execute (fun read ->
      (read.string "item_hash",
       read.string "state",
       read.bytesOrNone "annotation_blob"))
    |> Result.unwrap

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

  rows
  |> List.choose (fun (hashStr, state, blobOpt) ->
    match state, blobOpt with
    | "deprecated", Some blob when isHarmful blob -> Some(Hash hashStr)
    | _ -> None)
  |> Set.ofList

let getHarmfulFnHashes () : Task<Set<Hash>> =
  Task.FromResult(getHarmfulFnHashesSync ())
