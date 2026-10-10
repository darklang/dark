module LibDB.Queries

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


/// Render one page of the sync wire format straight from the database: the relay's whole answer to
/// `/sync/pull`. Returns the JSON and the cursor to hand back, which is the largest rowid on the page
/// (or `sinceSeq` when the page is empty, so a client at the end does not rewind).
///
/// Native per AGENTS.md's operations-per-frame rule: a page in Dark hex-encodes
/// 2,000 blobs through interpreted calls. The DECISIONS (cursor, page size,
/// identity) stay in Dark; only the encoding is here.
///
/// Serves ops NOT TAGGED TO A BRANCH, which is the line between two populations that both sit at
/// `effective = 0` and cannot be told apart by that column: ops a client PUSHED, which a relay stores
/// inert on purpose and must serve back, and this store's OWN unmerged branch work, which must not
/// leave through an unauthenticated route. `/sync/pull` needs no secret; reading a branch does. A
/// self-hosted relay is also somebody's authoring instance, so the second population is real there.
///
/// This is the SECOND writer of that shape; `SCM.Wire.wireEncodeAt` is the first, and the relay picks
/// between them per request, so a client meets both on the same endpoint. They have to agree field for
/// field. `packages/darklang/tests/scm/wire.dark` compares the two envelopes; change one shape
/// and change the other.
let exportPageJson
  (sinceSeq : int64)
  (limit : int64)
  (formatVersion : int64)
  (darkBuild : string)
  (kernelHash : string)
  (owner : string)
  : Task<string * int64> =
  task {
    let! rows =
      Sql.query
        """
        SELECT p.id, p.op_blob, p.origin_ts AS ts, p.rowid AS seq,
               (SELECT o.owner FROM op_owners o WHERE o.op_id = p.id LIMIT 1) AS author,
               COALESCE(p.commit_hash, '') AS commit_id
        FROM package_ops p
        WHERE p.rowid > @sinceSeq
          AND p.id NOT IN (SELECT op_id FROM op_branches)
        ORDER BY p.rowid ASC LIMIT @limit
        """
      |> Sql.parameters [ "sinceSeq", Sql.int64 sinceSeq; "limit", Sql.int64 limit ]
      |> Sql.executeAsync (fun read ->
        (read.string "id",
         read.bytes "op_blob",
         read.stringOrNone "ts" |> Option.defaultValue "",
         read.int64 "seq",
         read.stringOrNone "author" |> Option.defaultValue "",
         read.string "commit_id"))

    let cursor =
      rows |> List.fold (fun acc (_, _, _, seq, _, _) -> max acc seq) sinceSeq

    // The commit rows the page's ops name, so the receiver can file each op under the author's commit.
    // Rows, not a chain: see `SCM.Wire.SyncBundle.commits`.
    let commitHashes =
      rows
      |> List.choose (fun (_, _, _, _, _, c) -> if c = "" then None else Some c)
      |> List.distinct

    let! commits =
      if List.isEmpty commitHashes then
        Task.FromResult []
      else
        Sql.query
          """
          SELECT hash, message, author, origin_ts, COALESCE(parent, '') AS parent
          FROM commits WHERE hash IN (SELECT value FROM json_each(@hashes))
          """
        |> Sql.parameters
          [ "hashes",
            // Built by hand, not by `JsonSerializer.Serialize`: that is reflection-based, and a
            // published build can have reflection stripped -- which would make every pull fail
            // here, on the one path a relay serves most. A list of hashes needs no serializer.
            // (`PackageOpPlayback` builds its id array the same way, for the same reason.)
            Sql.string (
              "["
              + (commitHashes
                 |> List.map (fun (h : string) -> "\"" + h.Replace("\"", "") + "\"")
                 |> String.concat ",")
              + "]"
            ) ]
        |> Sql.executeAsync (fun read ->
          (read.string "hash",
           read.string "message",
           read.string "author",
           read.string "origin_ts",
           read.string "parent"))

    let out = new System.IO.MemoryStream()

    let render () =
      use writer = new System.Text.Json.Utf8JsonWriter(out)
      writer.WriteStartObject()
      writer.WriteNumber("formatVersion", formatVersion)
      writer.WriteString("darkBuild", darkBuild)
      writer.WriteString("kernelHash", kernelHash)
      writer.WriteString("owner", owner)
      writer.WriteNumber("cursor", cursor)
      writer.WriteStartArray("ops")

      for (id, blob, ts, _, author, commit) in rows do
        writer.WriteStartObject()
        writer.WriteString("id", id)
        // Lowercase hex, matching `Stdlib.Blob.toHex`, because clients decode it with the same rules.
        writer.WriteString("blobHex", System.Convert.ToHexStringLower blob)
        writer.WriteString("ts", ts)
        writer.WriteString("author", author)
        writer.WriteString("commit", commit)
        writer.WriteEndObject()

      writer.WriteEndArray()
      writer.WriteStartArray("commits")

      for (hash, message, author, originTs, parent) in commits do
        writer.WriteStartObject()
        writer.WriteString("hash", hash)
        writer.WriteString("message", message)
        writer.WriteString("author", author)
        writer.WriteString("originTs", originTs)
        writer.WriteString("parent", parent)
        writer.WriteEndObject()

      writer.WriteEndArray()
      writer.WriteEndObject()

    render ()
    return (System.Text.Encoding.UTF8.GetString(out.ToArray()), cursor)
  }


/// Whether an op binds a name, read from its tag without decoding it. See
/// `BS.PT.PackageOp.bindsAName` for why this is worth having.
let opBindsAName (opBlob : byte[]) : bool = BS.PT.PackageOp.bindsAName opBlob


/// Decode an op_blob into a PackageOp. The one F# primitive Dark needs to read STRUCTURED ops, since
/// the binary format is not Dark-decodable; the query that selects the blobs is Dark. `id` is error
/// context only.
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
          // `locations`, main-scoped, which is what this whole query is. This arm is unreachable in
          // practice, since every caller pre-filters the empty-target case, and it must still name a
          // real table so that stops being load-bearing.
          $"""
          pd.depends_on_hash IN (
            SELECT tl.item_hash FROM locations tl
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


/// Main's DRAFT: the ops not yet committed and not tagged to a branch. This is what
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
        $"SELECT id, op_blob FROM package_ops WHERE {Inserts.draftWhere}
          ORDER BY created_at ASC, rowid ASC"
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


/// The mask that keeps main's uncommitted draft out of a branch's view: for every name whose LIVE
/// binding was written by a draft op, a synthetic `SetName` back to the name's last COMMITTED
/// version, or a synthetic `Unbind` when the name was born in the draft. Prepended to a branch's
/// overlay, so a branch resolves through committed main plus its own work -- the branch's own ops
/// come later in the overlay list and win over the mask.
///
/// Synthetic means never stored: these ops exist only inside an in-memory overlay PM.
let mainDraftMaskOps () : Task<List<PT.PackageOp>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT l.owner, l.modules, l.name, l.item_type,
          (SELECT l2.item_hash
           FROM locations l2 JOIN package_ops p2 ON p2.id = l2.op_id
           WHERE l2.owner = l.owner AND l2.modules = l.modules AND l2.name = l.name
             AND l2.source <> 'unbind' AND p2.commit_hash IS NOT NULL
           ORDER BY l2.origin_ts DESC LIMIT 1) AS committed_hash
        FROM locations l JOIN package_ops p ON p.id = l.op_id
        WHERE l.unlisted_at IS NULL AND l.source <> 'unbind'
          AND p.commit_hash IS NULL
          AND p.id NOT IN (SELECT op_id FROM op_branches)
        """
      |> Sql.executeAsync (fun read ->
        let loc : PT.PackageLocation =
          { owner = read.string "owner"
            modules = (read.string "modules").Split('.') |> Array.toList
            name = read.string "name" }
        let kind = read.string "item_type" |> PT.ItemKind.fromString
        (loc, kind, read.stringOrNone "committed_hash"))
    return
      rows
      |> List.map (fun (loc, kind, committed) ->
        match committed with
        | Some hash ->
          PT.PackageOp.SetName(
            loc,
            PT.Reference.fromHashAndKind (PT.Hash hash, kind),
            None
          )
        | None -> PT.PackageOp.Unbind(loc, None))
  }


/// One row of main's log as `getMainOpsWithIds` read it. `op` is None when this build cannot decode it.
type MainOpRow =
  { id : System.Guid
    op : Option<PT.PackageOp>
    originTs : Option<string>
    commitHash : Option<string> }

/// Every op NOT tagged to a branch, committed or not, decodable or not, from one SELECT over
/// `Inserts.mainWhere`. Branch ops are branch-pending rather than main WIP.
///
/// "WIP" does NOT mean "uncommitted": `Draft.rebuild` re-inserts what this returns, so filtering to the
/// draft here would delete all of history and put back only the uncommitted part. And it reads the
/// stamps, the commits and what it could not decode from the same rows, because the rebuild deletes by
/// that clause and four separate reads could each see a different log.
let getMainOpsWithIds () : Task<List<MainOpRow>> =
  task {
    return!
      Sql.query
        // rowid breaks ties: created_at is second-resolution and a batch shares it, and the pairing
        // downstream (HashStabilization) is by adjacency.
        $"SELECT id, op_blob, origin_ts, commit_hash FROM package_ops
          WHERE {Inserts.mainWhere}
          ORDER BY created_at ASC, rowid ASC"
      |> Sql.executeAsync (fun read ->
        let id = read.uuid "id"
        { id = id
          op = BS.PT.PackageOp.tryDeserialize id (read.bytes "op_blob")
          originTs = read.stringOrNone "origin_ts"
          commitHash = read.stringOrNone "commit_hash" })
  }

/// Main's decodable ops, committed or not. See `getMainOpsWithIds`. Skipping an op for reading never
/// becomes deleting it for writing, because `Inserts.wholeMainDeletes` spares the same ops by id.
let getWipOps () : Task<List<PT.PackageOp>> =
  task {
    let! rows = getMainOpsWithIds ()
    return rows |> List.choose _.op
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
///
/// Skips what this build cannot decode, like every other reader of the log: a commit can hold a peer's
/// op on a newer format, and a listing is the last thing that should die because one is in the way.
let getCommitOps (commitHash : Hash) : Task<List<PT.PackageOp>> =
  task {
    let (Hash commitHashStr) = commitHash
    let! decoded =
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        WHERE commit_hash = @commit_hash
        ORDER BY origin_ts ASC, rowid ASC
        """
      |> Sql.parameters [ "commit_hash", Sql.string commitHashStr ]
      |> Sql.executeAsync (fun read ->
        BS.PT.PackageOp.tryDeserialize (read.uuid "id") (read.bytes "op_blob"))
    return decoded |> List.choose (fun o -> o)
  }


/// Current deprecation state for a single item.
/// None -> not deprecated.
/// Some (kind, message) -> annotation from the latest non-superseded row.
let getCurrentDeprecation
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  // The read itself is `Deprecations.standing`, shared with the fold's writer and with the
  // authoring paths that ask whether an op they are about to drop says what already stands.
  Deprecations.standing itemHash itemKind


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
/// <fn getCurrentDeprecation> as <param branchId> sees it.
///
/// The branch chain's own `Deprecate`/`Undeprecate` ops win over main's row, latest by `origin_ts`.
/// A branch that undeprecates something main deprecated sees it live; a branch that deprecates
/// something main calls live sees it deprecated, with its own kind and message. Main is an ordinary
/// id here, and its chain is empty, so this collapses to the plain read.
/// What a branch's own chain says about each item's deprecation: `Some(kind, message)` where the
/// chain deprecates it, `None` where the chain UNdeprecates it, and absent where the chain is
/// silent and main's answer stands.
///
/// The fold is main-only by design: a branch's ops sit inert and never reach a projection, so a
/// `Deprecate` authored on a branch would change nothing there. Names solve that with an in-memory
/// overlay rather than a per-branch table (`chainBindingsByHash`), and deprecations get the same
/// treatment. Ops arrive oldest-first and the last one wins.
///
/// `Undeprecate` is what lets a branch say "not here" about something main deprecated, which is the
/// ancestor-override the schema comment describes. Main is an ordinary id here with an empty chain,
/// so both readers below collapse to the plain main read.
///
/// The fold itself is `Deprecations.chainStanding`, shared with the authoring side rather than
/// written again here: it also has to know what stands on a branch, to decide whether the op it is
/// about to drop as a duplicate says something new. Two copies could disagree, and then what
/// authoring decides and what this reads are answers about the same branch that do not match.
let private chainDeprecationOverlay
  (branchId : PT.BranchId)
  : Task<Map<string, PT.ItemKind * Option<PT.DeprecationKind * string>>> =
  task {
    let! ops = Branches.chainOverlayOps branchId
    return Deprecations.chainStanding ops
  }


/// <fn getCurrentDeprecation> as <param branchId> sees it.
let getCurrentDeprecationFor
  (branchId : PT.BranchId)
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  task {
    let! overlay = chainDeprecationOverlay branchId
    let (Hash wanted) = itemHash

    match Map.tryFind wanted overlay with
    | Some(_kind, answer) -> return answer
    | None -> return! getCurrentDeprecation itemHash itemKind
  }


/// <fn getDeprecationSets> as <param branchId> sees it: main's, plus what the branch chain says.
let getDeprecationSetsFor (branchId : PT.BranchId) : Task<DeprecationSets> =
  task {
    let! mainSets = getDeprecationSets ()
    let! overlay = chainDeprecationOverlay branchId

    if Map.isEmpty overlay then
      return mainSets
    else
      let deprecated =
        overlay
        |> Map.fold
          (fun acc h entry ->
            match entry with
            | _kind, Some _ -> Set.add (Hash h) acc
            | _kind, None -> Set.remove (Hash h) acc)
          mainSets.allDeprecated

      // `hidden` is "deprecated with no live caller", computed by the main query against main's
      // callers. A branch's own additions are not run through that: the caller graph it would need
      // is the branch's, and reporting them as hidden would hide items the branch still calls.
      return { allDeprecated = deprecated; hidden = mainSets.hidden }
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


/// The impls main has deprecated. A deprecated impl is not a dispatch candidate:
/// deprecating one of two rivals is how the ambiguity finding says to settle it.
///
/// Main's answer only. Every caller wants <fn getDeprecatedTraitImplHashesFor> instead, which is
/// why this one is private.
let private getDeprecatedTraitImplHashesOnMain () : Task<Set<string>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT DISTINCT d.item_hash
        FROM deprecations d
        WHERE d.item_kind = 'impl'
          AND d.unlisted_at IS NULL
          AND d.state = 'deprecated'
          AND NOT EXISTS (
            SELECT 1 FROM deprecations later
            WHERE later.item_hash = d.item_hash
              AND later.item_kind = d.item_kind
              AND later.unlisted_at IS NULL
              AND later.state <> 'deprecated'
              AND COALESCE(later.origin_ts, '') > COALESCE(d.origin_ts, ''))
        """
      |> Sql.executeAsync (fun read -> read.string "item_hash")
    return Set.ofList rows
  }


/// <fn getDeprecatedTraitImplHashesOnMain> as <param branchId> sees it.
///
/// `deprecations` has no `branch_id`: a branch's `Deprecate` op sits inert in the op log and never
/// reaches the projection, the same as a branch's names. Names answer that with an in-memory
/// overlay and so does this, so an impl retired on a branch stops being a candidate THERE and
/// stays one on main, and an impl main retired can be revived on a branch with `Undeprecate`.
///
/// The candidate path and the display path both read this, so `dark view` and dispatch agree on a
/// branch. Main is an ordinary id with an empty chain, so main collapses to the plain read.
let getDeprecatedTraitImplHashesFor (branchId : PT.BranchId) : Task<Set<string>> =
  task {
    let! onMain = getDeprecatedTraitImplHashesOnMain ()
    let! overlay = chainDeprecationOverlay branchId

    if Map.isEmpty overlay then
      return onMain
    else
      return
        overlay
        |> Map.fold
          (fun acc h entry ->
            match entry with
            | PT.ItemKind.TraitImpl, Some _ -> Set.add h acc
            | PT.ItemKind.TraitImpl, None -> Set.remove h acc
            | _ -> acc)
          onMain
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
    Sql.query
      """
      SELECT item_hash, state, annotation_blob
      FROM deprecations
      WHERE item_kind = 'fn'
        AND unlisted_at IS NULL
      """
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
