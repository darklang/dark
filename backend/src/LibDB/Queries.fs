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
/// field. `backend/testfiles/execution/scm/sync-wire.dark` compares the two envelopes; change one shape
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
let getDraftOps () : Task<List<PT.PackageOp>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        -- effective = 1: excludes client-pushed inert ops; see Inserts.draftDeletes.
        WHERE effective = 1
          AND commit_hash IS NULL
          AND id NOT IN (SELECT op_id FROM op_branches)
        ORDER BY created_at ASC, rowid ASC
        """
      |> Sql.executeAsync (fun read ->
        BS.PT.PackageOp.tryDeserialize (read.uuid "id") (read.bytes "op_blob"))
    return rows |> List.choose (fun o -> o)
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


/// Every op NOT tagged to a branch, committed or not. Branch ops are branch-pending rather
/// than main WIP.
///
/// "WIP" does NOT mean "uncommitted": there is a `commit_hash` column and this deliberately
/// ignores it. `Draft.rebuild` re-inserts what this returns, so filtering to the draft here
/// would delete all of history and put back only the uncommitted part.
let getWipOps () : Task<List<PT.PackageOp>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        -- Branch (op_branches-tagged) ops are effective=0 branch-pending state, NOT main WIP.
        -- Excluding them keeps main authoring's WIP-refresh from sweeping a branch's ops into
        -- main (re-inserting them effective=1 + folding). Branch isolation.
        --
        -- effective = 1: excludes client-pushed inert ops; see Inserts.draftDeletes.
        WHERE effective = 1
          AND id NOT IN (SELECT op_id FROM op_branches)
        -- rowid breaks ties: created_at is second-resolution and a batch shares it, and the pairing
        -- downstream (HashStabilization) is by adjacency.
        ORDER BY created_at ASC, rowid ASC
        """
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"

        // Skips what this build cannot decode. A synced store holds ops it did not write, kept
        // unapplied on purpose so a later build can read them, so every reader of the main log
        // meets them. `Inserts.wholeMainDeletes` excludes the same ops from its DELETE, so skipping
        // one for reading never becomes deleting it for writing.
        BS.PT.PackageOp.tryDeserialize opId opBlob)

    return rows |> List.choose (fun o -> o)
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

/// <fn getCurrentDeprecation> as <param branchId> sees it.
///
/// The branch chain's own `Deprecate`/`Undeprecate` ops win over main's row, latest by `origin_ts`.
/// A branch that undeprecates something main deprecated sees it live; a branch that deprecates
/// something main calls live sees it deprecated, with its own kind and message. Main is an ordinary
/// id here, and its chain is empty, so this collapses to the plain read.
let getCurrentDeprecationFor
  (branchId : PT.BranchId)
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  task {
    let! ops = Branches.chainOverlayOps branchId
    let (Hash wanted) = itemHash

    // Last one wins: the ops arrive oldest-first, same as every other overlay fold here.
    let fromBranch =
      ops
      |> List.fold
        (fun acc op ->
          match op with
          | PT.PackageOp.Deprecate(target, kind, message) ->
            let (Hash h) = target.hash
            if h = wanted then Some(Some(kind, message)) else acc
          | PT.PackageOp.Undeprecate target ->
            let (Hash h) = target.hash
            if h = wanted then Some None else acc
          | _ -> acc)
        None

    match fromBranch with
    | Some answer -> return answer
    | None -> return! getCurrentDeprecation itemHash itemKind
  }


/// The deprecation state a BRANCH sees, layered over main's.
///
/// The fold is main-only by design -- a branch's ops sit inert and never reach a projection -- so a
/// `Deprecate` authored on a branch changed nothing there, and `view` on the branch went on calling
/// the item live. Names solve this with an in-memory overlay rather than a per-branch table
/// (`chainBindingsByHash`), and deprecations get the same treatment: main's rows, then the chain's
/// own Deprecate/Undeprecate ops applied in `origin_ts` order, last one winning.
///
/// `Undeprecate` is what lets a branch say "not here" about something main deprecated, which is the
/// ancestor-override the schema comment has always described.
///
/// Main is an ordinary branch id here, and its chain is empty, so this collapses to the plain read.
let private chainDeprecationOverlay
  (branchId : PT.BranchId)
  : Task<Map<string, bool>> =
  task {
    let! ops = Branches.chainOverlayOps branchId

    return
      ops
      |> List.fold
        (fun acc op ->
          match op with
          | PT.PackageOp.Deprecate(target, _, _) ->
            let (Hash h) = target.hash
            Map.add h true acc
          | PT.PackageOp.Undeprecate target ->
            let (Hash h) = target.hash
            Map.add h false acc
          | _ -> acc)
        Map.empty
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
          (fun acc h isDeprecated ->
            if isDeprecated then Set.add (Hash h) acc else Set.remove (Hash h) acc)
          mainSets.allDeprecated

      // `hidden` is "deprecated with no live caller", computed by the main query against main's
      // callers. A branch's own additions are not run through that: the caller graph it would need
      // is the branch's, and reporting them as hidden would hide items the branch still calls.
      return { allDeprecated = deprecated; hidden = mainSets.hidden }
  }

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
/// made on an unrelated branch, and inheriting from the chain above is the other half.
///
/// The whole CHAIN, not just (this branch, main). A branch off a branch inherits its parent's pins
/// the same way a first-level branch inherits main's: it forked from that state, so a pin the parent
/// made applies until the child says otherwise. Reading main's alone would let a child silently
/// follow something its parent had deliberately pinned.
let private getPropagationPolicy
  (branchId : PT.BranchId)
  (policy : string)
  : Task<Set<string * string * string>> =
  task {
    // Nearest first, and the NEAREST branch with a row for a key decides -- whatever policy that row
    // names. Taking every row in the chain would put one key in both the pin set and the follow set,
    // and `isPinned` consults pins first, so an ancestor's `pin` would beat a nearer `follow`.
    let! chain = Branches.branchChain branchId

    let! rows =
      Sql.query
        "SELECT branch_id, policy, owner, modules, name FROM propagation_policy"
      |> Sql.executeAsync (fun read ->
        (read.string "branch_id",
         read.string "policy",
         read.string "owner",
         read.string "modules",
         read.string "name"))

    let rank = chain |> List.mapi (fun i b -> (string b, i)) |> Map.ofList

    let nearestPerKey : List<(string * string * string) * string> =
      rows
      |> List.choose (fun (bid, pol, owner, modules, name) ->
        Map.tryFind bid rank
        |> Option.map (fun r -> ((owner, modules, name), (r, pol))))
      // `List.groupBy` here is Prelude's, which returns a Map rather than a list of pairs.
      |> List.groupBy fst
      |> Map.toList
      |> List.map (fun (key, candidates) ->
        let (_, pol) = candidates |> List.map snd |> List.minBy fst
        (key, pol))

    return
      nearestPerKey
      |> List.filter (fun (_, pol) -> pol = policy)
      |> List.map fst
      |> Set.ofList
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
