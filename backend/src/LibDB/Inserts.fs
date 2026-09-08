module LibDB.Inserts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
open LibSerialization.Hashing


/// The content-addressed id for a PackageOp. See `Hashing.computeOpRowId`, shared by every path
/// that mints or looks up an op id.
let computeOpHash (op : PT.PackageOp) : System.Guid = Hashing.computeOpRowId op


/// The authoring stamp. Defined once in `LibDB.OriginTs` -- see there for why it's monotonic and
/// why it must not be duplicated.
let nextOriginTs () : string = OriginTs.next ()


/// Insert PackageOps and fold them into the projections, resolving each op's origin_ts via `tsFor`
/// and its committing commit via `commitFor`. Same contract as `insertAndApplyOps`; the two
/// resolvers let callers PRESERVE existing values instead of resetting them for every op.
/// Which of these locations do NOT currently bind the hash the op names.
///
/// The filter that separates a REVERT from a no-op re-author: re-running the same authoring command
/// also produces a duplicate `SetName`, and that one really is nothing to do.
let private notCurrentlyBound (ops : List<PT.PackageOp>) : Task<List<PT.PackageOp>> =
  task {
    let candidates =
      ops
      |> List.choose (fun op ->
        match op with
        | PT.PackageOp.SetName(location, target, _) ->
          Some(op, location, target.hash)
        | _ -> None)

    if List.isEmpty candidates then
      return []
    else
      let keyParams =
        candidates
        |> List.mapi (fun i (_, location : PT.PackageLocation, _) ->
          ($"key_{i}",
           Sql.string (
             String.concat
               "\u0000"
               [ location.owner; String.concat "." location.modules; location.name ]
           )))

      let keyClause =
        candidates |> List.mapi (fun i _ -> $"@key_{i}") |> String.concat ", "

      let! rows =
        Sql.query
          $"""
          SELECT owner, modules, name, item_hash
          FROM locations
          WHERE unlisted_at IS NULL
            AND owner || char(0) || modules || char(0) || name IN ({keyClause})
          """
        |> Sql.parameters keyParams
        |> Sql.executeAsync (fun read ->
          ((read.string "owner", read.string "modules", read.string "name"),
           read.string "item_hash"))

      let live = Map.ofList rows

      return
        candidates
        |> List.filter (fun (_, location, Hash h) ->
          let key =
            (location.owner, String.concat "." location.modules, location.name)
          match Map.tryFind key live with
          | Some bound -> bound <> h
          | None -> true)
        |> List.map (fun (op, _, _) -> op)
  }


let rec insertAndApplyOpsWith
  (tsFor : System.Guid -> string)
  (commitFor : System.Guid -> string option)
  (source : string)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  task {
    if List.isEmpty ops then
      return 0L
    else
      // Phase 1: insert with applied=false. Stamps are assigned in list order, so sequential edits
      // within one wall-clock millisecond are still ordered by creation for the LWW.
      let opsWithIds =
        ops
        |> List.map (fun op ->
          let opId = computeOpHash op
          let opBlob = BS.PT.PackageOp.serialize opId op
          (opId, op, opBlob, tsFor opId, commitFor opId))

      // Two statements per op, one transaction. The id is the content hash: an
      // identical op main already runs affects 0 rows and is skipped below. But the
      // row can also exist at effective=0 (branch-authored, or synced for review);
      // main authoring it now means it runs here, so the conflict clause flips it
      // effective and it folds like a fresh insert. The untag goes in the same
      // breath: an effective op is never tagged (see Branches.storeDeltaOpsStamped).
      let statements =
        opsWithIds
        |> List.collect (fun (opId, _op, opBlob, originTs, commitHash) ->
          let insert =
            """
            INSERT INTO package_ops
              (id, op_blob, applied, origin_ts, commit_hash)
            VALUES (@id, @op_blob, @applied, @origin_ts, @commit_hash)
            ON CONFLICT(id) DO UPDATE
              SET effective = 1,
                  applied = 0,
                  origin_ts = excluded.origin_ts,
                  commit_hash = excluded.commit_hash
              WHERE package_ops.effective = 0
            """

          let parameters =
            [ "id", Sql.uuid opId
              "op_blob", Sql.bytes opBlob
              "applied", Sql.bool false
              "origin_ts", Sql.string originTs
              "commit_hash",
              (match commitHash with
               | Some h -> Sql.string h
               | None -> Sql.dbnull) ]

          let untag = "DELETE FROM op_branches WHERE op_id = @id"

          [ (insert, [ parameters ]); (untag, [ [ "id", Sql.uuid opId ] ]) ])

      // The insert's count per op; the untag's is not interesting.
      let rowsAffected =
        statements
        |> Sql.executeTransactionSync
        |> List.chunkBySize 2
        |> List.map (fun pair -> List.item 0 pair)

      // What was inserted, as opposed to skipped as a duplicate.
      let insertedCount = rowsAffected |> List.sumBy int64

      let insertedOpsWithIds =
        List.zip opsWithIds rowsAffected
        |> List.filter (fun (_, affected) -> affected > 0)
        |> List.map fst

      let opsToApply = insertedOpsWithIds |> List.map (fun (_, op, _, _, _) -> op)
      let insertedOpIds =
        insertedOpsWithIds |> List.map (fun (opId, _, _, _, _) -> opId)

      do! PackageOpPlayback.applyOpsFrom source opsToApply

      // An `Add*` the log already held is not folded again, and does not need to be, except for one
      // thing: the names its body reached its callees through in THIS parse. Two names can hold one
      // body, and a caller written against either is the same op; without this the second name's
      // callers had no edge under that name.
      let ignored =
        List.zip opsWithIds rowsAffected
        |> List.filter (fun (_, affected) -> affected = 0)
        |> List.map (fun ((_, op, _, _, _), _) -> op)
      do! PackageOpPlayback.recordDependenciesOnly ignored

      // A `SetName` already in the log, for a name bound to something else right now, is a revert:
      // unsayable as a `SetName` (`PT.restatingBinding`), so it is re-authored as the decision it
      // is. Never recurses -- what goes back in is a `Decision`, and only `SetName` produces one.
      let! toRestate = notCurrentlyBound ignored
      let! restated =
        if List.isEmpty toRestate then
          Task.FromResult 0L
        else
          toRestate
          |> List.choose (PT.restatingBinding (nextOriginTs ()))
          |> insertAndApplyOpsWith tsFor commitFor source

      // Bookkeeping only: the fold above already ran, so a failure here costs a redundant re-fold on
      // the next pass, not correctness.
      if not (List.isEmpty insertedOpIds) then
        try
          let updateStatements =
            insertedOpIds
            |> List.map (fun opId ->
              let sql =
                "UPDATE package_ops SET applied = @applied \
                 WHERE id = @id"
              let parameters = [ "applied", Sql.bool true; "id", Sql.uuid opId ]
              (sql, [ parameters ]))

          updateStatements |> Sql.executeTransactionSync |> ignore<List<int>>
        with ex ->
          System.Console.Error.WriteLine(
            $"Warning: Failed to mark {List.length insertedOpIds} ops as applied: {ex.Message}"
          )

      // The restatements count: they are ops this call authored, and a caller reporting "0 ops" for
      // a revert that did land would be the same lie in a different place.
      return insertedCount + restated
  }


/// Insert PackageOps and fold them into the projections. Returns the count actually inserted, so an
/// op the store already runs counts 0. Insert with applied=false, fold, then mark applied=true, so a
/// mid-fold failure leaves the ops identifiable and retryable. Commit-free: no commit_hash, so every
/// op is live.
/// The `owner` field is the first part of a package name, such as
/// `Darklang.Stdlib.List.map`. Names beginning with `Darklang` are treated as
/// bundled first-party code, so only trusted seeding may create those bindings;
/// guest and sync writes reject them.
let reservedOwners : Set<string> = Set.ofList [ "Darklang" ]

/// The first operation that binds OR unbinds a name under a protected owner.
/// `None` means no protected location is touched. (The old model also had to
/// chase renames unlisting other bindings of a shared hash; that heuristic is
/// gone -- a `SetName` changes exactly its own location, and retiring a name is
/// an explicit `Unbind` -- so the location arms here are the whole surface.)
let reservedOwnerViolation (ops : List<PT.PackageOp>) : Option<string> =
  let ownersBound (op : PT.PackageOp) : List<string> =
    match op with
    | PT.PackageOp.SetName(loc, _, _) -> [ loc.owner ]
    | PT.PackageOp.Unbind(loc, _) -> [ loc.owner ]
    | _ -> []
  ops
  |> List.collect ownersBound
  |> List.tryFind (fun owner -> Set.contains owner reservedOwners)
  |> Option.map (fun owner ->
    $"cannot bind a package name under the reserved owner \"{owner}\"; it is reserved for the bundled standard library")

/// Detect a parser placeholder instead of a real content hash. Placeholders
/// are empty or contain the package location; real hashes contain only hex
/// digits and are produced during stabilization.
let private isPlaceholderHash (PT.Hash hash : PT.Hash) : bool =
  hash = "" || hash |> Seq.exists (fun c -> not (System.Uri.IsHexDigit c))

/// A rejection reason when a package operation still has a placeholder hash.
/// `None` means all hashes are content hashes.
let placeholderHashViolation (ops : List<PT.PackageOp>) : Option<string> =
  let hashesOf (op : PT.PackageOp) : List<PT.Hash> =
    match op with
    | PT.PackageOp.AddType t -> [ t.hash ]
    | PT.PackageOp.AddValue v -> [ v.hash ]
    | PT.PackageOp.AddFn f -> [ f.hash ]
    | PT.PackageOp.SetName(_, target, _) -> [ target.hash ]
    | _ -> []
  ops
  |> List.collect hashesOf
  |> List.tryFind isPlaceholderHash
  |> Option.map (fun (PT.Hash h) ->
    $"cannot store package ops with the placeholder hash \"{h}\"; stabilize them first (WrittenTypesToProgramTypes.stabilizePackageOps)")

let insertAndApplyOps (ops : List<PT.PackageOp>) : Task<int64> =
  insertAndApplyOpsWith (fun _ -> nextOriginTs ()) (fun _ -> None) "op" ops


/// Insert ops that PROPAGATION authored, marking their bindings as such: 'propagation' joins 'op'
/// and 'resolution' in `locations.source`. Without it the bindings are indistinguishable from ones
/// you typed, and `dark commit` can't say which entries you edited and which followed. It has to be
/// recorded at the point of repoint, since rendering the two versions doesn't distinguish them
/// either: an older version's references resolve differently once superseded.
let insertAndApplyPropagatedOps (ops : List<PT.PackageOp>) : Task<int64> =
  insertAndApplyOpsWith (fun _ -> nextOriginTs ()) (fun _ -> None) "propagation" ops


/// Insert ops as main WIP (commit-free: no commit_hash, the op is live once folded).
let insertAndApplyOpsAsWip (ops : List<PT.PackageOp>) : Task<int64> =
  insertAndApplyOps ops


/// The draft's rows: main's uncommitted ops and the bindings they wrote. The `resolution` overlay is
/// kept; `discard` must not silently revert a synced resolution into a divergence.
///
/// `effective = 1` is the same clause `Queries.getWipOps` carries and for the same reason: ops a client
/// pushed to this store are inert, untagged and uncommitted, so without it a discard here deletes data
/// this store is only holding for someone else.
/// Safely insert package operations submitted by RUNNING Dark code -- a guest
/// `run`, or ops that arrived over sync. Rejects protected `Darklang` bindings
/// and unstabilized hashes before insertion; trusted seeding does not come
/// through here.
let insertUntrustedOps (ops : List<PT.PackageOp>) : Task<Result<int64, string>> =
  task {
    match reservedOwnerViolation ops, placeholderHashViolation ops with
    | Some reason, _
    | None, Some reason -> return Error reason
    | None, None ->
      let! count = insertAndApplyOpsAsWip ops
      return Ok count
  }

let draftDeletes : List<string> =
  [ "DELETE FROM locations WHERE source <> 'resolution'
     AND op_id IN (SELECT id FROM package_ops
                   WHERE effective = 1
                     AND commit_hash IS NULL
                     AND id NOT IN (SELECT op_id FROM op_branches))"
    "DELETE FROM package_ops
     WHERE effective = 1
       AND commit_hash IS NULL
       AND id NOT IN (SELECT op_id FROM op_branches)" ]

/// Every main op and what it wrote, EXCEPT the ids in `keep`: the ops this build cannot decode, which
/// the caller has read by id. Deleting those would delete a peer's committed op for good because this
/// binary is the wrong version to parse it; left in place they change no projection, and the next build
/// that can read them applies them. Branch-tagged ops are never main's and are left alone too.
let wholeMainDeletes (keep : Set<System.Guid>) : List<string> =
  let keepUnreadable =
    if Set.isEmpty keep then
      ""
    else
      let quoted =
        keep
        |> Set.toList
        |> List.map (fun (g : System.Guid) -> $"'{g.ToString()}'")
        |> String.concat ","
      $" AND id NOT IN ({quoted})"
  [ "DELETE FROM locations WHERE source <> 'resolution'"
    "DELETE FROM deprecations"
    // Decisions are folded from `Decision` ops like everything else, so a rewrite that re-folds
    // the surviving ops clears them first: otherwise a discarded pin loses its op and keeps the
    // pin, and the next edit honours a decision with nothing in the log behind it.
    //
    // Main only. A branch's rows are keyed by its own id, and no main rewrite may touch them.
    $"DELETE FROM propagation_policy WHERE branch_id = '{PT.BranchId.Main}'"
    // `effective = 1`: excludes client-pushed inert ops; see `draftDeletes`.
    $"DELETE FROM package_ops WHERE effective = 1 AND id NOT IN (SELECT op_id FROM op_branches){keepUnreadable}" ]

/// Main's op ids this build cannot decode. What `wholeMainDeletes` keeps.
let unreadableMainOpIds () : Task<Set<System.Guid>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        WHERE effective = 1
          AND id NOT IN (SELECT op_id FROM op_branches)
        """
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let readable =
          (BS.PT.PackageOp.tryDeserialize opId (read.bytes "op_blob"))
          |> Option.isSome
        (opId, readable))
    return rows |> List.filter (snd >> not) |> List.map fst |> Set.ofList
  }

/// Delete, re-insert and re-fold as ONE transaction. `deletes` run first, in order; then every op is
/// inserted (or, if its row survived the deletes at `effective = 0`, flipped effective and untagged, as
/// `insertAndApplyOpsWith` does) and the ones that landed are folded on the same connection; then the
/// commit. ONE transaction is the whole point: split across four (delete; insert; fold; mark applied)
/// a crash after the first deletes main's draft, or all of main, with nothing to put back.
///
/// `applied = 1` at insert is right because insert, fold and commit are one unit: a throw anywhere rolls
/// all of it back and the store is exactly as it was. The fold opens nothing of its own on a connection
/// it is handed, which is what lets it run inside this transaction; a Fumble call in here would open a
/// second connection and wait on the lock this one holds.
let rewriteOpsAtomically
  (deletes : List<string>)
  (tsFor : System.Guid -> string)
  (commitFor : System.Guid -> string option)
  (source : string)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  task {
    use conn = new Microsoft.Data.Sqlite.SqliteConnection(LibDB.Sqlite.connString)
    do! conn.OpenAsync()
    // Outside the transaction, where a pragma takes effect.
    do
      use pragma = conn.CreateCommand()
      pragma.CommandText <- "PRAGMA busy_timeout=5000;"
      pragma.ExecuteNonQuery() |> ignore<int>
    use tx = conn.BeginTransaction()
    // After BeginTransaction: a command created on the connection now carries the transaction.
    let ctx = PreparedBatch.newCtx conn
    try
      for d in deletes do
        do! PreparedBatch.exec ctx d (fun _ -> ())

      let inserted = ResizeArray<PT.PackageOp>()
      for op in ops do
        let opId = computeOpHash op
        let blob = BS.PT.PackageOp.serialize opId op
        let! n =
          PreparedBatch.execRows ctx "INSERT INTO package_ops (id, op_blob, applied, origin_ts, commit_hash)
             VALUES ($id, $blob, 1, $ts, $commit)
             ON CONFLICT(id) DO UPDATE
               SET effective = 1, applied = 1,
                   origin_ts = excluded.origin_ts, commit_hash = excluded.commit_hash
               WHERE package_ops.effective = 0" (fun cmd ->
            PreparedBatch.pUuid cmd "$id" opId
            PreparedBatch.p cmd "$blob" blob
            PreparedBatch.p cmd "$ts" (tsFor opId)
            PreparedBatch.pOpt cmd "$commit" (commitFor opId))
        do!
          PreparedBatch.exec
            ctx
            "DELETE FROM op_branches WHERE op_id = $id"
            (fun cmd -> PreparedBatch.pUuid cmd "$id" opId)
        if n > 0 then inserted.Add op

      do!
        PackageOpPlayback.applyOpsOnConnectionFrom conn source (List.ofSeq inserted)
      tx.Commit()
      Caching.invalidateAll ()
      return int64 inserted.Count
    finally
      PreparedBatch.disposeCtx ctx
  }


/// Bulk-import synced ops (id, op_blob-as-hex, origin_ts) in ONE transaction, committed into
/// <param commitHash> ("" = leave uncommitted). Arriving ops are somebody else's finished work,
/// not YOUR draft, so an import commits them on the way in; otherwise the first `dark status` after
/// a pull would report the peer's whole history as things you changed.
///
/// The decode-hex + bulk INSERT lives in F# because Dark's per-op insert is far too slow for a real
/// log. origin_ts is preserved (the LWW stamp), INSERT OR IGNORE dedups by content id, and ops land
/// unapplied for the caller to fold, at effective=1 so they take effect. Returns how many were
/// newly inserted.
let importOpsBulk
  (commitHash : string)
  (records : List<string * string * string>)
  : Task<int64> =
  task {
    if List.isEmpty records then
      return 0L
    else
      // Advance our clock past everything in this batch BEFORE anything is stored, so the next
      // thing authored here sorts after what we just learned about. See `OriginTs.observe`: a peer
      // whose clock is ahead would otherwise win every contested name forever.
      records |> List.iter (fun (_, _, originTs) -> OriginTs.observe originTs)

      // A malformed record (non-uuid id, non-hex blob) must SKIP rather than throw, or one bad
      // record on the wire rejects the whole batch. Blobs are validated again at fold time, which
      // also skips bad ops, so nothing malformed reaches a projection.
      let paramRows =
        records
        |> List.choose (fun (id, blobHex, originTs) ->
          try
            Some
              [ "id", Sql.uuid (System.Guid.Parse id)
                "op_blob", Sql.bytes (System.Convert.FromHexString blobHex)
                "origin_ts", Sql.string originTs
                "commit_hash",
                (if commitHash = "" then Sql.dbnull else Sql.string commitHash) ]
          with ex ->
            System.Console.Error.WriteLine(
              $"importOpsBulk: skipping malformed record id={id}: {ex.Message}"
            )
            None)

      if List.isEmpty paramRows then
        return 0L
      else
        let sql =
          """
          INSERT OR IGNORE INTO package_ops
            (id, op_blob, applied, effective, origin_ts, commit_hash)
          VALUES (@id, @op_blob, 0, 1, @origin_ts, @commit_hash)
          """

        // An op id is a content hash, so an op arriving from a peer's MAIN can already be here as
        // a branch's inert copy: the same code, authored on a branch that has not merged. The
        // insert above ignores it, so promote it -- it is a main op now, whatever else holds it.
        // `applied = 0` re-arms the fold, which is what binds the name; the branch keeps its tag,
        // the state a merge leaves behind. Only ever 0 -> 1: nothing here makes a main op inert.
        let promote =
          """
          UPDATE package_ops
             SET effective = 1,
                 applied = 0,
                 commit_hash = COALESCE(commit_hash, @commit_hash)
           WHERE id = @id AND effective = 0
          """

        let promoteRows =
          paramRows
          |> List.map (fun row ->
            row |> List.filter (fun (k, _) -> k = "id" || k = "commit_hash"))

        let affected = Sql.executeTransactionSync [ (sql, paramRows) ]
        let promoted = Sql.executeTransactionSync [ (promote, promoteRows) ]
        // Both counts: an op that was inert here and is now effective is as new to main as one
        // that never arrived at all.
        return (affected |> List.sumBy int64) + (promoted |> List.sumBy int64)
  }


/// RELAY store path: bulk-insert the pushed ops AND record ownership (op_id, owner) in ONE
/// transaction. Unlike importOpsBulk this does NOT fold: a relay serves op blobs, not projections.
/// The op_owners rows let it serve "your stuff" back by identity. Malformed records are skipped,
/// and owner="" stores ops without recording ownership. Returns the count of newly-stored ops.
let storeOpsWithOwner
  (owner : string)
  (records : List<string * string * string>)
  : Task<int64> =
  task {
    if List.isEmpty records then
      return 0L
    else
      let valid =
        records
        |> List.choose (fun (id, blobHex, originTs) ->
          try
            Some(
              System.Guid.Parse id,
              System.Convert.FromHexString blobHex,
              originTs
            )
          with ex ->
            System.Console.Error.WriteLine(
              $"storeOpsWithOwner: skipping malformed record id={id}: {ex.Message}"
            )
            None)

      if List.isEmpty valid then
        return 0L
      else
        let opRows =
          valid
          |> List.map (fun (id, blob, ts) ->
            [ "id", Sql.uuid id
              "op_blob", Sql.bytes blob
              "origin_ts", Sql.string ts ])

        // `effective = 0`: in the log, NEVER folded into this store's own main. Queued-for-folding
        // is not enough, since `growIfNeeded` folds everything `applied = 0 AND effective = 1` on
        // the next startup. A client pushes its whole log, package tree included, and names bind
        // last-writer-wins over the whole store -- `Darklang.Matter.router` among them -- so anyone
        // who could write to a relay could change what that relay itself runs. Hosted ops are DATA:
        // the relay serves the blobs back verbatim and its own code stays what its binary seeded.
        let insertOps =
          "INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts)
           VALUES (@id, @op_blob, 0, 0, @origin_ts)"

        let statements =
          if owner = "" then
            [ (insertOps, opRows) ]
          else
            let ownerRows =
              valid
              |> List.map (fun (id, _, _) ->
                [ "op_id", Sql.uuid id; "owner", Sql.string owner ])

            let insertOwners =
              "INSERT OR IGNORE INTO op_owners (op_id, owner) VALUES (@op_id, @owner)"

            [ (insertOps, opRows); (insertOwners, ownerRows) ]

        // One transaction; the ops-insert counts come first (statement order), so truncate to the
        // op rows to report NEW ops rather than owner rows.
        let affected = Sql.executeTransactionSync statements
        return affected |> List.truncate (List.length opRows) |> List.sumBy int64
  }


/// Commit every currently-uncommitted MAIN op into one commit. The package RELOAD path uses this:
/// the `.dark` files on disk are the shipped baseline, not your uncommitted draft, so leaving them
/// uncommitted would open every `dark status` on "5,000 items changed".
///
/// The hash is derived from what it commits (message + count + newest stamp), so two instances that
/// reload the same packages compute the same id rather than inventing divergent ones.
///
/// DEV CAVEAT: a reload sweeps a genuine un-committed local draft into the baseline commit too.
/// That's tolerable only because reload is a dev-loop tool.
let commitAllAsBaseline (message : string) : Task<string> =
  task {
    let! summary =
      Sql.query
        """
        SELECT COUNT(*) AS n, COALESCE(MAX(origin_ts), '') AS latest
        FROM package_ops
        WHERE commit_hash IS NULL AND id NOT IN (SELECT op_id FROM op_branches)
        """
      |> Sql.executeRowAsync (fun read -> (read.int64 "n", read.string "latest"))

    let (count, latest) = summary

    if count = 0L then
      return ""
    else
      let material = $"{message}|{count}|{latest}"

      let hash =
        material
        |> System.Text.Encoding.UTF8.GetBytes
        |> System.Security.Cryptography.SHA256.HashData
        |> System.Convert.ToHexString
        |> fun h -> h.Substring(0, 16).ToLowerInvariant()

      do!
        Sql.query
          "INSERT OR REPLACE INTO commits (hash, message, author, origin_ts)
           VALUES (@hash, @message, 'system', @origin_ts)"
        |> Sql.parameters
          [ "hash", Sql.string hash
            "message", Sql.string message
            "origin_ts", Sql.string (nextOriginTs ()) ]
        |> Sql.executeStatementAsync

      do!
        Sql.query
          "UPDATE package_ops SET commit_hash = @hash
           WHERE commit_hash IS NULL AND id NOT IN (SELECT op_id FROM op_branches)"
        |> Sql.parameters [ "hash", Sql.string hash ]
        |> Sql.executeStatementAsync

      // A commit NAMES the ops it committed; it doesn't snapshot anything. The authoring refresh
      // rewrites main by delete-and-reinsert, and an op whose content changed comes back with a
      // new id and no commit, so an older baseline can end up naming nothing at all. Those rows
      // are tombstones, and `dark commits` otherwise fills up with commits over an empty set.
      do!
        Sql.query
          "DELETE FROM commits WHERE hash NOT IN
             (SELECT DISTINCT commit_hash FROM package_ops WHERE commit_hash IS NOT NULL)"
        |> Sql.executeStatementAsync

      return hash
  }
