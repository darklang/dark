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


/// Which of these doc ops say something the NAME does not currently say.
///
/// The doc half of the question `notCurrentlyBound` asks about bindings. A doc op the log already
/// holds is either a re-run of the same command (the register already says this, nothing to do) or
/// a RESTATEMENT: putting the text back to something the name held before, which is unsayable as
/// itself because ops are content-addressed. The register is what tells them apart.
///
/// A name with no row has never had a doc edited: its text comes from the declaration, so an op
/// saying something else is new rather than a restatement of the register.
let private docsNotCurrentlySaid
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageOp>> =
  task {
    let candidates =
      ops
      |> List.choose (fun op ->
        match op with
        | PT.PackageOp.UpdateDoc(location, part, text, _, _) ->
          Some(op, location, part, text)
        | _ -> None)

    if List.isEmpty candidates then
      return []
    else
      let key (location : PT.PackageLocation) (part : PT.DocPart) =
        String.concat
          "\u0000"
          [ location.owner
            String.concat "." location.modules
            location.name
            Docs.kind part
            Docs.within part ]

      let keyParams =
        candidates
        |> List.mapi (fun i (_, location, part, _) ->
          ($"key_{i}", Sql.string (key location part)))

      let keyClause =
        candidates |> List.mapi (fun i _ -> $"@key_{i}") |> String.concat ", "

      let! rows =
        Sql.query
          $"""
          SELECT owner, modules, name, kind, within, text
          FROM location_docs
          WHERE owner || char(0) || modules || char(0) || name || char(0)
                || kind || char(0) || within IN ({keyClause})
          """
        |> Sql.parameters keyParams
        |> Sql.executeAsync (fun read ->
          (String.concat
            "\u0000"
            [ read.string "owner"
              read.string "modules"
              read.string "name"
              read.string "kind"
              read.string "within" ],
           read.string "text"))

      let said = Map.ofList rows

      return
        candidates
        |> List.filter (fun (_, location, part, text) ->
          Map.tryFind (key location part) said <> Some text)
        |> List.map (fun (op, _, _, _) -> op)
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
      // unsayable as a `SetName` (`PT.restating`), so it is re-authored as the decision it is. An
      // `UpdateDoc` already in the log, saying something the target does not currently say, is the
      // same thing one level down, and goes back in stamped. Recursion terminates: a `SetName`
      // becomes a `Decision`, and a stamped `UpdateDoc` is a new op id, so neither is ignored again.
      let! toRestateNames = notCurrentlyBound ignored
      let! toRestateDocs = docsNotCurrentlySaid ignored
      let toRestate = toRestateNames @ toRestateDocs
      let! restated =
        if List.isEmpty toRestate then
          Task.FromResult 0L
        else
          toRestate
          |> List.choose (PT.restating (nextOriginTs ()))
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
// No reserved owners. `Darklang` used to be refused here for every write that was not trusted
// seeding, which meant guest `run` and arriving sync ops could not bind under it.
//
// Removed deliberately, 2026-09-12: the store is becoming the source, and the only sanctioned
// writer to `Darklang.*` was the reload from `packages/` that the bootstrapping arc deletes. So
// the protection had to go or the standard library would become uneditable.
//
// What it was defending, for whoever puts a security model back: a name binds last-writer-wins
// across the whole store, so anything that could write could rebind anything -- including the
// names the kernel itself resolves through, and including `Darklang.Matter.router`, which is
// what a server serves. The replacement is an op's AUTHORITY, checked at fold time, and it does
// not exist yet. Until it does, anyone who can write can rebind anything.

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
/// Insert package operations submitted by RUNNING Dark code -- a guest `run`, or ops that arrived
/// over sync. Trusted seeding does not come through here.
///
/// The only check left is the placeholder-hash one, which is a correctness check rather than a
/// permission: a parser that had no store to ask emits a location where a content hash belongs,
/// and storing that would bind a name to something that does not exist.
let insertUntrustedOps (ops : List<PT.PackageOp>) : Task<Result<int64, string>> =
  task {
    match placeholderHashViolation ops with
    | Some reason -> return Error reason
    | None ->
      let! count = insertAndApplyOpsAsWip ops
      return Ok count
  }

let draftDeletes : List<string> =
  [ "DELETE FROM locations WHERE source <> 'resolution'
     AND op_id IN (SELECT id FROM package_ops
                   WHERE effective = 1
                     AND commit_hash IS NULL
                     AND id NOT IN (SELECT op_id FROM op_branches)
                     AND id NOT IN (SELECT op_id FROM op_owners))"
    "DELETE FROM package_ops
     WHERE effective = 1
       AND commit_hash IS NULL
       AND id NOT IN (SELECT op_id FROM op_branches)
       AND id NOT IN (SELECT op_id FROM op_owners)" ]

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
    // Hosted ops excluded via `op_owners`; see `draftDeletes` for why.
    $"DELETE FROM package_ops WHERE effective = 1 AND id NOT IN (SELECT op_id FROM op_branches) AND id NOT IN (SELECT op_id FROM op_owners){keepUnreadable}" ]

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


/// The owners a SERVER will not let a push bind into its main.
///
/// `reservedOwners` used to sit on every write path, including local authoring, and that was
/// wrong -- your own machine is yours, and the check was deleted from all three. This is the same
/// idea at the one edge where it belongs: what a shared server's MAIN will accept from a stranger
/// over HTTP.
///
/// Why here and not everywhere. Main is what the pin names, what every fetch gets, and what the
/// server resolves its own router through. A push that binds `Darklang.*` there changes what the
/// server serves, for everyone, with no review. Your own namespace is yours to publish to freely:
/// somebody who has just logged in and written a function should be able to share it without a
/// branch, a PR or a person.
///
/// It stops ACCIDENTS rather than attacks -- the pushed owner is an unsigned string, so this is
/// about what a namespace accepts, not about who is asking. A branch push is unaffected: a branch
/// is isolated, nobody runs it, and review is what moves it to main.
let reservedOnServerMain : Set<string> = Set.ofList [ "Darklang" ]


/// The reserved names a batch of ops would bind, if any. Empty means the push is fine.
let private reservedBindings (ops : List<PT.PackageOp>) : List<string> =
  ops
  |> List.choose (fun op ->
    let loc =
      match op with
      | PT.PackageOp.SetName(loc, _, _) -> Some loc
      | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override _) -> Some loc
      | _ -> None

    match loc with
    | Some loc when Set.contains loc.owner reservedOnServerMain ->
      let modules = String.concat "." loc.modules
      Some(
        if modules = "" then
          $"{loc.owner}.{loc.name}"
        else
          $"{loc.owner}.{modules}.{loc.name}"
      )
    | _ -> None)
  |> List.distinct


/// The reserved names these records would bind into this store's main, ignoring ops it already
/// has -- those are content-addressed no-ops whatever they bind.
///
/// Ignoring them is not an optimisation. `dark push` sends the whole log, and every client's log
/// carries the `Darklang.*` baseline it was seeded with, so checking the raw batch refuses the
/// first push anybody ever makes -- including one that only adds to their own namespace.
let reservedBindingsIn
  (records : List<string * string * string>)
  : Task<List<string>> =
  task {
    let valid =
      records
      |> List.choose (fun (id, blobHex, _) ->
        try
          Some(System.Guid.Parse id, System.Convert.FromHexString blobHex)
        with _ ->
          None)

    if List.isEmpty valid then
      return []
    else
      let! existing =
        Sql.query
          "SELECT id FROM package_ops WHERE id IN (SELECT value FROM json_each(@ids))"
        |> Sql.parameters
          [ "ids",
            Sql.string (
              "["
              + (valid
                 |> List.map (fun (id, _) -> "\"" + string id + "\"")
                 |> String.concat ",")
              + "]"
            ) ]
        |> Sql.executeAsync (fun read -> read.string "id")

      let known = existing |> List.map (fun s -> s.ToLowerInvariant()) |> Set.ofList

      return
        valid
        |> List.filter (fun (id, _) ->
          not (Set.contains ((string id).ToLowerInvariant()) known))
        |> List.choose (fun (id, blob) -> BS.PT.PackageOp.tryDeserialize id blob)
        |> reservedBindings
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

        // Only ops this store does NOT already have can change anything -- ops are content
        // addressed, so a re-push of one already here is a no-op whatever it binds.
        //
        // This is not an optimisation, it is the difference between a working rule and one that
        // refuses everybody. `dark push` sends the whole log, and every client's log carries the
        // `Darklang.*` baseline it was seeded with. Checking the raw batch refuses the first push
        // anyone ever makes, including one that only adds to their OWN namespace. Measured: the
        // newcomer case failed on `Darklang.Internal.Test.WTTest`, which they had never touched.
        let! existing =
          Sql.query
            "SELECT id FROM package_ops WHERE id IN (SELECT value FROM json_each(@ids))"
          |> Sql.parameters
            [ "ids",
              // Built by hand, as the merge path does: the reflection serializer is disabled
              // under AOT.
              Sql.string (
                "["
                + (valid
                   |> List.map (fun (id, _, _) -> "\"" + string id + "\"")
                   |> String.concat ",")
                + "]"
              ) ]
          |> Sql.executeAsync (fun read -> read.string "id")

        let known =
          existing |> List.map (fun s -> s.ToLowerInvariant()) |> Set.ofList

        // Refuse the WHOLE batch, before storing any of it. A push is one act to the person making
        // it, and half of it landing is worse than none: the half that lands is live immediately,
        // and they have no way to know which half.
        let reserved =
          valid
          |> List.filter (fun (id, _, _) ->
            not (Set.contains ((string id).ToLowerInvariant()) known))
          |> List.choose (fun (id, blob, _) ->
            BS.PT.PackageOp.tryDeserialize id blob)
          |> reservedBindings

        if not (List.isEmpty reserved) then
          let shown = reserved |> List.truncate 5 |> String.concat ", "

          let andMore =
            if List.length reserved > 5 then
              $" (and {List.length reserved - 5} more)"
            else
              ""

          return
            Exception.raiseInternal
              ($"this server does not accept pushes that bind {shown}{andMore} into its main. "
               + "That namespace is reviewed: push a branch instead (`dark branch push`), and it "
               + "lands on main when the change is merged. Your own namespace takes a plain "
               + "`dark push`.")
              []
        else

          let opRows =
            valid
            |> List.map (fun (id, blob, ts) ->
              [ "id", Sql.uuid id
                "op_blob", Sql.bytes blob
                "origin_ts", Sql.string ts ])

          // `effective = 1`: pushed ops ARE folded into this store's main, like any other arriving
          // op. The fold happens in the builtin that calls this, and `growIfNeeded` finishes any
          // that were interrupted.
          //
          // This used to be `effective = 0`, so hosted ops were inert data the server served back
          // verbatim and never ran. That was a privilege-escalation defence: a name binds
          // last-writer-wins across the store, `Darklang.Matter.router` included, so anyone who
          // could push could change what the server itself runs.
          //
          // Given up deliberately, 2026-09-12, with `reservedOwners`. A server that cannot fold
          // cannot serve a seed of what it hosts, cannot show it in `/m`, and cannot be deployed to
          // by pushing -- and all three are wanted. The replacement is an op's AUTHORITY checked at
          // fold time, and it does not exist yet, so for now anyone who can push can rebind
          // anything here.
          let insertOps =
            "INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts)
           VALUES (@id, @op_blob, 0, 1, @origin_ts)"

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
