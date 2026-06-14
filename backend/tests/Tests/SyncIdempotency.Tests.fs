/// Tests the sync wire's core safety property: applying the
/// same op twice is a no-op. The receiver path is the existing
/// `Inserts.insertAndApplyOps`, which does `INSERT OR IGNORE INTO package_ops (id, …)`
/// — so a remote op already in the log is ignored. This is what makes tailnet-wide
/// sync safe: the same op can arrive from N peers without corrupting the log.
module Tests.SyncIdempotency

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite
open Microsoft.Data.Sqlite

module BS = LibSerialization.Binary.Serialization
module Inserts = LibDB.Inserts
module SyncCursors = LibDB.SyncCursors
module Conflicts = LibDB.Conflicts
module Remotes = LibDB.Remotes
module Sync = LibDB.Sync
module PMBlob = LibDB.RuntimeTypes.Blob
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

let private countOps () : Task<int64> =
  Sql.query "SELECT COUNT(*) as n FROM package_ops"
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

/// A synthetic op authoring stamp RELATIVE to actual now — no baked-in calendar year (these tests
/// run in any year). Positive minutes = future (beats a `now` re-stamp), negative = past (loses to
/// it). Same `origin_ts` format the schema/sync use.
let private relTs (minutesFromNow : float) : string =
  System.DateTime.UtcNow
    .AddMinutes(minutesFromNow)
    .ToString("yyyy-MM-ddTHH:mm:ss.fffZ")

// Isolation: these cases touch GLOBAL state (apply the op log to the shared store; the
// divergence check reads `locations`). The fix that actually removed the flake was keeping each
// test's global mutation SMALL — the heavy applies (`pullFromFile`, `applyWireBatch`) use 40-op
// slices, not the whole log, so the race window is ~1s not ~30s. `testSequenced` is kept as
// belt-and-suspenders (the full-suite-green run included it); the small windows are the real fix.
let tests =
  testSequenced
  <| testList
    "SyncIdempotency"
    [ testTask
        "re-applying an existing op is a no-op (INSERT OR IGNORE dedups by id)" {
        // grab a real op from the seeded log (as a remote peer would re-send it)
        let! existing =
          Sql.query
            "SELECT id, op_blob, branch_id, commit_hash FROM package_ops LIMIT 1"
          |> Sql.executeRowAsync (fun read ->
            let id = read.uuid "id"
            let blob = read.bytes "op_blob"
            let branchId : PT.BranchId = read.uuid "branch_id"
            let commitHash = read.stringOrNone "commit_hash"
            (id, blob, branchId, commitHash))
        let (opId, opBlob, branchId, commitHash) = existing
        let op = BS.PT.PackageOp.deserialize opId opBlob

        let! before = countOps ()
        // the sync receiver applies the SAME op again (as if from another tailnet peer)
        let! _ = Inserts.insertAndApplyOps branchId commitHash [ op ]
        let! after = countOps ()
        Expect.equal
          after
          before
          "package_ops count unchanged — the duplicate op was ignored (idempotent apply)"
      }

      testTask "opsSince read path: rowid(0) = whole log in order, rowid(max) = none" {
        let! total = countOps ()
        let! all = Inserts.opsSince 0L
        Expect.equal
          (int64 (List.length all))
          total
          "opsSince(0) returns every op in the log"
        // rowid is a monotonic cursor — results come back strictly ordered
        let rowids = all |> List.map (fun (r, _, _, _) -> r)
        Expect.equal
          rowids
          (List.sort rowids)
          "ops returned in ascending rowid order"
        // a cursor past the end returns nothing (a caller polls from its last-seen rowid)
        let maxRowid = if List.isEmpty rowids then 0L else List.max rowids
        let! none = Inserts.opsSince maxRowid
        Expect.isEmpty none "opsSince(maxRowid) returns no further ops"
      }

      // The cross-instance safety property behind dedup: the receiver decodes a wire blob to a
      // PackageOp and RE-HASHES it (insertAndApplyOps computes `computeOpHash op` for the id).
      // For INSERT OR IGNORE to dedup a re-sent op rather than fork the log, that re-hash must
      // reproduce the sender's stored id. This checks it holds for the ENTIRE log, not one op.
      testTask
        "wire round-trip: every op re-hashes to its stored id (serialize→decode→re-hash)" {
        let! wire = Inserts.opsSince 0L // (rowid, id, op_blob) exactly as stored = the wire payload
        Expect.isTrue
          (List.length wire > 0)
          "the seeded log has ops to send over the wire"
        let mismatches =
          wire
          |> List.filter (fun (_rowid, storedId, _originTs, blob) ->
            let op = BS.PT.PackageOp.deserialize storedId blob
            Inserts.computeOpHash op <> storedId)
        Expect.equal
          (List.length mismatches)
          0
          "every op keeps its content-hash id across the wire — so dedup (by id) is sound"
      }

      // Two fully-synced peers exchanging their whole logs must be a total no-op. This drives
      // the REAL receiver path (insertAndApplyOps → INSERT OR IGNORE → applyOps refold) over the
      // entire log, grouped by (branch, commit) the way a receiver applies a batch.
      testTask
        "re-applying the entire op log to an already-synced store changes nothing" {
        let! before = countOps ()
        let! ops =
          Sql.query
            "SELECT id, op_blob, branch_id, commit_hash FROM package_ops ORDER BY rowid"
          |> Sql.executeAsync (fun read ->
            let id = read.uuid "id"
            let blob = read.bytes "op_blob"
            let branchId : PT.BranchId = read.uuid "branch_id"
            let commitHash = read.stringOrNone "commit_hash"
            (BS.PT.PackageOp.deserialize id blob, branchId, commitHash))
        let groups = ops |> List.groupBy (fun (_, b, c) -> (b, c)) |> Map.toList
        for ((branchId, commitHash), g) in groups do
          let! _ =
            Inserts.insertAndApplyOps
              branchId
              commitHash
              (g |> List.map (fun (op, _, _) -> op))
          ()
        let! after = countOps ()
        Expect.equal
          after
          before
          "re-applying the whole log added no rows — INSERT OR IGNORE deduped every op"
      }

      // A LITERAL cross-store transfer: read ops from store A (the global seeded DB) and apply
      // them to a SEPARATE temp SQLite file (store B), proving the op log moves between two real
      // stores — the cross-instance round-trip the single-global-connection blocks at the LibDB
      // helper level. Store B's op-log table is created standalone (the canonical synced table);
      // the receiver's INSERT OR IGNORE runs against B's own connection.
      testTask
        "cross-store: ops read from store A transfer into a separate store B, idempotently" {
        // the wire payload, read from store A via the global LibDB connection
        let! fromA =
          Sql.query
            "SELECT id, op_blob, branch_id, commit_hash FROM package_ops ORDER BY rowid"
          |> Sql.executeAsync (fun read ->
            (read.string "id",
             read.bytes "op_blob",
             read.string "branch_id",
             read.stringOrNone "commit_hash"))
        Expect.isTrue (List.length fromA > 0) "store A has ops to send over the wire"

        let pathB =
          $"{System.IO.Path.GetTempPath()}sync-storeB-{System.Guid.NewGuid()}.db"
        try
          use connB =
            new SqliteConnection($"Data Source={pathB};Mode=ReadWriteCreate")
          connB.Open()

          let exec (sql : string) (ps : (string * obj) list) : unit =
            use cmd = connB.CreateCommand()
            cmd.CommandText <- sql
            ps
            |> List.iter (fun (k, v) ->
              cmd.Parameters.AddWithValue(k, v) |> ignore<SqliteParameter>)
            cmd.ExecuteNonQuery() |> ignore<int>

          // store B's canonical op-log table (FK to branches dropped — standalone op stream). PK matches
          // the real schema's composite (id, branch_id) so a same-id-different-branch op (which the global
          // store A can hold, esp. under parallel test runs) transfers as two rows, not one collapsed by id.
          exec
            "CREATE TABLE package_ops (id TEXT NOT NULL, op_blob BLOB NOT NULL, branch_id TEXT NOT NULL, commit_hash TEXT, origin_ts TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')), PRIMARY KEY (id, branch_id))"
            []

          let transfer () =
            for (id, blob, branchId, commitHash) in fromA do
              exec
                "INSERT OR IGNORE INTO package_ops (id, op_blob, branch_id, commit_hash) VALUES ($id, $blob, $branch, $commit)"
                [ "$id", box id
                  "$blob", box blob
                  "$branch", box branchId
                  "$commit",
                  (match commitHash with
                   | Some s -> box s
                   | None -> box System.DBNull.Value) ]

          let countB () : int64 =
            use cmd = connB.CreateCommand()
            cmd.CommandText <- "SELECT COUNT(*) FROM package_ops"
            cmd.ExecuteScalar() :?> int64

          // first exchange: store B mirrors store A's op log
          transfer ()
          Expect.equal
            (countB ())
            (int64 (List.length fromA))
            "every op transferred into store B"

          // second exchange (peers re-sync): INSERT OR IGNORE dedups by id — no growth in B
          transfer ()
          Expect.equal
            (countB ())
            (int64 (List.length fromA))
            "re-transfer is a no-op in store B — dedup by id holds across stores"

          connB.Close()
        finally
          if System.IO.File.Exists pathB then System.IO.File.Delete pathB
      }

      // sync_cursors — per-remote poll resume state (local-only)
      testTask "sync cursor: starts at 0, advances, and never moves backward" {
        let remote = $"peer-{System.Guid.NewGuid()}"
        let! start = SyncCursors.cursorFor remote
        Expect.equal
          start
          0L
          "an unseen remote's cursor starts at 0 (pull from the start)"

        do! SyncCursors.advanceCursor remote 5L
        let! atFive = SyncCursors.cursorFor remote
        Expect.equal atFive 5L "the cursor advanced to 5"

        // a stale/duplicate advance must NOT rewind the cursor
        do! SyncCursors.advanceCursor remote 3L
        let! stillFive = SyncCursors.cursorFor remote
        Expect.equal
          stillFive
          5L
          "a backward advance is ignored — the cursor is monotonic"

        do! SyncCursors.advanceCursor remote 10L
        let! atTen = SyncCursors.cursorFor remote
        Expect.equal atTen 10L "the cursor advances forward to 10"
      }

      // the cursor + opsSince together = resumable poll: fold the whole log, then a re-poll
      // from the cursor returns nothing new.
      testTask
        "sync cursor + opsSince: after folding the log, a re-poll returns no new ops" {
        let remote = $"hub-{System.Guid.NewGuid()}"
        // first poll: from cursor 0, pull the whole log
        let! cursor0 = SyncCursors.cursorFor remote
        let! firstBatch = Inserts.opsSince cursor0
        Expect.isTrue
          (List.length firstBatch > 0)
          "the first poll pulls the existing ops"

        // advance the cursor to the highest rowid we just folded
        let maxRowid = firstBatch |> List.map (fun (r, _, _, _) -> r) |> List.max
        do! SyncCursors.advanceCursor remote maxRowid

        // second poll: from the advanced cursor, nothing new (we already folded it all)
        let! cursor1 = SyncCursors.cursorFor remote
        let! secondBatch = Inserts.opsSince cursor1
        Expect.isEmpty
          secondBatch
          "a re-poll from the advanced cursor returns no already-folded ops"
      }

      // the cohesive receiver API: applyRemoteOps ties insert+apply+cursor-advance together
      testTask
        "Sync.applyRemoteOps applies a batch (idempotent) and advances the remote cursor" {
        let remote = $"sender-{System.Guid.NewGuid()}"
        // use a real seeded op as the 'remote' batch, with a synthetic sender rowid
        let! existing =
          Sql.query
            "SELECT id, op_blob, branch_id, commit_hash FROM package_ops LIMIT 1"
          |> Sql.executeRowAsync (fun read ->
            (read.uuid "id",
             read.bytes "op_blob",
             (read.uuid "branch_id" : PT.BranchId),
             read.stringOrNone "commit_hash"))
        let (opId, opBlob, branchId, commitHash) = existing
        let op = BS.PT.PackageOp.deserialize opId opBlob
        let senderRowid = 42L

        let! before = countOps ()
        let! (newCursor, _) =
          Sync.applyRemoteOps
            remote
            branchId
            commitHash
            [ (senderRowid, relTs (-60.0), op) ]
        let! after = countOps ()
        Expect.equal
          after
          before
          "the op already exists — INSERT OR IGNORE adds no row"
        Expect.equal
          newCursor
          senderRowid
          "the receiver advanced this remote's cursor to the sender rowid"

        let! persisted = SyncCursors.cursorFor remote
        Expect.equal
          persisted
          senderRowid
          "the advanced cursor persisted for this remote"

        // an empty batch is a no-op — the cursor stays put
        let! (afterEmpty, _) = Sync.applyRemoteOps remote branchId commitHash []
        Expect.equal
          afterEmpty
          senderRowid
          "an empty batch leaves the cursor unchanged"
      }

      // divergence detection: an incoming SetName rebinding a location to a DIFFERENT hash than
      // the current local binding is surfaced as data (never blocks).
      testTask
        "Sync.detectDivergences flags a SetName that rebinds a location to a different hash" {
        // a real current location binding from the seed
        let! loc =
          Sql.query
            "SELECT owner, modules, name, item_hash, item_type, branch_id FROM locations WHERE unlisted_at IS NULL LIMIT 1"
          |> Sql.executeRowAsync (fun read ->
            (read.string "owner",
             read.string "modules",
             read.string "name",
             read.string "item_hash",
             read.string "item_type",
             (read.uuid "branch_id" : PT.BranchId)))
        let (owner, modulesStr, name, existingHash, itemType, branchId) = loc
        let modules =
          if modulesStr = "" then [] else modulesStr.Split('.') |> Array.toList
        let location : PT.PackageLocation =
          { owner = owner; modules = modules; name = name }
        let kind = PT.ItemKind.fromString itemType

        // incoming SetName binding the SAME location to a DIFFERENT hash → divergence
        let differentTarget =
          PT.Reference.fromHashAndKind (PT.Hash(existingHash + "_DIFFERENT"), kind)
        let! divs =
          Sync.detectDivergences
            branchId
            [ PT.PackageOp.SetName(location, differentTarget) ]
        match divs with
        | [ (_loc, existing, incoming) ] ->
          Expect.equal
            existing
            existingHash
            "the divergence reports the existing local hash"
          Expect.equal
            incoming
            (existingHash + "_DIFFERENT")
            "and the incoming (remote) hash"
        | other ->
          failtest $"expected exactly one divergence, got {List.length other}"

        // binding to the SAME hash is NOT a divergence (idempotent re-bind)
        let sameTarget = PT.Reference.fromHashAndKind (PT.Hash existingHash, kind)
        let! none =
          Sync.detectDivergences
            branchId
            [ PT.PackageOp.SetName(location, sameTarget) ]
        Expect.isEmpty none "rebinding to the same hash is not a divergence"
      }

      // Sync.pullFromFile — the one-call `dark sync pull <file>` op: read a
      // PEER's data.db op log directly and apply it into THIS instance via the same receiver path
      // (op log + projections), resuming + persisting a per-peer cursor, no wire. Here the peer's
      // log is a copy of ours, so the apply is a no-op (already-synced peers) — what's under test is
      // reading a separate file's log, applying it, and the cursor advancing/persisting/resuming.
      testTask
        "Sync.pullFromFile pulls a peer's op log into the local instance + persists the cursor" {
        let srcPath =
          $"{System.IO.Path.GetTempPath()}sync-pull-src-{System.Guid.NewGuid()}.db"
        // Foreign Keys=False: the temp file carries only a standalone op log, not the full
        // relational state (branches rows) a real instance has — so package_ops' branch FK isn't
        // enforced here. The read + apply mechanics are what's under test.
        let srcConn =
          $"Data Source={srcPath};Mode=ReadWriteCreate;Foreign Keys=False"

        let execOn (sql : string) (ps : (string * obj) list) : unit =
          use conn = new SqliteConnection(srcConn)
          conn.Open()
          use cmd = conn.CreateCommand()
          cmd.CommandText <- sql
          ps
          |> List.iter (fun (k, v) ->
            cmd.Parameters.AddWithValue(k, v) |> ignore<SqliteParameter>)
          cmd.ExecuteNonQuery() |> ignore<int>
          conn.Close()

        try
          // build the source file: a standalone op log carrying a copy of our (global) log. PK is the
          // real schema's composite (id, branch_id) so a same-id-different-branch op copies as two rows.
          execOn
            "CREATE TABLE package_ops (id TEXT NOT NULL, op_blob BLOB NOT NULL, branch_id TEXT NOT NULL, commit_hash TEXT, origin_ts TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')), PRIMARY KEY (id, branch_id))"
            []
          let! ops =
            Sql.query
              // a 40-op slice (not the whole log): keeps the apply fast + the global-state window
              // tiny, so divergence detection's `locations` read doesn't race concurrent rebinds.
              "SELECT id, op_blob, branch_id, commit_hash FROM package_ops ORDER BY rowid LIMIT 40"
            |> Sql.executeAsync (fun read ->
              (read.string "id",
               read.bytes "op_blob",
               read.string "branch_id",
               read.stringOrNone "commit_hash"))
          Expect.isTrue
            (List.length ops > 0)
            "the local log has ops to copy into the peer file"
          for (id, blob, branch, commit) in ops do
            execOn
              "INSERT OR IGNORE INTO package_ops (id, op_blob, branch_id, commit_hash) VALUES ($i, $b, $br, $c)"
              [ "$i", box id
                "$b", box blob
                "$br", box branch
                "$c",
                (match commit with
                 | Some s -> box s
                 | None -> box System.DBNull.Value) ]

          // the peer's commits table — sync is committed-only and the file pull joins `commits` to
          // cursor on the commit-rowid, so the source needs the commit rows its ops reference (a real
          // peer always has them). Insert the distinct commit hashes in op order, so their rowids run
          // 1..K and the pull cursor lands on K (the last commit pulled).
          let distinctCommits =
            ops |> List.choose (fun (_, _, _, c) -> c) |> List.distinct
          Expect.isTrue
            (List.length distinctCommits > 0)
            "the copied slice has committed ops to ship (sync is committed-only)"
          execOn "CREATE TABLE commits (hash TEXT PRIMARY KEY)" []
          for h in distinctCommits do
            execOn "INSERT OR IGNORE INTO commits (hash) VALUES ($h)" [ "$h", box h ]

          // give the peer a content blob this instance lacks, to exercise the fetch-on-miss channel
          let blobHash = $"sync-pull-blob-{System.Guid.NewGuid()}"
          let blobBytes = System.Text.Encoding.UTF8.GetBytes blobHash
          execOn
            "CREATE TABLE package_blobs (hash TEXT PRIMARY KEY, length INTEGER NOT NULL, bytes BLOB NOT NULL, created_at TEXT DEFAULT (datetime('now')))"
            []
          execOn
            "INSERT INTO package_blobs (hash, length, bytes) VALUES ($h, $l, $b)"
            [ "$h", box blobHash
              "$l", box (int64 blobBytes.Length)
              "$b", box blobBytes ]

          // pullFromFile = the one-call `dark sync pull <file>` op: resume the stored cursor for
          // this peer, pull ops + fetch missing blobs, persist. Source rowids are 1..N (fresh
          // table), so the cursor lands on N = the op count; the apply is idempotent.
          // pull returns (cursor, divergences) — the wiring shape is exercised here; the divergence
          // *logic* (clean vs rebind) is covered by the isolated `detectDivergences` test above, which
          // controls the locations state and so isn't subject to the full-suite race.
          let! (cursor, _divergences) = Sync.pullFromFile srcPath
          Expect.equal
            cursor
            (int64 (List.length distinctCommits))
            "pull advanced the cursor to the source's last COMMIT-rowid (committed-only, commit-granular)"

          // the cursor is PERSISTED for this peer (keyed by the source path)
          let! stored = SyncCursors.cursorFor srcPath
          Expect.equal
            stored
            cursor
            "the advanced cursor was persisted for this peer"

          // fetch-on-miss: the peer's content blob we lacked is now present locally
          let! localBlob = PMBlob.get blobHash |> Ply.toTask
          Expect.equal
            localBlob
            (Some blobBytes)
            "pull fetched the peer's content blob we were missing"

          // resume: a second pull reads from the stored cursor → nothing beyond it, same cursor
          let! (cursor2, _) = Sync.pullFromFile srcPath
          Expect.equal
            cursor2
            cursor
            "the second pull resumes from the persisted cursor (no-op)"
        finally
          if System.IO.File.Exists srcPath then System.IO.File.Delete srcPath
      }

      // The RESOLUTION channel end-to-end over the file pull: a peer's `resolutions` table carries an
      // override decision; `pullFromFile` applies it to this instance's `locations` (the overlay). The
      // op/blob channels are empty here, so this isolates the resolution pull.
      testTask
        "Sync.pullFromFile applies a peer's resolutions (the override channel)" {
        let srcPath =
          $"{System.IO.Path.GetTempPath()}sync-resn-src-{System.Guid.NewGuid()}.db"
        let srcConn =
          $"Data Source={srcPath};Mode=ReadWriteCreate;Foreign Keys=False"
        let execOn (sql : string) (ps : (string * obj) list) : unit =
          use conn = new SqliteConnection(srcConn)
          conn.Open()
          use cmd = conn.CreateCommand()
          cmd.CommandText <- sql
          ps
          |> List.iter (fun (k, v) ->
            cmd.Parameters.AddWithValue(k, v) |> ignore<SqliteParameter>)
          cmd.ExecuteNonQuery() |> ignore<int>
          conn.Close()
        try
          // minimal peer store: empty op/commit/blob tables (those channels are no-ops) + one resolution
          execOn
            "CREATE TABLE package_ops (id TEXT NOT NULL, op_blob BLOB NOT NULL, branch_id TEXT NOT NULL, commit_hash TEXT, origin_ts TEXT, PRIMARY KEY (id, branch_id))"
            []
          execOn "CREATE TABLE commits (hash TEXT PRIMARY KEY)" []
          execOn
            "CREATE TABLE package_blobs (hash TEXT PRIMARY KEY, length INTEGER NOT NULL, bytes BLOB NOT NULL)"
            []
          execOn
            "CREATE TABLE resolutions (id TEXT PRIMARY KEY, owner TEXT, modules TEXT, name TEXT, item_type TEXT, chosen_hash TEXT, resolved_by TEXT, branch_id TEXT, at TEXT, created_at TEXT)"
            []
          let nm = "resnpull" + System.Guid.NewGuid().ToString().Replace("-", "")
          let chosen = System.String('b', 64)
          let at = System.DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
          execOn
            "INSERT INTO resolutions (id, owner, modules, name, item_type, chosen_hash, resolved_by, branch_id, at) VALUES ($id, $o, $m, $n, $t, $h, $by, $b, $at)"
            [ "$id", box (System.Guid.NewGuid() |> string)
              "$o", box "ResnPull"
              "$m", box "R"
              "$n", box nm
              "$t", box "fn"
              "$h", box chosen
              "$by", box "human"
              "$b", box (string PT.mainBranchId)
              "$at", box at ]
          let! _ = Sync.pullFromFile srcPath
          let! bound =
            Sql.query
              "SELECT item_hash FROM locations WHERE owner=@o AND modules=@m AND name=@n AND unlisted_at IS NULL LIMIT 1"
            |> Sql.parameters
              [ "o", Sql.string "ResnPull"; "m", Sql.string "R"; "n", Sql.string nm ]
            |> Sql.executeAsync (fun read -> read.string "item_hash")
          Expect.equal
            (List.tryHead bound)
            (Some chosen)
            "pull applied the peer's resolution → the location is bound to chosen"
        finally
          if System.IO.File.Exists srcPath then System.IO.File.Delete srcPath
      }

      // Sync wire codec (the HTTP transport): an op batch round-trips through encode → decode
      // byte-for-byte. The HTTP body reuses the existing op_blob bytes, so the wire carries exactly
      // what the file-based pull reads; only the carrier differs.
      testTask "Sync.encodeBatch/decodeBatch round-trips an op batch byte-for-byte" {
        let! batch = Sync.opsToSend 0L // the whole log as (rowid, id, op_blob)
        Expect.isTrue (List.length batch > 0) "there are ops to encode"
        let decoded = Sync.decodeBatch (Sync.encodeBatch batch)
        Expect.equal decoded batch "the op batch survives encode → decode unchanged"
        Expect.equal
          (Sync.decodeBatch (Sync.encodeBatch []))
          []
          "an empty batch round-trips to empty"
      }

      // TRUNCATION SAFETY: a connection dropped mid-response over the tailnet yields a partial body.
      // decodeBatch must FAIL on it (so the pull errors + retries next poll) rather than silently
      // decode a corrupt, partial op. (BinaryReader.ReadBytes returns short at EOF without throwing,
      // so the blob length is guarded explicitly.)
      testTask
        "Sync.decodeBatch rejects a truncated wire buffer (fail-safe, not silent garbage)" {
        let! batch = Sync.opsToSend 0L
        let wire = Sync.encodeBatch (List.truncate 3 batch)
        Expect.isTrue
          (wire.Length > 16)
          "the wire buffer has real content to truncate"
        // cut the buffer in half — lands mid-stream, so some read fails (a fixed field at EOF, or
        // the guarded short blob)
        let truncated = wire[0 .. (wire.Length / 2)]
        Expect.throws
          (fun () ->
            Sync.decodeBatch truncated
            |> ignore<List<int64 * System.Guid * string * byte[]>>)
          "a truncated wire buffer is rejected, not decoded into a partial op"
      }

      // SELF-VERSIONING WIRE (op-format-stability guard): every batch leads with a format version;
      // a version-skewed peer's batch is REJECTED at decode (fail-closed), not misread. This is what
      // makes "update one machine, not the other" refuse cleanly instead of corrupting the op log.
      testTask
        "Sync.decodeBatch rejects a wrong wire-format version (peer on a different Dark version)" {
        let! batch = Sync.opsToSend 0L
        let wire = Sync.encodeBatch (List.truncate 2 batch)
        // patch the leading version int32 to a bogus value (a future/older incompatible format)
        let tampered = Array.copy wire
        (System.BitConverter.GetBytes(9999)).CopyTo(tampered, 0)
        Expect.throws
          (fun () ->
            Sync.decodeBatch tampered
            |> ignore<List<int64 * System.Guid * string * byte[]>>)
          "a batch with a mismatched wire-format version is rejected, not misread"
      }

      // COMMITTED-ONLY is now the sync DEFAULT (`opsToSend` reads `opsSinceCommitted`): a peer syncs
      // your committed history, never your uncommitted mid-edit WIP (WIP stays local; own-device WIP
      // sync is deferred). `opsSinceCommitted` filters `commit_hash IS NOT NULL` (joins commits), so
      // it's a subset of the full log. Building block proven here: committed ⊆ all, non-empty.
      testTask
        "Sync committed-only scope: opsSinceCommitted ⊆ opsSince (the multi-author mechanism)" {
        let! all = Inserts.opsSince 0L
        let! committed = Inserts.opsSinceCommitted 0L
        Expect.isTrue
          (List.length committed > 0)
          "the seed has committed ops to ship"
        Expect.isTrue
          (List.length committed <= List.length all)
          "committed ops are a subset of all ops (WIP, if any, is excluded)"
        let allIds = all |> List.map (fun (_, id, _, _) -> id) |> Set.ofList
        let committedIds =
          committed |> List.map (fun (_, id, _, _) -> id) |> Set.ofList
        Expect.isTrue
          (Set.isSubset committedIds allIds)
          "every committed op is in the full op set (commit_hash filter narrows, never invents)"
      }

      // the full HTTP payload path: server encodes a batch → client decodes + applies it
      // through the same receiver as the file/POST path, advancing the cursor. Same fold, wire
      // source. (Idempotent here — the batch is our own log — so what's tested is decode→apply→cursor.)
      testTask
        "Sync.applyWireBatch applies an encoded batch + advances the cursor (server→client)" {
        // a 40-op slice (not the whole log) — keeps the global apply light, reducing the
        // shared-state contention that flakes the full parallel suite.
        let! fullBatch = Sync.opsToSend 0L
        let batch = List.truncate 40 fullBatch
        Expect.isTrue (List.length batch > 0) "there are ops to ship over the wire"
        let wire = Sync.encodeBatch batch
        let remote = $"wire-peer-{System.Guid.NewGuid()}"
        let! (cursor, _) = Sync.applyWireBatch remote PT.mainBranchId None wire
        let maxRowid = batch |> List.map (fun (r, _, _, _) -> r) |> List.max
        Expect.equal
          cursor
          maxRowid
          "applyWireBatch applied the decoded ops + advanced the cursor to the batch's max rowid"
      }

      // STEADY STATE: once two peers are converged, every poll's `/sync/events` returns an EMPTY wire
      // batch (no new ops). applyWireBatch must accept it cleanly — nothing applied, cursor unchanged,
      // no divergences, no error. This is the COMMON case after devices sync, so a bug here would
      // break autosync the moment it converges. (Safe under concurrent load: an empty batch mutates
      // nothing.)
      testTask
        "Sync.applyWireBatch on an empty wire batch is a clean no-op (the converged steady state)" {
        let remote = $"empty-peer-{System.Guid.NewGuid()}"
        let! before = SyncCursors.cursorFor remote // 0 — this fresh peer was never synced
        let! (cursor, divergences) =
          Sync.applyWireBatch remote PT.mainBranchId None (Sync.encodeBatch [])
        Expect.equal
          cursor
          before
          "an empty batch leaves the cursor unchanged (nothing applied) — the converged steady state"
        Expect.isEmpty divergences "an empty batch surfaces no divergences"
      }

      // AT-LEAST-ONCE delivery: a flaky tailnet may deliver the SAME wire batch twice (a pull that
      // timed out, got retried, but actually landed). The receiver must dedup — applying the same
      // batch twice leaves the same cursor + no double-apply (INSERT OR IGNORE by op id). This is the
      // wire-level counterpart to the op-id idempotency test; it pins the retry-safety of the HTTP path.
      testTask
        "Sync.applyWireBatch is idempotent over the wire (a retried pull double-delivers safely)" {
        let! fullBatch = Sync.opsToSend 0L
        let batch = List.truncate 20 fullBatch // light slice; these are our own log's ops (already present)
        let wire = Sync.encodeBatch batch
        let remote = $"retry-peer-{System.Guid.NewGuid()}"
        let! (cursor1, _) = Sync.applyWireBatch remote PT.mainBranchId None wire
        let! (cursor2, _) = Sync.applyWireBatch remote PT.mainBranchId None wire // same batch, redelivered
        Expect.equal
          cursor2
          cursor1
          "re-applying the same wire batch leaves the cursor unchanged — at-least-once delivery is safe"
      }

      // The CONFLICT STORE (Conflicts) — record an auto-resolved name-binding divergence, surface it,
      // dedup on re-detect, acknowledge ("the auto was right"). Isolated table → safe under load. This
      // is the foundation of `dark conflicts`: auto-resolve by policy + RECORD + raise + ack/override.
      testTask
        "Conflicts: record an auto-resolved divergence, list it, dedup on re-detect, acknowledge" {
        let remote = $"conflict-peer-{System.Guid.NewGuid()}"
        let loc = $"Stachu.Foo.bar-{System.Guid.NewGuid()}"
        do!
          Conflicts.record
            remote
            loc
            "hashLocal"
            "hashIncoming"
            "hashIncoming"
            "auto:last-writer-wins"
        // dedup — the same conflict re-detected on a re-pull doesn't pile up a second row
        do!
          Conflicts.record
            remote
            loc
            "hashLocal"
            "hashIncoming"
            "hashIncoming"
            "auto:last-writer-wins"
        let! all = Conflicts.list ()
        match
          all |> List.filter (fun (x : Conflicts.Conflict) -> x.location = loc)
        with
        | [ c ] ->
          Expect.equal
            c.localHash
            "hashLocal"
            "records what we had (the loser under last-writer-wins)"
          Expect.equal
            c.incomingHash
            "hashIncoming"
            "and what the peer sent (the auto-resolved winner)"
          Expect.isFalse c.acknowledged "starts un-acknowledged (raised to the user)"
          // ack — the common case ("the auto thing was right")
          do! Conflicts.acknowledge c.id
          let! after = Conflicts.getById c.id
          match after with
          | Some(ac : Conflicts.Conflict) ->
            Expect.isTrue ac.acknowledged "acknowledged after ack"
          | None -> failtest "conflict vanished after acknowledge"
        | other ->
          failtest
            $"expected exactly 1 recorded conflict (dedup), got {List.length other}"
      }

      // The receiver WIRING: `recordDivergences` (called by both pull paths after apply) turns each
      // detected divergence into a recorded conflict. Safe — only touches the isolated conflict table.
      testTask
        "Sync.recordDivergences records each detected divergence as a reviewable conflict" {
        let remote = $"divrec-{System.Guid.NewGuid()}"
        let loc = $"Foo.bar-{System.Guid.NewGuid()}"
        do! Sync.recordDivergences remote [ (loc, "hMine", "hTheirs") ]
        let! all = Conflicts.list ()
        match
          all |> List.filter (fun (c : Conflicts.Conflict) -> c.location = loc)
        with
        | [ c ] ->
          Expect.equal c.remote remote "recorded against the peer it came from"
          Expect.equal c.localHash "hMine" "records what we had"
          Expect.equal
            c.resolvedBy
            "auto:last-writer-wins"
            "tagged with the auto-resolution policy"
        | other ->
          failtest $"expected the divergence recorded once, got {List.length other}"
      }

      // END-TO-END conflict flow (as close to live as headless allows): establish a LOCAL binding,
      // then PULL a divergent one through the real receiver — and assert the conflict is auto-resolved
      // (last-writer-wins) AND recorded. Fresh GUID name → no collision; minimal global mutation.
      testTask
        "end-to-end: a divergent pull auto-resolves (LWW) AND records the conflict" {
        let nm = "cflo" + System.Guid.NewGuid().ToString().Replace("-", "")
        let loc : PT.PackageLocation =
          { owner = "ConflictTest"; modules = [ "X" ]; name = nm }
        let hashA = System.String('a', 64)
        let hashB = System.String('b', 64)
        let refA = PT.Reference.fromHashAndKind (PT.Hash hashA, PT.ItemKind.Fn)
        let refB = PT.Reference.fromHashAndKind (PT.Hash hashB, PT.ItemKind.Fn)
        // establish OUR local binding: name -> hashA
        let! _ =
          Inserts.insertAndApplyOps
            PT.mainBranchId
            None
            [ PT.PackageOp.SetName(loc, refA) ]
        // pull a DIVERGENT incoming bind (name -> hashB) through the real HTTP receiver path
        let remote = $"e2e-{System.Guid.NewGuid()}"
        let! (_cursor, divs) =
          Sync.applyRemoteOps
            remote
            PT.mainBranchId
            None
            [ (1L, relTs 60.0, PT.PackageOp.SetName(loc, refB)) ]
        Expect.isFalse
          (List.isEmpty divs)
          "the divergence (hashA vs hashB) was detected on the pull"
        // and it's recorded in the conflict store, tagged with both hashes + the peer
        let! all = Conflicts.list ()
        let fq = $"ConflictTest.X.{nm}"
        match
          all |> List.filter (fun (c : Conflicts.Conflict) -> c.location = fq)
        with
        | [ c ] ->
          Expect.equal
            c.localHash
            hashA
            "recorded what WE had (hashA — the loser under LWW)"
          Expect.equal
            c.incomingHash
            hashB
            "and the incoming (hashB — the auto-resolved winner)"
          Expect.equal c.remote remote "tagged with the peer it came from"
        | other ->
          failtest
            $"expected one recorded conflict for {fq}, got {List.length other}"
      }

      testTask
        "conflict resolve 'theirs' keeps the incoming bind + marks the conflict overridden" {
        let nm = "cfthe" + System.Guid.NewGuid().ToString().Replace("-", "")
        let loc : PT.PackageLocation =
          { owner = "ConflictTest"; modules = [ "R" ]; name = nm }
        let hashA = System.String('a', 64)
        let hashB = System.String('b', 64)
        let refA = PT.Reference.fromHashAndKind (PT.Hash hashA, PT.ItemKind.Fn)
        let refB = PT.Reference.fromHashAndKind (PT.Hash hashB, PT.ItemKind.Fn)
        let! _ =
          Inserts.insertAndApplyOps
            PT.mainBranchId
            None
            [ PT.PackageOp.SetName(loc, refA) ]
        let remote = $"rthe-{System.Guid.NewGuid()}"
        let! _ =
          Sync.applyRemoteOps
            remote
            PT.mainBranchId
            None
            [ (1L, relTs 60.0, PT.PackageOp.SetName(loc, refB)) ]
        let fq = $"ConflictTest.R.{nm}"
        let! all = Conflicts.list ()
        match
          all |> List.filter (fun (c : Conflicts.Conflict) -> c.location = fq)
        with
        | [ c ] ->
          let! ok = Sync.resolveConflict c.id false
          Expect.isTrue ok "resolveConflict 'theirs' found + resolved the conflict"
          match! Conflicts.getById c.id with
          | Some(c2 : Conflicts.Conflict) ->
            Expect.isTrue c2.overridden "the conflict is marked overridden"
          | None -> failtest "conflict vanished after resolve"
          // 'theirs' = keep the incoming (LWW winner): the binding stays on hashB
          let! rows =
            Sql.query
              "SELECT item_hash FROM locations WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL LIMIT 1"
            |> Sql.parameters
              [ "o", Sql.string "ConflictTest"
                "m", Sql.string "R"
                "n", Sql.string nm ]
            |> Sql.executeAsync (fun read -> read.string "item_hash")
          Expect.equal
            (List.tryHead rows)
            (Some hashB)
            "location stays bound to the incoming hash"
        | other ->
          failtest $"expected one conflict for {fq}, got {List.length other}"
      }

      testTask
        "conflict resolve 'mine' re-binds the location to OUR hash + marks overridden" {
        let nm = "cfmin" + System.Guid.NewGuid().ToString().Replace("-", "")
        let loc : PT.PackageLocation =
          { owner = "ConflictTest"; modules = [ "M" ]; name = nm }
        let hashA = System.String('a', 64)
        let hashB = System.String('b', 64)
        let refA = PT.Reference.fromHashAndKind (PT.Hash hashA, PT.ItemKind.Fn)
        let refB = PT.Reference.fromHashAndKind (PT.Hash hashB, PT.ItemKind.Fn)
        // PAST stamps (local 2020 < incoming 2021), so the incoming wins the pull by timestamp-LWW —
        // then the human 'mine' override RE-STAMPS our op to `now` (2025+), which beats both.
        let opA = PT.PackageOp.SetName(loc, refA)
        let! _ =
          Inserts.insertAndApplyOpsWithOrigin
            PT.mainBranchId
            None
            [ opA ]
            (Map.ofList [ (Inserts.computeOpHash opA, relTs (-120.0)) ])
        let remote = $"rmin-{System.Guid.NewGuid()}"
        let! _ =
          Sync.applyRemoteOps
            remote
            PT.mainBranchId
            None
            [ (1L, relTs (-60.0), PT.PackageOp.SetName(loc, refB)) ]
        // incoming (2021) won the pull; 'mine' re-stamps OUR op to now and must re-bind it to hashA
        let fq = $"ConflictTest.M.{nm}"
        let! all = Conflicts.list ()
        match
          all |> List.filter (fun (c : Conflicts.Conflict) -> c.location = fq)
        with
        | [ c ] ->
          let! ok = Sync.resolveConflict c.id true
          Expect.isTrue ok "resolveConflict 'mine' found + resolved the conflict"
          match! Conflicts.getById c.id with
          | Some(c2 : Conflicts.Conflict) ->
            Expect.isTrue c2.overridden "the conflict is marked overridden"
          | None -> failtest "conflict vanished after resolve"
          // 'mine' re-binds via a WIP SetName: the local projection now points at OUR hash
          let! rows =
            Sql.query
              "SELECT item_hash FROM locations WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL LIMIT 1"
            |> Sql.parameters
              [ "o", Sql.string "ConflictTest"
                "m", Sql.string "M"
                "n", Sql.string nm ]
            |> Sql.executeAsync (fun read -> read.string "item_hash")
          Expect.equal
            (List.tryHead rows)
            (Some hashA)
            "location re-bound to OUR hash (hashA)"
        | other ->
          failtest $"expected one conflict for {fq}, got {List.length other}"
      }

      testTask "Remotes: add (upsert) → list/urls include it → remove (idempotent)" {
        let nm = "rmt" + System.Guid.NewGuid().ToString().Replace("-", "")
        let url = $"http://{nm}.example:9922"
        // add, then re-add with a new url — upsert by name (no duplicate row)
        do! Remotes.add nm "http://stale:1"
        do! Remotes.add nm url
        let! listed = Remotes.list ()
        let mine = listed |> List.filter (fun (n, _) -> n = nm)
        match mine with
        | [ (_, u) ] -> Expect.equal u url "upsert kept ONE row with the latest url"
        | other ->
          failtest $"expected exactly one row for {nm}, got {List.length other}"
        // its url is in the daemon's poll set
        let! urls = Remotes.urls ()
        Expect.isTrue
          (List.contains url urls)
          "the registered url is in the poll set (urls)"
        // remove reports it existed; a second remove reports it didn't
        let! removed = Remotes.remove nm
        Expect.isTrue removed "remove of an existing remote returns true"
        let! removedAgain = Remotes.remove nm
        Expect.isFalse removedAgain "remove of a missing remote returns false"
        let! after = Remotes.list ()
        Expect.isFalse
          (after |> List.exists (fun (n, _) -> n = nm))
          "the remote is gone from the list after remove"
      }

      testTask
        "Sync.opKindsSince maps each SetName above the cursor to its target kind (fn/type/value)" {
        // a standalone peer db with three naming ops (fn, type, value) — opKindsSince reads SetName
        // target kinds; an Add* op (not a SetName) is ignored, proving the no-double-count counting.
        let path = $"{System.IO.Path.GetTempPath()}okinds-{System.Guid.NewGuid()}.db"
        try
          use conn = new SqliteConnection($"Data Source={path};Mode=ReadWriteCreate")
          conn.Open()

          let exec (sql : string) (ps : (string * obj) list) : unit =
            use cmd = conn.CreateCommand()
            cmd.CommandText <- sql
            ps
            |> List.iter (fun (k, v) ->
              cmd.Parameters.AddWithValue(k, v) |> ignore<SqliteParameter>)
            cmd.ExecuteNonQuery() |> ignore<int>

          exec
            "CREATE TABLE package_ops (id TEXT PRIMARY KEY, op_blob BLOB NOT NULL, branch_id TEXT NOT NULL, commit_hash TEXT, origin_ts TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')))"
            []

          let setName
            (md : string)
            (nm : string)
            (kind : PT.ItemKind)
            (h : string)
            : PT.PackageOp =
            let loc : PT.PackageLocation =
              { owner = "OK"; modules = [ md ]; name = nm }
            PT.PackageOp.SetName(loc, PT.Reference.fromHashAndKind (PT.Hash h, kind))

          let ops =
            [ setName "M" "f" PT.ItemKind.Fn (System.String('a', 64))
              setName "M" "t" PT.ItemKind.Type (System.String('b', 64))
              setName "M" "v" PT.ItemKind.Value (System.String('c', 64)) ]

          for op in ops do
            let id = Inserts.computeOpHash op
            let blob = BS.PT.PackageOp.serialize id op
            exec
              "INSERT INTO package_ops (id, op_blob, branch_id, commit_hash) VALUES ($id, $blob, $b, $c)"
              [ "$id", box (string id)
                "$blob", box blob
                "$b", box (string PT.mainBranchId)
                "$c", box System.DBNull.Value ]

          conn.Close()

          // from cursor 0 → all three, in rowid order
          let! kinds = Sync.opKindsSince path 0L
          Expect.equal
            kinds
            [ "fn"; "type"; "value" ]
            "each SetName mapped to its target item kind, in order"
          // above the last rowid → nothing
          let! none = Sync.opKindsSince path 99L
          Expect.isEmpty none "no ops above a high cursor"
        finally
          try
            System.IO.File.Delete path
          with _ ->
            ()
      }

      testTask
        "Conflicts.acknowledgeAll acknowledges every pending conflict (bulk ack)" {
        let loc = $"AckAll.X.{System.Guid.NewGuid()}"
        do!
          Conflicts.record
            "ackall-peer"
            loc
            (System.String('a', 64))
            (System.String('b', 64))
            (System.String('b', 64)) // chosen winner
            "auto:last-writer-wins"
        let! before = Conflicts.list ()
        Expect.isTrue
          (before
           |> List.exists (fun (c : Conflicts.Conflict) ->
             c.location = loc && not c.acknowledged))
          "the conflict is recorded and pending"
        let! n = Conflicts.acknowledgeAll ()
        Expect.isTrue
          (n >= 1)
          "acknowledgeAll reports it cleared at least the one we recorded"
        let! after = Conflicts.list ()
        match
          after |> List.filter (fun (c : Conflicts.Conflict) -> c.location = loc)
        with
        | [ c ] -> Expect.isTrue c.acknowledged "our conflict is now acknowledged"
        | other ->
          failtest $"expected one conflict for {loc}, got {List.length other}"
      }

      // ── timestamp-LWW: playback orders bindings by CREATION time, not arrival ──
      // An op authored EARLIER but applied LATER (it
      // arrived late via sync) must not override a later-created binding. Tested directly through
      // applySetName via insertAndApplyOpsWithOrigin (which stamps each op's origin_ts).
      let bindingOf (modul : string) (nm : string) : Task<Option<string>> =
        task {
          let! rows =
            Sql.query
              "SELECT item_hash FROM locations WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL LIMIT 1"
            |> Sql.parameters
              [ "o", Sql.string "TsLww"; "m", Sql.string modul; "n", Sql.string nm ]
            |> Sql.executeAsync (fun read -> read.string "item_hash")
          return List.tryHead rows
        }

      testTask
        "timestamp-LWW: an EARLIER-created op applied LATER does NOT override the newer binding" {
        let nm = "tslww" + System.Guid.NewGuid().ToString().Replace("-", "")
        let loc : PT.PackageLocation =
          { owner = "TsLww"; modules = [ "C" ]; name = nm }
        let refEarly =
          PT.Reference.fromHashAndKind (
            PT.Hash(System.String('1', 64)),
            PT.ItemKind.Fn
          )
        let refLate =
          PT.Reference.fromHashAndKind (
            PT.Hash(System.String('2', 64)),
            PT.ItemKind.Fn
          )
        let opEarly = PT.PackageOp.SetName(loc, refEarly)
        let opLate = PT.PackageOp.SetName(loc, refLate)
        let tsEarly = relTs (-120.0)
        let tsLate = relTs (-60.0)
        // apply the LATE-created op first → binds hash '2'
        let! _ =
          Inserts.insertAndApplyOpsWithOrigin
            PT.mainBranchId
            None
            [ opLate ]
            (Map.ofList [ (Inserts.computeOpHash opLate, tsLate) ])
        let! afterLate = bindingOf "C" nm
        Expect.equal
          afterLate
          (Some(System.String('2', 64)))
          "late-created op binds the name"
        // now apply the EARLIER-created op (arrives later) → must be SKIPPED as stale-by-creation
        let! _ =
          Inserts.insertAndApplyOpsWithOrigin
            PT.mainBranchId
            None
            [ opEarly ]
            (Map.ofList [ (Inserts.computeOpHash opEarly, tsEarly) ])
        let! afterEarly = bindingOf "C" nm
        Expect.equal
          afterEarly
          (Some(System.String('2', 64)))
          "an earlier-created op applied later does NOT override"
      }

      testTask
        "timestamp-LWW: convergence is order-INDEPENDENT — the other apply order yields the SAME winner" {
        let nm = "tsconv" + System.Guid.NewGuid().ToString().Replace("-", "")
        let loc : PT.PackageLocation =
          { owner = "TsLww"; modules = [ "D" ]; name = nm }
        let refEarly =
          PT.Reference.fromHashAndKind (
            PT.Hash(System.String('3', 64)),
            PT.ItemKind.Fn
          )
        let refLate =
          PT.Reference.fromHashAndKind (
            PT.Hash(System.String('4', 64)),
            PT.ItemKind.Fn
          )
        let opEarly = PT.PackageOp.SetName(loc, refEarly)
        let opLate = PT.PackageOp.SetName(loc, refLate)
        let tsEarly = relTs (-120.0)
        let tsLate = relTs (-60.0)
        // apply EARLY first, then LATE → LATE (max origin_ts) wins, same as the reverse order above
        let! _ =
          Inserts.insertAndApplyOpsWithOrigin
            PT.mainBranchId
            None
            [ opEarly ]
            (Map.ofList [ (Inserts.computeOpHash opEarly, tsEarly) ])
        let! _ =
          Inserts.insertAndApplyOpsWithOrigin
            PT.mainBranchId
            None
            [ opLate ]
            (Map.ofList [ (Inserts.computeOpHash opLate, tsLate) ])
        let! winner = bindingOf "D" nm
        Expect.equal
          winner
          (Some(System.String('4', 64)))
          "max-origin_ts wins regardless of apply order (no swap)"
      }

      ]
