/// Moving a STORE from one op-log format to the next.
///
/// After the flip there is no `packages/` to rebuild a store from, so a format change has to carry
/// the store forward in place. That divides into two cases with very different costs, and telling
/// them apart is the first thing this module does:
///
///   FORMAT-ONLY   the BINARY layout changed (`LibSerialization/Binary/*`). Op ids are derived from
///                 the DECODED op by `Hashing.computeOpRowId`, not from its bytes, so nothing's
///                 identity moves. The migration is a blob rewrite: decode with the old reader,
///                 re-encode with the new writer, leave every id alone, re-fold the projections.
///                 That is what this module does.
///
///   IDENTITY      the HASHING changed (`LibSerialization/Hashing/*`). Every op id, item hash and
///                 commit hash moves, every reference inside an op has to be remapped, and the
///                 commit chain has to be rebuilt parent-first. Not built; see the note at the
///                 bottom of this file for what it needs.
///
/// The distinction is not a judgement call, and this refuses rather than guessing: it recomputes
/// each op's id from the decoded op and compares it with the id the store has. If they disagree,
/// the store's ids were minted by a different hashing and the blob rewrite would be a lie.
module LibDB.StoreUpgrade

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module BaseFormat = LibSerialization.Binary.BaseFormat
module Hashing = LibSerialization.Hashing.Hashing


type Report =
  {
    from : uint32
    to_ : uint32
    /// Ops decoded and written back out.
    rewritten : int
    /// Ops this build cannot read at all: a peer's newer format, stored inert. Left EXACTLY as
    /// they are. Re-encoding is impossible and dropping them would lose work a later build can
    /// still apply, which is the promise the log makes.
    unreadable : int
    /// Where the pre-migration store was copied to.
    backup : string
  }


let private storedFormat () : uint32 =
  Releases.storedFormat () |> Option.defaultValue 1u


/// Where the pre-migration copy goes: beside the store, named for the version being moved TO.
///
/// Named for the target and not the source so it reads as "the store from before v2", which is
/// what someone rolling back is looking for.
let backupPathFor (target : uint32) : string =
  $"{Sqlite.currentDbPath}.pre-v{target}"


/// Move this store to the format this build writes.
///
/// The order is the whole safety argument. The backup lands FIRST, through SQLite's own backup API,
/// so the rollback target exists before anything is touched. The rewrite is then one transaction:
/// a store half-converted is a store where some ops are v1 and some v2 with nothing recording
/// which, and there is no reader that can sort that out afterwards.
///
/// Projections are dropped rather than converted. They are a cache over the log, and re-folding is
/// cheap next to getting it wrong.
let upgrade () : Task<Result<Report, string>> =
  task {
    let from = storedFormat ()
    let target = BaseFormat.currentVersion

    if from = target then
      return Error $"this store is already format {target}; nothing to do"
    elif from > target then
      // `Releases.runPending` refuses this at open, so reaching here means someone called directly.
      return
        Error
          $"this store is format {from} and this build writes {target}. A store from a NEWER \
            format cannot be read by trying harder; upgrade the binary instead."
    else

      let backup = backupPathFor target

      match Sqlite.Backup.toFile backup with
      | Error e -> return Error $"could not back the store up to {backup}: {e}"
      | Ok() ->

        // Read the whole log first, outside the write transaction: the decode is the part that can
        // throw, and it must not do so with the rewrite half-applied.
        let! rows =
          Sql.query "SELECT id, op_blob FROM package_ops ORDER BY rowid"
          |> Sql.executeAsync (fun read -> (read.uuid "id", read.bytes "op_blob"))

        let rewrites = ResizeArray<System.Guid * byte[]>()
        let mutable unreadable = 0
        let mutable identityMoved : Option<System.Guid> = None

        for (id, blob) in rows do
          match BS.PT.PackageOp.tryDeserialize id blob with
          | None -> unreadable <- unreadable + 1
          | Some op ->
            // The line between the two cases, checked per op rather than assumed. An id derived from
            // the decoded op must still be the id the store filed it under; if it is not, the hashing
            // moved and this is not the migration that store needs.
            if Hashing.computeOpRowId op <> id then
              if identityMoved = None then identityMoved <- Some id
            else
              rewrites.Add(id, BS.PT.PackageOp.serialize id op)

        match identityMoved with
        | Some id ->
          return
            Error
              $"op {id} re-derives a different id than the store filed it under, so this store's ids \
            were minted by a different hashing than this build uses. That is an identity-changing \
            migration, which re-mints the whole log; this only rewrites blobs. The store is \
            untouched and a copy is at {backup}."
        | None ->

          // `executeTransactionSync`, not hand-written BEGIN/COMMIT around separate calls: connections
          // are POOLED, so a `BEGIN` and the statements after it are not guaranteed to be on the
          // same one, and the transaction would silently cover nothing.
          //
          // The stamp goes in the SAME transaction as the bytes it describes. Outside it, a crash
          // between the two leaves a store whose blobs and whose claim about them disagree, which
          // is worse than either failure alone.
          let statements =
            [ ("CREATE TABLE IF NOT EXISTS store_meta \
                  (key TEXT PRIMARY KEY, value TEXT NOT NULL)",
               [ [] ])
              ("INSERT OR REPLACE INTO store_meta (key, value) VALUES ('format', @v)",
               [ [ "v", Sql.string (string target) ] ]) ]
            // Only when there is something to rewrite. An empty log is a real state (a store whose
            // ops have all been cut away) and a statement with no parameter sets is not worth
            // asking the driver to reason about.
            @ (if rewrites.Count = 0 then
                 []
               else
                 [ ("UPDATE package_ops SET op_blob = @blob WHERE id = @id",
                    rewrites
                    |> Seq.map (fun (id, blob) ->
                      [ "blob", Sql.bytes blob; "id", Sql.uuid id ])
                    |> List.ofSeq) ])

          let mutable failure : Option<string> = None

          try
            statements |> Sql.executeTransactionSync |> ignore<List<int>>
          with e ->
            failure <- Some e.Message

          match failure with
          | Some why ->
            return Error $"the rewrite failed and nothing was written: {why}"
          | None ->

            // Outside the transaction: re-folding runs the playback path, which opens its own connections.
            let! _ = Seed.rebuildProjections ()

            return
              Ok
                { from = from
                  to_ = target
                  rewritten = rewrites.Count
                  unreadable = unreadable
                  backup = backup }
  }


/// Put back the copy `upgrade` made on its way to <param target>.
///
/// Contents, not the file, through the same backup API: connections already open keep working and
/// see the restored data. Anything already read into memory is still the NEW store, so the caller
/// has to say to restart -- the same caveat `LocalStore.restoreFrom` carries.
let rollback (target : uint32) : Task<Result<string, string>> =
  task {
    let backup = backupPathFor target

    if not (System.IO.File.Exists backup) then
      return
        Error
          $"no pre-v{target} copy at {backup}. A rollback can only undo an upgrade this store \
            actually ran."
    else
      match Sqlite.Backup.fromFile backup with
      | Error e -> return Error $"could not restore {backup}: {e}"
      | Ok() -> return Ok backup
  }


// ---------------------
// The identity-changing case, and what it needs
// ---------------------
//
// NOT BUILT. Written down because the shape is settled and the cost is not, and because the next
// person to need it should not have to rediscover why it is bigger than it looks.
//
// It is reached by a change under `LibSerialization/Hashing/`, which moves every derived id at
// once: op ids (`Hashing.computeOpRowId`), item hashes (the content address an `AddFn` carries),
// and commit hashes (derived over message, author, stamp, PARENT and the sorted ids of the ops the
// commit names). So:
//
//   1. walk the log in order, decoding each op with the old reader
//   2. rewrite every hash REFERENCE inside it through the remap built so far -- a `SetName` points
//      at an item hash, a `Decision` at the versions it pins
//   3. re-encode, re-derive the id, record `old -> new`
//   4. rebuild `commits` PARENT FIRST, since a commit's id depends on its parent's; every commit
//      downstream of the first change moves even if its own contents did not
//   5. rewrite every table that stores an id or a hash as a foreign key: `op_owners`, `op_branches`,
//      `sync_pushed`, `seed_ops`, `commits.parent`, `package_ops.commit_hash`
//   6. drop the projections and re-fold
//
// Two things make it worth the care rather than the speed:
//
// - it is DETERMINISTIC by construction, and that is the property that matters. Ids are content
//   hashes, so two machines re-minting the same log independently arrive at the same ids, and a
//   push after the migration dedups to nothing. Test it by re-minting two copies and diffing every
//   id, which is what 8.F.f asks for.
// - it fails QUIETLY if any of step 5 is missed. A dangling `op_branches` row does not error; a
//   branch just silently loses part of its own frontier. `SCM.StoreHealth` already reports exactly
//   that class, so it is the check to run after, not a new one to write.
