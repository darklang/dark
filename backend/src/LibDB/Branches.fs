/// The branch REGISTRY: a branch is a stable id, a name a person can change, and a parent. Its ops
/// are the `package_ops` rows under its id, and what it binds is in the layered projections
/// (`LibDB.Layers`); authoring onto it is `LibDB.Inserts`.
module LibDB.Branches

open System.Threading.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization

/// Create a branch off `parentId` (main for a top-level branch). The fork point is tracked PER
/// NAME (branch_name_bases, recorded on authoring), not a whole-branch watermark, so there's no
/// base column here. Idempotent on id.
///
/// Branches are reclaimed only by an EXPLICIT archive. There is no TTL and no auto-archive, so an
/// abandoned agent branch stays live until someone says otherwise.
let createBranch
  (id : PT.BranchId)
  (name : string)
  (parentId : PT.BranchId)
  : Task<unit> =
  // Upsert: re-using an existing id REVIVES it (clears archived_at/merged_at), so an archived
  // branch re-created is active and visible again. Parent and name are first-write-wins, which
  // keeps branch identity stable.
  Sql.query
    "INSERT INTO branches (id, name, parent_id) VALUES (@id, @name, @parent)
     ON CONFLICT(id) DO UPDATE SET archived_at = NULL, merged_at = NULL"
  |> Sql.parameters
    [ "id", Sql.string (string id)
      "name", Sql.string name
      "parent", Sql.string (string parentId) ]
  |> Sql.executeStatementAsync

/// Register a branch if it is new, and leave an existing row alone. Never revives: that is
/// `createBranch`'s job, for the paths that mean it (a bundle arriving says the branch is alive
/// somewhere). Authoring goes through this one, so an edit cannot un-merge a branch by landing on it.
let registerIfNew
  (id : PT.BranchId)
  (name : string)
  (parentId : PT.BranchId)
  : Task<unit> =
  Sql.query
    "INSERT OR IGNORE INTO branches (id, name, parent_id) VALUES (@id, @name, @parent)"
  |> Sql.parameters
    [ "id", Sql.string (string id)
      "name", Sql.string name
      "parent", Sql.string (string parentId) ]
  |> Sql.executeStatementAsync

/// Merged or archived: finished, and not somewhere you can author. An op on a finished branch
/// is stranded, with nothing left to merge it.
let isFinished (branchId : PT.BranchId) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches
         WHERE id = @id AND (archived_at IS NOT NULL OR merged_at IS NOT NULL)"
      |> Sql.parameters [ "id", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

// Three questions, deliberately distinct: EXISTS (known here at all), LIVE (still somewhere you
// can author), LISTED (still worth naming, merged or not). Reads and writes want different
// answers, which is why the name lookups come as a pair rather than one lookup with a flag.

/// Does this branch exist at all? A registry row, or ops under its id.
let exists (branchId : PT.BranchId) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches WHERE id = @b
         UNION ALL
         SELECT 1 AS n FROM package_ops WHERE branch_id = @b
         LIMIT 1"
      |> Sql.parameters [ "b", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

let liveIdForName (name : string) : Task<Option<PT.BranchId>> =
  Sql.query
    "SELECT id FROM branches
     WHERE name = @name AND archived_at IS NULL AND merged_at IS NULL
     ORDER BY created_at DESC, rowid DESC LIMIT 1"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read ->
    PT.BranchId.ParseUnsafe(read.string "id"))

/// The branch's name, or None when this store has no such branch.
let nameOf (branchId : PT.BranchId) : Task<Option<string>> =
  Sql.query "SELECT name FROM branches WHERE id = @id"
  |> Sql.parameters [ "id", Sql.string (string branchId) ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "name")

let isLive (branchId : PT.BranchId) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches
         WHERE id = @id AND archived_at IS NULL AND merged_at IS NULL"
      |> Sql.parameters [ "id", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

/// The branch a REF refers to, without creating anything: a name, a full id, or an unambiguous id
/// prefix. All three get printed at people, and `dark branches` abbreviates ids to 8 characters,
/// so the prefix is the form most likely to be pasted. Mirrors `Cli.Branch.lookupRef`, which the
/// branch verbs go through.
///
/// A miss says WHY, because only one kind of miss should create a branch. A bare `None` would stand
/// for four different things at once, and the caller would silently start a branch named after a
/// peer's uuid, or after an ambiguous prefix.
type RefLookup =
  | Found of PT.BranchId
  /// A full id, and no branch here has it: a peer's, or a mispaste.
  | UnknownId of PT.BranchId
  /// An id prefix matching more than one branch.
  | Ambiguous of string
  /// Not a live name, not an id, not a prefix: a name this store has never had.
  | NoSuchName of string

let lookupRef (branchRef : string) : Task<RefLookup> =
  task {
    // Main is resolvable by name and by id like any other branch, and has to be: it has no
    // `branches` row, so neither the name lookup nor the id-prefix search below can find it.
    if
      branchRef = PT.BranchId.MainName
      || PT.BranchId.Parse branchRef = Some PT.BranchId.Main
    then
      return Found PT.BranchId.Main
    else
      match! liveIdForName branchRef with
      | Some id -> return Found id
      | None ->
        match PT.BranchId.Parse branchRef with
        | Some id ->
          let! exact = exists id
          return (if exact then Found id else UnknownId id)
        | None ->
          if String.length branchRef < 4 then
            // Too short to be a prefix worth guessing from; treat it as a name we don't have.
            return NoSuchName branchRef
          else
            let! matches =
              Sql.query
                "SELECT id FROM branches
                 WHERE id LIKE @prefix AND archived_at IS NULL
                 LIMIT 2"
              |> Sql.parameters [ "prefix", Sql.string (branchRef + "%") ]
              |> Sql.executeAsync (fun read ->
                PT.BranchId.ParseUnsafe(read.string "id"))

            match matches with
            | [ only ] -> return Found only
            | [] -> return NoSuchName branchRef
            | _ -> return Ambiguous branchRef
  }

/// Mint a branch for <param name>, and report whether this call is the one that created it.
///
/// Its own function because F# cannot compile awaits inside a `match` ARM into a static state machine
/// (FS3511, an error under Release). Each task here awaits at its own top level.
let private mintBranch
  (name : string)
  (parentId : PT.BranchId)
  : Task<PT.BranchId * bool> =
  task {
    // Check-then-insert is a race, and concurrent agents sharing a `DARK_BRANCH` is normal
    // here. The guard is inside the INSERT rather than a UNIQUE index, because same-named
    // branches are legal: one started on another instance is a DIFFERENT branch sharing a
    // label, so only the local case (one machine minting two) is prevented. One statement, so
    // SQLite's write lock makes it atomic; the loser writes nothing and re-reads the winner.
    let id = PT.BranchId.Id(System.Guid.NewGuid())

    let insert =
      Sql.query
        "INSERT INTO branches (id, name, parent_id)
         SELECT @id, @name, @parent
         WHERE NOT EXISTS (
           SELECT 1 FROM branches
           WHERE name = @name AND archived_at IS NULL AND merged_at IS NULL
         )"
      |> Sql.parameters
        [ "id", Sql.string (string id)
          "name", Sql.string name
          "parent", Sql.string (string parentId) ]
      |> Sql.executeStatementAsync

    do! insert
    let! winner = liveIdForName name

    return
      match winner with
      | Some w -> (w, w = id)
      // Only reachable if the row we just wrote was archived or merged in between, which means
      // someone deliberately finished it. Report the id we minted rather than inventing one.
      | None -> (id, true)
  }

/// Resolve a branch NAME to its id, starting the branch if that name has none. Returns the id and
/// whether it was just created (callers announce that; silently running on a branch you didn't
/// mean to start is indistinguishable from running on the one you did).
///
/// A NAME is what a person types: reusable, renameable, and two machines each starting a
/// `fix-auth` have two branches. An ID is what every internal reference uses (op tags, per-name
/// bases, relay bundles, parent links) and must survive a rename. This function is that boundary.
let resolveOrCreate
  (name : string)
  (parentId : PT.BranchId)
  : Task<PT.BranchId * bool> =
  task {
    let! existing = liveIdForName name

    match existing with
    | Some id -> return (id, false)
    | None -> return! mintBranch name parentId
  }

/// A branch's registered parent id; main when none is recorded, or the branch is unknown here.
let parentOf (branchId : PT.BranchId) : Task<PT.BranchId> =
  task {
    let! p =
      Sql.query "SELECT parent_id FROM branches WHERE id = @b"
      |> Sql.parameters [ "b", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "parent_id")
    return
      p |> Option.map PT.BranchId.ParseUnsafe |> Option.defaultValue PT.BranchId.Main
  }
