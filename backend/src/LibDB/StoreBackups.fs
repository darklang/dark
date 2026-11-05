/// Whole copies of the store: the backups kept beside it, and a snapshot written anywhere else.
///
/// One owner for the folder and its names. The upgrade path, `dark backups` and `update`'s smoke
/// test all copy the store, and each used to do it its own way: Dark built the folder path and the
/// file names and deleted files itself, agreeing with the upgrade path on names only by convention,
/// and the smoke test copied `data.db` and `data.db-wal` with `cp` while the store was open.
///
/// Every copy here goes through SQLite's online-backup API, never a file copy. The store runs in
/// WAL mode, so `data.db` alone can be missing what was last committed, and the backup API reads a
/// consistent snapshot of a store that is open. A restore puts contents in rather than swapping the
/// file, so connections already open keep working.
///
/// Every function takes the store's path rather than reading `Sqlite.currentDbPath`: the upgrade
/// path runs before anything else in LibDB is set up.
module LibDB.StoreBackups

open System
open System.IO
open Microsoft.Data.Sqlite



/// One copy in the `backups` folder, or a snapshot written elsewhere.
type Backup = { name : string; path : string; bytes : int64 }


/// The `backups` folder beside the store.
let dirFor (dbPath : string) : string =
  Path.Combine(Path.GetDirectoryName(Path.GetFullPath dbPath), "backups")

let private stampNow () : string = DateTime.UtcNow.ToString("yyyyMMddTHHmmssZ")

let private describe (path : string) : Backup =
  { name = Path.GetFileName path; path = path; bytes = FileInfo(path).Length }


/// Copy the database at <param source> into <param target>, which SQLite creates if it is not
/// there. Unpooled, so closing the connections folds any WAL the copy made back into the file.
let private copyInto (source : string) (target : string) : unit =
  use from = new SqliteConnection($"Data Source={source};Pooling=False")
  from.Open()
  use into =
    new SqliteConnection($"Data Source={target};Mode=ReadWriteCreate;Pooling=False")
  into.Open()
  from.BackupDatabase into

/// Write a copy of <param source> to <param target>.
///
/// A new file is written under a temporary name and renamed into place, so a copy that fails half
/// way never looks like a backup. An existing database is copied into rather than replaced: a file
/// moved over it would leave its old `-wal` beside the new contents.
let private writeCopy (source : string) (target : string) : unit =
  if File.Exists target then
    copyInto source target
  else
    let partial = target + ".partial"
    for leftover in [ partial; partial + "-wal"; partial + "-shm" ] do
      if File.Exists leftover then File.Delete leftover
    copyInto source partial
    File.Move(partial, target)

let private attempt (f : unit -> 'a) : Result<'a, string> =
  try
    Ok(f ())
  with e ->
    // The first line: a database error carries its stack trace in its message.
    Error((e.Message.Split '\n' |> Array.head).Trim())


/// Is <param name> one of ours? Only finished copies: not a copy still being written, and not
/// SQLite's own side files for one that is open.
let private isBackupName (name : string) : bool =
  name.StartsWith "data.db."
  && not (name.EndsWith ".partial")
  && not (name.EndsWith "-wal")
  && not (name.EndsWith "-shm")
  && not (name.EndsWith "-journal")

/// The backups, newest first by the time in the name, not by the name. Three prefixes share the
/// folder (`before-upgrade-`, `manual-`, `pre-restore-`), and a name sort groups by prefix before it
/// looks at the time. Every name's time comes before any other digits in it (an upgrade backup
/// names its build after the time), so ordering by the digits alone is right.
let list (dbPath : string) : List<Backup> =
  let dir = dirFor dbPath
  if not (Directory.Exists dir) then
    []
  else
    let digits (name : string) =
      String(name |> Seq.filter Char.IsDigit |> Array.ofSeq)
    Directory.GetFiles dir
    |> List.ofArray
    |> List.filter (fun p -> isBackupName (Path.GetFileName p))
    |> List.sortBy (fun p -> digits (Path.GetFileName p))
    |> List.rev
    |> List.map describe

let private find (dbPath : string) (name : string) : Result<Backup, string> =
  match list dbPath |> List.tryFind (fun b -> b.name = name) with
  | Some b -> Ok b
  | None -> Error $"no backup named \"{name}\""

/// Take a copy into the folder, named `data.db.<kind>-<time>`. Two in the same second get a
/// counter rather than one replacing the other.
let private take (dbPath : string) (kind : string) : Result<Backup, string> =
  attempt (fun () ->
    let dir = dirFor dbPath
    Directory.CreateDirectory dir |> ignore<DirectoryInfo>
    let stem = Path.Combine(dir, $"data.db.{kind}-{stampNow ()}")
    let target =
      Seq.initInfinite id
      |> Seq.map (fun i -> if i = 0 then stem else $"{stem}-{i + 1}")
      |> Seq.find (fun p ->
        not (File.Exists p) && not (File.Exists(p + ".partial")))
    writeCopy dbPath target
    describe target)

/// `dark backups now`.
let create (dbPath : string) : Result<Backup, string> = take dbPath "manual"

/// The copy taken before a store older than the layered shape is reset (`Releases.resetIfOlderShape`).
/// Always a new one: the reset drops the store's packages, branches and commits.
let beforeReset (dbPath : string) : Result<Backup, string> =
  take dbPath "before-layers"

/// Put the backup called <param name> in place of the store's contents. The store as it is now is
/// copied aside first (`data.db.pre-restore-<time>`), so a restore can itself be undone; that copy
/// is what this returns. Anything this process has already read is still the old store, so the
/// caller says to restart.
let restore (dbPath : string) (name : string) : Result<Backup, string> =
  match find dbPath name with
  | Error e -> Error e
  | Ok backup ->
    match take dbPath "pre-restore" with
    | Error e ->
      Error $"couldn't copy the current store aside ({e}); nothing restored"
    | Ok aside ->
      match attempt (fun () -> copyInto backup.path dbPath) with
      | Ok() -> Ok aside
      | Error e ->
        Error
          $"couldn't restore {name} ({e}). The store is unchanged; a copy is at {aside.path}"

/// Delete the backup called <param name>.
let delete (dbPath : string) (name : string) : Result<Backup, string> =
  find dbPath name
  |> Result.bind (fun backup ->
    attempt (fun () ->
      File.Delete backup.path
      backup))

/// Write a copy of the store to <param target>, anywhere but over the store itself, such as a
/// store to test a new release against. An existing database there has its contents replaced.
let snapshotTo (dbPath : string) (target : string) : Result<Backup, string> =
  let full = Path.GetFullPath target
  if full = Path.GetFullPath dbPath then
    Error $"{target} is the store itself"
  else
    attempt (fun () ->
      Directory.CreateDirectory(Path.GetDirectoryName full) |> ignore<DirectoryInfo>
      writeCopy dbPath full
      describe full)

/// The copy the upgrade path takes before it changes anything, named
/// `data.db.before-upgrade-<time>-to-<build>`. One per build it upgrades TO, and an existing one is
/// kept: a start that retries after a failed step must not replace the store as it was with the store
/// as the failure left it. Returns the copy's path, and whether it was taken just now.
let beforeUpgrade
  (dbPath : string)
  (build : string)
  : Result<string * bool, string> =
  let dir = dirFor dbPath
  let tag = if build.Length > 12 then build.Substring(0, 12) else build
  let suffix = $"-to-{tag}"
  let existing =
    if Directory.Exists dir then
      Directory.GetFiles(dir, $"data.db.before-upgrade-*{suffix}") |> Array.tryHead
    else
      None
  match existing with
  | Some kept -> Ok(kept, false)
  | None ->
    attempt (fun () ->
      Directory.CreateDirectory dir |> ignore<DirectoryInfo>
      let target =
        Path.Combine(dir, $"data.db.before-upgrade-{stampNow ()}{suffix}")
      writeCopy dbPath target
      (target, true))
