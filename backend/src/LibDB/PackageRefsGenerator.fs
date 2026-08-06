/// Writes `package-ref-hashes.txt` with current hashes from the DB.
/// PackageRefs.fs reads this file at startup.
module LibDB.PackageRefsGenerator

open Prelude

open Fumble
open LibDB.Sqlite

module PackageRefs = LibExecution.PackageRefs


/// Build the FQN key for a given item type and DB row.
/// Format: "type/{modules}.{name}" or "fn/{modules}.{name}"
let private buildKey (itemType : string) (modules : string) (name : string) =
  let prefix = if itemType = "type" then "type" else "fn"
  if modules = "" then $"{prefix}/{name}" else $"{prefix}/{modules}.{name}"


/// Path to the source-tree copy of the hash file (committed to git).
let private sourceTreePath =
  System.IO.Path.Combine(
    __SOURCE_DIRECTORY__,
    "../LibExecution/package-ref-hashes.txt"
  )
  |> System.IO.Path.GetFullPath


/// The `fqn -> hash` pairs already on disk, or empty if the file is missing or unreadable.
let private readExistingFile () : Map<string, string> =
  try
    if System.IO.File.Exists(sourceTreePath) then
      System.IO.File.ReadAllLines(sourceTreePath)
      |> Array.choose (fun line ->
        let line = line.Trim()
        if line = "" then
          None
        else
          match line.Split('|') with
          | [| fqn; hash |] -> Some(fqn, hash)
          | _ -> None)
      |> Map.ofArray
    else
      Map.empty
  with _ ->
    Map.empty


/// Query the DB for all current Darklang-owned locations and write
/// `package-ref-hashes.txt` in the source tree.
let generate () : Ply<unit> =
  uply {
    // Collect all referenced items from PackageRefs _lookup maps
    let typeRefKeys =
      PackageRefs.Type._lookup
      |> Map.toList
      |> List.map (fun ((modules, name), _hash) ->
        buildKey "type" (String.concat "." modules) name)
      |> Set.ofList

    let fnRefKeys =
      PackageRefs.Fn._lookup
      |> Map.toList
      |> List.map (fun ((modules, name), _hash) ->
        buildKey "fn" (String.concat "." modules) name)
      |> Set.ofList

    // Union in whatever the existing file already knew about.
    //
    // `_lookup` is only populated as each `PackageRefs` nested module initializes, which happens when
    // something touches it. A process that regenerates before touching them all -- or an older binary that
    // predates a ref entirely -- would otherwise silently write a *shorter* file, dropping refs. That is
    // not a theoretical concern: `growIfNeeded` regenerates on any startup that applies ops, so running a
    // previous release inside the source tree was enough to truncate the file, after which the next build
    // produced a binary that raised "PackageRefs: hash not found" on startup.
    //
    // Keys that no longer resolve are dropped below by the `List.choose` against the DB, so this
    // accumulates known refs without letting deleted ones linger.
    let existingKeys =
      readExistingFile () |> Map.toList |> List.map fst |> Set.ofList

    let allRefKeys = Set.unionMany [ typeRefKeys; fnRefKeys; existingKeys ]

    // Query all Darklang-owned locations from DB
    let! dbRows =
      Sql.query
        """
        SELECT item_type, modules, name, item_hash
        FROM locations
        WHERE owner = 'Darklang'
          AND unlisted_at IS NULL
        """
      |> Sql.executeAsync (fun read ->
        let itemType = read.string "item_type"
        let modules = read.string "modules"
        let name = read.string "name"
        let hash = read.string "item_hash"
        (buildKey itemType modules name, hash))

    let dbMap = dbRows |> Map.ofList

    // Preserves entries not found in the DB (e.g. RT types that share hashes with PT types and aren't in
    // locations), and, via `existingKeys` above, refs this process never registered.
    let existingMap = readExistingFile ()

    // Merge: DB values win, existing file fills gaps for referenced items
    let merged =
      allRefKeys
      |> Set.toList
      |> List.choose (fun key ->
        match Map.tryFind key dbMap with
        | Some hash -> Some(key, hash)
        | None ->
          match Map.tryFind key existingMap with
          | Some hash -> Some(key, hash)
          | None -> None)
      |> List.sortBy fst

    let lines = merged |> List.map (fun (key, hash) -> $"{key}|{hash}")

    // Always set the in-memory cache so PackageRefs lookups work
    let hashMap = merged |> Map.ofList
    PackageRefs.setHashes hashMap

    // Write the source-tree file (skip if the directory doesn't exist,
    // e.g. on installed CLIs where the source tree isn't available)
    let dir = System.IO.Path.GetDirectoryName(sourceTreePath)
    if System.IO.Directory.Exists(dir) then
      System.IO.File.WriteAllLines(sourceTreePath, lines |> Array.ofList)
      let totalWritten = List.length lines
      print $"  Wrote {totalWritten} package ref hashes to {sourceTreePath}"

    // Report any items referenced but not found anywhere
    let foundKeys = merged |> List.map fst |> Set.ofList
    let missing = Set.difference allRefKeys foundKeys

    if not (Set.isEmpty missing) then
      print
        $"  Warning: {Set.count missing} PackageRefs items not found in DB or existing file:"
      for key in missing do
        print $"    - {key}"
  }
