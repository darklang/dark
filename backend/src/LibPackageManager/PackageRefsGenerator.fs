/// Writes `package-ref-hashes.txt` with current hashes from the DB.
/// PackageRefs.fs reads this file at startup.
module LibPackageManager.PackageRefsGenerator

open Prelude

open Fumble
open LibDB.Db

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

    let allRefKeys = Set.union typeRefKeys fnRefKeys

    // Query all Darklang-owned locations from DB
    let! dbRows =
      Sql.query
        """
        SELECT item_type, modules, name, item_hash
        FROM locations
        WHERE owner = 'Darklang'
          AND deprecated_at IS NULL
        """
      |> Sql.executeAsync (fun read ->
        let itemType = read.string "item_type"
        let modules = read.string "modules"
        let name = read.string "name"
        let hash = read.string "item_hash"
        (buildKey itemType modules name, hash))

    let dbMap = dbRows |> Map.ofList

    // Read the existing file to preserve entries not found in DB
    // (e.g. RT types that share hashes with PT types and aren't in locations)
    let existingMap =
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
