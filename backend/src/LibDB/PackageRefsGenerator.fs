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
  let prefix =
    match itemType with
    | "type" -> "type"
    | "trait" -> "trait"
    | "impl" -> "impl"
    | _ -> "fn"
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


/// Query the DB for all current Darklang-owned locations, make them this process's
/// hashes, and, when `writeSourceFile`, write `package-ref-hashes.txt` in the source
/// tree. Only a reload's FINAL state belongs in that file: every process in the
/// checkout reads it at startup, so an intermediate state written there is a wrong
/// answer for whoever starts next.
let generate (writeSourceFile : bool) : Ply<unit> =
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

    let traitRefKeys =
      PackageRefs.Trait._lookup
      |> Map.toList
      |> List.map (fun ((modules, name), _hash) ->
        buildKey "trait" (String.concat "." modules) name)
      |> Set.ofList

    // Union in whatever the existing file already knew. `_lookup` populates only as
    // each `PackageRefs` module initializes, so a process that regenerates before
    // touching them all (or an older binary predating a ref) would write a SHORTER
    // file and drop refs. Keys that no longer resolve are dropped below by the
    // `List.choose` against the DB.
    let existingKeys =
      readExistingFile () |> Map.toList |> List.map fst |> Set.ofList

    let allRefKeys =
      Set.unionMany [ typeRefKeys; fnRefKeys; traitRefKeys; existingKeys ]

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

    // A HARDENED pin moving is refused here rather than reported as a diff. See
    // `PackageRefs.hardened` for which ones and why: the point is that the failure
    // arrives at the moment the identity moves, naming what moved and what it moved
    // to, instead of arriving in CI as "the worktree is dirty".
    //
    // The escape hatch is deliberate and deliberately awkward. Sometimes the move IS
    // correct (the type genuinely changed), and then re-running with the variable
    // set is the way to say so on purpose.
    let hardenedMoves =
      merged
      |> List.choose (fun (key, hash) ->
        if PackageRefs.hardened |> Set.contains key then
          match Map.tryFind key existingMap with
          | Some old when old <> hash -> Some(key, old, hash)
          | _ -> None
        else
          None)

    let repinAllowed =
      match System.Environment.GetEnvironmentVariable "DARK_REPIN_HARDENED" with
      | "1" -> true
      | _ -> false

    if not (List.isEmpty hardenedMoves) && not repinAllowed then
      let detail =
        hardenedMoves
        |> List.map (fun (key, old, hash) ->
          $"  {key}\n    was {old}\n    now {hash}")
        |> String.concat "\n"

      Exception.raiseInternal
        ("A hardened package ref moved. These are constructed by name throughout the kernel, so their "
         + "identity moving means something changed underneath the whole tree -- check that before "
         + "re-pinning. If the move is correct, re-run with DARK_REPIN_HARDENED=1.\n"
         + detail)
        []

    let lines = merged |> List.map (fun (key, hash) -> $"{key}|{hash}")

    // Always set the in-memory cache so PackageRefs lookups work
    let hashMap = merged |> Map.ofList
    PackageRefs.setHashes hashMap

    // Write the source-tree file (skip if the directory doesn't exist,
    // e.g. on installed CLIs where the source tree isn't available).
    //
    // Every process in this checkout reads this one file at startup, whatever its
    // DARK_CONFIG_RUNDIR, so a reload into a private rundir still writes shared state.
    // It used to be written from both fills, truncating first, so a CLI starting during
    // any reload anywhere in the checkout could load an empty map, a half-written one,
    // or the pre-resolution hashes, and raise FnNotFound on its first kernel call. That
    // was every false failure `gates all --parallel` reported: reload-is-reproducible
    // reloads twice while the other gates start dozens of CLIs. So: only the final fill
    // writes, an identical file is left alone, and a changed one is replaced by rename,
    // which readers see whole or not at all.
    let dir = System.IO.Path.GetDirectoryName(sourceTreePath)
    if writeSourceFile && System.IO.Directory.Exists(dir) then
      let newLines = lines |> Array.ofList
      let existingLines =
        try
          if System.IO.File.Exists(sourceTreePath) then
            System.IO.File.ReadAllLines(sourceTreePath)
          else
            [||]
        with _ ->
          [||]
      if existingLines = newLines then
        print $"  {newLines.Length} package ref hashes unchanged in {sourceTreePath}"
      else
        let tmp = $"{sourceTreePath}.{System.Environment.ProcessId}.tmp"
        System.IO.File.WriteAllLines(tmp, newLines)
        System.IO.File.Move(tmp, sourceTreePath, true)
        print $"  Wrote {newLines.Length} package ref hashes to {sourceTreePath}"

    // Report any items referenced but not found anywhere
    let foundKeys = merged |> List.map fst |> Set.ofList
    let missing = Set.difference allRefKeys foundKeys

    if not (Set.isEmpty missing) then
      print
        $"  Warning: {Set.count missing} PackageRefs items not found in DB or existing file:"
      for key in missing do
        print $"    - {key}"
  }
