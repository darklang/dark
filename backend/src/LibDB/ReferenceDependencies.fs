/// Branch-visible dependency reads and review traversal.
module LibDB.ReferenceDependencies

open System.Threading.Tasks
open FSharp.Control.Tasks
open Fumble
open Prelude
open LibExecution.ProgramTypes
open LibDB.Sqlite
open LibDB.ReferenceNames

module PT = LibExecution.ProgramTypes
module DE = DependencyExtractor

let private storedNames = ReferenceNamesContent.storedNames

let private asDependencies (names : Names) : List<DE.Dependency> =
  names
  |> List.map (fun (reference : PT.Reference, location) ->
    ({ hash = reference.hash; itemKind = reference.kind; location = location }
    : DE.Dependency))
  |> List.distinct

let private location owner modules name : PT.PackageLocation =
  { owner = owner
    modules = if modules = "" then [] else modules.Split('.') |> Array.toList
    name = name }

/// The shared index may contain alias names authored on other branches.
let private indexedNames (hashes : List<Hash>) =
  task {
    let mutable indexed = Map.empty
    let mutable seen = Set.empty
    let mutable ambiguous = Set.empty
    // One parameter keeps wide graphs independent of SQLite's parameter limit.
    // Write JSON explicitly so this path works in AOT builds too.
    use buffer = new System.IO.MemoryStream()
    use writer = new System.Text.Json.Utf8JsonWriter(buffer)
    writer.WriteStartArray()
    for (Hash hash) in hashes do
      writer.WriteStringValue(hash)
    writer.WriteEndArray()
    writer.Flush()
    let hashJson = System.Text.Encoding.UTF8.GetString(buffer.ToArray())
    let! rows =
      Sql.query
        """SELECT item_hash, depends_on_hash, depends_on_item_type,
                          depends_on_owner, depends_on_modules, depends_on_name
                   FROM package_dependencies
                   WHERE item_hash IN (SELECT value FROM json_each(@hashes)) AND depends_on_owner IS NOT NULL"""
      |> Sql.parameters [ "hashes", Sql.string hashJson ]
      |> Sql.executeAsync (fun row ->
        Hash(row.string "item_hash"),
        PT.Reference.fromHashAndKind (
          Hash(row.string "depends_on_hash"),
          PT.ItemKind.fromString (row.string "depends_on_item_type")
        ),
        location
          (row.string "depends_on_owner")
          (row.string "depends_on_modules")
          (row.string "depends_on_name"))
    for (hash, target, name) in rows do
      if Set.contains (hash, target) seen then ambiguous <- Set.add hash ambiguous
      seen <- Set.add (hash, target) seen
      indexed <-
        Map.add
          hash
          ((target, Some name)
           :: (Map.tryFind hash indexed |> Option.defaultValue []))
          indexed
    return indexed, ambiguous
  }

/// Find callers using the reference names visible on this branch.
/// Dependency queries and propagation both use this function.
let dependents
  (state : State)
  (branchBindings : Map<PT.PackageLocation, Option<PT.Reference>>)
  (targets : List<Queries.LocationTarget>)
  : Task<List<Queries.LocationDependent>> =
  task {
    let! hashes = Queries.getDependentHashesByTargets targets
    // The index still contains the original names. Include callers whose
    // saved reference names now point at one of the requested targets.
    let renamedCallers =
      state
      |> Map.toList
      |> List.choose (fun ((_, caller), names) ->
        if
          names
          |> List.exists (fun (target, current) ->
            targets
            |> List.exists (fun t ->
              target.kind = t.itemKind && current = Some t.location))
        then
          let (Hash h) = caller.hash
          Some h
        else
          None)
    let hashes = List.distinct (hashes @ renamedCallers)
    let! mainLocations = Queries.getLiveLocationsForHashes hashes

    let branchLocationsByHash : Map<Hash, List<PT.ItemKind * PT.PackageLocation>> =
      branchBindings
      |> Map.toList
      |> List.choose (fun (location, reference) ->
        reference
        |> Option.map (fun reference -> reference.hash, (reference.kind, location)))
      |> List.groupBy fst
      |> Map.map (List.map snd)

    // Include both inherited callers and callers added on the branch.
    // Several names can share the same body, and each needs to follow the edit.
    let batchDependents =
      hashes
      |> List.collect (fun h ->
        let fromBranch =
          Map.tryFind (Hash h) branchLocationsByHash |> Option.defaultValue []
        let fromMain = Map.tryFind h mainLocations |> Option.defaultValue []

        // Branch first so it wins the dedup below for a location both describe.
        let resolved = (fromBranch @ fromMain) |> List.distinctBy snd
        resolved
        |> List.map (fun (kind, loc) ->
          ({ itemHash = Hash h; itemKind = kind; itemLocation = loc }
          : Queries.LocationDependent)))

    // Check ambiguous indexed names even when this branch has no saved metadata.
    // Another branch may have contributed aliases for the same shared body.
    let! _, ambiguous = indexedNames (hashes |> List.map Hash)
    let! batchDependents =
      if Map.isEmpty state && Set.isEmpty ambiguous then
        Task.FromResult batchDependents
      else
        task {
          let affectedVersions = state |> Map.keys |> Seq.map snd |> Set.ofSeq
          let mutable originals = Map.empty
          let mutable matches = []
          for d in batchDependents do
            let target = PT.Reference.fromHashAndKind (d.itemHash, d.itemKind)
            if
              not (Set.contains target affectedVersions)
              && not (Set.contains target.hash ambiguous)
            then
              matches <- d :: matches
            else
              if not (Map.containsKey target originals) then
                let! names = storedNames target
                let names =
                  names
                  |> Option.defaultWith (fun () ->
                    Exception.raiseInternal
                      "Cannot read reference names: stored declaration is missing"
                      [])
                originals <- Map.add target names originals
              let deps =
                effectiveNames
                  state
                  d.itemLocation
                  target
                  (Microsoft.FSharp.Collections.Map.find target originals)
                |> asDependencies
              let follows =
                deps
                |> List.exists (fun edge ->
                  targets
                  |> List.exists (fun t ->
                    let followsTarget =
                      match edge.location with
                      | Some loc -> loc = t.location
                      | None -> List.contains edge.hash t.hashes
                    edge.itemKind = t.itemKind && followsTarget))
              if follows then matches <- d :: matches
          return List.rev matches
        }

    return
      batchDependents
      |> List.filter (fun dependent ->
        match Map.tryFind dependent.itemLocation branchBindings with
        | None -> true
        | Some current ->
          let reference =
            PT.Reference.fromHashAndKind (dependent.itemHash, dependent.itemKind)
          current = Some reference)

  }

/// One branch-visible binding snapshot for the entire review.
let liveBindings branch =
  task {
    let! main =
      Sql.query
        "SELECT owner, modules, name, item_type, item_hash FROM locations WHERE unlisted_at IS NULL"
      |> Sql.executeAsync (fun row ->
        location (row.string "owner") (row.string "modules") (row.string "name"),
        PT.Reference.fromHashAndKind (
          Hash(row.string "item_hash"),
          PT.ItemKind.fromString (row.string "item_type")
        ))
    let! changes = branchBindings branch
    return
      changes
      |> Map.fold
        (fun bindings name target ->
          match target with
          | Some target -> Map.add name target bindings
          | None -> Map.remove name bindings)
        (Map.ofList main)
  }

let private namedReferencesFrom
  state
  (live : List<PT.PackageLocation * PT.Reference>)
  =
  task {
    let hashes = live |> List.map (fun (_, target) -> target.hash) |> List.distinct
    let! indexed, ambiguous = indexedNames hashes
    let affected = state |> Map.keys |> Seq.map snd |> Set.ofSeq
    let needsBody (target : PT.Reference) =
      Set.contains target affected || Set.contains target.hash ambiguous
    // A shared index can contain aliases from another branch. Read the body whenever
    // saved names or ambiguous aliases require it, once per content version.
    let mutable originals = Map.empty
    let mutable missing = false
    for (_, target) in live do
      if needsBody target && not (Map.containsKey target originals) then
        let! names = storedNames target
        match names with
        | Some names -> originals <- Map.add target names originals
        | None -> missing <- true
    if missing then
      return Error "Cannot read reference names: stored declaration is missing"
    else
      return
        live
        |> List.collect (fun (name, caller) ->
          let names =
            match Map.tryFind caller originals with
            | Some original -> effectiveNames state name caller original
            | None -> Map.tryFind caller.hash indexed |> Option.defaultValue []
          names
          |> List.choose (fun (target, referenceName) ->
            referenceName
            |> Option.map (fun referenceName ->
              name, caller, target, referenceName)))
        |> List.distinct
        |> Ok
  }

/// Explicit dependency reads still return edges; reviews filter them before crossing into Dark.
let namedReferences branch sources =
  task {
    let! state = load branch
    let! live =
      if List.isEmpty sources then
        task {
          let! bindings = liveBindings branch
          return Map.toList bindings
        }
      else
        Task.FromResult(List.distinct sources)
    return! namedReferencesFrom state live
  }

type NamedReference =
  PT.PackageLocation * PT.Reference * PT.Reference * PT.PackageLocation
type StaleReference = NamedReference * PT.Reference

/// Build the reverse index once, then visit only callers reachable from each seed.
/// Only direct edges must be stale; downstream callers follow even if currently up to date.
let reviewGraph
  (live : Map<PT.PackageLocation, PT.Reference>)
  (edges : List<NamedReference>)
  (changed : List<PT.PackageLocation * PT.Reference>)
  : List<StaleReference> * List<PT.PackageLocation * PT.PackageLocation> =
  let stale =
    edges
    |> List.choose (fun ((_, _, target, dependency) as edge) ->
      match Map.tryFind dependency live with
      | Some current when current.hash <> target.hash -> Some(edge, current)
      | _ -> None)
  let reverse =
    if List.isEmpty changed then
      Map.empty
    else
      edges |> List.groupBy (fun (_, _, _, dependency) -> dependency)
  let callers name = Map.tryFind name reverse |> Option.defaultValue []
  let changedNames = changed |> List.map fst |> Set.ofList
  let mutable reported = Set.empty
  let result = ResizeArray<_>()
  for (seed, target) in changed do
    let pending = System.Collections.Generic.Queue<PT.PackageLocation>()
    let seen = System.Collections.Generic.HashSet<PT.PackageLocation>()
    let enqueue name = if seen.Add name then pending.Enqueue name
    for (usage, _, using, _) in callers seed do
      if using.hash <> target.hash then enqueue usage
    while pending.Count > 0 do
      let usage = pending.Dequeue()
      if
        not (Set.contains usage changedNames) && not (Set.contains usage reported)
      then
        reported <- Set.add usage reported
        result.Add(usage, seed)
      for (caller, _, _, _) in callers usage do
        enqueue caller
  stale, List.ofSeq result

/// Main's index can narrow the graph before any edges cross out of SQLite.
/// Saved names and ambiguous hashes are exceptions: read those callers even when
/// their indexed names look current, because their effective names may be stale.
let private reviewMain
  (state : State)
  (live : Map<PT.PackageLocation, PT.Reference>)
  (changed : List<PT.PackageLocation * PT.Reference>)
  =
  task {
    let! candidates =
      Sql.query
        """WITH stale AS MATERIALIZED (
             SELECT d.depends_on_owner AS owner,
                    d.depends_on_modules AS modules, d.depends_on_name AS name,
                    d.depends_on_hash AS hash
             FROM (SELECT DISTINCT depends_on_owner, depends_on_modules,
                          depends_on_name, depends_on_hash
                   FROM package_dependencies WHERE depends_on_owner IS NOT NULL) d
             JOIN locations l ON l.owner = d.depends_on_owner
               AND l.modules = d.depends_on_modules AND l.name = d.depends_on_name
               AND l.unlisted_at IS NULL
             WHERE l.item_hash <> d.depends_on_hash
           )
           SELECT DISTINCT d.item_hash
           FROM stale s CROSS JOIN package_dependencies d
           WHERE d.depends_on_owner = s.owner AND d.depends_on_modules = s.modules
             AND d.depends_on_name = s.name AND d.depends_on_hash = s.hash
           UNION
           SELECT item_hash FROM package_dependencies
           WHERE depends_on_owner IS NOT NULL
           GROUP BY item_hash, depends_on_hash, depends_on_item_type
           HAVING COUNT(*) > 1"""
      |> Sql.executeAsync (fun row -> Hash(row.string "item_hash"))
    let byHash = live |> Map.toList |> List.groupBy (fun (_, target) -> target.hash)
    let sources hashes =
      hashes
      |> List.distinct
      |> List.collect (fun hash -> Map.tryFind hash byHash |> Option.defaultValue [])
    let exceptional = state |> Map.keys |> List.map (fun (_, target) -> target.hash)
    let initialHashes = List.distinct (candidates @ exceptional)
    let! initial = namedReferencesFrom state (sources initialHashes)
    match initial with
    | Error message -> return Error message
    | Ok initial ->
      let mutable edges = initial
      let mutable fetched = Set.ofList initialHashes
      let mutable failure = None
      // Overrides can point to names absent from the index. They are already in
      // `edges`; SQL only discovers the remaining candidates, which are checked
      // against their effective names before traversal follows them.
      let fetchCallers (names : List<PT.PackageLocation>) =
        task {
          use buffer = new System.IO.MemoryStream()
          use writer = new System.Text.Json.Utf8JsonWriter(buffer)
          writer.WriteStartArray()
          for name in names do
            writer.WriteStartArray()
            writer.WriteStringValue(name.owner)
            writer.WriteStringValue(String.concat "." name.modules)
            writer.WriteStringValue(name.name)
            writer.WriteEndArray()
          writer.WriteEndArray()
          writer.Flush()
          let json = System.Text.Encoding.UTF8.GetString(buffer.ToArray())
          let! hashes =
            Sql.query
              """SELECT DISTINCT d.item_hash
                 FROM json_each(@names) n JOIN package_dependencies d
                   ON d.depends_on_owner = json_extract(n.value, '$[0]')
                  AND d.depends_on_modules = json_extract(n.value, '$[1]')
                  AND d.depends_on_name = json_extract(n.value, '$[2]')
                 WHERE d.depends_on_owner IS NOT NULL"""
            |> Sql.parameters [ "names", Sql.string json ]
            |> Sql.executeAsync (fun row -> Hash(row.string "item_hash"))
          let fresh =
            hashes |> List.filter (fun hash -> not (Set.contains hash fetched))
          if not (List.isEmpty fresh) then
            let! result = namedReferencesFrom state (sources fresh)
            match result with
            | Error message -> failure <- Some message
            | Ok extra ->
              edges <- extra @ edges
              fetched <- Set.union fetched (Set.ofList fresh)
        }
      if not (List.isEmpty changed) then
        let seedNames = changed |> List.map fst |> List.distinct
        do! fetchCallers seedNames
        let seeds = changed |> List.groupBy fst
        let mutable pending =
          edges
          |> List.choose (fun (usage, _, target, dependency) ->
            match Map.tryFind dependency seeds with
            | Some versions when
              versions
              |> List.exists (fun (_, current) -> current.hash <> target.hash)
              ->
              Some usage
            | _ -> None)
        // A seed reached from another seed must still expand its current edges.
        let mutable visited = Set.empty
        while not (List.isEmpty pending) && Option.isNone failure do
          let frontier =
            pending
            |> List.distinct
            |> List.filter (fun name -> not (Set.contains name visited))
          pending <- []
          if not (List.isEmpty frontier) then
            let frontierSet = Set.ofList frontier
            visited <- Set.union visited frontierSet
            do! fetchCallers frontier
            pending <-
              edges
              |> List.choose (fun (usage, _, _, dependency) ->
                if Set.contains dependency frontierSet then Some usage else None)
      match failure with
      | Some message -> return Error message
      | None -> return Ok(reviewGraph live (List.distinct edges) changed)
  }

/// Share binding and reference-name state across constraint and propagation review.
/// Main uses indexed candidates; branches retain the full overlay-aware graph.
let dependencyReview branch changed =
  task {
    let! state = load branch
    let! live = liveBindings branch
    if branch.IsMain then
      return! reviewMain state live changed
    else
      let! edges = namedReferencesFrom state (Map.toList live)
      return edges |> Result.map (fun edges -> reviewGraph live edges changed)
  }
