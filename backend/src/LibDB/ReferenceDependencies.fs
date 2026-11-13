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

/// <param hashes> as one JSON array, so a wide graph stays one SQLite parameter. Written by hand
/// so this path works in AOT builds too.
let private hashesJson (hashes : List<Hash>) : string =
  use buffer = new System.IO.MemoryStream()
  use writer = new System.Text.Json.Utf8JsonWriter(buffer)
  writer.WriteStartArray()
  for (Hash hash) in hashes do
    writer.WriteStringValue(hash)
  writer.WriteEndArray()
  writer.Flush()
  System.Text.Encoding.UTF8.GetString(buffer.ToArray())

/// Which of <param hashes> the shared index names one target under more than one name: the
/// `ambiguous` half of `indexedNames`, counted by SQLite rather than read row by row.
let ambiguousHashes (hashes : List<Hash>) : Task<Set<Hash>> =
  task {
    let! rows =
      Sql.query
        """SELECT DISTINCT item_hash
                   FROM package_dependencies
                   WHERE item_hash IN (SELECT value FROM json_each(@hashes)) AND depends_on_owner <> ''
                   GROUP BY item_hash, depends_on_hash, depends_on_item_type
                   HAVING COUNT(*) > 1"""
      |> Sql.parameters [ "hashes", Sql.string (hashesJson hashes) ]
      |> Sql.executeAsync (fun row -> Hash(row.string "item_hash"))
    return Set.ofList rows
  }

/// The shared index may contain alias names authored on other branches.
let indexedNames (hashes : List<Hash>) =
  task {
    let hashJson = hashesJson hashes
    let! rows =
      Sql.query
        """SELECT item_hash, depends_on_hash, depends_on_item_type,
                          depends_on_owner, depends_on_modules, depends_on_name
                   FROM package_dependencies
                   WHERE item_hash IN (SELECT value FROM json_each(@hashes)) AND depends_on_owner <> ''"""
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
    let indexed, _, ambiguous =
      rows
      |> List.fold
        (fun (indexed, seen, ambiguous) (hash, target, name) ->
          let ambiguous =
            if Set.contains (hash, target) seen then
              Set.add hash ambiguous
            else
              ambiguous
          let named =
            (target, Some name)
            :: (Map.tryFind hash indexed |> Option.defaultValue [])
          (Map.add hash named indexed, Set.add (hash, target) seen, ambiguous))
        (Map.empty, Set.empty, Set.empty)
    return indexed, ambiguous
  }

/// Find callers using the reference names visible on this branch.
/// Dependency queries and propagation both use this function.
let dependents
  (state : State)
  (branch : PT.BranchId)
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
    // Where the branch's view binds each of them; several names can share one body, and each
    // needs to follow the edit.
    let! live = Queries.getLiveLocationsForHashes branch hashes
    let batchDependents =
      hashes
      |> List.collect (fun h ->
        Map.tryFind h live
        |> Option.defaultValue []
        |> List.map (fun (kind, loc) ->
          ({ itemHash = Hash h; itemKind = kind; itemLocation = loc }
          : Queries.LocationDependent)))

    // Check ambiguous indexed names even when this branch has no saved metadata.
    // Another branch may have contributed aliases for the same shared body.
    let! ambiguous = ambiguousHashes (hashes |> List.map Hash)
    let! batchDependents =
      if Map.isEmpty state && Set.isEmpty ambiguous then
        Task.FromResult batchDependents
      else
        task {
          let affectedVersions = state |> Map.keys |> Seq.map snd |> Set.ofSeq
          let targetOf (d : Queries.LocationDependent) =
            PT.Reference.fromHashAndKind (d.itemHash, d.itemKind)
          let needsBody (target : PT.Reference) =
            Set.contains target affectedVersions
            || Set.contains target.hash ambiguous
          let! originals =
            batchDependents
            |> List.map targetOf
            |> List.filter needsBody
            |> List.distinct
            |> Ply.List.mapSequentially (fun target ->
              uply {
                let! names = storedNames target
                let names =
                  names
                  |> Option.defaultWith (fun () ->
                    Exception.raiseInternal
                      "Cannot read reference names: stored declaration is missing"
                      [])
                return (target, names)
              })
            |> Ply.map Map.ofList
            |> Ply.toTask
          return
            batchDependents
            |> List.filter (fun d ->
              let target = targetOf d
              if not (needsBody target) then
                true
              else
                effectiveNames
                  state
                  d.itemLocation
                  target
                  (Microsoft.FSharp.Collections.Map.find target originals)
                |> asDependencies
                |> List.exists (fun edge ->
                  targets
                  |> List.exists (fun t ->
                    let followsTarget =
                      match edge.location with
                      | Some loc -> loc = t.location
                      | None -> List.contains edge.hash t.hashes
                    edge.itemKind = t.itemKind && followsTarget)))
        }

    return batchDependents
  }

/// The bindings <param branch>'s view holds that also meet <param filter>, a condition on
/// `visible` that may read <param keys> as `@keys`.
let private bindingsWhere
  (branch : PT.BranchId)
  (filter : string)
  (keys : string)
  : Task<Map<PT.PackageLocation, PT.Reference>> =
  task {
    let! rows =
      Sql.query (
        Layers.view "@" Layers.locations
        + " SELECT owner, modules, name, item_type, item_hash FROM visible WHERE item_hash <> ''"
        + filter
      )
      |> Sql.parameters (("keys", Sql.string keys) :: Layers.sqlParams branch true)
      |> Sql.executeAsync (fun row ->
        location (row.string "owner") (row.string "modules") (row.string "name"),
        PT.Reference.fromHashAndKind (
          Hash(row.string "item_hash"),
          PT.ItemKind.fromString (row.string "item_type")
        ))
    return Map.ofList rows
  }

/// One branch-visible binding snapshot for the entire review: every name the view binds.
let liveBindings (branch : PT.BranchId) = bindingsWhere branch "" "[]"

/// The names the view binds to any of <param hashes>: what a review turns a caller hash into.
let private bindingsOfHashes
  (branch : PT.BranchId)
  (hashes : List<Hash>)
  : Task<Map<PT.PackageLocation, PT.Reference>> =
  if List.isEmpty hashes then
    Task.FromResult Map.empty
  else
    bindingsWhere
      branch
      " AND item_hash IN (SELECT value FROM json_each(@keys))"
      (hashes |> List.map (fun (Hash h) -> h) |> jsonStrings)

/// What the view binds at each of <param names>.
let private bindingsAt
  (branch : PT.BranchId)
  (names : List<PT.PackageLocation>)
  : Task<Map<PT.PackageLocation, PT.Reference>> =
  if List.isEmpty names then
    Task.FromResult Map.empty
  else
    bindingsWhere
      branch
      " AND (owner, modules, name) IN (SELECT json_extract(value, '$[0]'),
           json_extract(value, '$[1]'), json_extract(value, '$[2]') FROM json_each(@keys))"
      (names
       |> List.map (fun n -> [ n.owner; String.concat "." n.modules; n.name ])
       |> jsonRows)

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
    let! stored =
      live
      |> List.map snd
      |> List.filter needsBody
      |> ReferenceNamesContent.storedNamesEach
    if stored |> List.exists (fun (_, names) -> Option.isNone names) then
      return Error "Cannot read reference names: stored declaration is missing"
    else
      let originals =
        stored
        |> List.choose (fun (target, names) ->
          names |> Option.map (fun names -> (target, names)))
        |> Map.ofList
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
  // Adds the names not seen yet, in order; `queued` is reversed.
  let enqueue (seen, queued) name =
    if Set.contains name seen then
      (seen, queued)
    else
      (Set.add name seen, name :: queued)
  // Breadth-first, a level at a time: every name reached from the seed, each once, in visit order.
  let reached (seed, target : PT.Reference) : List<PT.PackageLocation> =
    let rec walk frontier seen levels =
      if List.isEmpty frontier then
        levels |> List.rev |> List.concat
      else
        let seen, queued =
          frontier
          |> List.collect (fun usage ->
            callers usage |> List.map (fun (caller, _, _, _) -> caller))
          |> List.fold enqueue (seen, [])
        walk (List.rev queued) seen (frontier :: levels)
    let seen, queued =
      callers seed
      |> List.choose (fun (usage, _, using, _) ->
        if using.hash <> target.hash then Some usage else None)
      |> List.fold enqueue (Set.empty, [])
    walk (List.rev queued) seen []
  let result, _ =
    changed
    |> List.fold
      (fun acc (seed, target) ->
        reached (seed, target)
        |> List.fold
          (fun (result, reported) usage ->
            if
              not (Set.contains usage changedNames)
              && not (Set.contains usage reported)
            then
              ((usage, seed) :: result, Set.add usage reported)
            else
              (result, reported))
          acc)
      ([], Set.empty)
  stale, List.rev result

/// The dependency index narrows the graph before any edges cross out of SQLite: only callers of a name
/// whose version in <param branch>'s view is not the one the edge recorded. Saved names and ambiguous
/// hashes are exceptions: read those callers even when their indexed names look current, because their
/// effective names may be stale.
let private reviewIndexed
  (branch : PT.BranchId)
  (state : State)
  (changed : List<PT.PackageLocation * PT.Reference>)
  =
  task {
    let! candidates =
      Sql.query (
        Layers.view "@" Layers.locations
        // From each binding to the callers recorded against another version of its name: one
        // probe of `idx_package_dependencies_target` per binding.
        + """
           SELECT DISTINCT d.item_hash
           FROM visible l CROSS JOIN package_dependencies d
             ON d.depends_on_owner = l.owner AND d.depends_on_modules = l.modules
            AND d.depends_on_name = l.name
           WHERE d.depends_on_hash <> l.item_hash AND l.item_hash <> ''
           UNION
           SELECT item_hash FROM package_dependencies
           WHERE depends_on_owner <> ''
           GROUP BY item_hash, depends_on_hash, depends_on_item_type
           HAVING COUNT(*) > 1"""
      )
      |> Sql.parameters (Layers.sqlParams branch true)
      |> Sql.executeAsync (fun row -> Hash(row.string "item_hash"))
    // Each hash's names in the view, in name order. Read for just these hashes: the review never
    // asks about any other binding, and reading all of them was most of what it cost.
    let sources hashes =
      task {
        let hashes = List.distinct hashes
        let! bound = bindingsOfHashes branch hashes
        let byHash =
          bound |> Map.toList |> List.groupBy (fun (_, target) -> target.hash)
        return
          hashes
          |> List.collect (fun hash ->
            Map.tryFind hash byHash |> Option.defaultValue [])
      }
    let exceptional = state |> Map.keys |> List.map (fun (_, target) -> target.hash)
    let initialHashes = List.distinct (candidates @ exceptional)
    let! initialSources = sources initialHashes
    let! initial = namedReferencesFrom state initialSources
    match initial with
    | Error message -> return Error message
    | Ok initial ->
      // Overrides can point to names absent from the index. They are already in
      // `edges`; SQL only discovers the remaining candidates, which are checked
      // against their effective names before traversal follows them.
      let fetchCallers
        (edges : List<NamedReference>, fetched : Set<Hash>)
        (names : List<PT.PackageLocation>)
        : Task<Result<List<NamedReference> * Set<Hash>, string>> =
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
                 WHERE d.depends_on_owner <> ''"""
            |> Sql.parameters [ "names", Sql.string json ]
            |> Sql.executeAsync (fun row -> Hash(row.string "item_hash"))
          let fresh =
            hashes |> List.filter (fun hash -> not (Set.contains hash fetched))
          if List.isEmpty fresh then
            return Ok(edges, fetched)
          else
            let! freshSources = sources fresh
            match! namedReferencesFrom state freshSources with
            | Error message -> return Error message
            | Ok extra ->
              return Ok(extra @ edges, Set.union fetched (Set.ofList fresh))
        }
      // A seed reached from another seed must still expand its current edges.
      let rec expand edges fetched visited pending =
        task {
          let frontier =
            pending
            |> List.distinct
            |> List.filter (fun name -> not (Set.contains name visited))
          if List.isEmpty frontier then
            return Ok edges
          else
            let frontierSet = Set.ofList frontier
            match! fetchCallers (edges, fetched) frontier with
            | Error message -> return Error message
            | Ok(edges, fetched) ->
              let pending =
                edges
                |> List.choose (fun (usage, _, _, dependency) ->
                  if Set.contains dependency frontierSet then Some usage else None)
              return! expand edges fetched (Set.union visited frontierSet) pending
        }
      let! expanded =
        if List.isEmpty changed then
          Task.FromResult(Ok initial)
        else
          task {
            let seedNames = changed |> List.map fst |> List.distinct
            match! fetchCallers (initial, Set.ofList initialHashes) seedNames with
            | Error message -> return Error message
            | Ok(edges, fetched) ->
              let seeds = changed |> List.groupBy fst
              let pending =
                edges
                |> List.choose (fun (usage, _, target, dependency) ->
                  match Map.tryFind dependency seeds with
                  | Some versions when
                    versions
                    |> List.exists (fun (_, current) -> current.hash <> target.hash)
                    ->
                    Some usage
                  | _ -> None)
              return! expand edges fetched Set.empty pending
          }
      match expanded with
      | Error message -> return Error message
      | Ok edges ->
        let edges = List.distinct edges
        // The graph only asks what each edge's dependency is bound to now.
        let! live =
          edges
          |> List.map (fun (_, _, _, dependency) -> dependency)
          |> List.distinct
          |> bindingsAt branch
        return Ok(reviewGraph live edges changed)
  }

/// Share binding and reference-name state across constraint and propagation review.
let dependencyReview branch changed =
  task {
    let! state = load branch
    return! reviewIndexed branch state changed
  }
