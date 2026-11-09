module LibDB.ProgramTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module PackageLocation = LibDB.PackageLocation


// Name resolution, as a branch sees it: every read is over `Layers.visible`, the first layer of the
// branch's view holding a live row for a name. Content is shared and read by hash.

/// <param select> over the visible bindings of a view (`visible`, with `seq` and `rank`).
let private overVisible (select : string) : string =
  Layers.view "@" Layers.locations + " " + select

let private readLocation (read : RowReader) : PT.PackageLocation =
  let modulesStr = read.string "modules"
  { owner = read.string "owner"
    modules = PackageLocation.modulesOfColumn modulesStr
    name = read.string "name" }

let private findItem
  (branch : PT.BranchId)
  (itemType : string)
  (location : PT.PackageLocation)
  : Ply<Option<Hash>> =
  uply {
    // The name's binding, whatever kind it holds: a branch that rebound a fn's name to a value
    // has no fn there, whatever main says. The few live rows at the name, ranked by the view.
    let! rows =
      Sql.query
        "SELECT branch_id, draft, item_hash, item_type FROM locations
         WHERE owner = @owner AND modules = @modules AND name = @name AND live = 1"
      |> Sql.parameters
        [ "owner", Sql.string location.owner
          "modules", Sql.string (String.concat "." location.modules)
          "name", Sql.string location.name ]
      |> Sql.executeAsync (fun read ->
        (read.string "branch_id",
         read.int64 "draft" <> 0L,
         (read.string "item_hash", read.string "item_type")))
    return
      match Layers.pick (Layers.chainOf branch) rows with
      | Some(h, t) when h <> "" && t = itemType -> Some(Hash h)
      | _ -> None
  }

let private getItem<'a>
  (table : string)
  (lookupColumn : string)
  (deserialize : Hash -> byte[] -> 'a)
  (hash : Hash)
  : Ply<Option<'a>> =
  uply {
    let (Hash hashStr) = hash
    return!
      Sql.query
        $"""
        SELECT pt_def
        FROM {table}
        WHERE {lookupColumn} = @hash
        """
      |> Sql.parameters [ "hash", Sql.string hashStr ]
      |> Sql.executeRowOptionAsync (fun read -> read.bytes "pt_def")
      |> Task.map (Option.map (deserialize hash))
  }

/// The names <param hash> holds on <param branch>: the nearest layer's first (a branch's own names
/// before what it inherits), newest binding first. A caller that labels content for display and wants
/// main's name to win asks main first (`pmLocations`, `ScmBindings.LocationOf`).
let private getItemLocations
  (branch : PT.BranchId)
  (itemType : string)
  (hash : Hash)
  : Ply<List<PT.PackageLocation>> =
  uply {
    let (Hash hashStr) = hash
    // Every live row at every name some live row binds to this hash, so the view can be judged per
    // name: a branch that rebound or removed the name hides main's row there.
    let! rows =
      Sql.query
        "SELECT rowid AS seq, branch_id, draft, owner, modules, name, item_hash, item_type
         FROM locations
         WHERE live = 1 AND (owner, modules, name) IN
           (SELECT owner, modules, name FROM locations
            WHERE item_hash = @item_hash AND item_type = @item_type AND live = 1)"
      |> Sql.parameters
        [ "item_hash", Sql.string hashStr; "item_type", Sql.string itemType ]
      |> Sql.executeAsync (fun read ->
        (readLocation read,
         (read.string "branch_id",
          read.int64 "draft" <> 0L,
          (read.int64 "seq", read.string "item_hash", read.string "item_type"))))
    let chain = Layers.chainOf branch
    return
      rows
      |> List.groupBy fst
      |> Map.toList
      |> List.choose (fun (loc, entries) ->
        let candidates = entries |> List.map snd
        match Layers.pick chain candidates with
        | Some(seq, h, t) when h = hashStr && t = itemType ->
          let rank =
            candidates
            |> List.choose (fun (b, d, (s, _, _)) ->
              if s = seq then Layers.rankIn chain b d else None)
            |> List.tryHead
            |> Option.defaultValue 0
          Some(rank, -seq, loc)
        | _ -> None)
      |> List.sortBy (fun (rank, negSeq, _) -> (rank, negSeq))
      |> List.map (fun (_, _, loc) -> loc)
  }

/// Every name this hash has EVER had on <param branch>'s view, live ones first.
///
/// Separate from `getItemLocations` because only live-only is safe for logic: a cascade repointing
/// a superseded name would undo a rename. This one is for DISPLAY -- viewing an old version whose
/// name moved on, where a former name beats a bare hash.
let private getItemLocationsEverNamed
  (branch : PT.BranchId)
  (itemType : string)
  (hash : Hash)
  : Ply<List<PT.PackageLocation>> =
  uply {
    let (Hash hashStr) = hash
    let! rows =
      Sql.query (
        Layers.cte "@"
        + " SELECT l.owner, l.modules, l.name FROM locations l
            JOIN layer y ON y.branch_id = l.branch_id AND y.draft = l.draft
            WHERE l.item_hash = @item_hash AND l.item_type = @item_type
            ORDER BY l.live DESC, y.rank, l.rowid DESC"
      )
      |> Sql.parameters (
        [ "item_hash", Sql.string hashStr; "item_type", Sql.string itemType ]
        @ Layers.sqlParams branch true
      )
      |> Sql.executeAsync readLocation
    return List.distinct rows
  }


module Type =
  let find branch = findItem branch "type"
  let get = getItem "package_types" "hash" BS.PT.PackageType.deserialize
  let getLocations branch = getItemLocations branch "type"
  let getLocationsEverNamed branch = getItemLocationsEverNamed branch "type"


module Trait =
  let find branch = findItem branch "trait"
  let get = getItem "package_traits" "hash" BS.PT.Trait.deserialize
  let getLocations branch = getItemLocations branch "trait"
  let getLocationsEverNamed branch = getItemLocationsEverNamed branch "trait"

  /// Every live trait name's last segment on <param branch>, once.
  let names (branch : PT.BranchId) : Ply<HashSet<string>> =
    uply {
      let! rows =
        Sql.query (
          overVisible
            "SELECT DISTINCT name FROM visible WHERE item_type = 'trait' AND item_hash <> ''"
        )
        |> Sql.parameters (Layers.sqlParams branch true)
        |> Sql.executeAsync (fun read -> read.string "name")
      return HashSet<string>(rows)
    }

module TraitImpl =
  let find branch = findItem branch "impl"

  /// Every impl hash <param branch> binds at some name: what makes an impl dispatchable there.
  let liveHashes (branch : PT.BranchId) : Ply<HashSet<string>> =
    uply {
      let! rows =
        Sql.query (
          overVisible
            "SELECT DISTINCT item_hash FROM visible WHERE item_type = 'impl' AND item_hash <> ''"
        )
        |> Sql.parameters (Layers.sqlParams branch true)
        |> Sql.executeAsync (fun read -> read.string "item_hash")
      return HashSet<string>(rows)
    }
  let get = getItem "package_trait_impls" "hash" BS.PT.TraitImpl.deserialize
  let getLocations branch = getItemLocations branch "impl"
  let getLocationsEverNamed branch = getItemLocationsEverNamed branch "impl"


module Value =
  let find branch = findItem branch "value"
  let get = getItem "package_values" "hash" BS.PT.PackageValue.deserialize
  let getLocations branch = getItemLocations branch "value"
  let getLocationsEverNamed branch = getItemLocationsEverNamed branch "value"

module Fn =
  let find branch = findItem branch "fn"
  let get = getItem "package_functions" "hash" BS.PT.PackageFn.deserialize
  let getLocations branch = getItemLocations branch "fn"
  let getLocationsEverNamed branch = getItemLocationsEverNamed branch "fn"

  /// Whether a function hash is active and bound under `owner` on any branch, asked one hash at a
  /// time. The CLI uses this to skip consumer approval for bundled Darklang functions; the instance
  /// policy, run policy, and function ceilings still apply.
  ///
  /// One indexed lookup per hash (`idx_locations_item_hash`), rather than every bundled hash read
  /// up front: a command asks about the few dozen functions it calls, and loading all of them (a
  /// string and a set entry for each of thousands of rows) was the largest allocation of a short
  /// run.
  let ownedBy (owner : string) : string -> bool =
    let ask =
      System.Func<string, bool>(fun hash ->
        Sql.query
          """
          SELECT 1 FROM locations
          WHERE item_hash = @hash AND owner = @owner AND item_type = 'fn' AND live = 1
            AND item_hash <> ''
          LIMIT 1
          """
        |> Sql.parameters [ "hash", Sql.string hash; "owner", Sql.string owner ]
        |> Sql.executeExistsSync)
    // Hot path, so answers are remembered for the life of this function: `callerIsBundled` asks
    // about every frame of the chain on each call of a native builtin. Concurrent because spread
    // workers ask too.
    let known = System.Collections.Concurrent.ConcurrentDictionary<string, bool>()
    fun hash -> known.GetOrAdd(hash, ask)

module Test =
  let find branch = findItem branch "test"
  let get = getItem "package_tests" "hash" BS.PT.PackageTest.deserialize
  let getLocations branch = getItemLocations branch "test"
  let getLocationsEverNamed branch = getItemLocationsEverNamed branch "test"



/// Split a search query into lowercase tokens for name/doc matching.
let private tokenizeQuery (s : string) : List<string> =
  let spaced =
    System.Text.RegularExpressions.Regex.Replace(s, "([a-z0-9])([A-Z])", "$1 $2")
  spaced.Split([| ' '; '\t'; '.'; '-'; '_'; '/'; ','; ':' |])
  |> Array.toList
  |> List.map (fun t -> t.Trim().ToLower())
  |> List.filter (fun t -> t <> "")


/// Only when asked for: listing every module is a scan of every visible name, and a caller after
/// one kind of item (the traits, the tests) never reads it.
let private wantsModules (query : PT.Search.SearchQuery) : bool =
  query.entityTypes.IsEmpty
  || List.contains PT.Search.EntityType.Module query.entityTypes

/// The condition on a row's `owner` and `modules` (aliased `l`) that picks the module paths a search
/// names, and its parameters.
let private submoduleFilter
  (query : PT.Search.SearchQuery)
  : string * List<string * Microsoft.Data.Sqlite.SqliteParameter> =
  let currentModule = String.concat "." query.currentModule

  if query.exactMatch then
    if System.String.IsNullOrEmpty query.text then
      match query.currentModule with
      | [ owner ] -> ("""(owner = @owner)""", [ "owner", Sql.string owner ])
      | owner :: moduleParts ->
        let modulesPath = String.concat "." moduleParts
        ("""(owner = @owner AND modules = @modulesPath)""",
         [ "owner", Sql.string owner; "modulesPath", Sql.string modulesPath ])
      | [] -> ("""(1 = 1)""", [])
    else
      ($"(l.modules = @currentModule || '.' || @searchText)
         OR ({PackageLocation.modulePathSql} = @currentModule || '.' || @searchText)",
       [ "currentModule", Sql.string currentModule
         "searchText", Sql.string query.text ])
  else if System.String.IsNullOrEmpty currentModule then
    match query.searchDepth with
    | PT.Search.SearchDepth.OnlyDirectDescendants ->
      ("""(owner LIKE '%' || @searchText || '%')""",
       [ "searchText", Sql.string query.text ])
    | PT.Search.SearchDepth.AllDescendants ->
      ($"((owner LIKE '%%' || @searchText || '%%')
            OR (modules LIKE '%%' || @searchText || '%%')
            OR ({PackageLocation.modulePathSql} LIKE '%%' || @searchText || '%%'))",
       [ "searchText", Sql.string query.text ])
  else
    let directChildPattern = currentModule + ".%"
    match query.searchDepth with
    | PT.Search.SearchDepth.OnlyDirectDescendants ->
      ($"((modules LIKE @directChildPattern AND modules LIKE '%%' || @searchText || '%%')
            OR ({PackageLocation.modulePathSql} LIKE @directChildPattern AND {PackageLocation.modulePathSql} LIKE '%%' || @searchText || '%%'))",
       [ "currentModule", Sql.string currentModule
         "directChildPattern", Sql.string directChildPattern
         "searchText", Sql.string query.text ])
    | PT.Search.SearchDepth.AllDescendants ->
      ($"((modules LIKE @directChildPattern AND modules LIKE '%%' || @searchText || '%%')
            OR ({PackageLocation.modulePathSql} LIKE @directChildPattern AND {PackageLocation.modulePathSql} LIKE '%%' || @searchText || '%%'))",
       [ "currentModule", Sql.string currentModule
         "directChildPattern", Sql.string directChildPattern
         "searchText", Sql.string query.text ])

/// A module path as a search names it: the owner, then the modules, unless they already start with it.
let private modulePathOf (owner : string) (modules : string) : List<string> =
  let moduleParts = PackageLocation.modulesOfColumn modules
  if List.tryHead moduleParts = Some owner then
    moduleParts
  else
    owner :: moduleParts

/// `visible`'s test for a module path `p`: some name there is visible and bound.
let private visibleAtPath =
  Layers.existsVisible
    Layers.locations
    "t.owner = p.owner AND t.modules = p.modules AND t.item_hash <> ''"

/// The module paths a search names that hold something visible, in order.
let private searchSubmodules
  (branch : PT.BranchId)
  (query : PT.Search.SearchQuery)
  : Ply<List<List<string>>> =
  uply {
    if not (wantsModules query) then
      return []
    else
      let (submoduleCondition, sqlParams) = submoduleFilter query

      // The condition is on the module path alone, so it is asked once per distinct path, from the
      // index, and only the paths that pass are checked against the view. Filtering `visible`
      // instead checked every row of every module in the store, which made each listing a scan
      // of all of it.
      let! paths =
        overVisible
          $"""
      SELECT p.owner AS owner, p.modules AS modules
      FROM (SELECT DISTINCT l.owner, l.modules FROM locations l
            WHERE l.live = 1 AND {submoduleCondition}) p
      WHERE {visibleAtPath}
      ORDER BY p.owner, p.modules
      """
        |> Sql.query
        |> Sql.parameters (sqlParams @ Layers.sqlParams branch true)
        |> Sql.executeAsync (fun read ->
          modulePathOf (read.string "owner") (read.string "modules"))
      return paths
  }

/// `searchSubmodules` reduced to what a listing shows: the segment after the query's module in each
/// path, distinct and sorted. Reduced here rather than in Dark (it mirrors
/// `Query.getDirectSubmodules`) because at the root of a large store this is hundreds of module paths
/// collapsing to a handful of names.
///
/// A path's segment is known before its visibility is, and a listing needs only one visible path
/// per segment. So the paths come from the index first, and then one path per segment is checked
/// against the view; the others are checked only for a segment whose first path is not visible.
/// Checking every path made the store root's listing a check of every module in the store.
let private searchChildModules
  (branch : PT.BranchId)
  (query : PT.Search.SearchQuery)
  : Ply<List<string>> =
  uply {
    if not (wantsModules query) then
      return []
    else
      let (submoduleCondition, sqlParams) = submoduleFilter query
      let! candidates =
        $"SELECT DISTINCT l.owner, l.modules FROM locations l
          WHERE l.live = 1 AND {submoduleCondition}"
        |> Sql.query
        |> Sql.parameters sqlParams
        |> Sql.executeAsync (fun read ->
          (read.string "owner", read.string "modules"))

      // `List.skip` throws past the end; Dark's `List.drop` yields []. Match Dark.
      let rec dropN n (xs : List<string>) =
        if n <= 0 then
          xs
        else
          match xs with
          | [] -> []
          | _ :: rest -> dropN (n - 1) rest

      let depth = List.length query.currentModule
      let bySegment =
        candidates
        |> List.choose (fun (owner, modules) ->
          match dropN depth (modulePathOf owner modules) with
          | next :: _ when next <> "" -> Some(next, (owner, modules))
          | _ -> None)
        |> List.groupBy fst
        |> Map.toList
        // Each segment's paths split into the one checked first and the rest.
        |> List.choose (fun (segment, entries) ->
          match List.map snd entries with
          | first :: rest -> Some(segment, first, rest)
          | [] -> None)

      let visibleAmong (paths : List<string * string>) : Task<Set<string * string>> =
        if List.isEmpty paths then
          Task.FromResult Set.empty
        else
          overVisible
            $"""
      SELECT p.owner AS owner, p.modules AS modules
      FROM (SELECT json_extract(value, '$[0]') AS owner, json_extract(value, '$[1]') AS modules
            FROM json_each(@paths)) p
      WHERE {visibleAtPath}
      """
          |> Sql.query
          |> Sql.parameters (
            [ "paths",
              Sql.string (jsonRows (paths |> List.map (fun (o, m) -> [ o; m ]))) ]
            @ Layers.sqlParams branch true
          )
          |> Sql.executeAsync (fun read ->
            (read.string "owner", read.string "modules"))
          |> Task.map Set.ofList

      let! firstVisible =
        visibleAmong (bySegment |> List.map (fun (_, first, _) -> first))
      let unsettled =
        bySegment
        |> List.filter (fun (_, first, _) -> not (Set.contains first firstVisible))
      let! laterVisible =
        visibleAmong (unsettled |> List.collect (fun (_, _, rest) -> rest))

      return
        bySegment
        |> List.filter (fun (_, first, rest) ->
          Set.contains first firstVisible
          || List.exists (fun p -> Set.contains p laterVisible) rest)
        |> List.map (fun (segment, _, _) -> segment)
        |> List.distinct
        |> List.sort
  }


/// The kinds of item a search returns, each with the table holding its content.
let private searchKinds : List<string * string * PT.Search.EntityType> =
  [ "type", "package_types", PT.Search.EntityType.Type
    "value", "package_values", PT.Search.EntityType.Value
    "trait", "package_traits", PT.Search.EntityType.Trait
    "impl", "package_trait_impls", PT.Search.EntityType.TraitImpl
    "fn", "package_functions", PT.Search.EntityType.Fn
    "test", "package_tests", PT.Search.EntityType.Test ]

/// Whether the item at `l` has a doc comment matching <param pattern>.
///
/// Asked of the hashes whose description matches, rather than of the joined content row: the
/// description sits after the definition in that row, so reading it read every item's whole
/// definition, while `idx_{table}_description` holds just the hash and the description.
let private describedAs (contentTable : string) (pattern : string) : string =
  $"l.item_hash IN (SELECT d.hash FROM {contentTable} d WHERE d.description LIKE '%%' || {pattern} || '%%')"

/// Every requested kind's search rows, from one query: for each, the kind's position in
/// `searchKinds`, its hash and location, and its definition when <param withDefinition> is set.
/// A listing wants names and hashes only, and never reads a definition.
///
/// One query rather than one per kind: the view is narrowed to the matching names once, and the
/// statement is prepared once. Rows come back by kind, then by name.
let private searchRows
  (branch : PT.BranchId)
  (query : PT.Search.SearchQuery)
  (withDefinition : bool)
  : Task<List<int64 * Hash * PT.PackageLocation * Option<byte[]>>> =
  let currentModule = String.concat "." query.currentModule

  // A dotted query (e.g. "List.map") spans module path + name, so the
  // full qualified path must be matched, not just the name column.
  let isQualified = query.text.Contains "."

  // Multi-token searches require every significant token to match either the
  // qualified location or doc comment. Dotted queries use the same rule, so
  // their tokens may match non-contiguous segments of a qualified location.
  // Single unqualified queries of 3+ chars also search docs.
  let tokens = tokenizeQuery query.text
  let useTokenSearch = (not query.exactMatch) && (List.length tokens > 1)

  // Ignore short filler tokens in multi-token searches, unless every token is short.
  let matchTokens =
    let kept = tokens |> List.filter (fun t -> String.length t > 2)
    if List.isEmpty kept then tokens else kept

  let nameCondition (contentTable : string) =
    if useTokenSearch then
      matchTokens
      |> List.mapi (fun i _ ->
        // Match each token against the qualified location and docs. Restricting
        // this to the item name would prevent module-path tokens from matching.
        let qualifiedName = PackageLocation.qualifiedNameSql
        let described = describedAs contentTable ("@tok" + string i)

        $"({qualifiedName} LIKE '%%' || @tok{i} || '%%' " + $"OR {described})")
      |> String.concat " AND "
    elif query.exactMatch then
      if isQualified then
        $"({PackageLocation.qualifiedNameSql} = @searchText
          OR {PackageLocation.qualifiedNameSql} LIKE '%%.' || @searchText)"
      else
        "l.name = @searchText"
    else if isQualified then
      $"{PackageLocation.qualifiedNameSql} LIKE '%%' || @searchText || '%%'"
    else if String.length query.text > 2 then
      let described = describedAs contentTable "@searchText"
      $"(l.name LIKE '%%' || @searchText || '%%'
        OR {described})"
    else
      "l.name LIKE '%' || @searchText || '%'"

  let tokenParams =
    if useTokenSearch then
      matchTokens |> List.mapi (fun i tok -> $"tok{i}", Sql.string tok)
    else
      []

  let locationCondition =
    if
      System.String.IsNullOrEmpty currentModule
      && query.searchDepth = PT.Search.SearchDepth.AllDescendants
    then
      "1 = 1"
    // Nothing lives directly at the root: its children are owners, which are submodules.
    else if System.String.IsNullOrEmpty currentModule then
      "0 = 1"
    // A module path is `Owner.Rest`, or the whole path without its owner (`view Stdlib.List.head`).
    // Spelled as `modules IN` so the lookup goes through `idx_locations_modules` rather than past
    // every name the view holds.
    else
      match query.searchDepth with
      | PT.Search.SearchDepth.OnlyDirectDescendants ->
        "l.modules IN (@rest, @modules) AND (l.owner = @owner OR l.modules = @modules)"
      | PT.Search.SearchDepth.AllDescendants ->
        "(l.owner = @owner
          AND (@rest = '' OR l.modules = @rest OR l.modules LIKE @rest || '.%'))
         OR l.modules = @modules OR l.modules LIKE @modules || '.%'"

  let requested =
    searchKinds
    |> List.indexed
    |> List.filter (fun (_, (_, _, entityType)) ->
      query.entityTypes.IsEmpty || List.contains entityType query.entityTypes)

  if List.isEmpty requested then
    Task.FromResult []
  else
    let itemTypes =
      requested
      |> List.map (fun (_, (itemType, _, _)) -> $"'{itemType}'")
      |> String.concat ", "
    let definitionColumn = if withDefinition then ", c.pt_def" else ""
    let perKind =
      requested
      |> List.map (fun (i, (itemType, contentTable, _)) ->
        $"SELECT {i} AS kind, c.hash AS lookup_id{definitionColumn},\n"
        + "       l.owner AS owner, l.modules AS modules, l.name AS name\n"
        + "FROM found l\n"
        + $"JOIN {contentTable} c ON l.item_hash = c.hash\n"
        + $"WHERE l.item_type = '{itemType}'\n"
        + $"  AND {nameCondition contentTable}\n")
      |> String.concat "UNION ALL\n"

    overVisible ""
    + ",\n  found AS MATERIALIZED (\n"
    + "    SELECT l.item_type, l.item_hash, l.owner, l.modules, l.name\n"
    + "    FROM visible l\n"
    + "    WHERE l.item_hash <> ''\n"
    + $"      AND l.item_type IN ({itemTypes})\n"
    + $"      AND ({locationCondition}))\n"
    + perKind
    // Deterministic order; SQLite's default is rowid order, which shifts whenever
    // the package set changes.
    + "ORDER BY kind, owner, modules, name"
    |> Sql.query
    |> Sql.parameters (
      [ "owner",
        Sql.string (List.tryHead query.currentModule |> Option.defaultValue "")
        "rest",
        Sql.string (
          match query.currentModule with
          | _ :: rest -> String.concat "." rest
          | [] -> ""
        )
        "modules", Sql.string currentModule
        "searchText", Sql.string query.text ]
      @ tokenParams
      @ Layers.sqlParams branch true
    )
    |> Sql.executeAsync (fun read ->
      (read.int64 "kind",
       Hash(read.string "lookup_id"),
       readLocation read,
       (if withDefinition then Some(read.bytes "pt_def") else None)))


/// The rows of one kind, named by its item type, in the order they came.
let private rowsOf
  (rows : List<int64 * Hash * PT.PackageLocation * Option<byte[]>>)
  (itemType : string)
  : List<Hash * PT.PackageLocation * Option<byte[]>> =
  let index = searchKinds |> List.findIndex (fun (t, _, _) -> t = itemType) |> int64
  rows
  |> List.choose (fun (i, hash, location, definition) ->
    if i = index then Some(hash, location, definition) else None)


let search
  (branch : PT.BranchId)
  (query : PT.Search.SearchQuery)
  : Ply<PT.Search.SearchResults> =
  uply {
    let! submodules = searchSubmodules branch query
    let! rows = searchRows branch query true

    let located itemType deserialize =
      rowsOf rows itemType
      |> List.map (fun (hash, location, definition) ->
        ({ entity = deserialize hash (Option.get definition); location = location }
        : PT.LocatedItem<_>))

    return
      { submodules = submodules
        types = located "type" BS.PT.PackageType.deserialize
        values = located "value" BS.PT.PackageValue.deserialize
        fns = located "fn" BS.PT.PackageFn.deserialize
        traits = located "trait" BS.PT.Trait.deserialize
        impls = located "impl" BS.PT.TraitImpl.deserialize
        tests = located "test" BS.PT.PackageTest.deserialize }
  }


/// `search`'s answer with only the name and hash of each item, and only the direct children of the
/// query's module: what a listing reads.
type NamesAndHashes =
  { submodules : List<string>
    types : List<string * Hash>
    values : List<string * Hash>
    fns : List<string * Hash>
    traits : List<string * Hash>
    impls : List<string * Hash>
    tests : List<string * Hash> }

/// <see cref="search"/>, the same rows in the same order, without reading any definition.
let searchNamesAndHashes
  (branch : PT.BranchId)
  (query : PT.Search.SearchQuery)
  : Ply<NamesAndHashes> =
  uply {
    let! submodules = searchChildModules branch query
    let! rows = searchRows branch query false

    let pairs itemType =
      rowsOf rows itemType
      |> List.map (fun (hash, location, _) -> (location.name, hash))

    return
      { submodules = submodules
        types = pairs "type"
        values = pairs "value"
        fns = pairs "fn"
        traits = pairs "trait"
        impls = pairs "impl"
        tests = pairs "test" }
  }
