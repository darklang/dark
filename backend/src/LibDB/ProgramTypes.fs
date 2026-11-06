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

  /// Active function hashes bound under `owner` on any branch. The CLI uses this to skip consumer
  /// approval for bundled Darklang functions; the instance policy, run policy, and function ceilings
  /// still apply. A hash set keeps checking bundled function hashes fast.
  let hashesOwnedBy
    (owner : string)
    : Ply<System.Collections.Generic.HashSet<string>> =
    uply {
      let! hashes =
        Sql.query
          """
          SELECT DISTINCT item_hash
          FROM locations
          WHERE owner = @owner AND item_type = 'fn' AND live = 1 AND item_hash <> ''
          """
        |> Sql.parameters [ "owner", Sql.string owner ]
        |> Sql.executeAsync (fun read -> read.string "item_hash")
      return System.Collections.Generic.HashSet<string>(hashes)
    }

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


let search
  (branch : PT.BranchId)
  (query : PT.Search.SearchQuery)
  : Ply<PT.Search.SearchResults> =
  uply {
    let currentModule = String.concat "." query.currentModule

    // Only when asked for: listing every module is a scan of every visible name, and a caller after
    // one kind of item (the traits, the tests) never reads it.
    let wantsModules =
      query.entityTypes.IsEmpty
      || List.contains PT.Search.EntityType.Module query.entityTypes

    let! submodules =
      if not wantsModules then
        Task.FromResult []
      else
        let (submoduleCondition, sqlParams) =
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

        overVisible
          $"""
      SELECT DISTINCT owner, modules
      FROM visible l
      WHERE l.item_hash <> ''
        AND {submoduleCondition}
      ORDER BY owner, modules
      """
        |> Sql.query
        |> Sql.parameters (sqlParams @ Layers.sqlParams branch true)
        |> Sql.executeAsync (fun read ->
          let owner = read.string "owner"
          let modulesStr = read.string "modules"
          let moduleParts = PackageLocation.modulesOfColumn modulesStr
          if List.tryHead moduleParts = Some owner then
            moduleParts
          else
            owner :: moduleParts)

    let makeEntityQuery
      (itemType : string)
      (contentTable : string)
      (joinColumn : string)
      deserializeFn
      =
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

      let nameCondition =
        if useTokenSearch then
          matchTokens
          |> List.mapi (fun i _ ->
            // Match each token against the qualified location and docs. Restricting
            // this to the item name would prevent module-path tokens from matching.
            let qualifiedName = PackageLocation.qualifiedNameSql

            $"({qualifiedName} LIKE '%%' || @tok{i} || '%%' "
            + $"OR c.description LIKE '%%' || @tok{i} || '%%')")
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
          "(l.name LIKE '%' || @searchText || '%'
            OR c.description LIKE '%' || @searchText || '%')"
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

      overVisible ""
      + $"SELECT c.{joinColumn} as lookup_id, c.pt_def, l.owner, l.modules, l.name\n"
      + $"FROM visible l\n"
      + $"JOIN {contentTable} c ON l.item_hash = c.{joinColumn}\n"
      + "WHERE l.item_hash <> ''\n"
      + $"  AND l.item_type = '{itemType}'\n"
      + $"  AND ({locationCondition})\n"
      + $"  AND {nameCondition}\n"
      // Deterministic order; SQLite's default is rowid order, which shifts whenever
      // the package set changes.
      + "  ORDER BY l.owner, l.modules, l.name"
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
        let hash = Hash(read.string "lookup_id")
        let definition = read.bytes "pt_def"
        let owner = read.string "owner"
        let modulesStr = read.string "modules"
        let name = read.string "name"
        let entity = deserializeFn hash definition
        let location : PT.PackageLocation =
          { owner = owner
            modules = PackageLocation.modulesOfColumn modulesStr
            name = name }
        ({ entity = entity; location = location } : PT.LocatedItem<_>))

    let isEntityRequested entity =
      query.entityTypes.IsEmpty || List.contains entity query.entityTypes

    let! types =
      if isEntityRequested PT.Search.EntityType.Type then
        makeEntityQuery "type" "package_types" "hash" BS.PT.PackageType.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.PackageType.PackageType>>> []

    let! values =
      if isEntityRequested PT.Search.EntityType.Value then
        makeEntityQuery
          "value"
          "package_values"
          "hash"
          BS.PT.PackageValue.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.PackageValue.PackageValue>>> []

    let! traits =
      if isEntityRequested PT.Search.EntityType.Trait then
        makeEntityQuery "trait" "package_traits" "hash" BS.PT.Trait.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.Trait.Trait>>> []

    let! impls =
      if isEntityRequested PT.Search.EntityType.TraitImpl then
        makeEntityQuery
          "impl"
          "package_trait_impls"
          "hash"
          BS.PT.TraitImpl.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.TraitImpl.TraitImpl>>> []

    let! fns =
      if isEntityRequested PT.Search.EntityType.Fn then
        makeEntityQuery "fn" "package_functions" "hash" BS.PT.PackageFn.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.PackageFn.PackageFn>>> []

    let! tests =
      if isEntityRequested PT.Search.EntityType.Test then
        makeEntityQuery "test" "package_tests" "hash" BS.PT.PackageTest.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.PackageTest.PackageTest>>> []

    return
      { submodules = submodules
        types = types
        values = values
        fns = fns
        traits = traits
        impls = impls
        tests = tests }
  }
