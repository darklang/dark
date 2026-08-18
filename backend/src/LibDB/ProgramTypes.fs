module LibDB.ProgramTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


// Single-scope "main": every row is on the main branch, so name resolution needs
// no branch filter. The latest SetName at a location wins -> ORDER BY created_at DESC.

let private findItem
  (itemType : string)
  (location : PT.PackageLocation)
  : Ply<Option<Hash>> =
  uply {
    let modulesStr = String.concat "." location.modules

    return!
      Sql.query
        $"""
        SELECT item_hash
        FROM locations
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND item_type = '{itemType}'
          AND unlisted_at IS NULL
        -- `rowid DESC` breaks the tie: `created_at` is second-resolution, so same-second rows order
        -- arbitrarily, and the caller here takes the head.
        ORDER BY created_at DESC, rowid DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "owner", Sql.string location.owner
          "modules", Sql.string modulesStr
          "name", Sql.string location.name ]
      |> Sql.executeRowOptionAsync (fun read -> Hash(read.string "item_hash"))
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

let private getItemLocations
  (itemType : string)
  (hash : Hash)
  : Ply<List<PT.PackageLocation>> =
  uply {
    let (Hash hashStr) = hash

    return!
      Sql.query
        $"""
        SELECT owner, modules, name
        FROM locations
        WHERE item_hash = @item_hash
          AND item_type = '{itemType}'
          AND unlisted_at IS NULL
        -- `rowid DESC` breaks the tie: `created_at` is second-resolution, so same-second rows order
        -- arbitrarily, and the caller here takes the head.
        ORDER BY created_at DESC, rowid DESC
        """
      |> Sql.parameters [ "item_hash", Sql.string hashStr ]
      |> Sql.executeAsync (fun read ->
        let modulesStr = read.string "modules"
        { owner = read.string "owner"
          modules = modulesStr.Split('.') |> Array.toList
          name = read.string "name" })
  }

/// Every name this hash has EVER had, live ones first.
///
/// Separate from `getItemLocations` because the two answer different questions and
/// only one of them is safe for logic. "What is bound here now" must be live-only: a
/// cascade that repointed a superseded name would undo someone's rename. "What do I
/// call this thing on screen" is the other question, and there a name the item used
/// to have beats a bare 64-character hash every time.
///
/// The case is ordinary: viewing an OLD version of an item. Its name moved on to the
/// newer version, so it has no live row, and rendering it as `<hash:d6f972b3>` tells
/// you nothing about what you're looking at.
let private getItemLocationsEverNamed
  (itemType : string)
  (hash : Hash)
  : Ply<List<PT.PackageLocation>> =
  uply {
    let (Hash hashStr) = hash

    return!
      Sql.query
        $"""
        SELECT owner, modules, name
        FROM locations
        WHERE item_hash = @item_hash
          AND item_type = '{itemType}'
        ORDER BY (unlisted_at IS NULL) DESC, created_at DESC
        """
      |> Sql.parameters [ "item_hash", Sql.string hashStr ]
      |> Sql.executeAsync (fun read ->
        let modulesStr = read.string "modules"
        { owner = read.string "owner"
          modules = modulesStr.Split('.') |> Array.toList
          name = read.string "name" })
  }


module Type =
  let find = findItem "type"
  let get = getItem "package_types" "hash" BS.PT.PackageType.deserialize
  let getLocations = getItemLocations "type"
  let getLocationsEverNamed = getItemLocationsEverNamed "type"

module Value =
  let find = findItem "value"
  let get = getItem "package_values" "hash" BS.PT.PackageValue.deserialize
  let getLocations = getItemLocations "value"
  let getLocationsEverNamed = getItemLocationsEverNamed "value"

module Fn =
  let find = findItem "fn"
  let get = getItem "package_functions" "hash" BS.PT.PackageFn.deserialize
  let getLocations = getItemLocations "fn"
  let getLocationsEverNamed = getItemLocationsEverNamed "fn"


/// Split a search query into lowercase tokens for name/doc matching.
let private tokenizeQuery (s : string) : List<string> =
  let spaced =
    System.Text.RegularExpressions.Regex.Replace(s, "([a-z0-9])([A-Z])", "$1 $2")
  spaced.Split([| ' '; '\t'; '.'; '-'; '_'; '/'; ','; ':' |])
  |> Array.toList
  |> List.map (fun t -> t.Trim().ToLower())
  |> List.filter (fun t -> t <> "")


let search (query : PT.Search.SearchQuery) : Ply<PT.Search.SearchResults> =
  uply {
    let currentModule = String.concat "." query.currentModule

    let! submodules =
      let (submoduleCondition, sqlParams) =
        if query.exactMatch then
          if System.String.IsNullOrEmpty query.text then
            let parts = currentModule.Split('.') |> Array.toList
            match parts with
            | [ owner ] -> ("""(owner = @owner)""", [ "owner", Sql.string owner ])
            | owner :: moduleParts ->
              let modulesPath = String.concat "." moduleParts
              ("""(owner = @owner AND modules = @modulesPath)""",
               [ "owner", Sql.string owner; "modulesPath", Sql.string modulesPath ])
            | [] -> ("""(1 = 1)""", [])
          else
            ("""(modules = @currentModule || '.' || @searchText)
               OR (owner || '.' || modules = @currentModule || '.' || @searchText)""",
             [ "currentModule", Sql.string currentModule
               "searchText", Sql.string query.text ])
        else if System.String.IsNullOrEmpty currentModule then
          match query.searchDepth with
          | PT.Search.SearchDepth.OnlyDirectDescendants ->
            ("""(owner LIKE '%' || @searchText || '%')""",
             [ "searchText", Sql.string query.text ])
          | PT.Search.SearchDepth.AllDescendants ->
            ("""((owner LIKE '%' || @searchText || '%')
                  OR (modules LIKE '%' || @searchText || '%')
                  OR (owner || '.' || modules LIKE '%' || @searchText || '%'))""",
             [ "searchText", Sql.string query.text ])
        else
          let directChildPattern = currentModule + ".%"
          match query.searchDepth with
          | PT.Search.SearchDepth.OnlyDirectDescendants ->
            ("""((modules LIKE @directChildPattern AND modules LIKE '%' || @searchText || '%')
                  OR (owner || '.' || modules LIKE @directChildPattern AND owner || '.' || modules LIKE '%' || @searchText || '%'))""",
             [ "currentModule", Sql.string currentModule
               "directChildPattern", Sql.string directChildPattern
               "searchText", Sql.string query.text ])
          | PT.Search.SearchDepth.AllDescendants ->
            ("""((modules LIKE @directChildPattern AND modules LIKE '%' || @searchText || '%')
                  OR (owner || '.' || modules LIKE @directChildPattern AND owner || '.' || modules LIKE '%' || @searchText || '%'))""",
             [ "currentModule", Sql.string currentModule
               "directChildPattern", Sql.string directChildPattern
               "searchText", Sql.string query.text ])

      $"""
      SELECT DISTINCT owner, modules
      FROM locations l
      WHERE l.unlisted_at IS NULL
        AND {submoduleCondition}
      ORDER BY owner, modules
      """
      |> Sql.query
      |> Sql.parameters sqlParams
      |> Sql.executeAsync (fun read ->
        let owner = read.string "owner"
        let modulesStr = read.string "modules"
        let moduleParts = modulesStr.Split('.') |> Array.toList
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
            let qualifiedName = "(l.owner || '.' || l.modules || '.' || l.name)"

            $"({qualifiedName} LIKE '%%' || @tok{i} || '%%' "
            + $"OR c.description LIKE '%%' || @tok{i} || '%%')")
          |> String.concat " AND "
        elif query.exactMatch then
          if isQualified then
            "((l.owner || '.' || l.modules || '.' || l.name) = @searchText
              OR (l.owner || '.' || l.modules || '.' || l.name) LIKE '%.' || @searchText)"
          else
            "l.name = @searchText"
        else if isQualified then
          "(l.owner || '.' || l.modules || '.' || l.name) LIKE '%' || @searchText || '%'"
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
        else if System.String.IsNullOrEmpty currentModule then
          "((l.modules = @modules) OR (l.owner || '.' || l.modules = @fqname))"
        else
          match query.searchDepth with
          | PT.Search.SearchDepth.OnlyDirectDescendants ->
            "((l.modules = @modules) OR (l.owner || '.' || l.modules = @fqname))"
          | PT.Search.SearchDepth.AllDescendants ->
            "((l.modules = @modules) OR (l.owner || '.' || l.modules = @fqname) OR (l.modules LIKE @modules || '.%') OR (l.owner || '.' || l.modules LIKE @fqname || '.%'))"

      $"SELECT c.{joinColumn} as lookup_id, c.pt_def, l.owner, l.modules, l.name\n"
      + $"FROM locations l\n"
      + $"JOIN {contentTable} c ON l.item_hash = c.{joinColumn}\n"
      + "WHERE l.unlisted_at IS NULL\n"
      + $"  AND l.item_type = '{itemType}'\n"
      + $"  AND ({locationCondition})\n"
      + $"  AND {nameCondition}\n"
      // Without this the order is whatever SQLite happens to produce, which is rowid order and therefore
      // shifts whenever the package set changes. That made results reshuffle between runs for no reason
      // the reader could see, and made any before/after diff of `search` output pure noise.
      //
      // No branch filter: `locations` has no branch_id on this branch. A branch is an overlay, and a
      // branch-scoped read goes through the overlay helpers rather than through this SQL.
      + "  ORDER BY l.owner, l.modules, l.name"
      |> Sql.query
      |> Sql.parameters (
        [ "modules", Sql.string currentModule
          "fqname", Sql.string currentModule
          "searchText", Sql.string query.text ]
        @ tokenParams
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
            modules = modulesStr.Split('.') |> Array.toList
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

    let! fns =
      if isEntityRequested PT.Search.EntityType.Fn then
        makeEntityQuery "fn" "package_functions" "hash" BS.PT.PackageFn.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.PackageFn.PackageFn>>> []

    return { submodules = submodules; types = types; values = values; fns = fns }
  }
