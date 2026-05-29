module LibDB.ProgramTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


/// Build a branch-aware SQL WHERE clause and ORDER BY for name resolution.
/// branchChain = [current; parent; grandparent; ...; main]
/// Current branch sees WIP + committed, ancestors see committed only.
let private buildBranchFilter (branchChain : List<PT.BranchId>) =
  match branchChain with
  | [] -> ("1 = 0", []) // shouldn't happen
  | [ single ] ->
    // Only one branch (main with no chain) - see everything
    let filter = "(branch_id = @branch_0)"
    let parms = [ "branch_0", Sql.uuid single ]
    (filter, parms)
  | _current :: ancestors ->
    // Current branch: WIP + committed; ancestors: committed only
    let branchParams =
      branchChain |> List.mapi (fun i id -> $"branch_{i}", Sql.uuid id)

    let currentParam = "@branch_0"

    let ancestorParams =
      ancestors |> List.mapi (fun i _ -> $"@branch_{i + 1}") |> String.concat ", "

    let filter =
      $"(branch_id = {currentParam} OR (branch_id IN ({ancestorParams}) AND commit_hash IS NOT NULL))"

    // Note: ORDER BY is appended by the caller via buildBranchOrderBy
    (filter, branchParams)


let private buildBranchOrderBy (branchChain : List<PT.BranchId>) : string =
  let caseClauses =
    branchChain
    |> List.mapi (fun i _ -> $"WHEN @branch_{i} THEN {i}")
    |> String.concat " "

  $"CASE branch_id {caseClauses} END, CASE WHEN commit_hash IS NULL THEN 0 ELSE 1 END, created_at DESC"


let private findItem
  (itemType : string)
  (branchChain : List<PT.BranchId>)
  (location : PT.PackageLocation)
  : Ply<Option<Hash>> =
  uply {
    let modulesStr = String.concat "." location.modules
    let (branchFilter, branchParams) = buildBranchFilter branchChain
    let orderBy = buildBranchOrderBy branchChain

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
          AND {branchFilter}
        ORDER BY {orderBy}
        LIMIT 1
        """
      |> Sql.parameters (
        [ "owner", Sql.string location.owner
          "modules", Sql.string modulesStr
          "name", Sql.string location.name ]
        @ branchParams
      )
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
  (branchChain : List<PT.BranchId>)
  (hash : Hash)
  : Ply<List<PT.PackageLocation>> =
  uply {
    let (Hash hashStr) = hash
    let (branchFilter, branchParams) = buildBranchFilter branchChain
    let orderBy = buildBranchOrderBy branchChain

    return!
      Sql.query
        $"""
        SELECT owner, modules, name
        FROM locations
        WHERE item_hash = @item_hash
          AND item_type = '{itemType}'
          AND unlisted_at IS NULL
          AND {branchFilter}
        ORDER BY {orderBy}
        """
      |> Sql.parameters ([ "item_hash", Sql.string hashStr ] @ branchParams)
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

module Value =
  let find = findItem "value"
  let get = getItem "package_values" "hash" BS.PT.PackageValue.deserialize
  let getLocations = getItemLocations "value"

module Fn =
  let find = findItem "fn"
  let get = getItem "package_functions" "hash" BS.PT.PackageFn.deserialize
  let getLocations = getItemLocations "fn"


/// Split a search query into lowercase tokens for name/doc matching.
let private tokenizeQuery (s : string) : List<string> =
  let spaced =
    System.Text.RegularExpressions.Regex.Replace(s, "([a-z0-9])([A-Z])", "$1 $2")
  spaced.Split([| ' '; '\t'; '.'; '-'; '_'; '/'; ','; ':' |])
  |> Array.toList
  |> List.map (fun t -> t.Trim().ToLower())
  |> List.filter (fun t -> t <> "")


let search
  (branchChain : List<PT.BranchId>)
  (query : PT.Search.SearchQuery)
  : Ply<PT.Search.SearchResults> =
  uply {
    let currentModule = String.concat "." query.currentModule
    let (branchFilter, branchParams) = buildBranchFilter branchChain

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
        AND {branchFilter}
      """
      |> Sql.query
      |> Sql.parameters (sqlParams @ branchParams)
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
      // item name or doc comment. Single unqualified queries of 3+ chars also search docs.
      let tokens = tokenizeQuery query.text
      let useTokenSearch =
        (not query.exactMatch) && (not isQualified) && (List.length tokens > 1)

      // Ignore short filler tokens in multi-token searches, unless every token is short.
      let matchTokens =
        let kept = tokens |> List.filter (fun t -> String.length t > 2)
        if List.isEmpty kept then tokens else kept

      let nameCondition =
        if useTokenSearch then
          matchTokens
          |> List.mapi (fun i _ ->
            $"(l.name LIKE '%%' || @tok{i} || '%%' OR c.description LIKE '%%' || @tok{i} || '%%')")
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
      + $"  AND {branchFilter}"
      |> Sql.query
      |> Sql.parameters (
        [ "modules", Sql.string currentModule
          "fqname", Sql.string currentModule
          "searchText", Sql.string query.text ]
        @ tokenParams
        @ branchParams
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
