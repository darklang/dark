module LibPackageManager.ProgramTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


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
      $"(branch_id = {currentParam} OR (branch_id IN ({ancestorParams}) AND commit_id IS NOT NULL))"

    // Note: ORDER BY is appended by the caller via buildBranchOrderBy
    (filter, branchParams)


let private buildBranchOrderBy (branchChain : List<PT.BranchId>) : string =
  let caseClauses =
    branchChain
    |> List.mapi (fun i _ -> $"WHEN @branch_{i} THEN {i}")
    |> String.concat " "

  $"CASE branch_id {caseClauses} END, CASE WHEN commit_id IS NULL THEN 0 ELSE 1 END, created_at DESC"


module Type =
  let find
    (branchChain : List<PT.BranchId>)
    (location : PT.PackageLocation)
    : Ply<Option<PT.FQTypeName.Package>> =
    uply {
      let modulesStr = String.concat "." location.modules
      let (branchFilter, branchParams) = buildBranchFilter branchChain
      let orderBy = buildBranchOrderBy branchChain

      return!
        Sql.query
          $"""
          SELECT item_id
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = 'type'
            AND deprecated_at IS NULL
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
        |> Sql.executeRowOptionAsync (fun read -> read.uuid "item_id")
    }

  let get (id : uuid) : Ply<Option<PT.PackageType.PackageType>> =
    uply {
      return!
        Sql.query
          """
          SELECT pt_def
          FROM package_types
          WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "pt_def")
        |> Task.map (Option.map (BinarySerialization.PT.PackageType.deserialize id))
    }

  let getLocation
    (branchChain : List<PT.BranchId>)
    (id : uuid)
    : Ply<Option<PT.PackageLocation>> =
    uply {
      let (branchFilter, branchParams) = buildBranchFilter branchChain
      let orderBy = buildBranchOrderBy branchChain

      return!
        Sql.query
          $"""
          SELECT owner, modules, name
          FROM locations
          WHERE item_id = @item_id
            AND item_type = 'type'
            AND deprecated_at IS NULL
            AND {branchFilter}
          ORDER BY {orderBy}
          LIMIT 1
          """
        |> Sql.parameters ([ "item_id", Sql.uuid id ] @ branchParams)
        |> Sql.executeRowOptionAsync (fun read ->
          let modulesStr = read.string "modules"
          { owner = read.string "owner"
            modules = modulesStr.Split('.') |> Array.toList
            name = read.string "name" })
    }


module Value =
  let find
    (branchChain : List<PT.BranchId>)
    (location : PT.PackageLocation)
    : Ply<Option<PT.FQValueName.Package>> =
    uply {
      let modulesStr = String.concat "." location.modules
      let (branchFilter, branchParams) = buildBranchFilter branchChain
      let orderBy = buildBranchOrderBy branchChain

      return!
        Sql.query
          $"""
          SELECT item_id
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = 'value'
            AND deprecated_at IS NULL
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
        |> Sql.executeRowOptionAsync (fun read -> read.uuid "item_id")
    }

  let get (id : uuid) : Ply<Option<PT.PackageValue.PackageValue>> =
    uply {
      return!
        Sql.query
          """
          SELECT pt_def
          FROM package_values
          WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "pt_def")
        |> Task.map (Option.map (BinarySerialization.PT.PackageValue.deserialize id))
    }

  let getLocation
    (branchChain : List<PT.BranchId>)
    (id : uuid)
    : Ply<Option<PT.PackageLocation>> =
    uply {
      let (branchFilter, branchParams) = buildBranchFilter branchChain
      let orderBy = buildBranchOrderBy branchChain

      return!
        Sql.query
          $"""
          SELECT owner, modules, name
          FROM locations
          WHERE item_id = @item_id
            AND item_type = 'value'
            AND deprecated_at IS NULL
            AND {branchFilter}
          ORDER BY {orderBy}
          LIMIT 1
          """
        |> Sql.parameters ([ "item_id", Sql.uuid id ] @ branchParams)
        |> Sql.executeRowOptionAsync (fun read ->
          let modulesStr = read.string "modules"
          { owner = read.string "owner"
            modules = modulesStr.Split('.') |> Array.toList
            name = read.string "name" })
    }


module Fn =
  let find
    (branchChain : List<PT.BranchId>)
    (location : PT.PackageLocation)
    : Ply<Option<PT.FQFnName.Package>> =
    uply {
      let modulesStr = String.concat "." location.modules
      let (branchFilter, branchParams) = buildBranchFilter branchChain
      let orderBy = buildBranchOrderBy branchChain

      return!
        Sql.query
          $"""
          SELECT item_id
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = 'fn'
            AND deprecated_at IS NULL
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
        |> Sql.executeRowOptionAsync (fun read -> read.uuid "item_id")
    }

  let get (id : uuid) : Ply<Option<PT.PackageFn.PackageFn>> =
    uply {
      return!
        Sql.query
          """
          SELECT pt_def
          FROM package_functions
          WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "pt_def")
        |> Task.map (Option.map (BinarySerialization.PT.PackageFn.deserialize id))
    }

  let getLocation
    (branchChain : List<PT.BranchId>)
    (id : uuid)
    : Ply<Option<PT.PackageLocation>> =
    uply {
      let (branchFilter, branchParams) = buildBranchFilter branchChain
      let orderBy = buildBranchOrderBy branchChain

      return!
        Sql.query
          $"""
          SELECT owner, modules, name
          FROM locations
          WHERE item_id = @item_id
            AND item_type = 'fn'
            AND deprecated_at IS NULL
            AND {branchFilter}
          ORDER BY {orderBy}
          LIMIT 1
          """
        |> Sql.parameters ([ "item_id", Sql.uuid id ] @ branchParams)
        |> Sql.executeRowOptionAsync (fun read ->
          let modulesStr = read.string "modules"
          { owner = read.string "owner"
            modules = modulesStr.Split('.') |> Array.toList
            name = read.string "name" })
    }


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
            ("""((modules LIKE @directChildPattern AND modules NOT LIKE @nestedPattern AND modules LIKE '%' || @searchText || '%')
                  OR (owner || '.' || modules LIKE @directChildPattern AND owner || '.' || modules NOT LIKE @nestedPattern AND owner || '.' || modules LIKE '%' || @searchText || '%'))""",
             [ "currentModule", Sql.string currentModule
               "directChildPattern", Sql.string directChildPattern
               "nestedPattern", Sql.string (currentModule + ".%.%")
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
      WHERE l.deprecated_at IS NULL
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

    let makeEntityQuery (itemType : string) (contentTable : string) deserializeFn =
      let nameCondition =
        if query.exactMatch then
          "l.name = @searchText"
        else
          "l.name LIKE '%' || @searchText || '%'"

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

      $"SELECT c.id, c.pt_def, l.owner, l.modules, l.name\n"
      + $"FROM locations l\n"
      + $"JOIN {contentTable} c ON l.item_id = c.id\n"
      + "WHERE l.deprecated_at IS NULL\n"
      + $"  AND l.item_type = '{itemType}'\n"
      + $"  AND ({locationCondition})\n"
      + $"  AND {nameCondition}\n"
      + $"  AND {branchFilter}"
      |> Sql.query
      |> Sql.parameters (
        [ "modules", Sql.string currentModule
          "fqname", Sql.string currentModule
          "searchText", Sql.string query.text ]
        @ branchParams
      )
      |> Sql.executeAsync (fun read ->
        let id = read.uuid "id"
        let definition = read.bytes "pt_def"
        let owner = read.string "owner"
        let modulesStr = read.string "modules"
        let name = read.string "name"
        let entity = deserializeFn id definition
        let location : PT.PackageLocation =
          { owner = owner
            modules = modulesStr.Split('.') |> Array.toList
            name = name }
        ({ entity = entity; location = location } : PT.LocatedItem<_>))

    let isEntityRequested entity =
      query.entityTypes.IsEmpty || List.contains entity query.entityTypes

    let! types =
      if isEntityRequested PT.Search.EntityType.Type then
        makeEntityQuery
          "type"
          "package_types"
          BinarySerialization.PT.PackageType.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.PackageType.PackageType>>> []

    let! values =
      if isEntityRequested PT.Search.EntityType.Value then
        makeEntityQuery
          "value"
          "package_values"
          BinarySerialization.PT.PackageValue.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.PackageValue.PackageValue>>> []

    let! fns =
      if isEntityRequested PT.Search.EntityType.Fn then
        makeEntityQuery
          "fn"
          "package_functions"
          BinarySerialization.PT.PackageFn.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.PackageFn.PackageFn>>> []

    return { submodules = submodules; types = types; values = values; fns = fns }
  }
