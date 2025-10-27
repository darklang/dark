module LibPackageManager.ProgramTypes

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


module Type =
  let find
    ((branchId, location) : Option<PT.BranchID> * PT.PackageLocation)
    : Ply<Option<PT.FQTypeName.Package>> =
    uply {
      let modulesStr = String.concat "." location.modules

      return!
        Sql.query
          """
          SELECT item_id
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = 'type'
            AND deprecated_at IS NULL
            AND (branch_id IS NULL OR branch_id = @branch_id)
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "owner", Sql.string location.owner
            "modules", Sql.string modulesStr
            "name", Sql.string location.name
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
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
    ((branchId, id) : Option<PT.BranchID> * uuid)
    : Ply<Option<PT.PackageLocation>> =
    uply {
      return!
        Sql.query
          """
          SELECT owner, modules, name
          FROM locations
          WHERE item_id = @item_id
            AND item_type = 'type'
            AND deprecated_at IS NULL
            AND (branch_id IS NULL OR branch_id = @branch_id)
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "item_id", Sql.uuid id
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
        |> Sql.executeRowOptionAsync (fun read ->
          let modulesStr = read.string "modules"
          { owner = read.string "owner"
            modules = modulesStr.Split('.') |> Array.toList
            name = read.string "name" })
    }


module Value =
  let find
    ((branchId, location) : Option<PT.BranchID> * PT.PackageLocation)
    : Ply<Option<PT.FQValueName.Package>> =
    uply {
      let modulesStr = String.concat "." location.modules

      return!
        Sql.query
          """
          SELECT item_id
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = 'value'
            AND deprecated_at IS NULL
            AND (branch_id IS NULL OR branch_id = @branch_id)
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "owner", Sql.string location.owner
            "modules", Sql.string modulesStr
            "name", Sql.string location.name
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
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
    ((branchId, id) : Option<PT.BranchID> * uuid)
    : Ply<Option<PT.PackageLocation>> =
    uply {
      return!
        Sql.query
          """
          SELECT owner, modules, name
          FROM locations
          WHERE item_id = @item_id
            AND item_type = 'value'
            AND deprecated_at IS NULL
            AND (branch_id IS NULL OR branch_id = @branch_id)
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "item_id", Sql.uuid id
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
        |> Sql.executeRowOptionAsync (fun read ->
          let modulesStr = read.string "modules"
          { owner = read.string "owner"
            modules = modulesStr.Split('.') |> Array.toList
            name = read.string "name" })
    }


module Fn =
  let find
    ((branchId, location) : Option<PT.BranchID> * PT.PackageLocation)
    : Ply<Option<PT.FQFnName.Package>> =
    uply {
      let modulesStr = String.concat "." location.modules

      return!
        Sql.query
          """
          SELECT item_id
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = 'fn'
            AND deprecated_at IS NULL
            AND (branch_id IS NULL OR branch_id = @branch_id)
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "owner", Sql.string location.owner
            "modules", Sql.string modulesStr
            "name", Sql.string location.name
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
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
    ((branchId, id) : Option<PT.BranchID> * uuid)
    : Ply<Option<PT.PackageLocation>> =
    uply {
      return!
        Sql.query
          """
          SELECT owner, modules, name
          FROM locations
          WHERE item_id = @item_id
            AND item_type = 'fn'
            AND deprecated_at IS NULL
            AND (branch_id IS NULL OR branch_id = @branch_id)
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "item_id", Sql.uuid id
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
        |> Sql.executeRowOptionAsync (fun read ->
          let modulesStr = read.string "modules"
          { owner = read.string "owner"
            modules = modulesStr.Split('.') |> Array.toList
            name = read.string "name" })
    }



let search
  ((branchId, query) : Option<PT.BranchID> * PT.Search.SearchQuery)
  : Ply<PT.Search.SearchResults> =
  uply {
    let currentModule = String.concat "." query.currentModule

    let! submodules =
      let (submoduleCondition, sqlParams) =
        if query.exactMatch then
          // For exact search, we want direct children only
          if System.String.IsNullOrEmpty query.text then
            // Looking for entities IN currentModule (to check if module exists)
            let parts = currentModule.Split('.') |> Array.toList
            match parts with
            | [ owner ] ->
              // Top-level module like "Darklang" - look for entities owned by it
              ("""(owner = @owner)""", [ "owner", Sql.string owner ])
            | owner :: moduleParts ->
              // Nested module - extract owner and modules parts
              let modulesPath = String.concat "." moduleParts
              ("""(owner = @owner AND modules = @modulesPath)""",
               [ "owner", Sql.string owner; "modulesPath", Sql.string modulesPath ])
            | [] ->
              // Root - should not happen, but handle gracefully
              ("""(1 = 1)""", [])
          else
            // Looking for a specific named submodule
            ("""(modules = @currentModule || '.' || @searchText)
               OR (owner || '.' || modules = @currentModule || '.' || @searchText)""",
             [ "currentModule", Sql.string currentModule
               "searchText", Sql.string query.text ])
        else if
          // Fuzzy logic - handle root case specially
          System.String.IsNullOrEmpty currentModule
        then
          match query.searchDepth with
          | PT.Search.SearchDepth.OnlyDirectDescendants ->
            // At root with OnlyDirectDescendants - search for owners that match the text
            ("""(owner LIKE '%' || @searchText || '%')""",
             [ "searchText", Sql.string query.text ])
          | PT.Search.SearchDepth.AllDescendants ->
            // At root with AllDescendants - search all modules recursively
            ("""((owner LIKE '%' || @searchText || '%')
                  OR (modules LIKE '%' || @searchText || '%')
                  OR (owner || '.' || modules LIKE '%' || @searchText || '%'))""",
             [ "searchText", Sql.string query.text ])
        else
          // Not at root - fuzzy logic with depth control
          let directChildPattern = currentModule + ".%"
          match query.searchDepth with
          | PT.Search.SearchDepth.OnlyDirectDescendants ->
            // For OnlyDirectDescendants, we want modules that start with currentModule + "."
            // but don't have any additional "." after that (i.e., direct children only)
            ("""((modules LIKE @directChildPattern AND modules NOT LIKE @nestedPattern AND modules LIKE '%' || @searchText || '%')
                  OR (owner || '.' || modules LIKE @directChildPattern AND owner || '.' || modules NOT LIKE @nestedPattern AND owner || '.' || modules LIKE '%' || @searchText || '%'))""",
             [ "currentModule", Sql.string currentModule
               "directChildPattern", Sql.string directChildPattern
               "nestedPattern", Sql.string (currentModule + ".%.%")
               "searchText", Sql.string query.text ])
          | PT.Search.SearchDepth.AllDescendants ->
            // For AllDescendants, we want all modules that start with currentModule + "."
            // regardless of how many levels deep they are
            ("""((modules LIKE @directChildPattern AND modules LIKE '%' || @searchText || '%')
                  OR (owner || '.' || modules LIKE @directChildPattern AND owner || '.' || modules LIKE '%' || @searchText || '%'))""",
             [ "currentModule", Sql.string currentModule
               "directChildPattern", Sql.string directChildPattern
               "searchText", Sql.string query.text ])

      // When searching with a branch:
      // 1. Get anything with branch_id = NULL (base packages)
      // 2. Get anything with branch_id = @current_branch (branch-specific)
      // 3. If there's a branch-specific version of a package, it shadows the NULL version
      // 4. If branch-specific is deprecated, exclude both versions
      $"""
      SELECT DISTINCT owner, modules
      FROM locations l
      WHERE (l.branch_id IS NULL OR l.branch_id = @branch_id)
        AND l.deprecated_at IS NULL
        AND {submoduleCondition}
        -- Exclude if there's a deprecated branch-specific version shadowing this
        AND NOT EXISTS (
          SELECT 1 FROM locations l2
          WHERE l2.owner = l.owner
            AND l2.modules = l.modules
            AND l2.name = l.name
            AND l2.item_type = l.item_type
            AND l2.branch_id = @branch_id
            AND l2.deprecated_at IS NOT NULL
            AND l.branch_id IS NULL
        )
        -- If both NULL and branch-specific exist, only show branch-specific
        AND NOT EXISTS (
          SELECT 1 FROM locations l3
          WHERE l3.owner = l.owner
            AND l3.modules = l.modules
            AND l3.name = l.name
            AND l3.item_type = l.item_type
            AND l3.branch_id = @branch_id
            AND l3.deprecated_at IS NULL
            AND l.branch_id IS NULL
        )
      """
      |> Sql.query
      |> Sql.parameters
        ([ "branch_id",
           (match branchId with
            | Some id -> Sql.uuid id
            | None -> Sql.dbnull) ]
         @ sqlParams)
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

      // Location filter depends on depth and current location
      let locationCondition =
        if System.String.IsNullOrEmpty currentModule && query.searchDepth = PT.Search.SearchDepth.AllDescendants then
          // At root with AllDescendants - search everywhere
          "1 = 1"
        else if System.String.IsNullOrEmpty currentModule then
          // At root with OnlyDirectDescendants - search in direct children only
          "((l.modules = @modules) OR (l.owner || '.' || l.modules = @fqname))"
        else
          // Not at root - search in specified module path
          match query.searchDepth with
          | PT.Search.SearchDepth.OnlyDirectDescendants ->
            "((l.modules = @modules) OR (l.owner || '.' || l.modules = @fqname))"
          | PT.Search.SearchDepth.AllDescendants ->
            // Search in current module and all descendants
            "((l.modules = @modules) OR (l.owner || '.' || l.modules = @fqname) OR (l.modules LIKE @modules || '.%') OR (l.owner || '.' || l.modules LIKE @fqname || '.%'))"

      // Same shadowing logic as for submodules
      $"SELECT c.id, c.pt_def, l.owner, l.modules, l.name\n"
      + $"FROM locations l\n"
      + $"JOIN {contentTable} c ON l.item_id = c.id\n"
      + "WHERE (l.branch_id IS NULL OR l.branch_id = @branch_id)\n"
      + "  AND l.deprecated_at IS NULL\n"
      + $"  AND l.item_type = '{itemType}'\n"
      + $"  AND ({locationCondition})\n"
      + $"  AND {nameCondition}\n"
      + "  -- Exclude if there's a deprecated branch-specific version shadowing this\n"
      + "  AND NOT EXISTS (\n"
      + "    SELECT 1 FROM locations l2\n"
      + "    WHERE l2.owner = l.owner\n"
      + "      AND l2.modules = l.modules\n"
      + "      AND l2.name = l.name\n"
      + $"      AND l2.item_type = '{itemType}'\n"
      + "      AND l2.branch_id = @branch_id\n"
      + "      AND l2.deprecated_at IS NOT NULL\n"
      + "      AND l.branch_id IS NULL\n"
      + "  )\n"
      + "  -- If both NULL and branch-specific exist, only show branch-specific\n"
      + "  AND NOT EXISTS (\n"
      + "    SELECT 1 FROM locations l3\n"
      + "    WHERE l3.owner = l.owner\n"
      + "      AND l3.modules = l.modules\n"
      + "      AND l3.name = l.name\n"
      + $"      AND l3.item_type = '{itemType}'\n"
      + "      AND l3.branch_id = @branch_id\n"
      + "      AND l3.deprecated_at IS NULL\n"
      + "      AND l.branch_id IS NULL\n"
      + "  )"
      |> Sql.query
      |> Sql.parameters
        [ "branch_id",
          (match branchId with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull)
          "modules", Sql.string currentModule
          "fqname", Sql.string currentModule
          "searchText", Sql.string query.text ]
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
        makeEntityQuery "type" "package_types" BinarySerialization.PT.PackageType.deserialize
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
        makeEntityQuery "fn" "package_functions" BinarySerialization.PT.PackageFn.deserialize
      else
        Task.FromResult<List<PT.LocatedItem<PT.PackageFn.PackageFn>>> []

    return { submodules = submodules; types = types; values = values; fns = fns }
  }
