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
          SELECT id
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = 'type'
            AND deprecated_at IS NULL
            AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "owner", Sql.string location.owner
            "modules", Sql.string modulesStr
            "name", Sql.string location.name
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
        |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
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
          WHERE id = @id
            AND item_type = 'type'
            AND deprecated_at IS NULL
            AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "id", Sql.uuid id
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
          SELECT id
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = 'value'
            AND deprecated_at IS NULL
            AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "owner", Sql.string location.owner
            "modules", Sql.string modulesStr
            "name", Sql.string location.name
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
        |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
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
          WHERE id = @id
            AND item_type = 'value'
            AND deprecated_at IS NULL
            AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "id", Sql.uuid id
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
          SELECT id
          FROM locations
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
            AND item_type = 'fn'
            AND deprecated_at IS NULL
            AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "owner", Sql.string location.owner
            "modules", Sql.string modulesStr
            "name", Sql.string location.name
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
        |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
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
          WHERE id = @id
            AND item_type = 'fn'
            AND deprecated_at IS NULL
            AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
          ORDER BY created_at DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "id", Sql.uuid id
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
        |> Sql.executeRowOptionAsync (fun read ->
          let modulesStr = read.string "modules"
          { owner = read.string "owner"
            modules = modulesStr.Split('.') |> Array.toList
            name = read.string "name" })
    }



let search
  ((_branchId, query) : Option<PT.BranchID> * PT.Search.SearchQuery)
  : Ply<PT.Search.SearchResults> =
  // TODO: use _branchId when implementing branch-aware search
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
          // At root - search for owners that match the text
          ("""(owner LIKE '%' || @searchText || '%')""",
           [ "searchText", Sql.string query.text ])
        else
          // Not at root - fuzzy logic with direct descendants only
          // For OnlyDirectDescendants, we want modules that start with currentModule + "."
          // but don't have any additional "." after that (i.e., direct children only)
          let directChildPattern = currentModule + ".%"
          ("""((modules LIKE @directChildPattern AND modules NOT LIKE @nestedPattern AND modules LIKE '%' || @searchText || '%')
                OR (owner || '.' || modules LIKE @directChildPattern AND owner || '.' || modules NOT LIKE @nestedPattern AND owner || '.' || modules LIKE '%' || @searchText || '%'))""",
           [ "currentModule", Sql.string currentModule
             "directChildPattern", Sql.string directChildPattern
             "nestedPattern", Sql.string (currentModule + ".%.%")
             "searchText", Sql.string query.text ])

      $"""
      SELECT DISTINCT owner, modules
      FROM locations
      WHERE deprecated_at IS NULL
        AND {submoduleCondition}
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

    let makeEntityQuery (itemType : string) (contentTable : string) deserializeFn =
      let nameCondition =
        if query.exactMatch then
          "l.name = @searchText"
        else
          "l.name LIKE '%' || @searchText || '%'"

      $"SELECT c.id, c.pt_def, l.owner, l.modules, l.name\n"
      + $"FROM locations l\n"
      + $"JOIN {contentTable} c ON l.id = c.id\n"
      + "WHERE l.deprecated_at IS NULL\n"
      + $"  AND l.item_type = '{itemType}'\n"
      + "  AND ((l.modules = @modules) OR (l.owner || '.' || l.modules = @fqname))\n"
      + $"  AND {nameCondition}"
      |> Sql.query
      |> Sql.parameters
        [ "modules", Sql.string currentModule
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
