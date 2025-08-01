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
  let find (name : PT.PackageType.Name) : Ply<Option<PT.FQTypeName.Package>> =
    uply {
      return!
        Sql.query
          """
          SELECT id
          FROM package_types_v0
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
          """
        |> Sql.parameters
          [ "owner", Sql.string name.owner
            "modules", Sql.string (String.concat "." name.modules)
            "name", Sql.string name.name ]
        |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
    }

  let get (id : uuid) : Ply<Option<PT.PackageType.PackageType>> =
    uply {
      return!
        Sql.query
          """
          SELECT pt_def
          FROM package_types_v0
          WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "pt_def")
        |> Task.map (Option.map (BinarySerialization.PT.PackageType.deserialize id))
    }


module Constant =
  let find
    (name : PT.PackageConstant.Name)
    : Ply<Option<PT.FQConstantName.Package>> =
    uply {
      return!
        Sql.query
          """
          SELECT id
          FROM package_constants_v0
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
          """
        |> Sql.parameters
          [ "owner", Sql.string name.owner
            "modules", Sql.string (String.concat "." name.modules)
            "name", Sql.string name.name ]
        |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
    }

  let get (id : uuid) : Ply<Option<PT.PackageConstant.PackageConstant>> =
    uply {
      return!
        Sql.query
          """
          SELECT pt_def
          FROM package_constants_v0
          WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "pt_def")
        |> Task.map (
          Option.map (BinarySerialization.PT.PackageConstant.deserialize id)
        )
    }


module Fn =
  let find (name : PT.PackageFn.Name) : Ply<Option<PT.FQFnName.Package>> =
    uply {
      return!
        Sql.query
          """
          SELECT id
          FROM package_functions_v0
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
          """
        |> Sql.parameters
          [ "owner", Sql.string name.owner
            "modules", Sql.string (String.concat "." name.modules)
            "name", Sql.string name.name ]
        |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
    }

  let get (id : uuid) : Ply<Option<PT.PackageFn.PackageFn>> =
    uply {
      return!
        Sql.query
          """
          SELECT pt_def
          FROM package_functions_v0
          WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "pt_def")
        |> Task.map (Option.map (BinarySerialization.PT.PackageFn.deserialize id))
    }



let search (query : PT.Search.SearchQuery) : Ply<PT.Search.SearchResults> =
  uply {
    let currentModule = String.concat "." query.currentModule

    let! submodules =
      """
      SELECT DISTINCT owner, modules
      FROM (
        SELECT owner, modules FROM package_types_v0
        WHERE (modules LIKE @currentModule || '%' AND modules LIKE '%' || @searchText || '%')
           OR (owner || '.' || modules LIKE @currentModule || '%' AND owner || '.' || modules LIKE '%' || @searchText || '%')
        UNION ALL
        SELECT owner, modules FROM package_constants_v0
        WHERE (modules LIKE @currentModule || '%' AND modules LIKE '%' || @searchText || '%')
           OR (owner || '.' || modules LIKE @currentModule || '%' AND owner || '.' || modules LIKE '%' || @searchText || '%')
        UNION ALL
        SELECT owner, modules FROM package_functions_v0
        WHERE (modules LIKE @currentModule || '%' AND modules LIKE '%' || @searchText || '%')
           OR (owner || '.' || modules LIKE @currentModule || '%' AND owner || '.' || modules LIKE '%' || @searchText || '%')
      ) AS filtered_modules
      """
      |> Sql.query
      |> Sql.parameters
        [ "currentModule", Sql.string currentModule
          "searchText", Sql.string query.text ]
      |> Sql.executeAsync (fun read ->
        let owner = read.string "owner"
        let modulesStr = read.string "modules"
        let moduleParts = modulesStr.Split('.') |> Array.toList
        if List.tryHead moduleParts = Some owner then
          moduleParts
        else
          owner :: moduleParts)

    let makeEntityQuery table deserializeFn =
      "SELECT id, owner, modules, name, pt_def\n"
      + $"FROM {table}\n"
      + "WHERE ((modules = @modules) OR (owner || '.' || modules = @fqname))\n"
      + "  AND name LIKE '%' || @searchText || '%'"
      |> Sql.query
      |> Sql.parameters
        [ "modules", Sql.string currentModule
          "fqname", Sql.string currentModule
          "searchText", Sql.string query.text ]
      |> Sql.executeAsync (fun read ->
        let id = read.uuid "id"
        let definition = read.bytes "pt_def"
        deserializeFn id definition)

    let isEntityRequested entity =
      query.entityTypes.IsEmpty || List.contains entity query.entityTypes

    let! types =
      if isEntityRequested PT.Search.EntityType.Type then
        makeEntityQuery
          "package_types_v0"
          BinarySerialization.PT.PackageType.deserialize
      else
        Task.FromResult []

    let! constants =
      if isEntityRequested PT.Search.EntityType.Constant then
        makeEntityQuery
          "package_constants_v0"
          BinarySerialization.PT.PackageConstant.deserialize
      else
        Task.FromResult []

    let! fns =
      if isEntityRequested PT.Search.EntityType.Fn then
        makeEntityQuery
          "package_functions_v0"
          BinarySerialization.PT.PackageFn.deserialize
      else
        Task.FromResult []

    return
      { submodules = submodules; types = types; constants = constants; fns = fns }
  }
