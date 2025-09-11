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
          SELECT hash
          FROM package_types_v0
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
          """
        |> Sql.parameters
          [ "owner", Sql.string name.owner
            "modules", Sql.string (String.concat "." name.modules)
            "name", Sql.string name.name ]
        |> Sql.executeRowOptionAsync (fun read -> Hash(read.string "hash"))
    }

  let get (hash : Hash) : Ply<Option<PT.PackageType.PackageType>> =
    uply {
      let (Hash hashStr) = hash
      return!
        Sql.query
          """
          SELECT hash, pt_def
          FROM package_types_v0
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read ->
          let hash = read.string "hash"
          let ptDef = read.bytes "pt_def"
          let typ = BinarySerialization.PT.PackageType.deserialize hash ptDef
          { typ with hash = Hash hash })
    }


module Value =
  let find (name : PT.PackageValue.Name) : Ply<Option<PT.FQValueName.Package>> =
    uply {
      return!
        Sql.query
          """
          SELECT hash
          FROM package_values_v0
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
          """
        |> Sql.parameters
          [ "owner", Sql.string name.owner
            "modules", Sql.string (String.concat "." name.modules)
            "name", Sql.string name.name ]
        |> Sql.executeRowOptionAsync (fun read -> Hash(read.string "hash"))
    }

  let get (hash : Hash) : Ply<Option<PT.PackageValue.PackageValue>> =
    uply {
      let (Hash hashStr) = hash
      return!
        Sql.query
          """
          SELECT hash, pt_def
          FROM package_values_v0
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read ->
          let hash = read.string "hash"
          let ptDef = read.bytes "pt_def"
          let value = BinarySerialization.PT.PackageValue.deserialize hash ptDef
          { value with hash = Hash hash })
    }


module Fn =
  let find (name : PT.PackageFn.Name) : Ply<Option<PT.FQFnName.Package>> =
    uply {
      return!
        Sql.query
          """
          SELECT hash
          FROM package_functions_v0
          WHERE owner = @owner
            AND modules = @modules
            AND name = @name
          """
        |> Sql.parameters
          [ "owner", Sql.string name.owner
            "modules", Sql.string (String.concat "." name.modules)
            "name", Sql.string name.name ]
        |> Sql.executeRowOptionAsync (fun read -> Hash(read.string "hash"))
    }

  let get (hash : Hash) : Ply<Option<PT.PackageFn.PackageFn>> =
    uply {
      let (Hash hashStr) = hash
      return!
        Sql.query
          """
          SELECT hash, pt_def
          FROM package_functions_v0
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read ->
          let hash = read.string "hash"
          let ptDef = read.bytes "pt_def"
          let fn = BinarySerialization.PT.PackageFn.deserialize hash ptDef
          { fn with hash = Hash hash })
    }



let search (query : PT.Search.SearchQuery) : Ply<PT.Search.SearchResults> =
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
      FROM (
        SELECT owner, modules FROM package_types_v0
        WHERE {submoduleCondition}
        UNION ALL
        SELECT owner, modules FROM package_values_v0
        WHERE {submoduleCondition}
        UNION ALL
        SELECT owner, modules FROM package_functions_v0
        WHERE {submoduleCondition}
      ) AS filtered_modules
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

    let makeEntityQuery table deserializeFn =
      let nameCondition =
        if query.exactMatch then
          "name = @searchText"
        else
          "name LIKE '%' || @searchText || '%'"

      "SELECT hash, owner, modules, name, pt_def\n"
      + $"FROM {table}\n"
      + "WHERE ((modules = @modules) OR (owner || '.' || modules = @fqname))\n"
      + $"  AND {nameCondition}"
      |> Sql.query
      |> Sql.parameters
        [ "modules", Sql.string currentModule
          "fqname", Sql.string currentModule
          "searchText", Sql.string query.text ]
      |> Sql.executeAsync (fun read ->
        let hash = read.string "hash"
        let definition = read.bytes "pt_def"
        deserializeFn hash definition)

    let isEntityRequested entity =
      query.entityTypes.IsEmpty || List.contains entity query.entityTypes

    let! types =
      if isEntityRequested PT.Search.EntityType.Type then
        makeEntityQuery
          "package_types_v0"
          BinarySerialization.PT.PackageType.deserialize
      else
        Task.FromResult []

    let! values =
      if isEntityRequested PT.Search.EntityType.Value then
        makeEntityQuery
          "package_values_v0"
          BinarySerialization.PT.PackageValue.deserialize
      else
        Task.FromResult []

    let! fns =
      if isEntityRequested PT.Search.EntityType.Fn then
        makeEntityQuery
          "package_functions_v0"
          BinarySerialization.PT.PackageFn.deserialize
      else
        Task.FromResult []

    return { submodules = submodules; types = types; values = values; fns = fns }
  }
