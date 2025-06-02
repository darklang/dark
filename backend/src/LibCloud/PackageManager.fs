/// The package manager allows types and functions to be shared with other users
module LibCloud.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql

open Prelude

open Db

module BinarySerialization = LibBinarySerialization.BinarySerialization
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


let savePackageTypes (types : List<PT.PackageType.PackageType>) : Task<Unit> =
  types
  |> Task.iterInParallel (fun typ ->
    Sql.query
      "INSERT INTO package_types_v0
        (id, owner, modules, name, definition)
      VALUES
        (@id, @owner, @modules, @name, @definition)"
    |> Sql.parameters
      [ "id", Sql.uuid typ.id
        "owner", Sql.string typ.name.owner
        "modules", Sql.string (typ.name.modules |> String.concat ".")
        "name", Sql.string typ.name.name
        "definition", Sql.bytea (BinarySerialization.PackageType.serialize typ) ]
    |> Sql.executeStatementAsync)

let savePackageConstants
  (constants : List<PT.PackageConstant.PackageConstant>)
  : Task<Unit> =
  constants
  |> Task.iterInParallel (fun c ->
    Sql.query
      "INSERT INTO package_constants_v0
        (id, owner, modules, name, definition)
      VALUES
        (@id, @owner, @modules, @name, @definition)"
    |> Sql.parameters
      [ "id", Sql.uuid c.id
        "owner", Sql.string c.name.owner
        "modules", Sql.string (c.name.modules |> String.concat ".")
        "name", Sql.string c.name.name
        "definition", Sql.bytea (BinarySerialization.PackageConstant.serialize c) ]
    |> Sql.executeStatementAsync)

let savePackageFunctions (fns : List<PT.PackageFn.PackageFn>) : Task<Unit> =
  fns
  |> Task.iterInParallel (fun fn ->
    Sql.query
      "INSERT INTO package_functions_v0
        (id, owner, modules, name, definition)
      VALUES
        (@id, @owner, @modules, @name, @definition)"
    |> Sql.parameters
      [ "id", Sql.uuid fn.id
        "owner", Sql.string fn.name.owner
        "modules", Sql.string (fn.name.modules |> String.concat ".")
        "name", Sql.string fn.name.name
        "definition", Sql.bytea (BinarySerialization.PackageFn.serialize fn) ]
    |> Sql.executeStatementAsync)


let purge () : Task<unit> =
  task {
    do!
      Sql.query "DELETE FROM package_types_v0"
      |> Sql.parameters []
      |> Sql.executeStatementAsync

    do!
      Sql.query "DELETE FROM package_constants_v0"
      |> Sql.parameters []
      |> Sql.executeStatementAsync

    do!
      Sql.query "DELETE FROM package_functions_v0"
      |> Sql.parameters []
      |> Sql.executeStatementAsync
  }


// ------------------
// Fetching
// ------------------

let findFn (name : PT.PackageFn.Name) : Ply<Option<PT.FQFnName.Package>> =
  uply {
    return!
      "SELECT id
      FROM package_functions_v0
      WHERE owner = @owner
        AND modules = @modules
        AND name = @name"
      |> Sql.query
      |> Sql.parameters
        [ "owner", Sql.string name.owner
          "modules", Sql.string (name.modules |> String.concat ".")
          "name", Sql.string name.name ]
      |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
  }

let getFn (id : uuid) : Ply<Option<PT.PackageFn.PackageFn>> =
  uply {
    let! def =
      "SELECT definition
      FROM package_functions_v0
      WHERE id = @id"
      |> Sql.query
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeRowOptionAsync (fun read -> read.bytea "definition")

    return
      def |> Option.map (fun def -> BinarySerialization.PackageFn.deserialize id def)
  }


let findType (name : PT.PackageType.Name) : Ply<Option<PT.FQTypeName.Package>> =
  uply {
    return!
      "SELECT id
      FROM package_types_v0
      WHERE owner = @owner
        AND modules = @modules
        AND name = @name"
      |> Sql.query
      |> Sql.parameters
        [ "owner", Sql.string name.owner
          "modules", Sql.string (name.modules |> String.concat ".")
          "name", Sql.string name.name ]
      |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
  }

let getType (id : uuid) : Ply<Option<PT.PackageType.PackageType>> =
  uply {
    let! def =
      "SELECT definition
      FROM package_types_v0
      WHERE id = @id"
      |> Sql.query
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeRowOptionAsync (fun read -> read.bytea "definition")

    return
      def
      |> Option.map (fun def -> BinarySerialization.PackageType.deserialize id def)
  }


let findConstant
  (name : PT.PackageConstant.Name)
  : Ply<Option<PT.FQConstantName.Package>> =
  uply {
    return!
      "SELECT id
      FROM package_constants_v0
      WHERE owner = @owner
        AND modules = @modules
        AND name = @name"
      |> Sql.query
      |> Sql.parameters
        [ "owner", Sql.string name.owner
          "modules", Sql.string (name.modules |> String.concat ".")
          "name", Sql.string name.name ]
      |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
  }

let getConstant (id : uuid) : Ply<Option<PT.PackageConstant.PackageConstant>> =
  uply {
    let! def =
      "SELECT definition
      FROM package_constants_v0
      WHERE id = @id"
      |> Sql.query
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeRowOptionAsync (fun read -> read.bytea "definition")

    return
      def
      |> Option.map (fun def ->
        BinarySerialization.PackageConstant.deserialize id def)
  }


let withCache (f : 'key -> Ply<Option<'value>>) =
  let cache = System.Collections.Concurrent.ConcurrentDictionary<'key, 'value>()
  fun (key : 'key) ->
    uply {
      let mutable cached = Unchecked.defaultof<'value>
      let inCache = cache.TryGetValue(key, &cached)
      if inCache then
        return Some cached
      else
        let! result = f key
        match result with
        | Some v -> cache.TryAdd(key, v) |> ignore<bool>
        | None -> ()
        return result
    }


/// Search for packages based on the given query
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
      ("SELECT id, owner, modules, name, definition\n"
       + $"FROM {table}\n"
       + "WHERE ((modules = @modules) OR (owner || '.' || modules = @fqname))\n"
       + "  AND name LIKE '%' || @searchText || '%'")
      |> Sql.query
      |> Sql.parameters
        [ "modules", Sql.string currentModule
          "fqname", Sql.string currentModule
          "searchText", Sql.string query.text ]
      |> Sql.executeAsync (fun read ->
        let id = read.uuid "id"
        let definition = read.bytea "definition"
        deserializeFn id definition)

    let isEntityRequested entity =
      query.entityTypes.IsEmpty || List.contains entity query.entityTypes

    let! types =
      if isEntityRequested PT.Search.EntityType.Type then
        makeEntityQuery
          "package_types_v0"
          BinarySerialization.PackageType.deserialize
      else
        Task.FromResult []

    let! constants =
      if isEntityRequested PT.Search.EntityType.Constant then
        makeEntityQuery
          "package_constants_v0"
          BinarySerialization.PackageConstant.deserialize
      else
        Task.FromResult []

    let! fns =
      if isEntityRequested PT.Search.EntityType.Fn then
        makeEntityQuery
          "package_functions_v0"
          BinarySerialization.PackageFn.deserialize
      else
        Task.FromResult []

    return
      { submodules = submodules; types = types; constants = constants; fns = fns }
  }


let rt : RT.PackageManager =
  { getType =
      withCache (fun id ->
        uply {
          let! typ = getType id
          return typ |> Option.map PT2RT.PackageType.toRT
        })
    getFn =
      withCache (fun id ->
        uply {
          let! fn = getFn id
          return fn |> Option.map PT2RT.PackageFn.toRT
        })
    getConstant =
      withCache (fun id ->
        uply {
          let! c = getConstant id
          return c |> Option.map PT2RT.PackageConstant.toRT
        })

    init = uply { return () } }


let pt : PT.PackageManager =
  { findType = withCache findType
    findConstant = withCache findConstant
    findFn = withCache findFn

    getType = withCache getType
    getFn = withCache getFn
    getConstant = withCache getConstant

    search = search

    init = uply { return () } }
