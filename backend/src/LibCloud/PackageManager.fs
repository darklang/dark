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
    // Get all modules that match the current module path and search text
    let! submodules =
      "WITH all_modules AS (
        SELECT DISTINCT modules
        FROM (
          SELECT modules FROM package_types_v0
          UNION
          SELECT modules FROM package_constants_v0
          UNION
          SELECT modules FROM package_functions_v0
        ) AS all_items
      )
      SELECT modules
      FROM all_modules
      WHERE modules LIKE @modulePrefix || '%'
        AND modules LIKE '%' || @searchText || '%'"
      |> Sql.query
      |> Sql.parameters
        [ "modulePrefix",
          Sql.string (
            if query.currentModule.IsEmpty then
              ""
            else
              query.currentModule |> String.concat "." |> (fun s -> s + ".")
          )
          "searchText", Sql.string query.text ]
      |> Sql.executeAsync (fun read ->
        let modulesStr = read.string "modules"
        modulesStr.Split('.') |> Array.toList)

    let! types =
      if
        query.entityTypes.IsEmpty
        || query.entityTypes |> List.contains PT.Search.EntityType.Type
      then
        "SELECT id, owner, modules, name, definition
        FROM package_types_v0
        WHERE modules = @modules
          AND name LIKE '%' || @searchText || '%'"
        |> Sql.query
        |> Sql.parameters
          [ "modules", Sql.string (query.currentModule |> String.concat ".")
            "searchText", Sql.string query.text ]
        |> Sql.executeAsync (fun read ->
          let id = read.uuid "id"
          let definition = read.bytea "definition"
          BinarySerialization.PackageType.deserialize id definition)
      else
        Task.FromResult []

    let! constants =
      if
        query.entityTypes.IsEmpty
        || query.entityTypes |> List.contains PT.Search.EntityType.Constant
      then
        "SELECT id, owner, modules, name, definition
        FROM package_constants_v0
        WHERE modules = @modules
          AND name LIKE '%' || @searchText || '%'"
        |> Sql.query
        |> Sql.parameters
          [ "modules", Sql.string (query.currentModule |> String.concat ".")
            "searchText", Sql.string query.text ]
        |> Sql.executeAsync (fun read ->
          let id = read.uuid "id"
          let definition = read.bytea "definition"
          BinarySerialization.PackageConstant.deserialize id definition)
      else
        Task.FromResult []

    let! fns =
      if
        query.entityTypes.IsEmpty
        || query.entityTypes |> List.contains PT.Search.EntityType.Fn
      then
        "SELECT id, owner, modules, name, definition
        FROM package_functions_v0
        WHERE modules = @modules
          AND name LIKE '%' || @searchText || '%'"
        |> Sql.query
        |> Sql.parameters
          [ "modules", Sql.string (query.currentModule |> String.concat ".")
            "searchText", Sql.string query.text ]
        |> Sql.executeAsync (fun read ->
          let id = read.uuid "id"
          let definition = read.bytea "definition"
          BinarySerialization.PackageFn.deserialize id definition)
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

    search = search

    getType = withCache getType
    getFn = withCache getFn
    getConstant = withCache getConstant

    init = uply { return () } }
