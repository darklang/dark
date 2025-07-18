module LibPackageManager.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude
open Fumble
open LibDB.Db

open Microsoft.Data.Sqlite

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization

open LibPackageManager.Types


// Use the connection string from LibDB.Db

let savePackageTypes (types : List<PT.PackageType.PackageType>) : Task<unit> =
  task {
    if List.isEmpty types then return ()

    // Create statements for transaction
    let statements =
      types
      |> List.map (fun typ ->
        let sql =
          @"INSERT INTO package_types_v0
                (id, owner, modules, name, definition)
              VALUES
                (@id, @owner, @modules, @name, @definition)"

        let parameters =
          [ "id", Sql.uuid typ.id
            "owner", Sql.string typ.name.owner
            "modules", Sql.string (String.concat "." typ.name.modules)
            "name", Sql.string typ.name.name
            "definition", Sql.bytes (BinarySerialization.PackageType.serialize typ) ]

        (sql, [ parameters ]))

    // Execute all statements in a transaction
    Sql.executeTransactionSync statements |> ignore<List<int>>
  }

let savePackageConstants
  (constants : List<PT.PackageConstant.PackageConstant>)
  : Task<unit> =
  task {
    if List.isEmpty constants then return ()

    // Create statements for transaction
    let statements =
      constants
      |> List.map (fun c ->
        let sql =
          @"INSERT INTO package_constants_v0
                (id, owner, modules, name, definition)
              VALUES
                (@id, @owner, @modules, @name, @definition)"

        let parameters =
          [ "id", Sql.uuid c.id
            "owner", Sql.string c.name.owner
            "modules", Sql.string (String.concat "." c.name.modules)
            "name", Sql.string c.name.name
            "definition",
            Sql.bytes (BinarySerialization.PackageConstant.serialize c) ]

        (sql, [ parameters ]))

    // Execute all statements in a transaction
    Sql.executeTransactionSync statements |> ignore<List<int>>
  }

let savePackageFunctions (fns : List<PT.PackageFn.PackageFn>) : Task<unit> =
  task {
    if List.isEmpty fns then return ()

    // Create statements for transaction
    let statements =
      fns
      |> List.map (fun fn ->
        let sql =
          @"INSERT INTO package_functions_v0
                (id, owner, modules, name, definition)
              VALUES
                (@id, @owner, @modules, @name, @definition)"

        let parameters =
          [ "id", Sql.uuid fn.id
            "owner", Sql.string fn.name.owner
            "modules", Sql.string (String.concat "." fn.name.modules)
            "name", Sql.string fn.name.name
            "definition", Sql.bytes (BinarySerialization.PackageFn.serialize fn) ]

        (sql, [ parameters ]))

    // Execute all statements in a transaction
    Sql.executeTransactionSync statements |> ignore<List<int>>
  }


let purge () : Task<unit> =
  task {
    // Create statements for transaction
    let statements =
      [ "DELETE FROM package_types_v0"
        "DELETE FROM package_constants_v0"
        "DELETE FROM package_functions_v0" ]
      |> List.map (fun sql -> (sql, [ [] ]))

    // Execute all statements in a transaction
    Sql.executeTransactionSync statements |> ignore<List<int>>
  }

// ------------------
// Fetching
// ------------------

let findFn (name : PT.PackageFn.Name) : Ply<Option<PT.FQFnName.Package>> =
  uply {
    let! result =
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

    return result
  }

let getFn (id : uuid) : Ply<Option<PT.PackageFn.PackageFn>> =
  uply {
    let! result =
      Sql.query
        """
        SELECT definition
        FROM package_functions_v0
        WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeRowOptionAsync (fun read -> read.bytes "definition")

    return
      result
      |> Option.map (fun definition ->
        BinarySerialization.PackageFn.deserialize id definition)
  }


let findType (name : PT.PackageType.Name) : Ply<Option<PT.FQTypeName.Package>> =
  uply {
    let! result =
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

    return result
  }

let getType (id : uuid) : Ply<Option<PT.PackageType.PackageType>> =
  uply {
    let! result =
      Sql.query
        """
        SELECT definition
        FROM package_types_v0
        WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeRowOptionAsync (fun read -> read.bytes "definition")

    return
      result
      |> Option.map (fun definition ->
        BinarySerialization.PackageType.deserialize id definition)
  }


let findConstant
  (name : PT.PackageConstant.Name)
  : Ply<Option<PT.FQConstantName.Package>> =
  uply {
    let! result =
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

    return result
  }

let getConstant (id : uuid) : Ply<Option<PT.PackageConstant.PackageConstant>> =
  uply {
    let! result =
      Sql.query
        """
        SELECT definition
        FROM package_constants_v0
        WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeRowOptionAsync (fun read -> read.bytes "definition")

    return
      result
      |> Option.map (fun definition ->
        BinarySerialization.PackageConstant.deserialize id definition)
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
      "SELECT id, owner, modules, name, definition\n"
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
        let definition = read.bytes "definition"
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
