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
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes


let savePackageTypes (types : List<PT.PackageType.T>) : Task<Unit> =
  types
  |> Task.iterInParallel (fun typ ->
    Sql.query
      "INSERT INTO package_types_v0
        (tlid, id, owner, modules, typename, version, definition)
      VALUES
        (@tlid, @id, @owner, @modules, @typename, @version, @definition)"
    |> Sql.parameters
      [ "tlid", Sql.tlid typ.tlid
        "id", Sql.uuid typ.id
        "owner", Sql.string typ.name.owner
        "modules", Sql.string (typ.name.modules |> String.concat ".")
        "typename", Sql.string typ.name.name
        "version", Sql.int typ.name.version
        "definition", Sql.bytea (BinarySerialization.PackageType.serialize typ) ]
    |> Sql.executeStatementAsync)


let savePackageConstants (constants : List<PT.PackageConstant.T>) : Task<Unit> =
  constants
  |> Task.iterInParallel (fun c ->
    Sql.query
      "INSERT INTO package_constants_v0
        (tlid, id, owner, modules, name, version, definition)
      VALUES
        (@tlid, @id, @owner, @modules, @name, @version, @definition)"
    |> Sql.parameters
      [ "tlid", Sql.tlid c.tlid
        "id", Sql.uuid c.id
        "owner", Sql.string c.name.owner
        "modules", Sql.string (c.name.modules |> String.concat ".")
        "name", Sql.string c.name.name
        "version", Sql.int c.name.version
        "definition", Sql.bytea (BinarySerialization.PackageConstant.serialize c) ]
    |> Sql.executeStatementAsync)

let savePackageFunctions (fns : List<PT.PackageFn.T>) : Task<Unit> =
  fns
  |> Task.iterInParallel (fun fn ->
    Sql.query
      "INSERT INTO package_functions_v0
        (tlid, id, owner, modules, fnname, version, definition)
      VALUES
        (@tlid, @id, @owner, @modules, @fnname, @version, @definition)"
    |> Sql.parameters
      [ "tlid", Sql.tlid fn.tlid
        "id", Sql.uuid fn.id
        "owner", Sql.string fn.name.owner
        "modules", Sql.string (fn.name.modules |> String.concat ".")
        "fnname", Sql.string fn.name.name
        "version", Sql.int fn.name.version
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

let getFn (name : PT.FQFnName.Package) : Ply<Option<PT.PackageFn.T>> =
  uply {
    let! fn =
      "SELECT id, definition
      FROM package_functions_v0
      WHERE owner = @owner
        AND modules = @modules
        AND fnname = @name
        AND version = @version"
      |> Sql.query
      |> Sql.parameters
        [ "owner", Sql.string name.owner
          "modules", Sql.string (name.modules |> String.concat ".")
          "name", Sql.string name.name
          "version", Sql.int name.version ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.uuid "id", read.bytea "definition"))

    return
      fn
      |> Option.map (fun (id, def) ->
        BinarySerialization.PackageFn.deserialize id def)
  }

let getFnByTLID (tlid : tlid) : Ply<Option<PT.PackageFn.T>> =
  uply {
    let! fn =
      "SELECT id, definition
      FROM package_functions_v0
      WHERE tlid = @tlid"
      |> Sql.query
      |> Sql.parameters [ "tlid", Sql.tlid tlid ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.uuid "id", read.bytea "definition"))

    return
      fn
      |> Option.map (fun (id, def) ->
        BinarySerialization.PackageFn.deserialize id def)
  }

let getType (name : PT.FQTypeName.Package) : Ply<Option<PT.PackageType.T>> =
  uply {
    let! fn =
      "SELECT id, definition
      FROM package_types_v0
      WHERE owner = @owner
        AND modules = @modules
        AND typename = @name
        AND version = @version"
      |> Sql.query
      |> Sql.parameters
        [ "owner", Sql.string name.owner
          "modules", Sql.string (name.modules |> String.concat ".")
          "name", Sql.string name.name
          "version", Sql.int name.version ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.uuid "id", read.bytea "definition"))

    return
      fn
      |> Option.map (fun (id, def) ->
        BinarySerialization.PackageType.deserialize id def)
  }

let getConstant
  (name : PT.FQConstantName.Package)
  : Ply<Option<PT.PackageConstant.T>> =
  uply {
    let! fn =
      "SELECT id, definition
      FROM package_constants_v0
      WHERE owner = @owner
        AND modules = @modules
        AND name = @name
        AND version = @version"
      |> Sql.query
      |> Sql.parameters
        [ "owner", Sql.string name.owner
          "modules", Sql.string (name.modules |> String.concat ".")
          "name", Sql.string name.name
          "version", Sql.int name.version ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.uuid "id", read.bytea "definition"))

    return
      fn
      |> Option.map (fun (id, def) ->
        BinarySerialization.PackageConstant.deserialize id def)
  }


let packageManager : RT.PackageManager =
  let withCache (f : 'name -> Ply<Option<'value>>) =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<'name, 'value>()
    fun (name : 'name) ->
      uply {
        let mutable cached = Unchecked.defaultof<'value>
        let inCache = cache.TryGetValue(name, &cached)
        if inCache then
          return Some cached
        else
          let! result = f name
          match result with
          | Some v -> cache.TryAdd(name, v) |> ignore<bool>
          | None -> ()
          return result
      }


  { getType =
      withCache (fun name ->
        uply {
          let! typ = name |> PT2RT.FQTypeName.Package.fromRT |> getType
          return Option.map PT2RT.PackageType.toRT typ
        })

    getFn =
      withCache (fun name ->
        uply {
          let! typ = name |> PT2RT.FQFnName.Package.fromRT |> getFn
          return Option.map PT2RT.PackageFn.toRT typ
        })
    getFnByTLID =
      fun tlid ->
        uply {
          let! typ = tlid |> getFnByTLID
          return Option.map PT2RT.PackageFn.toRT typ
        }

    getConstant =
      withCache (fun name ->
        uply {
          let! typ = name |> PT2RT.FQConstantName.Package.fromRT |> getConstant
          return Option.map PT2RT.PackageConstant.toRT typ
        })

    init = uply { return () } }
