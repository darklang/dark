/// The package manager allows types and functions to be shared with other users
module LibCloud.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql

open Prelude
open Tablecloth
open Db
open Ply

module BinarySerialization = LibBinarySerialization.BinarySerialization
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes

let writeBody (tlid : tlid) (expr : PT.Expr) : Task<unit> =
  task {
    let binary = BinarySerialization.serializeExpr tlid expr
    return!
      Sql.query "UPDATE package_functions_v0 SET body = @body where tlid = @tlid"
      |> Sql.parameters [ "body", Sql.bytea binary; "tlid", Sql.tlid tlid ]
      |> Sql.executeStatementAsync
  }

let savePackageFunctions (fns : List<PT.PackageFn.T>) : Task<Unit> =
  fns
  |> Task.iterInParallel (fun fn ->
    let (PT.FnName.FnName name) = fn.name.name
    Sql.query
      "INSERT INTO package_functions_v0 (tlid, id, owner, modules, fnname, version, definition)
       VALUES (@tlid, @id, @owner, @modules, @fnname, @version, @definition)"
    |> Sql.parameters
      [ "tlid", Sql.tlid fn.tlid
        "id", Sql.uuid fn.id
        "owner", Sql.string fn.name.owner
        "modules", Sql.string (fn.name.modules |> String.concat ".")
        "fnname", Sql.string name
        "version", Sql.int fn.name.version
        "definition", Sql.bytea (BinarySerialization.serializePackageFn fn) ]
    |> Sql.executeStatementAsync)

let savePackageTypes (types : List<PT.PackageType.T>) : Task<Unit> =
  types
  |> Task.iterInParallel (fun typ ->
    let (PT.TypeName.TypeName name) = typ.name.name
    Sql.query
      "INSERT INTO package_types_v0 (tlid, id, owner, modules, typename, version, definition)
       VALUES (@tlid, @id, @owner, @modules, @typename, @version, @definition)"
    |> Sql.parameters
      [ "tlid", Sql.tlid typ.tlid
        "id", Sql.uuid typ.id
        "owner", Sql.string typ.name.owner
        "modules", Sql.string (typ.name.modules |> String.concat ".")
        "typename", Sql.string name
        "version", Sql.int typ.name.version
        "definition", Sql.bytea (BinarySerialization.serializePackageType typ) ]
    |> Sql.executeStatementAsync)


let savePackageConstants (constants : List<PT.PackageConstant.T>) : Task<Unit> =
  constants
  |> Task.iterInParallel (fun c ->
    let (PT.ConstantName.ConstantName name) = c.name.name
    Sql.query
      "INSERT INTO package_constants_v0 (tlid, id, owner, modules, name, version, definition)
       VALUES (@tlid, @id, @owner, @modules, @name, @version, @definition)"
    |> Sql.parameters
      [ "tlid", Sql.tlid c.tlid
        "id", Sql.uuid c.id
        "owner", Sql.string c.name.owner
        "modules", Sql.string (c.name.modules |> String.concat ".")
        "name", Sql.string name
        "version", Sql.int c.name.version
        "definition", Sql.bytea (BinarySerialization.serializePackageConstant c) ]
    |> Sql.executeStatementAsync)


// ------------------
// Fetching
// ------------------

let getFn (name : PT.FnName.Package) : Ply<Option<PT.PackageFn.T>> =
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
          "name",
          (match name.name with
           | PT.FnName.FnName n -> Sql.string n)
          "version", Sql.int name.version ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.uuid "id", read.bytea "definition"))

    return
      fn
      |> Option.map (fun (id, def) ->
        BinarySerialization.deserializePackageFn id def)
  }

let getType (name : PT.TypeName.Package) : Ply<Option<PT.PackageType.T>> =
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
          "name",
          (match name.name with
           | PT.TypeName.TypeName n -> Sql.string n)
          "version", Sql.int name.version ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.uuid "id", read.bytea "definition"))

    return
      fn
      |> Option.map (fun (id, def) ->
        BinarySerialization.deserializePackageType id def)
  }

let getConstant
  (name : PT.ConstantName.Package)
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
          "name",
          (match name.name with
           | PT.ConstantName.ConstantName n -> Sql.string n)
          "version", Sql.int name.version ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.uuid "id", read.bytea "definition"))

    return
      fn
      |> Option.map (fun (id, def) ->
        BinarySerialization.deserializePackageConstant id def)
  }


open System
open System.Threading

let cacheByNameSimple (fn : 'a -> Ply<'b>) : 'a -> Ply<'b> =
  // TODO: consider another structure
  let mutable cache : Map<'a, DateTime * 'b> = Map.empty

  fun name ->
    uply {
      let now = DateTime.Now
      match Map.tryFind name cache with
      | Some(exp, v) when exp < now -> return v
      | _ ->
        let! v = fn name
        let exp = now + TimeSpan.FromSeconds 30.
        cache <- Map.add name (exp, v) cache
        return v
    }


let cacheByNameWithSemaphore (fn : 'a -> Ply<Option<'b>>) : 'a -> Ply<Option<'b>> =
  let cache =
    Collections.Concurrent.ConcurrentDictionary<'a, DateTime * Option<'b>>()
  let ongoingComputations =
    Collections.Concurrent.ConcurrentDictionary<'a, SemaphoreSlim>()

  let tryGetValue key =
    match cache.TryGetValue(key) with
    | (true, value) -> Some value
    | (false, _) -> None

  fun name ->
    uply {
      let now = System.DateTime.Now
      match tryGetValue name with
      | Some(exp, Some v) when exp > now -> return Some v
      | _ ->
        debuG "cache miss" name
        let semaphore =
          ongoingComputations.GetOrAdd(name, (fun _ -> new SemaphoreSlim 1))
        try
          // Wait for the semaphore before proceeding
          do! semaphore.WaitAsync()

          // Double-check the cache after acquiring the semaphore
          match tryGetValue name with
          | Some(exp, Some v) when exp > now -> return Some v
          | _ ->
            let! v = fn name
            let exp = now + System.TimeSpan.FromSeconds 30.
            cache.[name] <- (exp, v)
            debuG "got" name
            return v
        finally
          semaphore.Release() |> ignore<int>
          if semaphore.CurrentCount = 1 then
            // Remove semaphore from dictionary if no other tasks are waiting on it

            let mutable removedSem = semaphore
            ongoingComputations.TryRemove(name, &removedSem) |> ignore<bool>
    }

let noCache (fn : 'a -> Ply<'b>) : 'a -> Ply<'b> = fun name -> fn name

let cacheByName = noCache

// CLEANUP this package manager should be removed, and all usages replaced with the
// one that fetches things from the `dark-packages` canvas' http endpoints
let packageManager : RT.PackageManager =
  { getType =
      cacheByName (fun name ->
        uply {
          let! typ = name |> PT2RT.TypeName.Package.fromRT |> getType
          return Option.map PT2RT.PackageType.toRT typ
        })

    getFn =
      cacheByName (fun name ->
        uply {
          let! typ = name |> PT2RT.FnName.Package.fromRT |> getFn
          return Option.map PT2RT.PackageFn.toRT typ
        })

    getConstant =
      cacheByName (fun name ->
        uply {
          let! typ = name |> PT2RT.ConstantName.Package.fromRT |> getConstant
          return Option.map PT2RT.PackageConstant.toRT typ
        })

    init = uply { return () } }
