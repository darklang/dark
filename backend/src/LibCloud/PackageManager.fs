/// The package manager allows types and functions to be shared with other users
module LibCloud.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql
open Microsoft.Data.Sqlite

open Prelude

open Db

module BinarySerialization = LibBinarySerialization.BinarySerialization
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


let connString = "Data Source=rundir/data.db"

let savePackageTypes (types : List<PT.PackageType.PackageType>) : Task<unit> =
  types
  |> Task.iterInParallel (fun typ ->
    task {
      use connection = new SqliteConnection(connString)
      do! connection.OpenAsync()

      use command = connection.CreateCommand()
      command.CommandText <-
        @"INSERT INTO package_types_v0
            (id, owner, modules, name, definition)
          VALUES
            (@id, @owner, @modules, @name, @definition)"

      command.Parameters.AddWithValue("@id", typ.id.ToString())
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue("@owner", typ.name.owner)
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue(
        "@modules",
        String.concat "." typ.name.modules
      )
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue("@name", typ.name.name)
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue(
        "@definition",
        BinarySerialization.PackageType.serialize typ
      )
      |> ignore<SqliteParameter>

      do! command.ExecuteNonQueryAsync() |> Async.AwaitTask |> Async.Ignore
    })

let savePackageConstants
  (constants : List<PT.PackageConstant.PackageConstant>)
  : Task<unit> =
  constants
  |> Task.iterInParallel (fun c ->
    task {
      use connection = new SqliteConnection(connString)
      do! connection.OpenAsync()

      use command = connection.CreateCommand()
      command.CommandText <-
        @"INSERT INTO package_constants_v0
            (id, owner, modules, name, definition)
          VALUES
            (@id, @owner, @modules, @name, @definition)"

      command.Parameters.AddWithValue("@id", c.id.ToString())
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue("@owner", c.name.owner)
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue(
        "@modules",
        String.concat "." c.name.modules
      )
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue("@name", c.name.name)
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue(
        "@definition",
        BinarySerialization.PackageConstant.serialize c
      )
      |> ignore<SqliteParameter>

      do! command.ExecuteNonQueryAsync() |> Async.AwaitTask |> Async.Ignore
    })

let savePackageFunctions
  (fns : List<PT.PackageFn.PackageFn>)
  : Task<unit> =
  fns
  |> Task.iterInParallel (fun fn ->
    task {
      use connection = new SqliteConnection(connString)
      do! connection.OpenAsync()

      use command = connection.CreateCommand()
      command.CommandText <-
        @"INSERT INTO package_functions_v0
            (id, owner, modules, name, definition)
          VALUES
            (@id, @owner, @modules, @name, @definition)"

      command.Parameters.AddWithValue("@id", fn.id.ToString())
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue("@owner", fn.name.owner)
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue(
        "@modules",
        String.concat "." fn.name.modules
      )
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue("@name", fn.name.name)
      |> ignore<SqliteParameter>
      command.Parameters.AddWithValue(
        "@definition",
        BinarySerialization.PackageFn.serialize fn
      )
      |> ignore<SqliteParameter>

      do! command.ExecuteNonQueryAsync() |> Async.AwaitTask |> Async.Ignore
    })


let purge () : Task<unit> =
  task {
    use connection = new SqliteConnection(connString)
    do! connection.OpenAsync()

    let queries =
      [ "DELETE FROM package_types_v0"
        "DELETE FROM package_constants_v0"
        "DELETE FROM package_functions_v0" ]

    for q in queries do
      use command = connection.CreateCommand()
      command.CommandText <- q
      do! command.ExecuteNonQueryAsync() |> Async.AwaitTask |> Async.Ignore
  }

// ------------------
// Fetching
// ------------------

let findFn (name : PT.PackageFn.Name) : Ply<Option<PT.FQFnName.Package>> =
  uply {
    use connection = new SqliteConnection(connString)
    let! _ = connection.OpenAsync() |> Async.AwaitTask

    let query =
      """
      SELECT id
      FROM package_functions_v0
      WHERE owner = @owner
        AND modules = @modules
        AND name = @name
    """

    use command = connection.CreateCommand()
    command.CommandText <- query
    command.Parameters.AddWithValue("@owner", name.owner) |> ignore<SqliteParameter>
    command.Parameters.AddWithValue("@modules", String.concat "." name.modules)
    |> ignore<SqliteParameter>
    command.Parameters.AddWithValue("@name", name.name) |> ignore<SqliteParameter>

    use! reader = command.ExecuteReaderAsync() |> Async.AwaitTask
    let! hasRows = reader.ReadAsync() |> Async.AwaitTask

    if hasRows then
      let id = reader.GetString(0) |> System.Guid.Parse
      return Some(id)
    else
      return None
  }

let getFn (id : uuid) : Ply<Option<PT.PackageFn.PackageFn>> =
  uply {
    use connection = new SqliteConnection(connString)
    let! _ = connection.OpenAsync() |> Async.AwaitTask

    let query =
      """
      SELECT definition
      FROM package_functions_v0
      WHERE id = @id
    """

    use command = connection.CreateCommand()
    command.CommandText <- query
    command.Parameters.AddWithValue("@id", id.ToString()) |> ignore<SqliteParameter>

    use! reader = command.ExecuteReaderAsync() |> Async.AwaitTask
    let! hasRows = reader.ReadAsync() |> Async.AwaitTask

    if hasRows then
      let definition = reader.GetValue(0) :?> byte[]
      return Some(BinarySerialization.PackageFn.deserialize id definition)
    else
      return None
  }

let getAllFnNames () : Ply<List<string>> =
  uply {
    use connection = new SqliteConnection(connString)
    let! _ = connection.OpenAsync() |> Async.AwaitTask

    let query =
      """
      SELECT modules, name
      FROM package_functions_v0
      """

    use command = connection.CreateCommand()
    command.CommandText <- query

    use! reader = command.ExecuteReaderAsync() |> Async.AwaitTask

    let mutable results = []
    while! reader.ReadAsync() |> Async.AwaitTask do
      let modules = reader.GetString(0)
      let name = reader.GetString(1)
      results <- (modules + "." + name) :: results

    return List.rev results
  }


let findType
  (name : PT.PackageType.Name)
  : Ply<Option<PT.FQTypeName.Package>> =
  uply {
    use connection = new SqliteConnection(connString)
    let! _ = connection.OpenAsync() |> Async.AwaitTask

    let query =
      """
      SELECT id
      FROM package_types_v0
      WHERE owner = @owner
        AND modules = @modules
        AND name = @name
    """

    use command = connection.CreateCommand()
    command.CommandText <- query
    command.Parameters.AddWithValue("@owner", name.owner) |> ignore<SqliteParameter>
    command.Parameters.AddWithValue("@modules", String.concat "." name.modules)
    |> ignore<SqliteParameter>
    command.Parameters.AddWithValue("@name", name.name) |> ignore<SqliteParameter>

    use! reader = command.ExecuteReaderAsync() |> Async.AwaitTask
    let! hasRows = reader.ReadAsync() |> Async.AwaitTask

    if hasRows then
      let id = reader.GetString(0) |> System.Guid.Parse
      return Some(id)
    else
      return None
  }

let getType (id : uuid) : Ply<Option<PT.PackageType.PackageType>> =
  uply {
    use connection = new SqliteConnection(connString)
    let! _ = connection.OpenAsync() |> Async.AwaitTask

    let query =
      """
      SELECT definition
      FROM package_types_v0
      WHERE id = @id
    """

    use command = connection.CreateCommand()
    command.CommandText <- query
    command.Parameters.AddWithValue("@id", id.ToString()) |> ignore<SqliteParameter>

    use! reader = command.ExecuteReaderAsync() |> Async.AwaitTask
    let! hasRows = reader.ReadAsync() |> Async.AwaitTask

    if hasRows then
      let definition = reader.GetValue(0) :?> byte[]
      return Some(BinarySerialization.PackageType.deserialize id definition)
    else
      return None
  }


let findConstant
  (name : PT.PackageConstant.Name)
  : Ply<Option<PT.FQConstantName.Package>> =
  uply {
    use connection = new SqliteConnection(connString)
    let! _ = connection.OpenAsync() |> Async.AwaitTask

    let query =
      """
      SELECT id
      FROM package_constants_v0
      WHERE owner = @owner
        AND modules = @modules
        AND name = @name
    """

    use command = connection.CreateCommand()
    command.CommandText <- query
    command.Parameters.AddWithValue("@owner", name.owner) |> ignore<SqliteParameter>
    command.Parameters.AddWithValue("@modules", String.concat "." name.modules)
    |> ignore<SqliteParameter>
    command.Parameters.AddWithValue("@name", name.name) |> ignore<SqliteParameter>

    use! reader = command.ExecuteReaderAsync() |> Async.AwaitTask
    let! hasRows = reader.ReadAsync() |> Async.AwaitTask

    if hasRows then
      let id = reader.GetString(0) |> System.Guid.Parse
      return Some(id)
    else
      return None
  }

let getConstant
  (id : uuid)
  : Ply<Option<PT.PackageConstant.PackageConstant>> =
  uply {
    use connection = new SqliteConnection(connString)
    let! _ = connection.OpenAsync() |> Async.AwaitTask

    let query =
      """
      SELECT definition
      FROM package_constants_v0
      WHERE id = @id
    """

    use command = connection.CreateCommand()
    command.CommandText <- query
    command.Parameters.AddWithValue("@id", id.ToString()) |> ignore<SqliteParameter>

    use! reader = command.ExecuteReaderAsync() |> Async.AwaitTask
    let! hasRows = reader.ReadAsync() |> Async.AwaitTask

    if hasRows then
      let definition = reader.GetValue(0) :?> byte[]
      return Some(BinarySerialization.PackageConstant.deserialize id definition)
    else
      return None
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

    getAllFnNames = getAllFnNames

    init = uply { return () } }
