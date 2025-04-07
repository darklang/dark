module LibPackageManager.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude
open Microsoft.Data.Sqlite

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization

open LibPackageManager.Types


// we should instead do things via LibSqlite or equivalent in LibCloud
let connString = "Data Source=./rundir/data.db"

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
      SELECT owner, modules, name
      FROM package_functions_v0
      """

    use command = connection.CreateCommand()
    command.CommandText <- query

    use! reader = command.ExecuteReaderAsync() |> Async.AwaitTask

    let mutable results = []
    while! reader.ReadAsync() |> Async.AwaitTask do
      let owner = reader.GetString(0)
      let modules = reader.GetString(1)
      let name = reader.GetString(2)
      results <- (owner + "." + modules + "." + name) :: results

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


let rt : RT.PackageManager =
  { getType =
      fun id ->
        uply {
          let! typ = getType id
          return typ |> Option.map PT2RT.PackageType.toRT
        }
    getFn =
      fun id ->
        uply {
          let! fn = getFn id
          return fn |> Option.map PT2RT.PackageFn.toRT
        }
    getConstant =
      fun id ->
        uply {
          let! c = getConstant id
          return c |> Option.map PT2RT.PackageConstant.toRT
        }

    init = uply { return () } }


let pt : PT.PackageManager =
  { findType =  findType
    findConstant =  findConstant
    findFn = findFn

    getType =  getType
    getFn =  getFn
    getConstant =  getConstant

    getAllFnNames = getAllFnNames

    init = uply { return () } }




// type RTCacheType =
//   | RTFunction of RT.PackageFn.PackageFn
//   | RTType of RT.PackageType.PackageType
//   | RTConstant of RT.PackageConstant.PackageConstant

// type PTCacheType =
//   | PTFunction of PT.PackageFn.PackageFn
//   | PTType of PT.PackageType.PackageType
//   | PTConstant of PT.PackageConstant.PackageConstant

// // module PackageCache =
// //   let rtCache = ConcurrentDictionary<uuid, RTCacheType>()
// //   let ptCache = ConcurrentDictionary<uuid, PTCacheType>()

// //   let getAllDefinitions (kind : string) : Task<List<uuid * byte[]>> =
// //     task {
// //       try
// //         use connection = new SqliteConnection(connString)
// //         do! connection.OpenAsync()

// //         use command = connection.CreateCommand()
// //         command.CommandText <- $"SELECT id, definition FROM {kind}"

// //         use! reader = command.ExecuteReaderAsync()
// //         let mutable results = []

// //         while! reader.ReadAsync() do
// //           let id = reader.GetGuid(0)
// //           let bytes = reader.GetFieldValue<byte[]>(1)
// //           results <- (id, bytes) :: results

// //         return results
// //       with ex ->
// //         debuG $"Database error querying {kind}" ex
// //         return []
// //     }

// //   let getDefinitionFromDB (kind : string) (id : uuid) : Task<Option<byte[]>> =
// //     task {
// //       try
// //         use connection = new SqliteConnection(connString)
// //         do! connection.OpenAsync()
// //         use command = connection.CreateCommand()

// //         let query = $"SELECT definition FROM {kind} WHERE id = '{id}'"
// //         command.CommandText <- query

// //         use! reader = command.ExecuteReaderAsync()
// //         let! hasRow = reader.ReadAsync()

// //         if hasRow then
// //           let bytes = reader.GetFieldValue<byte[]>(0)
// //           return Some bytes
// //         else
// //           debuG $"No data found for {id}" ()
// //           return None
// //       with ex ->
// //         debuG $"Database error querying {id}" ex
// //         return None
// //     }

// //   let prefillCache () =
// //     uply {
// //       let! fns = getAllDefinitions "package_functions_v0"
// //       let! types = getAllDefinitions "package_types_v0"
// //       let! constants = getAllDefinitions "package_constants_v0"

// //       for (id, bytes) in types do
// //         try
// //           let t = BinarySerialization.PackageType.deserialize id bytes
// //           let rt = t |> PT2RT.PackageType.toRT
// //           rtCache.TryAdd(id, RTType rt) |> ignore<bool>
// //           ptCache.TryAdd(id, PTType t) |> ignore<bool>
// //         with ex ->
// //           debuG $"Failed to deserialize type {id}" ex

// //       for (id, bytes) in fns do
// //         try
// //           let fn = BinarySerialization.PackageFn.deserialize id bytes
// //           let rt = fn |> PT2RT.PackageFn.toRT
// //           rtCache.TryAdd(id, RTFunction rt) |> ignore<bool>
// //           ptCache.TryAdd(id, PTFunction fn) |> ignore<bool>
// //         with ex ->
// //           debuG $"Failed to deserialize function {id}" ex

// //       for (id, bytes) in constants do
// //         try
// //           let c = BinarySerialization.PackageConstant.deserialize id bytes
// //           let rt = c |> PT2RT.PackageConstant.toRT
// //           rtCache.TryAdd(id, RTConstant rt) |> ignore<bool>
// //           ptCache.TryAdd(id, PTConstant c) |> ignore<bool>
// //         with ex ->
// //           debuG $"Failed to deserialize constant {id}" ex

// //       return ()
// //     }


// //   let getRTType (id : uuid) : Ply<Option<RT.PackageType.PackageType>> =
// //     uply {
// //       match rtCache.TryGetValue(id) with
// //       | true, RTType t -> return Some t
// //       | _ -> return None
// //     }

// //   let getRTFn (id : uuid) : Ply<Option<RT.PackageFn.PackageFn>> =
// //     uply {
// //       match rtCache.TryGetValue(id) with
// //       | true, RTFunction f -> return Some f
// //       | _ ->
// //         debuG $"Function {id} not found in cache" ()
// //         return None
// //     }

// //   let getRTConstant (id : uuid) : Ply<Option<RT.PackageConstant.PackageConstant>> =
// //     uply {
// //       match rtCache.TryGetValue(id) with
// //       | true, RTConstant c -> return Some c
// //       | _ -> return None
// //     }

// //   let getPTType (id : uuid) : Ply<Option<PT.PackageType.PackageType>> =
// //     uply {
// //       match ptCache.TryGetValue(id) with
// //       | true, PTType t -> return Some t
// //       | _ -> return None
// //     }

// //   let getPTFn (id : uuid) : Ply<Option<PT.PackageFn.PackageFn>> =
// //     uply {
// //       match ptCache.TryGetValue(id) with
// //       | true, PTFunction f -> return Some f
// //       | _ -> return None
// //     }

// //   let getPTConstant (id : uuid) : Ply<Option<PT.PackageConstant.PackageConstant>> =
// //     uply {
// //       match ptCache.TryGetValue(id) with
// //       | true, PTConstant c -> return Some c
// //       | _ -> return None
// //     }


// let findByName
//   (kind : string)
//   (owner : string)
//   (modules : List<string>)
//   (name : string)
//   : Ply<Option<uuid>> =
//   uply {
//     use connection = new SqliteConnection(connString)
//     do! connection.OpenAsync()
//     use command = connection.CreateCommand()

//     // TODO: update to use Fumble?

//     command.CommandText <-
//       $"SELECT id FROM {kind}
//         WHERE name = @name
//           AND owner = @owner
//           AND modules = @modules"

//     command.Parameters.AddWithValue("@name", name) |> ignore<SqliteParameter>
//     command.Parameters.AddWithValue("@owner", owner) |> ignore<SqliteParameter>
//     let modulesStr = String.concat "." modules
//     command.Parameters.AddWithValue("@modules", modulesStr)
//     |> ignore<SqliteParameter>

//     use! reader = command.ExecuteReaderAsync()
//     let! hasRow = reader.ReadAsync()

//     if hasRow then
//       let id = reader.GetGuid(0)
//       return Some id
//     else
//       return None
//   }

// let rt (_baseUrl : string) : RT.PackageManager =
//   { getType =
//     getFn =
//     getConstant =
//     init = uply { } }

// let pt (_baseUrl : string) : PT.PackageManager =
//   { findType =
//       fun (name : PT.PackageType.Name) ->
//         findByName
//           "package_types_v0"
//           name.owner
//           name.modules
//           name.name

//     findConstant =
//       fun (name : PT.PackageConstant.Name) ->
//         findByName
//           "package_constants_v0"
//           name.owner
//           name.modules
//           name.name

//     findFn =
//       fun (name : PT.PackageFn.Name) ->
//         findByName
//           "package_functions_v0"
//           name.owner
//           name.modules
//           name.name

//     getType =
//     getFn =
//     getConstant =

//     getAllFnNames =
//       fun () ->
//         uply {
//           use connection = new SqliteConnection(connString)
//           do! connection.OpenAsync()
//           use command = connection.CreateCommand()
//           command.CommandText <- "SELECT id FROM package_functions_v0"

//           use! reader = command.ExecuteReaderAsync()
//           let mutable results = []
//           while! reader.ReadAsync() do
//             results <- reader.GetString(0) :: results

//           return results
//         }

//     init = uply { return () } }
