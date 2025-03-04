/// Fetches package items from a web-based package manager, for CLI runtimes.
///
/// TODO: this currently assumes that the package items match the shape
/// of Dark types defined in @Darklang.LanguageTools.ProgramTypes
///
/// TODO: move this to something more CLI-focused,
/// and review if current usages are appropriate.
///
/// TODO: address: how can we best react to the shape of types at the corresponding endpoints changing?
///
/// At this point, we only expose the PT package manager over HTTP,
/// so the RT PM here just PT2RTs our way to success.
/// Longer-term, we'll have a Sqlite cache of packages here, and a generally
/// fancier setup, probably with lookups for both PT and RT stuff.
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

type RTCacheType =
  | RTFunction of RT.PackageFn.PackageFn
  | RTType of RT.PackageType.PackageType
  | RTConstant of RT.PackageConstant.PackageConstant

type PTCacheType =
  | PTFunction of PT.PackageFn.PackageFn
  | PTType of PT.PackageType.PackageType
  | PTConstant of PT.PackageConstant.PackageConstant

module PackageCache =
  let rtCache = ConcurrentDictionary<uuid, RTCacheType>()
  let ptCache = ConcurrentDictionary<uuid, PTCacheType>()

  let getAllDefinitions (kind : string) : Task<List<uuid * byte[]>> =
    task {
      try
        use connection = new SqliteConnection("Data Source=sqlite-db/sqlitedb.db")
        do! connection.OpenAsync()

        use command = connection.CreateCommand()
        command.CommandText <- $"SELECT id, definition FROM {kind}"

        use! reader = command.ExecuteReaderAsync()
        let mutable results = []

        while! reader.ReadAsync() do
          let id = reader.GetGuid(0)
          let bytes = reader.GetFieldValue<byte[]>(1)
          results <- (id, bytes) :: results

        return results
      with ex ->
        debuG $"Database error querying {kind}" ex
        return []
    }

  let getDefinitionFromDB (kind : string) (id : uuid) : Task<Option<byte[]>> =
    task {
      try
        use connection = new SqliteConnection("Data Source=sqlite-db/sqlitedb.db")
        do! connection.OpenAsync()
        use command = connection.CreateCommand()

        let query = $"SELECT definition FROM {kind} WHERE id = '{id}'"
        command.CommandText <- query

        use! reader = command.ExecuteReaderAsync()
        let! hasRow = reader.ReadAsync()

        if hasRow then
          let bytes = reader.GetFieldValue<byte[]>(0)
          return Some bytes
        else
          debuG $"No data found for {id}" ()
          return None
      with ex ->
        debuG $"Database error querying {id}" ex
        return None
    }

  let prefillCache () =
    uply {
      let! fns = getAllDefinitions "package_functions_v0"
      let! types = getAllDefinitions "package_types_v0"
      let! constants = getAllDefinitions "package_constants_v0"

      for (id, bytes) in types do
        try
          let t = BinarySerialization.PackageType.deserialize id bytes
          let rt = t |> PT2RT.PackageType.toRT
          rtCache.TryAdd(id, RTType rt) |> ignore<bool>
          ptCache.TryAdd(id, PTType t) |> ignore<bool>
        with ex ->
          debuG $"Failed to deserialize type {id}" ex

      for (id, bytes) in fns do
        try
          let fn = BinarySerialization.PackageFn.deserialize id bytes
          let rt = fn |> PT2RT.PackageFn.toRT
          rtCache.TryAdd(id, RTFunction rt) |> ignore<bool>
          ptCache.TryAdd(id, PTFunction fn) |> ignore<bool>
        with ex ->
          debuG $"Failed to deserialize function {id}" ex

      for (id, bytes) in constants do
        try
          let c = BinarySerialization.PackageConstant.deserialize id bytes
          let rt = c |> PT2RT.PackageConstant.toRT
          rtCache.TryAdd(id, RTConstant rt) |> ignore<bool>
          ptCache.TryAdd(id, PTConstant c) |> ignore<bool>
        with ex ->
          debuG $"Failed to deserialize constant {id}" ex

      return ()
    }


  let getRTType (id : uuid) : Ply<Option<RT.PackageType.PackageType>> =
    uply {
      match rtCache.TryGetValue(id) with
      | true, RTType t -> return Some t
      | _ -> return None
    }

  let getRTFn (id : uuid) : Ply<Option<RT.PackageFn.PackageFn>> =
    uply {
      match rtCache.TryGetValue(id) with
      | true, RTFunction f -> return Some f
      | _ ->
        debuG $"Function {id} not found in cache" ()
        return None
    }

  let getRTConstant (id : uuid) : Ply<Option<RT.PackageConstant.PackageConstant>> =
    uply {
      match rtCache.TryGetValue(id) with
      | true, RTConstant c -> return Some c
      | _ -> return None
    }

  let getPTType (id : uuid) : Ply<Option<PT.PackageType.PackageType>> =
    uply {
      match ptCache.TryGetValue(id) with
      | true, PTType t -> return Some t
      | _ -> return None
    }

  let getPTFn (id : uuid) : Ply<Option<PT.PackageFn.PackageFn>> =
    uply {
      match ptCache.TryGetValue(id) with
      | true, PTFunction f -> return Some f
      | _ -> return None
    }

  let getPTConstant (id : uuid) : Ply<Option<PT.PackageConstant.PackageConstant>> =
    uply {
      match ptCache.TryGetValue(id) with
      | true, PTConstant c -> return Some c
      | _ -> return None
    }

let findByName
  (dbPath : string)
  (kind : string)
  (owner : string)
  (modules : List<string>)
  (name : string)
  : Ply<Option<uuid>> =
  uply {
    use connection = new SqliteConnection($"Data Source={dbPath}")
    do! connection.OpenAsync()
    use command = connection.CreateCommand()

    let modulesStr = String.concat "." modules

    command.CommandText <-
      $"SELECT id FROM {kind} WHERE name = @name AND owner = @owner AND modules = @modules"
    command.Parameters.AddWithValue("@name", name) |> ignore<SqliteParameter>
    command.Parameters.AddWithValue("@owner", owner) |> ignore<SqliteParameter>
    command.Parameters.AddWithValue("@modules", modulesStr)
    |> ignore<SqliteParameter>

    use! reader = command.ExecuteReaderAsync()
    let! hasRow = reader.ReadAsync()

    if hasRow then
      let id = reader.GetGuid(0)
      return Some id
    else
      return None
  }

let rt (_baseUrl : string) : RT.PackageManager =
  { getType = PackageCache.getRTType
    getFn = PackageCache.getRTFn
    getConstant = PackageCache.getRTConstant
    init = uply { do! PackageCache.prefillCache () } }

let pt (_baseUrl : string) : PT.PackageManager =
  { findType =
      (fun (name : PT.PackageType.Name) ->
        findByName
          "sqlite-db/sqlitedb.db"
          "package_types_v0"
          name.owner
          name.modules
          name.name)

    findConstant =
      (fun (name : PT.PackageConstant.Name) ->
        findByName
          "sqlite-db/sqlitedb.db"
          "package_constants_v0"
          name.owner
          name.modules
          name.name)

    findFn =
      (fun (name : PT.PackageFn.Name) ->
        findByName
          "sqlite-db/sqlitedb.db"
          "package_functions_v0"
          name.owner
          name.modules
          name.name)

    getType = PackageCache.getPTType
    getFn = PackageCache.getPTFn
    getConstant = PackageCache.getPTConstant

    getAllFnNames =
      (fun () ->
        uply {
          let dbPath = "sqlite-db/sqlitedb.db"
          use connection = new SqliteConnection($"Data Source={dbPath}")
          do! connection.OpenAsync()
          use command = connection.CreateCommand()
          command.CommandText <- "SELECT id FROM package_functions_v0"

          use! reader = command.ExecuteReaderAsync()
          let mutable results = []
          while! reader.ReadAsync() do
            results <- reader.GetString(0) :: results

          return results
        })

    init = uply { return () } }
