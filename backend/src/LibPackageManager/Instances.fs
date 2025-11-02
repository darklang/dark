/// Manages Darklang instances for package synchronization.
module LibPackageManager.Instances


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


type Instance = { id : System.Guid; name : string; url : string }


/// Get an instance by ID
/// Returns None if not found
let getById (instanceId : System.Guid) : Ply<Option<Instance>> =
  uply {
    let! results =
      Sql.query
        """
        SELECT id, name, url
        FROM instances
        WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid instanceId ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"; name = read.string "name"; url = read.string "url" })

    return List.tryHead results
  }


/// Get an instance by name
/// Returns None if not found
let getByName (name : string) : Ply<Option<Instance>> =
  uply {
    let! results =
      Sql.query
        """
        SELECT id, name, url
        FROM instances
        WHERE name = @name
        """
      |> Sql.parameters [ "name", Sql.string name ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"; name = read.string "name"; url = read.string "url" })

    return List.tryHead results
  }


/// List all instances
let list () : Ply<List<Instance>> =
  uply {
    let! results =
      Sql.query
        """
        SELECT id, name, url
        FROM instances
        ORDER BY name ASC
        """
      |> Sql.parameters []
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"; name = read.string "name"; url = read.string "url" })

    return results
  }


/// Add a new instance
let add (name : string) (url : string) : Ply<Instance> =
  uply {
    let instanceId = System.Guid.NewGuid()

    do!
      Sql.query
        """
        INSERT INTO instances (id, name, url)
        VALUES (@id, @name, @url)
        """
      |> Sql.parameters
        [ "id", Sql.uuid instanceId; "name", Sql.string name; "url", Sql.string url ]
      |> Sql.executeStatementAsync

    return { id = instanceId; name = name; url = url }
  }


/// Remove an instance by ID
let remove (instanceId : System.Guid) : Task<unit> =
  task {
    do!
      Sql.query
        """
        DELETE FROM instances
        WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid instanceId ]
      |> Sql.executeStatementAsync
  }
