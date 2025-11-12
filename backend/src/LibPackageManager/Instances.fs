/// Manages what Darklang instances _this_ instance knows about
/// and is ready to sync with.
module LibPackageManager.Instances


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


type Instance = { id : System.Guid; name : string; url : string }


let getByID (instanceID : System.Guid) : Task<Option<Instance>> =
  """
  SELECT id, name, url
  FROM instances
  WHERE id = @id
  """
  |> Sql.query
  |> Sql.parameters [ "id", Sql.uuid instanceID ]
  |> Sql.executeRowOptionAsync (fun read ->
    { id = read.uuid "id"; name = read.string "name"; url = read.string "url" })


let getByName (name : string) : Task<Option<Instance>> =
  Sql.query
    """
    SELECT id, name, url
    FROM instances
    WHERE name = @name
    """
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read ->
    { id = read.uuid "id"; name = read.string "name"; url = read.string "url" })


let list () : Task<List<Instance>> =
  """
    SELECT id, name, url
    FROM instances
    ORDER BY name ASC
    """
  |> Sql.query
  |> Sql.parameters []
  |> Sql.executeAsync (fun read ->
    { id = read.uuid "id"; name = read.string "name"; url = read.string "url" })


let add (name : string) (url : string) : Ply<Instance> =
  uply {
    let instanceID = System.Guid.NewGuid()

    do!
      Sql.query
        """
        INSERT INTO instances (id, name, url)
        VALUES (@id, @name, @url)
        """
      |> Sql.parameters
        [ "id", Sql.uuid instanceID; "name", Sql.string name; "url", Sql.string url ]
      |> Sql.executeStatementAsync

    return { id = instanceID; name = name; url = url }
  }


let remove (id : System.Guid) : Task<unit> =
  Sql.query
    """
    DELETE FROM instances
    WHERE id = @id
    """
  |> Sql.parameters [ "id", Sql.uuid id ]
  |> Sql.executeStatementAsync
