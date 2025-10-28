module LibPackageManager.Branches

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


type Branch =
  { id : uuid
    name : string
    createdAt : NodaTime.Instant
    mergedAt : Option<NodaTime.Instant> }


/// List all branches
let list () : Task<List<Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name, created_at, merged_at
        FROM branches
        ORDER BY created_at DESC
        """
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          name = read.string "name"
          createdAt = read.instant "created_at"
          mergedAt = read.instantOrNone "merged_at" })
  }


/// Get a specific branch by ID
let get (branchId : uuid) : Task<Option<Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name, created_at, merged_at
        FROM branches
        WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid branchId ]
      |> Sql.executeRowOptionAsync (fun read ->
        { id = read.uuid "id"
          name = read.string "name"
          createdAt = read.instant "created_at"
          mergedAt = read.instantOrNone "merged_at" })
  }


/// Find branches by name (may return multiple if names collide)
let findByName (name : string) : Task<List<Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name, created_at, merged_at
        FROM branches
        WHERE name = @name
        ORDER BY created_at DESC
        """
      |> Sql.parameters [ "name", Sql.string name ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          name = read.string "name"
          createdAt = read.instant "created_at"
          mergedAt = read.instantOrNone "merged_at" })
  }


/// Create a new branch
let create (name : string) : Task<Branch> =
  task {
    let now = NodaTime.Instant.now ()
    let id = System.Guid.NewGuid()

    do!
      Sql.query
        """
        INSERT INTO branches (id, name, created_at, merged_at)
        VALUES (@id, @name, @created_at, NULL)
        """
      |> Sql.parameters
        [ "id", Sql.uuid id
          "name", Sql.string name
          "created_at", Sql.instant now ]
      |> Sql.executeNonQueryAsync
      |> Task.map (fun _ -> ())

    return
      { id = id
        name = name
        createdAt = now
        mergedAt = None }
  }


