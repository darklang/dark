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
    owner : string
    createdAt : NodaTime.Instant
    mergedAt : Option<NodaTime.Instant> }


/// List branches for a specific owner
let list (owner : string) : Task<List<Branch>> =
  """
  SELECT id, name, owner, created_at, merged_at
  FROM branches
  WHERE owner = @owner
  ORDER BY created_at DESC
  """
  |> Sql.query
  |> Sql.parameters [ "owner", Sql.string owner ]
  |> Sql.executeAsync (fun read ->
    { id = read.uuid "id"
      name = read.string "name"
      owner = read.string "owner"
      createdAt = read.instant "created_at"
      mergedAt = read.instantOrNone "merged_at" })


/// Get a specific branch by ID
let get (branchID : uuid) : Task<Option<Branch>> =
  """
  SELECT id, name, owner, created_at, merged_at
  FROM branches
  WHERE id = @id
  """
  |> Sql.query
  |> Sql.parameters [ "id", Sql.uuid branchID ]
  |> Sql.executeRowOptionAsync (fun read ->
    { id = read.uuid "id"
      name = read.string "name"
      owner = read.string "owner"
      createdAt = read.instant "created_at"
      mergedAt = read.instantOrNone "merged_at" })


/// Find branch by name for a specific owner
let findByName (owner : string) (name : string) : Task<Option<Branch>> =
  """
  SELECT id, name, owner, created_at, merged_at
  FROM branches
  WHERE owner = @owner AND name = @name
  """
  |> Sql.query
  |> Sql.parameters [ "owner", Sql.string owner; "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read ->
    { id = read.uuid "id"
      name = read.string "name"
      owner = read.string "owner"
      createdAt = read.instant "created_at"
      mergedAt = read.instantOrNone "merged_at" })


/// Create a new branch for a specific owner
///
/// CLEANUP consider just returning ID?
let create (owner : string) (name : string) : Task<Branch> =
  task {
    let now = NodaTime.Instant.now ()
    let id = System.Guid.NewGuid()

    do!
      """
        INSERT INTO branches (id, name, owner, created_at, merged_at)
        VALUES (@id, @name, @owner, @created_at, NULL)
        """
      |> Sql.query
      |> Sql.parameters
        [ "id", Sql.uuid id
          "name", Sql.string name
          "owner", Sql.string owner
          "created_at", Sql.instant now ]
      |> Sql.executeNonQueryAsync
      |> Task.map (fun _ -> ())

    return { id = id; name = name; owner = owner; createdAt = now; mergedAt = None }
  }
