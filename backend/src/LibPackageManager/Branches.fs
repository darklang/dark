module LibPackageManager.Branches

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


type BranchState =
  | Active
  | Merged
  | Abandoned

module BranchState =
  let toString (state : BranchState) : string =
    match state with
    | Active -> "active"
    | Merged -> "merged"
    | Abandoned -> "abandoned"

  let fromString (s : string) : BranchState =
    match s with
    | "active" -> Active
    | "merged" -> Merged
    | "abandoned" -> Abandoned
    | _ -> Exception.raiseInternal $"Invalid branch state: {s}" []


type Branch =
  { id : uuid
    createdBy : Option<uuid>
    title : string
    state : BranchState
    createdAt : NodaTime.Instant
    lastActiveAt : NodaTime.Instant
    mergedAt : Option<NodaTime.Instant> }


/// List all branches, optionally filtered by state
let list (stateFilter : Option<BranchState>) : Task<List<Branch>> =
  task {
    let whereClause =
      match stateFilter with
      | Some state -> $"WHERE state = '{BranchState.toString state}'"
      | None -> ""

    return!
      $"""
      SELECT id, created_by, title, state, created_at, last_active_at, merged_at
      FROM branches
      {whereClause}
      ORDER BY last_active_at DESC
      """
      |> Sql.query
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          createdBy = read.uuidOrNone "created_by"
          title = read.string "title"
          state = read.string "state" |> BranchState.fromString
          createdAt = read.instant "created_at"
          lastActiveAt = read.instant "last_active_at"
          mergedAt = read.instantOrNone "merged_at" })
  }


/// Get a specific branch by ID
let get (branchId : uuid) : Task<Option<Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, created_by, title, state, created_at, last_active_at, merged_at
        FROM branches
        WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid branchId ]
      |> Sql.executeRowOptionAsync (fun read ->
        { id = read.uuid "id"
          createdBy = read.uuidOrNone "created_by"
          title = read.string "title"
          state = read.string "state" |> BranchState.fromString
          createdAt = read.instant "created_at"
          lastActiveAt = read.instant "last_active_at"
          mergedAt = read.instantOrNone "merged_at" })
  }


/// Find branches by title (may return multiple if names collide)
let findByTitle (title : string) : Task<List<Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, created_by, title, state, created_at, last_active_at, merged_at
        FROM branches
        WHERE title = @title
        ORDER BY created_at DESC
        """
      |> Sql.parameters [ "title", Sql.string title ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          createdBy = read.uuidOrNone "created_by"
          title = read.string "title"
          state = read.string "state" |> BranchState.fromString
          createdAt = read.instant "created_at"
          lastActiveAt = read.instant "last_active_at"
          mergedAt = read.instantOrNone "merged_at" })
  }


/// Create a new branch
let create (createdBy : Option<uuid>) (title : string) : Task<Branch> =
  task {
    let now = NodaTime.Instant.now ()
    let id = System.Guid.NewGuid()

    do!
      Sql.query
        """
        INSERT INTO branches (id, created_by, title, state, created_at, last_active_at, merged_at)
        VALUES (@id, @created_by, @title, @state, @created_at, @last_active_at, NULL)
        """
      |> Sql.parameters
        [ "id", Sql.uuid id
          "created_by",
          (match createdBy with
           | Some accountId -> Sql.uuid accountId
           | None -> Sql.dbnull)
          "title", Sql.string title
          "state", Sql.string (BranchState.toString Active)
          "created_at", Sql.instant now
          "last_active_at", Sql.instant now ]
      |> Sql.executeNonQueryAsync
      |> Task.map (fun _ -> ())

    return
      { id = id
        createdBy = createdBy
        title = title
        state = Active
        createdAt = now
        lastActiveAt = now
        mergedAt = None }
  }


/// Update last_active_at timestamp for a branch
let updateLastActive (branchId : uuid) : Task<unit> =
  task {
    let now = NodaTime.Instant.now ()

    do!
      Sql.query
        """
        UPDATE branches
        SET last_active_at = @last_active_at
        WHERE id = @id
        """
      |> Sql.parameters
        [ "id", Sql.uuid branchId
          "last_active_at", Sql.instant now ]
      |> Sql.executeNonQueryAsync
      |> Task.map (fun _ -> ())

    return ()
  }


/// Ensure a 'main' branch exists, creating it if needed
let ensureMainBranch (createdBy : Option<uuid>) : Task<Branch> =
  task {
    // Try to find existing 'main' branch
    let! existing = findByTitle "main"

    match List.tryHead existing with
    | Some branch -> return branch
    | None ->
      // No main branch exists, create it
      return! create createdBy "main"
  }
