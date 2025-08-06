module LibPackageManager.Scripts

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

open Prelude

module RT = LibExecution.RuntimeTypes

type Script = { id : System.Guid; name : string; text : string }

/// List all scripts
let list () : Task<List<Script>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name, text
        FROM scripts_v0
        ORDER BY name
        """
      |> Sql.executeAsync (fun read ->
        { id = System.Guid.Parse(read.string "id")
          name = read.string "name"
          text = read.string "text" })
  }

/// Get a script by name
let get (name : string) : Task<Option<Script>> =
  task {
    let! result =
      Sql.query
        """
        SELECT id, name, text
        FROM scripts_v0
        WHERE name = @name
        """
      |> Sql.parameters [ "name", Sql.string name ]
      |> Sql.executeRowOptionAsync (fun read ->
        { id = System.Guid.Parse(read.string "id")
          name = read.string "name"
          text = read.string "text" })

    return result
  }


/// Add a new script
let add (name : string) (text : string) : Task<Result<Script, string>> =
  task {
    let id = System.Guid.NewGuid()
    try
      let! _ =
        Sql.query
          """
          INSERT INTO scripts_v0 (id, name, text)
          VALUES (@id, @name, @text)
          """
        |> Sql.parameters
          [ "id", Sql.string (string id)
            "name", Sql.string name
            "text", Sql.string text ]
        |> Sql.executeNonQueryAsync

      return Ok { id = id; name = name; text = text }
    with
    | :? SqliteException as e when e.Message.Contains("UNIQUE constraint failed") ->
      // Unique constraint violation
      return Error $"Script with name '{name}' already exists"
    | :? System.AggregateException as ae ->
      // Unwrap AggregateException to get the real exception
      let innerException = ae.InnerExceptions |> Seq.head
      match innerException with
      | :? SqliteException as e when e.Message.Contains("UNIQUE constraint failed") ->
        return Error $"Script with name '{name}' already exists"
      | _ -> return Error $"Failed to add script: {innerException.Message}"
    | e -> return Error $"Failed to add script: {e.Message}"
  }

/// Update an existing script's text
let update (name : string) (text : string) : Task<Result<unit, string>> =
  task {
    try
      let! rowsAffected =
        Sql.query
          """
          UPDATE scripts_v0
          SET text = @text
          WHERE name = @name
          """
        |> Sql.parameters [ "text", Sql.string text; "name", Sql.string name ]
        |> Sql.executeNonQueryAsync

      if rowsAffected = 0 then
        return Error $"Script '{name}' not found"
      else
        return Ok()
    with e ->
      return Error $"Failed to update script: {e.Message}"
  }

/// Delete a script by name
let delete (name : string) : Task<Result<unit, string>> =
  task {
    try
      let! rowsAffected =
        Sql.query
          """
          DELETE FROM scripts_v0
          WHERE name = @name
          """
        |> Sql.parameters [ "name", Sql.string name ]
        |> Sql.executeNonQueryAsync

      if rowsAffected = 0 then
        return Error $"Script '{name}' not found"
      else
        return Ok()
    with e ->
      return Error $"Failed to delete script: {e.Message}"
  }
