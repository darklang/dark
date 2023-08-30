/// Supports the storage (CRUD) of canvas-level user secrets
module LibCloud.Secret

// Secrets

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

open Prelude

module PT = LibExecution.ProgramTypes

let getCanvasSecrets (canvasID : CanvasID) : Task<List<PT.Secret.T>> =
  Sql.query
    "SELECT name, value, version
     FROM secrets_v0
     WHERE canvas_id = @canvasID
     ORDER BY created_at DESC"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read ->
    { name = read.string "name"
      value = read.string "value"
      version = read.int "version" })

let insert
  (canvasID : CanvasID)
  (name : string)
  (value : string)
  (version : int)
  : Task<unit> =
  Sql.query
    "INSERT INTO secrets_v0
    (canvas_id, name, value, version)
    VALUES (@canvasID, @name, @value, @version)"
  |> Sql.parameters
    [ "canvasID", Sql.uuid canvasID
      "name", Sql.string name
      "value", Sql.string value
      "version", Sql.int version ]
  |> Sql.executeStatementAsync

let delete (canvasID : CanvasID) (name : string) (version : int) : Task<unit> =
  Sql.query
    "DELETE FROM secrets_v0
      WHERE canvas_id = @canvasID
        AND name = @Name
        AND version = @version"
  |> Sql.parameters
    [ "canvasID", Sql.uuid canvasID
      "name", Sql.string name
      "version", Sql.int version ]
  |> Sql.executeStatementAsync
