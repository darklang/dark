/// Supports the storage (CRUD) of canvas-level user secrets
module LibBackend.Secret

// Secrets

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

open Prelude
open Prelude.Tablecloth
open Tablecloth

module PT = LibExecution.ProgramTypes

let getCanvasSecrets (canvasID : CanvasID) : Task<List<PT.Secret.T>> =
  Sql.query
    "SELECT secret_name, secret_value
     FROM secrets_v0
     WHERE canvas_id = @canvasID
     ORDER BY created_at DESC"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read ->
    { name = read.string "secret_name"; value = read.string "secret_value" })

let insert (canvasID : CanvasID) (name : string) (value : string) : Task<unit> =
  Sql.query
    "INSERT INTO secrets_v0
    (canvas_id, secret_name, secret_value)
    VALUES (@canvasID, @secretName, @secretValue)"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "secretName", Sql.string name
                      "secretValue", Sql.string value ]
  |> Sql.executeStatementAsync

let delete (canvasID : CanvasID) (name : string) : Task<unit> =
  Sql.query
    "DELETE FROM secrets_v0
      WHERE canvas_id = @canvasID
        AND secret_name = @secretName"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "secretName", Sql.string name ]
  |> Sql.executeStatementAsync
