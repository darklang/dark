module LibBackend.Secret

// Secrets

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp.Tasks
open Npgsql
open Db

open Prelude
open Prelude.TableCloth
open Tablecloth

type Secret = { name : string; value : string }

let getCanvasSecrets (canvasID : CanvasID) : Task<List<Secret>> =
  Sql.query
    "SELECT secret_name, secret_value
     FROM secrets
     WHERE canvas_id = @canvasID
     ORDER BY created_at DESC"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync
       (fun read ->
         { name = read.string "secret_name"; value = read.string "secret_value" })

// let insert (canvasID : CanvasID) (name : string) (value : string) : unit
//     =
//   Db.run
//     "INSERT INTO secrets
//     (canvas_id, secret_name, secret_value)
//     VALUES ($1, $2, $3)"
//     ~params:[Uuid canvas_id; String name; String value]
//     ~name:"insert secret for canvas"
//
