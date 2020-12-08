module LibBackend.Canvas

// Functions related to Canvases

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp
open Npgsql
open LibBackend.Db
open FSharp.Data
open System.Text.RegularExpressions

open Prelude
open LibExecution.SharedTypes
open LibExecution.RuntimeTypes

let fetchReleventTLIDsForHTTP (host : string)
                              (canvasID : CanvasID)
                              (path : string)
                              (method : string)
                              : Task<List<tlid>> =

  // The pattern `$2 like name` is deliberate, to leverage the DB's
  // pattern matching to solve our routing.
  Sql.query "SELECT tlid
             FROM toplevel_oplists
             WHERE canvas_id = @canvasID
               AND ((module = 'HTTP'
                     AND @path like name
                     AND modifier = @method)
               OR tipe <> 'handler'::toplevel_type)"
  |> Sql.parameters [ "path", Sql.string path
                      "method", Sql.string method
                      "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.int64 "tlid")

let canvasIDForCanvas (owner : UserID) (canvasName : string) : Task<CanvasID> =
  printfn $"calling canvasIDForCanvas {owner} {canvasName}"
  if canvasName.Length > 64 then
    failwith $"Canvas name was length {canvasName.Length}, must be <= 64"
  else
    // TODO: we create the canvas if it doesn't exist here, seems like a poor choice
    Sql.query "SELECT canvas_id(@newUUID, @owner, @canvasName)"
    |> Sql.parameters [ "newUUID", Sql.uuid (System.Guid.NewGuid())
                        "owner", Sql.uuid owner
                        "canvasName", Sql.string canvasName ]
    |> Sql.executeRowAsync (fun read -> read.uuid "canvas_id")
    |> debugTask "canvasID"


// split into owner and canvasName
let ownerNameFromHost (host : string) : string =
  match host.Split [| '-' |] |> Seq.toList with
  | owner :: _rest -> owner
  | _ -> host

let canvasNameFromCustomDomain host : Task<Option<string>> =
  Sql.query "SELECT canvas
             FROM custom_domains
             WHERE host = @host"
  |> Sql.parameters [ "host", Sql.string host ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "canvas")
