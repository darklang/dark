module LibBackend.Serialization

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp
open Ply
open Npgsql

open LibExecution.Runtime
open LibExecution.Framework

let gid = Shortcuts.gid

[<DllImport("../_build/default/backend/libbackend/libbackend.a",
            CallingConvention = CallingConvention.Cdecl)>]
extern string camlLibbackend__Serialize__oplist_of_binary_string_4913()

let defaultConnection =
  let cs =
    Sql.host "localhost"
    |> Sql.port 5432
    |> Sql.username "dark"
    |> Sql.password "eapnsdc"
    |> Sql.database "devdb"
    // |> Sql.sslMode SslMode.Require
    |> Sql.config "Pooling=true"
    |> Sql.formatConnectionString

  let conn = new NpgsqlConnection(cs)
  conn.Open()
  conn

module Sql =
  let throwOrReturn (result : Async<Result<'a, exn>>) =
    task {
      let! result = result |> Async.StartImmediateAsTask

      match result with
      | Ok result -> return result
      | Error exn -> return raise exn
    }

let fetchReleventTLIDsForHTTP (host : string)
                              (canvasID : CanvasID)
                              (path : string)
                              (method : string)
                              : Task<List<tlid>> =

  Sql.existingConnection defaultConnection
  // The pattern `$2 like name` is deliberate, to leverage the DB's
  // pattern matching to solve our routing.
  |> Sql.query "SELECT tlid
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
  |> Sql.throwOrReturn

let canvasIDForCanvas (canvasName : string) : CanvasID = System.Guid.NewGuid()
let userIDForUsername (user : string) : UserID = System.Guid.NewGuid()

let loadCachedToplevels (host : string)
                        (owner : UserID)
                        (tlids : List<tlid>)
                        : Result<List<byte array>, List<string>> =
  Error([])

let ocamlRenderedToJson (data : byte array) : string = ""

let parseOCamlOplistJSON (json : string) : Option<Toplevel> = None

let loadHttpHandlersFromCache (host : string)
                              (canvasID : CanvasID)
                              (owner : UserID)
                              (path : string)
                              (method : string)
                              : Task<List<Toplevel>> =
  task {
    let! tlids = fetchReleventTLIDsForHTTP host canvasID path method

    let binaryTLs = loadCachedToplevels host canvasID tlids |> Result.defaultValue []

    let jsonTLs = List.map ocamlRenderedToJson binaryTLs

    let tls = List.choose parseOCamlOplistJSON jsonTLs

    return tls
  }




// FSTODO: if we need speed, switch to transferring using protobufs instead of JSON
// where do we get the protobuf files?
// - 1) generate using ocaml types (maybe deprecated? ocaml-protoc)
// - 2) generate using FSharp types (possible?)
// - 3) hand-write them (converters on both ends)
