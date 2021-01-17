module LibBackend.ProgramSerialization.SQL

// The functions to fetch the Serialized Dark programs from the DB.

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp.Tasks
open Npgsql
open LibBackend.Db
open System.Text.RegularExpressions

open Prelude
open ProgramTypes

module Http = LibExecution.Http

// FSTODO This is for testing only as it blows away the old oplist, which is
// needed for undos.
let saveCachedToplevelForTestingOnly
  (canvasID : CanvasID)
  (ownerID : UserID)
  (tl : Toplevel)
  : Task<unit> =
  let module_, path, modifier =
    match tl with
    | TLDB _
    | TLFunction _
    | TLType _ -> None, None, None
    | TLHandler h ->
        // FSTODO munge path for postgres, see munge_name in canvas.ml
        match h.spec with
        | Handler.HTTP (path, modifier, _) ->
            Some "HTTP", Some(Http.routeToPostgresPattern path), Some modifier
        | Handler.Worker (name, _) -> Some "Worker", Some name, Some "_"
        | Handler.OldWorker (modulename, name, _) ->
            Some modulename, Some name, Some "_"
        | Handler.Cron (name, interval, _) -> Some "CRON", Some name, Some interval
        | Handler.REPL (name, _) -> Some "REPL", Some name, Some "_"

  let sqlType : string =
    match tl with
    | TLDB _ -> "db"
    | TLHandler _ -> "handler"
    | TLFunction _ -> "user_function"
    | TLType _ -> "user_tipe"

  let cacheBinary = OCamlInterop.toplevelToCachedBinary tl // FSTODO pos
  let (oplistBinary : byte array) = [||] // FSTODO get an actual oplist
  let digest = OCamlInterop.Binary.digest ()
  let pos = Some "{ \"x\": 0, \"y\": 0 }"

  Sql.query
    "INSERT INTO toplevel_oplists
       (canvas_id, account_id, tlid, digest, tipe, name, module, modifier,
        data, rendered_oplist_cache, deleted, pos)
     VALUES (@canvasID, @ownerID, @tlid, @digest, @tipe::toplevel_type,
             @path, @module, @modifier, @oplistData, @cacheData, @deleted, @pos)
     ON CONFLICT (canvas_id, tlid) DO UPDATE
     SET account_id = @ownerID,
         digest = @digest,
         tipe = @tipe::toplevel_type,
         name = @path,
         module = @module,
         modifier = @modifier,
         data = @oplistData,
         rendered_oplist_cache = @cacheData,
         deleted = @deleted,
         pos = @pos"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "ownerID", Sql.uuid ownerID
                      "tlid", Sql.id (tl.toTLID ())
                      "digest", Sql.string digest
                      "tipe", Sql.string sqlType
                      "path", Sql.stringOrNone path
                      "module", Sql.stringOrNone module_
                      "modifier", Sql.stringOrNone modifier
                      "oplistData", Sql.bytea oplistBinary
                      "@cacheData", Sql.bytea cacheBinary
                      "deleted", Sql.bool false // FSTODO
                      "pos", Sql.jsonbOrNone pos ] // FSTODO
  |> Sql.executeStatementAsync

let saveHttpHandlersToCache
  (canvasID : CanvasID)
  (ownerID : UserID)
  (tls : List<Toplevel>)
  : Task<unit> =
  task {
    let results =
      tls
      |> List.map
           (fun tl -> (saveCachedToplevelForTestingOnly canvasID ownerID tl) :> Task)
      |> List.toArray

    return Task.WaitAll(results)
  }
