module LibCloud.Toplevels

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Sqlite

open Prelude

module BS = LibSerialization.Binary.Serialization
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


/// Container of DBs — single-instance Dark, so global. Handlers are
/// gone (Worker / Cron / REPL had no live consumers; HTTP went earlier
/// with the BwdServer rewrite). What remains is just the user-defined
/// Datastores.
type T = { dbs : Map<tlid, PT.DB.T>; deletedDBs : Map<tlid, PT.DB.T> }


let addDB (deleted : Serialize.Deleted) (db : PT.DB.T) (c : T) : T =
  match deleted with
  | Serialize.NotDeleted -> { c with dbs = Map.add db.tlid db c.dbs }
  | Serialize.Deleted -> { c with deletedDBs = Map.add db.tlid db c.deletedDBs }


let addDBs (tls : List<Serialize.Deleted * PT.DB.T>) (c : T) : T =
  List.fold (fun acc (deleted, db) -> addDB deleted db acc) c tls


// NOTE: If you add a new verification here, please ensure all places that
// load toplevels / apply ops correctly load the requisite data.
let verify (c : T) : T =
  let dupedNames =
    c.dbs
    |> Map.values
    |> List.groupBy _.name
    |> Map.filter (fun db -> List.length db > 1)
    |> Map.keys

  match dupedNames with
  | [] -> c
  | dupes ->
    let dupes = String.concat "," dupes
    Exception.raiseInternal $"Duplicate DB names: {dupes}" []


// -------------------------
//  Loading/saving
//  -------------------------

let empty : T = { dbs = Map.empty; deletedDBs = Map.empty }


let loadFrom (tlids : List<tlid>) : Task<T> =
  task {
    try
      let! dbs = Serialize.loadToplevels tlids
      return empty |> addDBs dbs |> verify
    with e ->
      let tags = [ "tlids", tlids :> obj ]
      return Exception.reraiseAsPageable "toplevel load failed" tags e
  }


let loadAllDBs () : Task<T> =
  task {
    let! tlids = Serialize.fetchTLIDsForAllDBs ()
    return! loadFrom tlids
  }


let deleteToplevelForever (tlid : tlid) : Task<unit> =
  // CLEANUP: set deleted column in toplevels_v0 to be not nullable
  Sql.query "DELETE from toplevels_v0 WHERE tlid = @tlid"
  |> Sql.parameters [ "tlid", Sql.id tlid ]
  |> Sql.executeStatementAsync


/// Save just the TLIDs listed (callers may load more tlids to support
/// calling/testing these TLs, even though those TLs do not need to be updated).
/// Two-tuple matches the old shape: `(db, deleted)`.
let saveTLIDs (dbs : List<PT.DB.T * Serialize.Deleted>) : Task<unit> =
  try
    dbs
    |> Task.iterInParallel (fun (db, deleted) ->
      task {
        let serializedToplevel = BS.PT.Toplevel.serialize db.tlid db

        let deleted =
          match deleted with
          | Serialize.Deleted -> true
          | Serialize.NotDeleted -> false

        return!
          Sql.query
            "INSERT INTO toplevels_v0
              (tlid, digest, tipe, name,
               module, modifier, deleted, data, updated_at)
            VALUES
              (@tlid, @digest, @typ, @name,
               @module, @modifier, @deleted, @data, datetime('now'))
            ON CONFLICT (tlid)
              DO UPDATE SET
                digest = @digest,
                tipe = @typ,
                name = @name,
                module = @module,
                modifier = @modifier,
                deleted = @deleted,
                data = @data,
                updated_at = datetime('now')"
          |> Sql.parameters
            [ "tlid", Sql.tlid db.tlid
              "digest", Sql.string "fsharp"
              "typ", Sql.string "db"
              "name", Sql.stringOrNone (Some db.name)
              "module", Sql.stringOrNone None
              "modifier", Sql.stringOrNone None
              "deleted", Sql.bool deleted
              "data", Sql.bytes serializedToplevel ]
          |> Sql.executeStatementAsync
      })
  with e ->
    Exception.reraiseAsPageable "toplevel save failed" [] e


let toProgram (c : T) : Ply<RT.Program> =
  uply {
    let dbs =
      c.dbs
      |> Map.values
      |> List.map (fun db -> (db.name, PT2RT.DB.toRT db))
      |> Map.ofList

    return { dbs = dbs }
  }
