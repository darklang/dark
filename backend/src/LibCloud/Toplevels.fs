module LibCloud.Toplevels

// Functions related to apps (formerly canvases). Renamed from
// LibCloud.Canvas; surface API is the same.

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble
open LibSqlite.Db

open Prelude

module BS = LibSerialization.Binary.Serialization
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


// The "App" concept is being dismantled (per post-review user direction:
// "I want to fully remove the App/Canvas concept ... only thing we might
// 'bind' something to is the Account"). For now, keep these entry points
// as identity functions over the account ID — that's the scope key the
// rest of the system needs (user_data, traces, etc) and there's no real
// app indirection to maintain. The `apps_v0` table is gone; nothing
// reads from it. A future commit can rename `account_id` columns →
// `account_id` and drop the accountID plumbing entirely.

let create (accountID : Option<UserID>) : Task<uuid> =
  task { return accountID |> Option.defaultValue (System.Guid.NewGuid()) }

let getAppsForAccount (accountID : UserID) : Task<List<uuid>> =
  task { return [ accountID ] }

let getOrCreateForAccount (accountID : UserID) : Task<uuid> =
  task { return accountID }

/// <summary>
/// Canvas data - contains metadata along with basic handlers, DBs, etc.
/// </summary>
/// <remarks>
/// This includes just a subset of the key program data. It is rare that all of
/// the data for a canvas will be loaded. In addition, there is other canvas
/// data which is meaningful, such as creation date. These can be fetched separately.
/// </remarks>
type T =
  { id : uuid

    handlers : Map<tlid, PT.Handler.T>
    dbs : Map<tlid, PT.DB.T>
    deletedHandlers : Map<tlid, PT.Handler.T>
    deletedDBs : Map<tlid, PT.DB.T> }

let addToplevel (deleted : Serialize.Deleted) (tl : PT.Toplevel.T) (c : T) : T =
  let tlid = PT.Toplevel.toTLID tl

  match deleted, tl with
  | Serialize.NotDeleted, PT.Toplevel.TLHandler h ->
    { c with handlers = Map.add tlid h c.handlers }
  | Serialize.NotDeleted, PT.Toplevel.TLDB db ->
    { c with dbs = Map.add tlid db c.dbs }
  | Serialize.Deleted, PT.Toplevel.TLHandler h ->
    { c with deletedHandlers = Map.add tlid h c.deletedHandlers }
  | Serialize.Deleted, PT.Toplevel.TLDB db ->
    { c with deletedDBs = Map.add tlid db c.deletedDBs }


let addToplevels (tls : List<Serialize.Deleted * PT.Toplevel.T>) (c : T) : T =
  List.fold (fun acc (deleted, tl) -> addToplevel deleted tl acc) c tls

// NOTE: If you add a new verification here, please ensure all places that
// load canvases/apply ops correctly load the requisite data.
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

let empty (id : uuid) =
  { id = id
    handlers = Map.empty
    dbs = Map.empty
    deletedHandlers = Map.empty
    deletedDBs = Map.empty }

let loadFrom (id : uuid) (tlids : List<tlid>) : Task<T> =
  task {
    try
      // load
      let! tls = Serialize.loadToplevels id tlids

      let c = empty id

      return c |> addToplevels tls |> verify
    with e ->
      let tags = [ "tlids", tlids :> obj ]
      return Exception.reraiseAsPageable "toplevel load failed" tags e
  }

let loadAllDBs (id : uuid) : Task<T> =
  task {
    let! tlids = Serialize.fetchTLIDsForAllDBs id
    return! loadFrom id tlids
  }


let deleteToplevelForever (accountID : uuid) (tlid : tlid) : Task<unit> =
  // CLEANUP: set deleted column in toplevels_v0 to be not nullable
  Sql.query
    "DELETE from toplevels_v0
      WHERE account_id = @accountID
        AND tlid = @tlid"
  |> Sql.parameters [ "accountID", Sql.uuid accountID; "tlid", Sql.id tlid ]
  |> Sql.executeStatementAsync

let toplevelToDBTypeString (tl : PT.Toplevel.T) : string =
  match tl with
  | PT.Toplevel.TLDB _ -> "db"
  | PT.Toplevel.TLHandler _ -> "handler"

/// Save just the TLIDs listed (a canvas may load more tlids to support
/// calling/testing these TLs, even though those TLs do not need to be updated)
let saveTLIDs
  (id : uuid)
  (toplevels : List<PT.Toplevel.T * Serialize.Deleted>)
  : Task<unit> =
  try
    // Use ops rather than just set of toplevels, because toplevels may
    // have been deleted or undone, and therefore not appear, but it's
    // important to record them.
    toplevels
    |> Task.iterInParallel (fun (tl, deleted) ->
      task {
        let string2option (s : string) : Option<string> =
          if s = "" then None else Some s

        let tlid = PT.Toplevel.toTLID tl

        let routingNames =
          match tl with
          | PT.Toplevel.TLHandler({ spec = spec }) ->
            match spec with
            | PT.Handler.HTTP _ ->
              Some(
                PTParser.Handler.Spec.toModule spec,
                Routing.routeToPostgresPattern (PTParser.Handler.Spec.toName spec),
                PTParser.Handler.Spec.toModifier spec
              )
            | PT.Handler.Worker _
            | PT.Handler.Cron _
            | PT.Handler.REPL _ ->
              Some(
                PTParser.Handler.Spec.toModule spec,
                PTParser.Handler.Spec.toName spec,
                PTParser.Handler.Spec.toModifier spec
              )
          | PT.Toplevel.TLDB db -> Some("", db.name, "")

        let (module_, name, modifier) =
          // Only save info used to find handlers when the handler has not been deleted
          if deleted = Serialize.NotDeleted then
            match routingNames with
            | Some(module_, name, modifier) ->
              (string2option module_, string2option name, string2option modifier)
            | None -> None, None, None
          else
            None, None, None

        let serializedToplevel = BS.PT.Toplevel.serialize (PT.Toplevel.toTLID tl) tl

        let deleted =
          match deleted with
          | Serialize.Deleted -> true
          | Serialize.NotDeleted -> false

        return!
          Sql.query
            "INSERT INTO toplevels_v0
              (account_id, tlid, digest, tipe, name,
               module, modifier, deleted, data, updated_at)
            VALUES
              (@accountID, @tlid, @digest, @typ, @name,
               @module, @modifier, @deleted, @data, datetime('now'))
            ON CONFLICT (account_id, tlid)
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
            [ "accountID", Sql.uuid id
              "tlid", Sql.tlid tlid
              "digest", Sql.string "fsharp"
              "typ", Sql.string (toplevelToDBTypeString tl)
              "name", Sql.stringOrNone name
              "module", Sql.stringOrNone module_
              "modifier", Sql.stringOrNone modifier
              "deleted", Sql.bool deleted
              "data", Sql.bytes serializedToplevel ]
          |> Sql.executeStatementAsync
      })
  with e ->
    Exception.reraiseAsPageable "toplevel save failed" [ "accountID", id ] e


let toProgram (c : T) : Ply<RT.Program> =
  uply {
    let dbs =
      c.dbs
      |> Map.values
      |> List.map (fun db -> (db.name, PT2RT.DB.toRT db))
      |> Map.ofList

    return { accountID = System.Guid.Empty; dbs = dbs }
  }
