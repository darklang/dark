/// <summary>Serializing to the DB.</summary>
/// <remarks>
/// Serialization formats and binary conversions are stored elsewhere
/// </remarks>
module LibCloud.Serialize

open System.Threading.Tasks
//open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble

open LibSqlite.Db

open Prelude

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module PTParser = LibExecution.ProgramTypesParser


// --------------------------------------------------------
// Load serialized data from the DB
// --------------------------------------------------------

type LoadAmount =
  | LiveToplevels
  | IncludeDeletedToplevels

type Deleted =
  | Deleted
  | NotDeleted

let loadToplevels
  (canvasID : uuid)
  (tlids : List<tlid>)
  : Task<List<Deleted * PT.Toplevel.T>> =
  task {
    let! data =
      if List.isEmpty tlids then
        // If no specific TLIDs requested, return empty list
        Task.FromResult []
      else
        // Create IN clause with parameters for each TLID
        let tlidParams =
          tlids |> List.mapi (fun i tlid -> ($"tlid{i}", Sql.tlid tlid))
        let tlidPlaceholders =
          tlids |> List.mapi (fun i _ -> $"@tlid{i}") |> String.concat ", "

        let query =
          $"SELECT tlid, data, deleted
           FROM toplevels_v0
           WHERE app_id = @canvasID
             AND tlid IN ({tlidPlaceholders})
             AND (
               tipe = 'db'
               OR tipe = 'handler'
             )"

        Sql.query query
        |> Sql.parameters (("canvasID", Sql.uuid canvasID) :: tlidParams)
        |> Sql.executeAsync (fun read ->
          (read.tlid "tlid", read.bytes "data", read.bool "deleted"))

    return
      data
      |> List.map (fun (tlid, tl, deleted) ->
        let isDeleted = if deleted then Deleted else NotDeleted
        (isDeleted, BS.PT.Toplevel.deserialize tlid tl))
  }


let fetchTLIDsForAllDBs (canvasID : uuid) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevels_v0
    WHERE app_id = @canvasID
      AND tipe = 'db'
      AND deleted = 0"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

