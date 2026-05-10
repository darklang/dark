/// <summary>Serializing to the DB.</summary>
/// <remarks>
/// Serialization formats and binary conversions are stored elsewhere
/// </remarks>
module LibCloud.Serialize

open System.Threading.Tasks
//open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble

open LibDB.Sqlite

open Prelude

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


// --------------------------------------------------------
// Load serialized data from the DB
// --------------------------------------------------------

type LoadAmount =
  | LiveToplevels
  | IncludeDeletedToplevels

type Deleted =
  | Deleted
  | NotDeleted

let loadToplevels (tlids : List<tlid>) : Task<List<Deleted * PT.DB.T>> =
  task {
    let! data =
      if List.isEmpty tlids then
        Task.FromResult []
      else
        let tlidParams =
          tlids |> List.mapi (fun i tlid -> ($"tlid{i}", Sql.tlid tlid))
        let tlidPlaceholders =
          tlids |> List.mapi (fun i _ -> $"@tlid{i}") |> String.concat ", "

        let query =
          $"SELECT tlid, data, deleted
           FROM toplevels_v0
           WHERE tlid IN ({tlidPlaceholders})
             AND tipe = 'db'"

        Sql.query query
        |> Sql.parameters tlidParams
        |> Sql.executeAsync (fun read ->
          (read.tlid "tlid", read.bytes "data", read.bool "deleted"))

    return
      data
      |> List.map (fun (tlid, tl, deleted) ->
        let isDeleted = if deleted then Deleted else NotDeleted
        (isDeleted, BS.PT.Toplevel.deserialize tlid tl))
  }


let fetchTLIDsForAllDBs () : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevels_v0
    WHERE tipe = 'db'
      AND deleted = 0"
  |> Sql.executeAsync (fun read -> read.tlid "tlid")
