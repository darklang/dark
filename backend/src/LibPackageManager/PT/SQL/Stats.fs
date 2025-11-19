/// might be nice to have stats around more nuanced things:
///
/// - total ops
///
/// (what we have)
/// - total types (structures) in DB
/// - total vals (structures) in DB
/// - total fns (structures) in DB
///
/// - total _located_ types on `main`
/// - total _located_ vals on `main`
/// - total _located_ fns on `main`
module LibPackageManager.PT.SQL.Stats

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


type Stats = { types : int64; values : int64; fns : int64 }

// Stats count total unique content items, not branch-specific views
let get () : Ply<Stats> =
  uply {
    let count table =
      Sql.query $"SELECT COUNT(DISTINCT id) as count FROM {table}"
      |> Sql.executeRowAsync (fun read -> read.int64 "count")

    let! typesCount = count "package_types"
    let! valuesCount = count "package_values"
    let! fnsCount = count "package_functions"

    return { types = typesCount; values = valuesCount; fns = fnsCount }
  }
