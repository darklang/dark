module LibPackageManager.Stats

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
    let countQuery table =
      Sql.query $"SELECT COUNT(DISTINCT id) as count FROM {table}"
      |> Sql.executeRowAsync (fun read -> read.int64 "count")

    let! typesCount = countQuery "package_types"
    let! valuesCount = countQuery "package_values"
    let! fnsCount = countQuery "package_functions"

    return { types = typesCount; values = valuesCount; fns = fnsCount }
  }
