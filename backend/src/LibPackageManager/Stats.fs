module LibPackageManager.Stats

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


type Stats = { types : int64; values : int64; fns : int64 }

let get () : Ply<Stats> =
  uply {
    let! typesCount =
      Sql.query
        """
        SELECT COUNT(DISTINCT id) as count
        FROM package_types
        """
      |> Sql.executeRowAsync (fun read -> read.int64 "count")

    let! valuesCount =
      Sql.query
        """
        SELECT COUNT(DISTINCT id) as count
        FROM package_values
        """
      |> Sql.executeRowAsync (fun read -> read.int64 "count")

    let! fnsCount =
      Sql.query
        """
        SELECT COUNT(DISTINCT id) as count
        FROM package_functions
        """
      |> Sql.executeRowAsync (fun read -> read.int64 "count")

    return
      { types = typesCount
        values = valuesCount
        fns = fnsCount }
  }
