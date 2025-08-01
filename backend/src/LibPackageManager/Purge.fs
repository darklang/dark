module LibPackageManager.Purge

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

let purge () : Task<unit> =
  task {
    [ "DELETE FROM package_types_v0"
      "DELETE FROM package_constants_v0"
      "DELETE FROM package_functions_v0" ]
    |> List.map (fun sql -> (sql, [ [] ]))
    |> Sql.executeTransactionSync
    |> ignore<List<int>>
  }
