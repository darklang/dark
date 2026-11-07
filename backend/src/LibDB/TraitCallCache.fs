/// The reload's trait-call cache (`trait_call_resolutions`): what the at-rest checker answered
/// for each body, so the next reload asks it only about the bodies whose inputs changed.
///
/// The store keeps the answers as text it never reads. What they mean, and what makes one
/// reusable, is `Builtins.Matter.Libs.PM.TraitCalls`'s business.
module LibDB.TraitCallCache

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open LibDB.Sqlite

open Prelude

/// One body's answer: how many nodes the body has, and what the checker resolved in it, by
/// node position rather than node id (the ids are new on every parse).
type Entry = { nodes : int; resolutions : string }

/// A store made before the table existed has no cache, rather than an error.
let private exists () : bool =
  Sql.query
    "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'trait_call_resolutions'"
  |> Sql.executeExistsSync

/// Every entry stored under <param environment>, by item.
let load (environment : string) : Task<Map<string, Entry>> =
  task {
    if not (exists ()) then
      return Map.empty
    else
      let! rows =
        Sql.query
          "SELECT item_hash, nodes, resolutions FROM trait_call_resolutions
           WHERE environment_hash = @environment"
        |> Sql.parameters [ "environment", Sql.string environment ]
        |> Sql.executeAsync (fun read ->
          (read.string "item_hash",
           { nodes = read.int "nodes"; resolutions = read.string "resolutions" }))
      return Map.ofList rows
  }

/// Make <param entries>, under <param environment>, the whole cache. A reload checks the whole
/// tree, so an entry it did not write is for a body or an environment that is gone.
let replace (environment : string) (entries : List<string * Entry>) : unit =
  if exists () then
    let insert =
      "INSERT OR REPLACE INTO trait_call_resolutions
         (environment_hash, item_hash, nodes, resolutions)
       VALUES (@environment, @item, @nodes, @resolutions)"
    let rows =
      entries
      |> List.map (fun (item, entry) ->
        [ "environment", Sql.string environment
          "item", Sql.string item
          "nodes", Sql.int entry.nodes
          "resolutions", Sql.string entry.resolutions ])
    [ yield "DELETE FROM trait_call_resolutions", [ [] ]
      if not (List.isEmpty rows) then yield insert, rows ]
    |> Sql.executeTransactionSync
    |> ignore<List<int>>
