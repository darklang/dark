/// Functions related to Accounts. An account represents the developer
/// behind a commit / trace. Accounts have no metadata beyond `id` +
/// `name` today; richer profile data (email, etc.) is out of scope.
module LibCloud.Account

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

type Account = { id : AccountID; name : string }

/// Pre-allocated UUIDs for the seeded accounts. These IDs are part
/// of the API surface — Dark code references them by literal — so
/// don't rotate.
module IDs =
  let darklang : AccountID = System.Guid.Parse "00000000-0000-0000-0000-000000000001"
  let stachu : AccountID = System.Guid.Parse "00000000-0000-0000-0000-000000000002"
  let paul : AccountID = System.Guid.Parse "00000000-0000-0000-0000-000000000003"
  let feriel : AccountID = System.Guid.Parse "00000000-0000-0000-0000-000000000004"

/// Resolve the current process's account. Single-instance Dark
/// today, so this is just Darklang. When multi-account lands this
/// would consult an env var / config / login state.
let resolve () : AccountID = IDs.darklang

let getAccountByName (name : string) : Task<Option<AccountID>> =
  Sql.query "SELECT id FROM accounts_v0 WHERE name = @name"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")

let getAccountNameByID (id : AccountID) : Task<Option<string>> =
  Sql.query "SELECT name FROM accounts_v0 WHERE id = @id"
  |> Sql.parameters [ "id", Sql.uuid id ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "name")
