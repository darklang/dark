/// `LibDB.Config` is the mutable, per-install local key/value store (`config_v0`): the CLI
/// entry-point pointer and per-user settings. Deliberately NOT content-addressed and NOT synced.
///
/// These cover the get/set/upsert contract that boot-time entry-point resolution relies on.
module Tests.Config

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude

open TestUtils.TestUtils

module Config = LibDB.Config
open LibDB.Sqlite

let tests =
  testList
    "LibDB.Config"
    [ testTask "set then get round-trips" {
        do! Config.set "test.config.k1" "hello"
        let! v = Config.get "test.config.k1"
        Expect.equal v (Some "hello") "get returns the value that was set"
      }

      testTask "an unset key resolves to None" {
        let! v = Config.get "test.config.definitely-absent-xyz"
        Expect.equal v None "absent key -> None"
      }

      testTask "set is an upsert (a second set overwrites)" {
        do! Config.set "test.config.k2" "a"
        do! Config.set "test.config.k2" "b"
        let! v = Config.get "test.config.k2"
        Expect.equal v (Some "b") "the later value wins"
      }

      // The guarantee: a credential is not in a file Dark is allowed to open. `configGet` refusing
      // secret keys was never enough, because a plain SELECT on `config_v0` needs only
      // `package-read`.
      testTask "a secret round-trips WITHOUT touching config_v0" {
        let key = Config.secretPrefix + "https://test.example"
        do! Config.set key "a-test-secret-value"

        let! v = Config.get key
        Expect.equal
          v
          (Some "a-test-secret-value")
          "the secret is readable through Config.get, which F# uses"

        // The whole point, asserted the way the exploit did it.
        let! leaked =
          Sql.query
            "SELECT COUNT(*) as n FROM config_v0 WHERE key LIKE 'sync.secret.%'"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")

        Expect.equal
          leaked
          0L
          "no secret-prefixed row is in config_v0, where any package-read grant could select it"
      }

      testTask "the credential store is not the package store" {
        // Same path would mean `package-read` reaches it, and the separation is decorative.
        Expect.notEqual
          (System.IO.Path.GetFullPath(Config.credentialsPath ()))
          (System.IO.Path.GetFullPath LibDB.Sqlite.currentDbPath)
          "credentials live beside the store, never in it"
      } ]
