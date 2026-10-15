/// `LibDB.Config` is the mutable, per-install local key/value store (`config_v0`): the current
/// branch, the scheduler knobs, the tracing knobs. Deliberately NOT content-addressed.
///
/// These cover the get/set/upsert contract the boot-time settings read relies on.
module Tests.Config

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude

open TestUtils.TestUtils

module Config = LibDB.Config

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
      } ]
