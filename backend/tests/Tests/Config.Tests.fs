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
      }

      test "check refuses a key nothing reads, and the keys only the runtime writes" {
        let check = Config.check (fun _ -> true)
        Expect.isError (check "trace.recrod" "on") "a typo is not a key"
        Expect.isError
          (check "current_branch" "x")
          "the branch moves with selectBranch"
        Expect.isError (check "current_branch_name" "x") "and so does its name"
        Expect.isError (check "instance.id" "x") "the id is made once"
        Expect.isOk (check "test.anything" "x") "test keys are free"
        Expect.isOk (check "live.show" "Some.View") "Dark's own keys take any value"
      }

      test "check takes only what the boot reads back" {
        let check = Config.check (fun name -> name = "Darklang.Known.fn")
        Expect.isError
          (check "exec.storePollMs" "5")
          "the boot ignores a poll under 10 ms"
        Expect.isOk (check "exec.storePollMs" "10") "10 ms will do"
        Expect.isError (check "exec.workers" "-1") "no negative counts"
        Expect.isError
          (check "exec.workers" "99999999999")
          "workers is read as an Int32"
        Expect.isOk (check "trace.maxMb" "99999999999") "the caps are Int64"
        Expect.isOk
          (check "exec.spreadCrossover" "-1")
          "a negative crossover turns spreading off"
        Expect.isError (check "trace.record" "yes") "on or off"
        Expect.isOk
          (check "exec.policy" "Darklang.Known.fn")
          "a policy the boot finds"
        Expect.isError
          (check "exec.policy" "Zzz.Nope.nope")
          "and not one it does not"
        Expect.isOk (check "exec.policy" "") "empty is round robin again"
        Expect.isError (check "instance.name" "  ") "a name is not blank"
        Expect.isOk (check "instance.name" "laptop") "a name"
      }

      testTask "instance.id is made once, and kept" {
        do! Config.ensureInstance ()
        let! first = Config.get "instance.id"
        do! Config.ensureInstance ()
        let! second = Config.get "instance.id"
        Expect.isSome first "an id is made"
        Expect.equal second first "and never changes"
      } ]
