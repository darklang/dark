/// Tests for LibDB.Remotes — the registered sync-peer store behind `dark remote`.
///
/// Covers the upsert-by-name semantics, ordered list/urls, and remove's existed-flag.
/// The `sync_remotes` table is created by schema.sql at test-DB migration time.
module Tests.Remotes

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module Remotes = LibDB.Remotes

// Unique names per run so parallel suites / reruns don't collide on the shared test DB.
let private uniq (label : string) : string = $"{label}-{System.Guid.NewGuid()}"


let addThenList =
  testTask "add registers a remote that list returns as (name, url)" {
    let name = uniq "box"
    do! Remotes.add name "http://127.0.0.1:9922"
    let! all = Remotes.list ()
    Expect.contains
      all
      (name, "http://127.0.0.1:9922")
      "added remote shows up in list"
  }

let addIsUpsertByName =
  testTask "re-adding a name updates its url (idempotent upsert, no duplicate row)" {
    let name = uniq "laptop"
    do! Remotes.add name "https://old.example"
    do! Remotes.add name "https://new.example"
    let! all = Remotes.list ()
    let matching = all |> List.filter (fun (n, _) -> n = name)
    Expect.equal
      matching
      [ (name, "https://new.example") ]
      "single row, url updated to the latest"
  }

let urlsReturnsPollTargets =
  testTask "urls returns just the pollable urls of registered remotes" {
    let name = uniq "peer"
    let url = uniq "https://peer" + ".ts.net"
    do! Remotes.add name url
    let! urls = Remotes.urls ()
    Expect.contains urls url "the remote's url is in the poll set"
  }

let removeReportsExisted =
  testTask "remove returns true for a known remote, false for an unknown one" {
    let name = uniq "ephemeral"
    do! Remotes.add name "http://gone.example"
    let! existed = Remotes.remove name
    Expect.isTrue existed "removing a registered remote reports existed=true"
    let! again = Remotes.remove name
    Expect.isFalse again "removing it a second time reports existed=false"
    let! all = Remotes.list ()
    Expect.isFalse
      (all |> List.exists (fun (n, _) -> n = name))
      "removed remote is gone from the list"
  }


let tests =
  testList
    "remotes"
    [ addThenList; addIsUpsertByName; urlsReturnsPollTargets; removeReportsExisted ]
