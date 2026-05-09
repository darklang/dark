module Tests.Toplevels

// Coverage for LibCloud.Toplevels + LibCloud.Serialize storage roundtrip.
// Ported from pre-rewrite Tests.Canvas (which is gone).

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open TestUtils.TestUtils

module Toplevels = LibCloud.Toplevels
module Serialize = LibCloud.Serialize
module PT = LibExecution.ProgramTypes


let testDBOplistRoundtrip =
  // Save a DB through Toplevels.saveTLIDs, load it back through
  // Serialize.loadToplevels, expect identity. Catches regressions in
  // the binary-serialization → SQLite → binary-deserialization path.
  testTask "db oplist roundtrip" {
    let db = testDB "myDB" PT.TInt64
    do! Toplevels.saveTLIDs [ (db, Serialize.NotDeleted) ]
    let! dbs = Serialize.loadToplevels [ db.tlid ]
    Expect.equal dbs [ Serialize.NotDeleted, db ] "db oplist roundtrip"
  }


let tests = testList "app" [ testDBOplistRoundtrip ]
