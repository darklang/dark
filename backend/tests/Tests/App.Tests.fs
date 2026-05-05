module Tests.App

// Coverage for LibCloud.App + LibCloud.Serialize storage roundtrip.
// Ported from pre-rewrite Tests.Canvas (which is gone).

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open TestUtils.TestUtils

module App = LibCloud.App
module Serialize = LibCloud.Serialize
module PT = LibExecution.ProgramTypes


let testDBOplistRoundtrip =
  // Save a TLDB through App.saveTLIDs, load it back through
  // Serialize.loadToplevels, expect identity. Catches regressions in
  // the binary-serialization → SQLite → binary-deserialization path.
  testTask "db oplist roundtrip" {
    let! accountID = initializeTestCanvas "db_oplist_roundtrip"
    let db = testDB "myDB" PT.TInt64
    let tl = PT.Toplevel.TLDB db

    do! App.saveTLIDs accountID [ (tl, Serialize.NotDeleted) ]
    let! tls = Serialize.loadToplevels accountID [ db.tlid ]
    Expect.equal tls [ Serialize.NotDeleted, tl ] "db oplist roundtrip"
  }


let tests = testList "app" [ testDBOplistRoundtrip ]
