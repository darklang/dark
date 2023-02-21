module Tests.UserDB

// Most DB tests are in db.tests
// These ones are the ones we couldn't fit into that format well.

open Expecto
open Prelude
open TestUtils.TestUtils

open System.Threading.Tasks
open FSharp.Control.Tasks

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Exe = LibExecution.Execution

let p = Parser.parseRTExpr

let nullsAddedToMissingColumn =
  testTask "test for the hack that columns get null in all rows to start" {
    let! meta = initializeTestCanvas (Randomized "null-added-to-col")

    // Add DB with 1 col, add value
    let db1 : RT.DB.T =
      { tlid = gid (); name = "MyDB"; version = 0; cols = [ "x", RT.TStr ] }

    let! state = executionStateFor meta (Map [ "MyDB", db1 ]) Map.empty
    let expr = p @"DB.set_v1 { x = ""v"" } ""i"" MyDB"
    let! (result : RT.Dval) = Exe.executeExpr state Map.empty expr
    Expect.equal (RT.DObj(Map [ "x", RT.DStr "v" ])) result "no nulls"

    // Add a 2nd row, fetch values
    let db2 = { db1 with cols = [ "x", RT.TStr; "y", RT.TStr ] }

    // don't call executionStateFor as that will clear the DB
    let state =
      { state with program = { state.program with dbs = (Map [ "MyDB", db2 ]) } }

    let expr = p "(DB.getAllWithKeys_v2 MyDB)"
    let! (result : RT.Dval) = Exe.executeExpr state Map.empty expr

    // Check there's a null in there
    Expect.equal
      (RT.DObj(Map [ "i", RT.DObj(Map [ "x", RT.DStr "v"; "y", RT.DNull ]) ]))
      result
      "has nulls"
  }


let tests = testList "UserDB" [ nullsAddedToMissingColumn ]
