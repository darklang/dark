module Tests.Canvas

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Fumble

open Prelude
open TestUtils.TestUtils
open LibDB.Db

module Canvas = LibCloud.Canvas
module Serialize = LibCloud.Serialize
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module Account = LibCloud.Account

let testDBOplistRoundtrip : Test =
  testTask "db oplist roundtrip" {
    let! canvasID = initializeTestCanvas "db_oplist_roundtrip"
    let db = testDB "myDB" PT.TInt64
    let tl = PT.Toplevel.TLDB db

    do! Canvas.saveTLIDs canvasID [ (tl, Serialize.NotDeleted) ]
    let! tls = Serialize.loadToplevels canvasID [ db.tlid ]
    Expect.equal tls [ Serialize.NotDeleted, tl ] "db oplist roundtrip"
  }


let testHttpOplistRoundtrip =
  testTask "test http oplist roundtrip" {
    let! canvasID = initializeTestCanvas "http_oplist_roundtrip"
    let h = testHttpRouteHandler "/path" "GET" (PT.EInt64(gid (), 5L))
    do! Canvas.saveTLIDs canvasID [ (PT.Toplevel.TLHandler h, Serialize.NotDeleted) ]
    let! (c : Canvas.T) =
      Canvas.loadHttpHandlers
        canvasID
        (PTParser.Handler.Spec.toName h.spec)
        (PTParser.Handler.Spec.toModifier h.spec)
    Expect.equal (c.handlers[h.tlid]) h "Handlers should be equal"
  }


let testHttpLoadIgnoresDeletedHandler =
  testTask "Http load ignores deleted handler" {
    let! canvasID = initializeTestCanvas "http-load-ignores-deleted-handler"
    let handler = testHttpRouteHandler "/path" "GET" (PT.EInt64(gid (), 5L))
    do!
      Canvas.saveTLIDs
        canvasID
        [ (PT.Toplevel.TLHandler handler, Serialize.Deleted) ]

    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers
        canvasID
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)

    Expect.equal c2.handlers.Count 0 "handler is not loaded"

    // In addition, check that the row is formatted correctly in the DB. We expect
    // name, module, and modifier to be null because otherwise they can be found by
    // Http searches
    let! dbRow =
      Sql.query
        "SELECT name, module, modifier, deleted
         FROM toplevels_v0
         WHERE canvas_id = @canvasID
           AND tlid = @tlid"
      |> Sql.parameters
        [ "canvasID", Sql.uuid canvasID; "tlid", Sql.tlid handler.tlid ]
      |> Sql.executeRowAsync (fun read ->
        read.stringOrNone "name",
        read.stringOrNone "module",
        read.stringOrNone "modifier",
        read.boolOrNone "deleted")

    Expect.equal dbRow (None, None, None, Some true) "Row should be cleared"
  }


let testSetHandlerAfterDelete =
  testTask "handler set after delete" {
    let! canvasID = initializeTestCanvas "set-handlder-after-delete"
    let! e1 = parsePTExpr "5 + 3"
    let! e2 = parsePTExpr "5 + 2"
    let h1 = testHttpRouteHandler "/path" "GET" e1
    let h2 = testHttpRouteHandler "/path" "GET" e2

    // Just the deleted handler
    do! Canvas.saveTLIDs canvasID [ (PT.Toplevel.TLHandler h1, Serialize.Deleted) ]

    let! (c1 : Canvas.T) = Canvas.loadAll canvasID

    Expect.equal (Map.find h1.tlid c1.deletedHandlers) (Some h1) "deleted in deleted"
    Expect.equal c1.deletedHandlers.Count 1 "only deleted in deleted"
    Expect.equal c1.handlers.Count 0 "deleted not in handlers"


    // And the new one (the deleted is still there)
    do!
      Canvas.saveTLIDs canvasID [ (PT.Toplevel.TLHandler h2, Serialize.NotDeleted) ]

    let! (c2 : Canvas.T) = Canvas.loadAll canvasID

    Expect.equal (Map.find h1.tlid c2.deletedHandlers) (Some h1) "deleted in deleted"
    Expect.equal c2.deletedHandlers.Count 1 "only deleted still in deleted"
    Expect.equal (Map.find h2.tlid c2.handlers) (Some h2) "live is in handlers"
    Expect.equal c2.handlers.Count 1 "only live is in handlers"
  }



let testLoadAllDBs =
  testTask "load all dbs" {
    let! canvasID = initializeTestCanvas "load-all-dbs"
    let typ = PT.TString
    let db1 : PT.DB.T = { tlid = gid (); name = "Books"; version = 0; typ = typ }
    let db2 : PT.DB.T = { tlid = gid (); name = "Books2"; version = 0; typ = typ }
    let db3 : PT.DB.T = { tlid = gid (); name = "Books3"; version = 0; typ = typ }
    do!
      Canvas.saveTLIDs
        canvasID
        [ (PT.Toplevel.TLDB db1, Serialize.Deleted)
          (PT.Toplevel.TLDB db2, Serialize.NotDeleted)
          (PT.Toplevel.TLDB db3, Serialize.NotDeleted) ]

    let! (c : Canvas.T) = Canvas.loadAll canvasID
    let ids = Map.values c.dbs |> List.map _.tlid |> Set
    Expect.equal ids (Set [ db2.tlid; db3.tlid ]) "Loaded only undeleted dbs"
  }

let tests =
  testList
    "canvas"
    [ testHttpOplistRoundtrip
      testDBOplistRoundtrip
      testHttpLoadIgnoresDeletedHandler
      testSetHandlerAfterDelete
      testLoadAllDBs ]
