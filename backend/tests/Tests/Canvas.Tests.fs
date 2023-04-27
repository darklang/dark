module Tests.Canvas

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth
open TestUtils.TestUtils

module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module Account = LibBackend.Account

let parse = Parser.ProgramTypes.parseIgnoringUser

let testDBOplistRoundtrip : Test =
  testTask "db oplist roundtrip" {
    let! canvasID = initializeTestCanvas "db_oplist_roundtrip"

    let db = testDB "myDB" PT.TInt
    let oplist =
      [ PT.UndoTL db.tlid; PT.RedoTL db.tlid; PT.UndoTL db.tlid; PT.RedoTL db.tlid ]

    do!
      Canvas.saveTLIDs
        canvasID
        [ (db.tlid, oplist, PT.Toplevel.TLDB db, Canvas.NotDeleted) ]
    let! ops = Serialize.loadOplists Serialize.LiveToplevels canvasID [ db.tlid ]
    Expect.equal ops [ (db.tlid, oplist) ] "db oplist roundtrip"
  }


let testHttpOplistRoundtrip =
  testTask "test http oplist roundtrip" {
    let! canvasID = initializeTestCanvas "http_oplist_roundtrip"

    let handler = testHttpRouteHandler "/path" "GET" (PT.EInt(gid (), 5L))
    let oplist = [ PT.SetHandler handler ]
    do!
      Canvas.saveTLIDs
        canvasID
        [ (handler.tlid, oplist, PT.Toplevel.TLHandler handler, Canvas.NotDeleted) ]
    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers
        canvasID
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)
    Expect.equal (c2.handlers[handler.tlid]) handler "Handlers should be equal"
  }


let testHttpOplistLoadsUserTypes =
  testTask "httpOplistLoadsUserTypes" {
    let! canvasID = initializeTestCanvas "http_oplist_loads_user_tipes"

    let handler = testHttpRouteHandler "/path" "GET" (PT.EInt(gid (), 5L))
    let typ =
      testUserRecordType
        ({ modules = []; typ = "test-tipe"; version = 0 })
        ("age", PT.TInt)
        []

    do!
      Canvas.saveTLIDs
        canvasID
        [ (handler.tlid,
           [ PT.SetHandler handler ],
           PT.Toplevel.TLHandler handler,
           Canvas.NotDeleted)
          (typ.tlid, [ PT.SetType typ ], PT.Toplevel.TLType typ, Canvas.NotDeleted) ]

    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers
        canvasID
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)
    Expect.equal (c2.userTypes[typ.tlid]) typ "user types"
  }

let testUndoTooFarDoesntBreak =
  testTask "undo too far doesnt break" {
    let! canvasID = initializeTestCanvas "undo too far doesnt break"
    let handler = testHttpRouteHandler "/path" "GET" (PT.EInt(gid (), 5L))
    do!
      Canvas.saveTLIDs
        canvasID
        [ (handler.tlid,
           [ PT.SetHandler handler
             PT.UndoTL handler.tlid
             PT.UndoTL handler.tlid
             PT.UndoTL handler.tlid ],
           PT.Toplevel.TLHandler handler,
           Canvas.NotDeleted) ]

    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers
        canvasID
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)

    Expect.equal c2.handlers[handler.tlid] handler "handler is not loaded"

    // In addition, check that the row is formatted correctly in the DB. We expect
    // name, module, and modifier to be null because otherwise they can be found by
    // Http searches
    let! dbRow =
      Sql.query
        "SELECT name, module, modifier, deleted
         FROM toplevel_oplists_v0
         WHERE canvas_id = @canvasID
           AND tlid = @tlid"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                          "tlid", Sql.tlid handler.tlid ]
      |> Sql.executeRowAsync (fun read ->
        read.stringOrNone "name",
        read.stringOrNone "module",
        read.stringOrNone "modifier",
        read.boolOrNone "deleted")

    Expect.equal
      dbRow
      (Some "/path", Some "HTTP_BASIC", Some "GET", Some false)
      "Row should be there"
  }




let testHttpLoadIgnoresDeletedHandler =
  testTask "Http load ignores deleted handler" {
    let! canvasID = initializeTestCanvas "http-load-ignores-deleted-handler"
    let handler = testHttpRouteHandler "/path" "GET" (PT.EInt(gid (), 5L))
    do!
      Canvas.saveTLIDs
        canvasID
        [ (handler.tlid,
           [ PT.SetHandler handler; PT.DeleteTL handler.tlid ],
           PT.Toplevel.TLHandler handler,
           Canvas.Deleted) ]

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
         FROM toplevel_oplists_v0
         WHERE canvas_id = @canvasID
           AND tlid = @tlid"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                          "tlid", Sql.tlid handler.tlid ]
      |> Sql.executeRowAsync (fun read ->
        read.stringOrNone "name",
        read.stringOrNone "module",
        read.stringOrNone "modifier",
        read.boolOrNone "deleted")

    Expect.equal dbRow (None, None, None, Some true) "Row should be cleared"
  }

let testHttpLoadIgnoresDeletedFns =
  testTask "Http load ignores deleted fns" {
    let! canvasID = initializeTestCanvas "http-load-ignores-deleted-fns"

    let handler = testHttpRouteHandler "/path" "GET" (PT.EInt(gid (), 5L))
    let f = testUserFn "testfn" [] [] (PT.TVariable "a") (parse "5 + 3")
    let fNew = testUserFn "testfnNew" [] [] (PT.TVariable "a") (parse "6 + 4")

    do!
      Canvas.saveTLIDs
        canvasID
        [ (handler.tlid,
           [ PT.SetHandler handler ],
           PT.Toplevel.TLHandler handler,
           Canvas.NotDeleted)
          (f.tlid, [ PT.SetFunction f ], PT.Toplevel.TLFunction f, Canvas.NotDeleted) ]
    // TLIDs are saved in parallel, so do them in separate calls
    do!
      Canvas.saveTLIDs
        canvasID
        [ (f.tlid,
           [ PT.DeleteFunction f.tlid ],
           PT.Toplevel.TLFunction f,
           Canvas.Deleted)
          (fNew.tlid,
           [ PT.SetFunction fNew ],
           PT.Toplevel.TLFunction fNew,
           Canvas.NotDeleted) ]

    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers
        canvasID
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)

    Expect.equal c2.handlers[handler.tlid] handler "handler is loaded "
    Expect.equal c2.userFunctions.Count 1 "only one function is loaded from cache"
    Expect.equal c2.userFunctions[fNew.tlid] fNew "later func is loaded"
  }


let testDbRename =
  testTask "DB rename" {
    let! canvasID = initializeTestCanvas "db-rename"

    let dbID = gid ()
    let ops = [ PT.CreateDB(dbID, "Books", PT.TString); PT.RenameDB(dbID, "BsCode") ]
    let canvas = Canvas.fromOplist canvasID [] ops
    Expect.equal canvas.dbs[dbID].name "BsCode" "Datastore is created"
  }

let testSetHandlerAfterDelete =
  testTask "handler set after delete" {
    let! canvasID = initializeTestCanvas "set-handlder-after-delete"
    let e1 = (parse "5 + 3")
    let e2 = (parse "5 + 2")
    let h1 = testHttpRouteHandler "/path" "GET" e1
    let h2 = testHttpRouteHandler "/path" "GET" e2
    let op1 = PT.SetHandler h1
    let op2 = PT.DeleteTL h1.tlid
    let op3 = PT.SetHandler h2

    // Just the deleted handler
    do!
      Canvas.saveTLIDs
        canvasID
        [ (h1.tlid, [ op1; op2 ], PT.Toplevel.TLHandler h1, Canvas.Deleted) ]

    let! (c2 : Canvas.T) = Canvas.loadAll canvasID

    Expect.equal c2.deletedHandlers[h1.tlid] h1 "deleted in deleted"
    Expect.equal c2.deletedHandlers.Count 1 "only deleted in deleted"
    Expect.equal c2.handlers.Count 0 "deleted not in handlers"

    // And the new one (the deleted is still there)
    do!
      Canvas.saveTLIDs
        canvasID
        [ (h2.tlid, [ op3 ], PT.Toplevel.TLHandler h2, Canvas.NotDeleted) ]

    let! (c3 : Canvas.T) = Canvas.loadAll canvasID

    Expect.equal c3.deletedHandlers[h1.tlid] h1 "deleted still in deleted"
    Expect.equal c3.deletedHandlers.Count 1 "only deleted still in deleted"
    Expect.equal c3.handlers[h2.tlid] h2 "live is in handlers"
    Expect.equal c3.handlers.Count 1 "only live is in handlers"
  }

let testSetFunctionAfterDelete =
  testTask "function set after delete" {
    let! canvasID = initializeTestCanvas "db-set-function-after-delete"
    let f1 = testUserFn "testfn" [] [] (PT.TVariable "a") (parse "5 + 3")
    let f2 = testUserFn "testfn" [] [] (PT.TVariable "a") (parse "6 + 4")
    let op1 = PT.SetFunction f1
    let op2 = PT.DeleteFunction f1.tlid
    let op3 = PT.SetFunction f2

    // Just the deleted handler
    do!
      Canvas.saveTLIDs
        canvasID
        [ (f1.tlid, [ op1; op2 ], PT.Toplevel.TLFunction f1, Canvas.Deleted) ]

    let! (c2 : Canvas.T) = Canvas.loadAll canvasID

    Expect.equal c2.deletedUserFunctions[f1.tlid] f1 "deleted in deleted"
    Expect.equal c2.deletedUserFunctions.Count 1 "only deleted in deleted"
    Expect.equal c2.userFunctions.Count 0 "deleted not in handlers"

    // And the new one (the deleted is still there)
    do!
      Canvas.saveTLIDs
        canvasID
        [ (f2.tlid, [ op3 ], PT.Toplevel.TLFunction f2, Canvas.NotDeleted) ]

    let! (c3 : Canvas.T) = Canvas.loadAll canvasID

    Expect.equal c3.deletedUserFunctions[f1.tlid] f1 "deleted still in deleted"
    Expect.equal c3.deletedUserFunctions.Count 1 "only deleted still in deleted"
    Expect.equal c3.userFunctions[f2.tlid] f2 "live is in handlers"
    Expect.equal c3.userFunctions.Count 1 "only live is in handlers"
  }


let testLoadAllDBs =
  testTask "load all dbs" {
    let! canvasID = initializeTestCanvas "load-all-dbs"
    let dbid1, dbid2, dbid3 = gid (), gid (), gid ()
    let typ = PT.TString
    let ops1 = [ PT.CreateDB(dbid1, "Books", typ); PT.DeleteTL dbid1 ]
    let ops2 = [ PT.CreateDB(dbid2, "Books2", typ) ]
    let ops3 = [ PT.CreateDB(dbid3, "Books3", typ) ]
    let c1 = Canvas.empty canvasID |> Canvas.addOps (ops1 @ ops2 @ ops3) []
    do!
      Canvas.saveTLIDs
        canvasID
        [ (dbid1, ops1, PT.Toplevel.TLDB c1.deletedDBs[dbid1], Canvas.Deleted)
          (dbid2, ops2, PT.Toplevel.TLDB c1.dbs[dbid2], Canvas.NotDeleted)
          (dbid3, ops3, PT.Toplevel.TLDB c1.dbs[dbid3], Canvas.NotDeleted) ]

    let! (c2 : Canvas.T) = Canvas.loadAll canvasID
    let ids = Map.values c2.dbs |> List.map (fun db -> db.tlid) |> Set
    Expect.equal ids (Set [ dbid2; dbid3 ]) "Loaded only undeleted dbs"
  }


let testCanvasVerificationDuplicationCreation =
  testTask "canvas verification duplication creation" {
    let! canvasID = initializeTestCanvas "canvas-verification-duplication-creation"
    let dbID1, dbID2 = gid (), gid ()
    let ops =
      [ PT.CreateDB(dbID1, "Books", PT.TString)
        PT.CreateDB(dbID2, "Books", PT.TInt) ]
    try
      Canvas.empty canvasID |> Canvas.addOps [] ops |> ignore<Canvas.T>
      Expect.equal false true "should not verify"
    with
    | _ -> ()
  }

let testCanvasVerificationDuplicationCreationOffDisk =
  testTask "canvas verification duplication creation off disk" {
    let! canvasID =
      initializeTestCanvas "canvas-verification-duplication-creation-off-disk"

    let dbID1, dbID2 = gid (), gid ()
    // same name
    let ops1 = [ PT.CreateDB(dbID1, "Books", PT.TInt) ]
    let ops2 = [ PT.CreateDB(dbID2, "Books", PT.TBool) ]
    let c1 = Canvas.empty canvasID |> Canvas.addOps (ops1 @ ops2) []
    do!
      Canvas.saveTLIDs
        canvasID
        [ (dbID1, ops1, PT.Toplevel.TLDB c1.dbs[dbID1], Canvas.NotDeleted)
          (dbID2, ops2, PT.Toplevel.TLDB c1.dbs[dbID2], Canvas.NotDeleted) ]

    // CLEANUP: i'm not sure that it works or that it tests what it's supposed to test
    try
      let! (_ : Canvas.T) =
        match LibBackend.Op.requiredContextToValidateOplist ops2 with
        | LibBackend.Op.NoContext -> Canvas.loadTLIDs canvasID [ dbID2 ]
        | LibBackend.Op.AllDatastores -> Canvas.loadAll canvasID

      Expect.equal false true "should not verify"
    with
    | _ -> ()
  }

let testCanvasVerificationDuplicationRenaming =
  testTask "canvas verification duplication renaming" {
    let! canvasID = initializeTestCanvas "canvas-verification-duplication-renaming"
    let dbid1, dbid2 = gid (), gid ()
    let ops =
      [ PT.CreateDB(dbid1, "Books", PT.TString)
        PT.CreateDB(dbid2, "Books2", PT.TInt)
        PT.RenameDB(dbid2, "Books") ]
    try
      Canvas.empty canvasID |> Canvas.addOps [] ops |> ignore<Canvas.T>
      Expect.equal false true "should not verify"
    with
    | _ -> ()
  }

let testCanvasVerificationNoError =
  testTask "canvas verification no error" {
    let! canvasID = initializeTestCanvas "canvas-verification-no-error"
    let dbid1, dbid2 = gid (), gid ()
    let ops =
      [ PT.CreateDB(dbid1, "Books", PT.TString)
        PT.CreateDB(dbid2, "Books2", PT.TInt) ]
    try
      Canvas.empty canvasID |> Canvas.addOps [] ops |> ignore<Canvas.T>
    with
    | _ -> Expect.equal false true "should verify"
  }

let testCanvasVerificationUndoRenameDupedName =
  testTask "canvas verification undo rename duped name" {
    let! canvasID = initializeTestCanvas "canvas-verification-undo-rename-duped-name"
    let dbid1, dbid2 = gid (), gid ()
    let ops1 =
      [ PT.CreateDB(dbid1, "Books", PT.TInt)
        PT.TLSavepoint dbid1
        PT.DeleteTL dbid1
        PT.CreateDB(dbid2, "Books", PT.TString) ]
    let ops2 = ops1 @ [ PT.UndoTL dbid1 ]
    try
      Canvas.empty canvasID |> Canvas.addOps [] ops1 |> ignore<Canvas.T>
    with
    | _ -> Expect.equal false true "should initially verify"

    try
      Canvas.empty canvasID |> Canvas.addOps [] ops2 |> ignore<Canvas.T>
      Expect.equal false true "should not verify anymore"
    with
    | _ -> ()
  }


let tests =
  testList
    "canvas"
    [ testHttpOplistRoundtrip
      testDBOplistRoundtrip
      testHttpOplistLoadsUserTypes
      testHttpLoadIgnoresDeletedFns
      testHttpLoadIgnoresDeletedHandler
      testUndoTooFarDoesntBreak
      testDbRename
      testSetHandlerAfterDelete
      testSetFunctionAfterDelete
      testLoadAllDBs
      testCanvasVerificationDuplicationCreation
      testCanvasVerificationDuplicationCreationOffDisk
      testCanvasVerificationDuplicationRenaming
      testCanvasVerificationNoError
      testCanvasVerificationUndoRenameDupedName ]
