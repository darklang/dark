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
module CanvasClone = LibBackend.CanvasClone
module Account = LibBackend.Account

let parse = Parser.parsePTExpr

let hop (h : PT.Handler.T) = PT.SetHandler(h.tlid, h)

let testDBOplistRoundtrip : Test =
  testTask "db oplist roundtrip" {
    let! meta = initializeTestCanvas "db_oplist_roundtrip"

    let db = testDB "myDB" []
    let oplist =
      [ PT.UndoTL db.tlid; PT.RedoTL db.tlid; PT.UndoTL db.tlid; PT.RedoTL db.tlid ]

    do!
      Canvas.saveTLIDs
        meta
        [ (db.tlid, oplist, PT.Toplevel.TLDB db, Canvas.NotDeleted) ]
    let! ops = Serialize.loadOplists Serialize.LiveToplevels meta.id [ db.tlid ]
    Expect.equal ops [ (db.tlid, oplist) ] "db oplist roundtrip"
  }


let testHttpOplistRoundtrip =
  testTask "test http oplist roundtrip" {
    let! meta = initializeTestCanvas "http_oplist_roundtrip"

    let handler = testHttpRouteHandler "/path" "GET" (PT.EInteger(gid (), 5L))
    let oplist = [ hop handler ]
    do!
      Canvas.saveTLIDs
        meta
        [ (handler.tlid, oplist, PT.Toplevel.TLHandler handler, Canvas.NotDeleted) ]
    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers
        meta
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)
    Expect.equal (c2.handlers[handler.tlid]) handler "Handlers should be equal"
  }


let testHttpOplistLoadsUserTypes =
  testTask "httpOplistLoadsUserTypes" {
    let! meta = initializeTestCanvas "http_oplist_loads_user_tipes"

    let handler = testHttpRouteHandler "/path" "GET" (PT.EInteger(gid (), 5L))
    let typ = testUserType "test-tipe" [ ("age", PT.TInt) ]
    do!
      Canvas.saveTLIDs
        meta
        [ (handler.tlid,
           [ hop handler ],
           PT.Toplevel.TLHandler handler,
           Canvas.NotDeleted)
          (typ.tlid, [ PT.SetType typ ], PT.Toplevel.TLType typ, Canvas.NotDeleted) ]

    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers
        meta
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)
    Expect.equal (c2.userTypes[typ.tlid]) typ "user types"
  }

let testUndoTooFarDoesntBreak =
  testTask "undo too far doesnt break" {
    let! meta = initializeTestCanvas "undo too far doesnt break"
    let handler = testHttpRouteHandler "/path" "GET" (PT.EInteger(gid (), 5L))
    do!
      Canvas.saveTLIDs
        meta
        [ (handler.tlid,
           [ hop handler
             PT.UndoTL handler.tlid
             PT.UndoTL handler.tlid
             PT.UndoTL handler.tlid ],
           PT.Toplevel.TLHandler handler,
           Canvas.NotDeleted) ]

    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers
        meta
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)

    Expect.equal c2.handlers[handler.tlid] handler "handler is not loaded"

    // In addition, check that the row is formatted correctly in the DB. We expect
    // name, module, and modifier to be null because otherwise they can be found by
    // Http searches
    let! dbRow =
      Sql.query
        "SELECT name, module, modifier, deleted
         FROM toplevel_oplists
         WHERE canvas_id = @canvasID
           AND tlid = @tlid"
      |> Sql.parameters [ "canvasID", Sql.uuid meta.id
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
    let! meta = initializeTestCanvas "http-load-ignores-deleted-handler"
    let handler = testHttpRouteHandler "/path" "GET" (PT.EInteger(gid (), 5L))
    do!
      Canvas.saveTLIDs
        meta
        [ (handler.tlid,
           [ hop handler; PT.DeleteTL handler.tlid ],
           PT.Toplevel.TLHandler handler,
           Canvas.Deleted) ]

    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers
        meta
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)

    Expect.equal c2.handlers.Count 0 "handler is not loaded"

    // In addition, check that the row is formatted correctly in the DB. We expect
    // name, module, and modifier to be null because otherwise they can be found by
    // Http searches
    let! dbRow =
      Sql.query
        "SELECT name, module, modifier, deleted
         FROM toplevel_oplists
         WHERE canvas_id = @canvasID
           AND tlid = @tlid"
      |> Sql.parameters [ "canvasID", Sql.uuid meta.id
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
    let! meta = initializeTestCanvas "http-load-ignores-deleted-fns"

    let handler = testHttpRouteHandler "/path" "GET" (PT.EInteger(gid (), 5L))
    let f = testUserFn "testfn" [] (parse "5 + 3")
    let fNew = testUserFn "testfnNew" [] (parse "6 + 4")

    do!
      Canvas.saveTLIDs
        meta
        [ (handler.tlid,
           [ hop handler ],
           PT.Toplevel.TLHandler handler,
           Canvas.NotDeleted)
          (f.tlid, [ PT.SetFunction f ], PT.Toplevel.TLFunction f, Canvas.NotDeleted) ]
    // TLIDs are saved in parallel, so do them in separate calls
    do!
      Canvas.saveTLIDs
        meta
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
        meta
        (PTParser.Handler.Spec.toName handler.spec)
        (PTParser.Handler.Spec.toModifier handler.spec)

    Expect.equal c2.handlers[handler.tlid] handler "handler is loaded "
    Expect.equal c2.userFunctions.Count 1 "only one function is loaded from cache"
    Expect.equal c2.userFunctions[fNew.tlid] fNew "later func is loaded"
  }


let testDbCreateWithOrblankName =
  testTask "DB create with orblank name" {
    let! meta = initializeTestCanvas "db-create-with-orblank-name"

    let dbid = gid ()
    let nameID = gid ()
    let colNameID = gid ()
    let colTypeID = gid ()
    let name = "Books"
    let db : PT.DB.T =
      { tlid = dbid
        name = name
        nameID = nameID
        version = 0
        cols =
          [ { name = None; nameID = colNameID; typ = None; typeID = colTypeID } ] }

    let ops =
      [ PT.CreateDBWithBlankOr(dbid, nameID, name)
        PT.AddDBCol(dbid, colNameID, colTypeID) ]
    let canvas = Canvas.fromOplist meta [] ops
    Expect.equal (canvas.dbs[dbid]) db "Datastore is created"
  }

let testDbRename =
  testTask "DB rename" {
    let! meta = initializeTestCanvas "db-rename"

    let dbid = gid ()
    let nameID = gid ()
    let colNameID = gid ()
    let colTypeID = gid ()
    let name = "Books"
    let ops =
      [ PT.CreateDBWithBlankOr(dbid, nameID, name)
        PT.AddDBCol(dbid, colNameID, colTypeID)
        PT.RenameDBname(dbid, "BsCode") ]
    let canvas = Canvas.fromOplist meta [] ops
    Expect.equal canvas.dbs[dbid].name "BsCode" "Datastore is created"
  }

let testSetHandlerAfterDelete =
  testTask "handler set after delete" {
    let! meta = initializeTestCanvas "set-handlder-after-delete"
    let e1 = (parse "5 + 3")
    let e2 = (parse "5 + 2")
    let h1 = testHttpRouteHandler "/path" "GET" e1
    let h2 = testHttpRouteHandler "/path" "GET" e2
    let op1 = hop h1
    let op2 = PT.DeleteTL h1.tlid
    let op3 = hop h2

    // Just the deleted handler
    do!
      Canvas.saveTLIDs
        meta
        [ (h1.tlid, [ op1; op2 ], PT.Toplevel.TLHandler h1, Canvas.Deleted) ]

    let! (c2 : Canvas.T) = Canvas.loadAll meta

    Expect.equal c2.deletedHandlers[h1.tlid] h1 "deleted in deleted"
    Expect.equal c2.deletedHandlers.Count 1 "only deleted in deleted"
    Expect.equal c2.handlers.Count 0 "deleted not in handlers"

    // And the new one (the deleted is still there)
    do!
      Canvas.saveTLIDs
        meta
        [ (h2.tlid, [ op3 ], PT.Toplevel.TLHandler h2, Canvas.NotDeleted) ]

    let! (c3 : Canvas.T) = Canvas.loadAll meta

    Expect.equal c3.deletedHandlers[h1.tlid] h1 "deleted still in deleted"
    Expect.equal c3.deletedHandlers.Count 1 "only deleted still in deleted"
    Expect.equal c3.handlers[h2.tlid] h2 "live is in handlers"
    Expect.equal c3.handlers.Count 1 "only live is in handlers"
  }

let testSetFunctionAfterDelete =
  testTask "function set after delete" {
    let! meta = initializeTestCanvas "db-set-function-after-delete"
    let f1 = testUserFn "testfn" [] (parse "5 + 3")
    let f2 = testUserFn "testfn" [] (parse "6 + 4")
    let op1 = PT.SetFunction f1
    let op2 = PT.DeleteFunction f1.tlid
    let op3 = PT.SetFunction f2

    // Just the deleted handler
    do!
      Canvas.saveTLIDs
        meta
        [ (f1.tlid, [ op1; op2 ], PT.Toplevel.TLFunction f1, Canvas.Deleted) ]

    let! (c2 : Canvas.T) = Canvas.loadAll meta

    Expect.equal c2.deletedUserFunctions[f1.tlid] f1 "deleted in deleted"
    Expect.equal c2.deletedUserFunctions.Count 1 "only deleted in deleted"
    Expect.equal c2.userFunctions.Count 0 "deleted not in handlers"

    // And the new one (the deleted is still there)
    do!
      Canvas.saveTLIDs
        meta
        [ (f2.tlid, [ op3 ], PT.Toplevel.TLFunction f2, Canvas.NotDeleted) ]

    let! (c3 : Canvas.T) = Canvas.loadAll meta

    Expect.equal c3.deletedUserFunctions[f1.tlid] f1 "deleted still in deleted"
    Expect.equal c3.deletedUserFunctions.Count 1 "only deleted still in deleted"
    Expect.equal c3.userFunctions[f2.tlid] f2 "live is in handlers"
    Expect.equal c3.userFunctions.Count 1 "only live is in handlers"
  }


let testLoadAllDBs =
  testTask "load all dbs" {
    let! meta = initializeTestCanvas "load-all-dbs"
    let dbid1, dbid2, dbid3 = gid (), gid (), gid ()
    let nameid1, nameid2, nameid3 = gid (), gid (), gid ()
    let ops1 = [ PT.CreateDBWithBlankOr(dbid1, nameid1, "Books"); PT.DeleteTL dbid1 ]
    let ops2 = [ PT.CreateDBWithBlankOr(dbid2, nameid2, "Books2") ]
    let ops3 = [ PT.CreateDBWithBlankOr(dbid3, nameid3, "Books3") ]
    let c1 = Canvas.empty meta |> Canvas.addOps (ops1 @ ops2 @ ops3) []
    do!
      Canvas.saveTLIDs
        meta
        [ (dbid1, ops1, PT.Toplevel.TLDB c1.deletedDBs[dbid1], Canvas.Deleted)
          (dbid2, ops2, PT.Toplevel.TLDB c1.dbs[dbid2], Canvas.NotDeleted)
          (dbid3, ops3, PT.Toplevel.TLDB c1.dbs[dbid3], Canvas.NotDeleted) ]

    let! (c2 : Canvas.T) = Canvas.loadAll meta
    let ids = Map.values c2.dbs |> List.map (fun db -> db.tlid) |> Set
    Expect.equal ids (Set [ dbid2; dbid3 ]) "Loaded only undeleted dbs"
  }


let testCanvasVerificationDuplicationCreation =
  testTask "canvas verification duplication creation" {
    let! meta = initializeTestCanvas "canvas-verification-duplication-creation"
    let dbid1, dbid2 = gid (), gid ()
    let nameid1, nameid2 = gid (), gid ()
    let ops =
      [ PT.CreateDBWithBlankOr(dbid1, nameid1, "Books")
        PT.CreateDBWithBlankOr(dbid2, nameid2, "Books") ]
    try
      Canvas.empty meta |> Canvas.addOps [] ops |> ignore<Canvas.T>
      Expect.equal false true "should not verify"
    with
    | _ -> ()
  }

let testCanvasVerificationDuplicationCreationOffDisk =
  testTask "canvas verification duplication creation off disk" {
    let! meta =
      initializeTestCanvas "canvas-verification-duplication-creation-off-disk"

    let dbid1, dbid2 = gid (), gid ()
    let nameid1, nameid2 = gid (), gid ()
    // same name
    let ops1 = [ PT.CreateDBWithBlankOr(dbid1, nameid1, "Books") ]
    let ops2 = [ PT.CreateDBWithBlankOr(dbid2, nameid2, "Books") ]
    let c1 = Canvas.empty meta |> Canvas.addOps (ops1 @ ops2) []
    do!
      Canvas.saveTLIDs
        meta
        [ (dbid1, ops1, PT.Toplevel.TLDB c1.dbs[dbid1], Canvas.NotDeleted)
          (dbid2, ops2, PT.Toplevel.TLDB c1.dbs[dbid2], Canvas.NotDeleted) ]

    // CLEANUP: i'm not sure that it works or that it tests what it's supposed to test
    try
      let! (_ : Canvas.T) =
        match LibBackend.Op.requiredContextToValidateOplist ops2 with
        | LibBackend.Op.NoContext -> Canvas.loadTLIDs meta [ dbid2 ]
        | LibBackend.Op.AllDatastores -> Canvas.loadAll meta

      Expect.equal false true "should not verify"
    with
    | _ -> ()
  }

let testCanvasVerificationDuplicationRenaming =
  testTask "canvas verification duplication renaming" {
    let! meta = initializeTestCanvas "canvas-verification-duplication-renaming"
    let dbid1, dbid2 = gid (), gid ()
    let nameid1, nameid2 = gid (), gid ()
    let ops =
      [ PT.CreateDBWithBlankOr(dbid1, nameid1, "Books")
        PT.CreateDBWithBlankOr(dbid2, nameid2, "Books2")
        PT.RenameDBname(dbid2, "Books") ]
    try
      Canvas.empty meta |> Canvas.addOps [] ops |> ignore<Canvas.T>
      Expect.equal false true "should not verify"
    with
    | _ -> ()
  }

let testCanvasVerificationNoError =
  testTask "canvas verification no error" {
    let! meta = initializeTestCanvas "canvas-verification-no-error"
    let dbid1, dbid2 = gid (), gid ()
    let nameid1, nameid2 = gid (), gid ()
    let ops =
      [ PT.CreateDBWithBlankOr(dbid1, nameid1, "Books")
        PT.CreateDBWithBlankOr(dbid2, nameid2, "Books2") ]
    try
      Canvas.empty meta |> Canvas.addOps [] ops |> ignore<Canvas.T>
    with
    | _ -> Expect.equal false true "should verify"
  }

let testCanvasVerificationUndoRenameDupedName =
  testTask "canvas verification undo rename duped name" {
    let! meta = initializeTestCanvas "canvas-verification-undo-rename-duped-name"
    let dbid1, dbid2 = gid (), gid ()
    let nameid1, nameid2 = gid (), gid ()
    let ops1 =
      [ PT.CreateDBWithBlankOr(dbid1, nameid1, "Books")
        PT.TLSavepoint dbid1
        PT.DeleteTL dbid1
        PT.CreateDBWithBlankOr(dbid2, nameid2, "Books") ]
    let ops2 = ops1 @ [ PT.UndoTL dbid1 ]
    try
      Canvas.empty meta |> Canvas.addOps [] ops1 |> ignore<Canvas.T>
    with
    | _ -> Expect.equal false true "should initially verify"

    try
      Canvas.empty meta |> Canvas.addOps [] ops2 |> ignore<Canvas.T>
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
      testDbCreateWithOrblankName
      testDbRename
      testSetHandlerAfterDelete
      testSetFunctionAfterDelete
      testLoadAllDBs
      testCanvasVerificationDuplicationCreation
      testCanvasVerificationDuplicationCreationOffDisk
      testCanvasVerificationDuplicationRenaming
      testCanvasVerificationNoError
      testCanvasVerificationUndoRenameDupedName ]
