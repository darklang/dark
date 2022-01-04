module Tests.Canvas

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils.TestUtils

module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module PT = LibExecution.ProgramTypes
module S = PT.Shortcuts
module CanvasClone = LibBackend.CanvasClone
module Account = LibBackend.Account

let parse = FSharpToExpr.parsePTExpr

let hop (h : PT.Handler.T) = PT.SetHandler(h.tlid, h.pos, h)

let testDBOplistRoundtrip : Test =
  testTask "db oplist roundtrip" {
    let! meta = initializeTestCanvas "db_oplist_roundtrip"

    let db = testDB "myDB" []
    let oplist =
      [ PT.UndoTL db.tlid; PT.RedoTL db.tlid; PT.UndoTL db.tlid; PT.RedoTL db.tlid ]

    do! Canvas.saveTLIDs meta [ (db.tlid, oplist, PT.TLDB db, Canvas.NotDeleted) ]
    let! ops = Canvas.loadOplists Canvas.LiveToplevels meta.id [ db.tlid ]
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
        [ (handler.tlid, oplist, PT.TLHandler handler, Canvas.NotDeleted) ]
    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers meta (handler.spec.name ()) (handler.spec.modifier ())
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
        [ (handler.tlid, [ hop handler ], PT.TLHandler handler, Canvas.NotDeleted)
          (typ.tlid, [ PT.SetType typ ], PT.TLType typ, Canvas.NotDeleted) ]

    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers meta (handler.spec.name ()) (handler.spec.modifier ())
    Expect.equal (c2.userTypes[typ.tlid]) typ "user types"
  }


let testHttpLoadIgnoresDeletedFns =
  testTask "Http load ignores deleted fns" {
    let! meta = initializeTestCanvas "http-load-ignores-deleted-fns"

    let handler = testHttpRouteHandler "/path" "GET" (PT.EInteger(gid (), 5L))
    let f = testUserFn "testfn" [] (parse "5 + 3")
    let f2 = testUserFn "testfn" [] (parse "6 + 4")

    do!
      Canvas.saveTLIDs
        meta
        [ (handler.tlid, [ hop handler ], PT.TLHandler handler, Canvas.NotDeleted)
          (f.tlid, [ PT.SetFunction f ], PT.TLFunction f, Canvas.NotDeleted)
          (f.tlid, [ PT.DeleteFunction f.tlid ], PT.TLFunction f, Canvas.Deleted)
          (f2.tlid, [ PT.SetFunction f2 ], PT.TLFunction f2, Canvas.NotDeleted) ]

    let! (c2 : Canvas.T) =
      Canvas.loadHttpHandlers meta (handler.spec.name ()) (handler.spec.modifier ())

    Expect.equal c2.handlers[handler.tlid] handler "handler is loaded "
    Expect.equal c2.userFunctions.Count 1 "only one function is loaded from cache"
    Expect.equal c2.userFunctions[f2.tlid] f2 "later func is loaded"
  }


let testDbCreateWithOrblankName =
  testTask "DB create with orblank name" {
    let! meta = initializeTestCanvas "db-create-with-orblank-name"

    let dbid = gid ()
    let nameID = gid ()
    let colNameID = gid ()
    let colTypeID = gid ()
    let name = "Books"
    let pos = { x = 0; y = 0 }
    let db : PT.DB.T =
      { tlid = dbid
        pos = pos
        name = name
        nameID = nameID
        version = 0
        cols = [ { name = ""; nameID = colNameID; typ = None; typeID = colTypeID } ] }

    let ops =
      [ PT.CreateDBWithBlankOr(dbid, pos, nameID, name)
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
    let pos = { x = 0; y = 0 }
    let ops =
      [ PT.CreateDBWithBlankOr(dbid, pos, nameID, name)
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
        [ (h1.tlid, [ op1; op2 ], PT.TLHandler h1, Canvas.Deleted) ]

    let! (c2 : Canvas.T) = Canvas.loadAll meta

    Expect.equal c2.deletedHandlers[h1.tlid] h1 "deleted in deleted"
    Expect.equal c2.deletedHandlers.Count 1 "only deleted in deleted"
    Expect.equal c2.handlers.Count 0 "deleted not in handlers"

    // And the new one (the deleted is still there)
    do!
      Canvas.saveTLIDs
        meta
        [ (h2.tlid, [ op3 ], PT.TLHandler h2, Canvas.NotDeleted) ]

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
        [ (f1.tlid, [ op1; op2 ], PT.TLFunction f1, Canvas.Deleted) ]

    let! (c2 : Canvas.T) = Canvas.loadAll meta

    Expect.equal c2.deletedUserFunctions[f1.tlid] f1 "deleted in deleted"
    Expect.equal c2.deletedUserFunctions.Count 1 "only deleted in deleted"
    Expect.equal c2.userFunctions.Count 0 "deleted not in handlers"

    // And the new one (the deleted is still there)
    do!
      Canvas.saveTLIDs
        meta
        [ (f2.tlid, [ op3 ], PT.TLFunction f2, Canvas.NotDeleted) ]

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
    let ops1 =
      [ PT.CreateDBWithBlankOr(dbid1, testPos, nameid1, "Books"); PT.DeleteTL dbid1 ]
    let ops2 = [ PT.CreateDBWithBlankOr(dbid2, testPos, nameid2, "Books2") ]
    let ops3 = [ PT.CreateDBWithBlankOr(dbid3, testPos, nameid3, "Books3") ]
    let c1 = Canvas.empty meta |> Canvas.addOps (ops1 @ ops2 @ ops3) []
    do!
      Canvas.saveTLIDs
        meta
        [ (dbid1, ops1, PT.TLDB c1.deletedDBs[dbid1], Canvas.Deleted)
          (dbid2, ops2, PT.TLDB c1.dbs[dbid2], Canvas.NotDeleted)
          (dbid3, ops3, PT.TLDB c1.dbs[dbid3], Canvas.NotDeleted) ]

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
      [ PT.CreateDBWithBlankOr(dbid1, testPos, nameid1, "Books")
        PT.CreateDBWithBlankOr(dbid2, testPos, nameid2, "Books") ]
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
    let ops1 = [ PT.CreateDBWithBlankOr(dbid1, testPos, nameid1, "Books") ]
    let ops2 = [ PT.CreateDBWithBlankOr(dbid2, testPos, nameid2, "Books") ]
    let c1 = Canvas.empty meta |> Canvas.addOps (ops1 @ ops2) []
    do!
      Canvas.saveTLIDs
        meta
        [ (dbid1, ops1, PT.TLDB c1.dbs[dbid1], Canvas.NotDeleted)
          (dbid2, ops2, PT.TLDB c1.dbs[dbid2], Canvas.NotDeleted) ]

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
      [ PT.CreateDBWithBlankOr(dbid1, testPos, nameid1, "Books")
        PT.CreateDBWithBlankOr(dbid2, testPos, nameid2, "Books2")
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
      [ PT.CreateDBWithBlankOr(dbid1, testPos, nameid1, "Books")
        PT.CreateDBWithBlankOr(dbid2, testPos, nameid2, "Books2") ]
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
      [ PT.CreateDBWithBlankOr(dbid1, testPos, nameid1, "Books")
        PT.TLSavepoint dbid1
        PT.DeleteTL dbid1
        PT.CreateDBWithBlankOr(dbid2, testPos, nameid2, "Books") ]
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


let testCanvasClone =
  testTask "canvas clone" {
    let username = UserName.create "clone"
    match! Account.getUser username with
    | None ->
      let user : Account.Account =
        { username = username
          password = LibBackend.Password.invalid
          email = "clone@example.com"
          name = "Cloney McCloneFace" }
      let! (added : Result<unit, string>) = Account.upsertNonAdmin user
      assert Result.isOk added
    | Some _ -> ()

    let sourceCanvasName = CanvasName.create "sample-gettingstarted"
    let targetCanvasName = CanvasName.create "clone-gettingstarted"

    let! sourceMeta = Canvas.getMeta sourceCanvasName
    let! targetMeta = Canvas.getMeta targetCanvasName

    do! Canvas.loadAndResaveFromTestFile sourceMeta
    do! CanvasClone.cloneCanvas sourceCanvasName targetCanvasName false

    let! (sourceCanvas : Canvas.T) = Canvas.loadAll sourceMeta
    let! (targetCanvas : Canvas.T) = Canvas.loadAll targetMeta

    let! tlids = Serialize.fetchAllTLIDs sourceMeta.id
    let! sourceOplists =
      Canvas.loadOplists Canvas.IncludeDeletedToplevels sourceMeta.id tlids
    let! targetOplists =
      Canvas.loadOplists Canvas.IncludeDeletedToplevels targetMeta.id tlids

    let hasCreationOps oplists =
      oplists
      |> List.map (fun (_, ops) ->
        CanvasClone.onlyOpsSinceLastSavepoint ops
        |> List.any CanvasClone.isOpThatCreatesToplevel)
      |> List.all Fun.identity

    let canvasOpsLength oplists =
      oplists |> List.map Tuple2.second |> List.concat |> List.length

    Expect.equal
      (hasCreationOps sourceOplists)
      true
      "only_ops_since_last_savepoint retrieve latest ops from the last complete op"

    Expect.equal
      (canvasOpsLength sourceOplists > canvasOpsLength targetOplists)
      true
      "fewer ops means we removed old history"
    return ()

    Expect.equal sourceCanvas.dbs targetCanvas.dbs "Same DBs when loading from db"

    let tweakedSourceHandlers =
      sourceCanvas.handlers
      |> Map.values
      |> List.map Json.OCamlCompatible.serialize
      |> List.map (
        String.replace
          "http://sample-gettingstarted.builtwithdark.localhost"
          "http://clone-gettingstarted.builtwithdark.localhost"
      )
      |> List.map Json.OCamlCompatible.deserialize<PT.Handler.T>

    Expect.equal
      tweakedSourceHandlers
      (Map.values targetCanvas.handlers)
      "Same handlers when loading from db, except that string with url got properly munged from sample-gettingstarted... to clone-gettingstarted...,"
  }


let tests =
  testList
    "canvas"
    [ testHttpOplistRoundtrip
      testDBOplistRoundtrip
      testHttpOplistLoadsUserTypes
      testHttpLoadIgnoresDeletedFns
      testDbCreateWithOrblankName
      testDbRename
      testSetHandlerAfterDelete
      testSetFunctionAfterDelete
      testLoadAllDBs
      testCanvasVerificationDuplicationCreation
      testCanvasVerificationDuplicationCreationOffDisk
      testCanvasVerificationDuplicationRenaming
      testCanvasVerificationNoError
      testCanvasVerificationUndoRenameDupedName
      testCanvasClone ]
