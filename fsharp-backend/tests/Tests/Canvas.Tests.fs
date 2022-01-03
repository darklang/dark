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
    let! meta = initializeTestCanvas "db-set-handlder-after-delete"
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


// let t_load_all_dbs_from_cache () =
//   clear_test_data () ;
//   let host = "test-http_oplist_loads_user_tipes" in
//   let oplist =
//     [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
//     ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books2")
//     ; CreateDBWithBlankOr (dbid3, pos, nameid3, "Books3")
//     ; DeleteTL dbid ]
//   in
//   let c1 = ops2c_exn host oplist in
//   Canvas.serialize_only [dbid; dbid2; dbid3] !c1 ;
//   let c2 =
//     Canvas.load_all_dbs_from_cache host
//     |> Result.map_error ~f:(String.concat ~sep:", ")
//     |> Prelude.Result.ok_or_internal_exception "Canvas load error"
//   in
//   AT.check AT.bool "dbs are loaded from cache" true (!c2.ops = []) ;
//   AT.check
//     (AT.list testable_id)
//     "Loaded only undeleted dbs"
//     (List.sort ~compare:compare_id [dbid2; dbid3])
//     (!c2.dbs |> IDMap.keys |> List.sort ~compare:compare_id)
//
//
// let t_canvas_verification_duplicate_creation () =
//   let ops =
//     [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
//     ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books") ]
//   in
//   let c = ops2c "test-verify_create" ops in
//   AT.check AT.bool "should not verify" false (Result.is_ok c)
//
//
// let t_canvas_verification_duplicate_creation_off_disk () =
//   clear_test_data () ;
//   let host = "test-verify_rename" in
//   let ops = [CreateDBWithBlankOr (dbid, pos, nameid, "Books")] in
//   let c1 = ops2c_exn host ops in
//   Canvas.serialize_only [dbid] !c1 ;
//   let c2 =
//     let ops = [CreateDBWithBlankOr (dbid2, pos, nameid, "Books")] in
//     match Op.required_context_to_validate_oplist ops with
//     | NoContext ->
//         Canvas.load_only_tlids ~tlids:[dbid2] host ops
//     | AllDatastores ->
//         Canvas.load_with_dbs ~tlids:[dbid2] host ops
//   in
//   AT.check AT.bool "should not verify" false (Result.is_ok c2)
//
//
// let t_canvas_verification_duplicate_renaming () =
//   let ops =
//     [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
//     ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books2")
//     ; RenameDBname (dbid2, "Books") ]
//   in
//   let c = ops2c "test-verify_rename" ops in
//   AT.check AT.bool "should not verify" false (Result.is_ok c)
//
//
// let t_canvas_verification_no_error () =
//   let ops =
//     [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
//     ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books2") ]
//   in
//   let c = ops2c "test-verify_okay" ops in
//   AT.check AT.bool "should verify" true (Result.is_ok c)
//
//
// let t_canvas_verification_undo_rename_duped_name () =
//   let ops1 =
//     [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
//     ; TLSavepoint dbid
//     ; DeleteTL dbid
//     ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books") ]
//   in
//   let c = ops2c "test-verify_undo_1" ops1 in
//   AT.check AT.bool "should initially verify" true (Result.is_ok c) ;
//   let ops2 = ops1 @ [UndoTL dbid] in
//   let c2 = ops2c "test-verify_undo_2" ops2 in
//   AT.check AT.bool "should then fail to verify" false (Result.is_ok c2)
//
//
// let t_canvas_clone () =
//   Canvas.load_and_resave_from_test_file "sample-gettingstarted" ;
//   Account.insert_user
//     ~username:"clone"
//     ~email:"clone@example.com"
//     ~name:"clone"
//     ()
//   |> Result.ok_or_Exception.raiseInternal ;
//   Canvas_clone.clone_canvas
//     ~from_canvas_name:"sample-gettingstarted"
//     ~to_canvas_name:"clone-gettingstarted"
//     ~preserve_history:false
//   |> Result.ok_or_Exception.raiseInternal ;
//   let sample_canvas =
//     Canvas.load_all "sample-gettingstarted" []
//     |> Tc.Result.map_error (String.concat ~sep:", ")
//     |> Result.ok_or_Exception.raiseInternal
//   in
//   let cloned_canvas : Canvas.canvas ref =
//     Canvas.load_all "clone-gettingstarted" []
//     |> Tc.Result.map_error (String.concat ~sep:", ")
//     |> Result.ok_or_Exception.raiseInternal
//   in
//   let cloned_canvas_from_cache : Canvas.canvas ref =
//     Canvas.load_all_from_cache "clone-gettingstarted"
//     |> Tc.Result.map_error (String.concat ~sep:", ")
//     |> Result.ok_or_Exception.raiseInternal
//   in
//   (* canvas.ops is not [op list], it is [(tlid, op list) list] *)
//   let canvas_ops_length (c : Canvas.canvas) =
//     c.ops |> List.map ~f:snd |> List.join |> List.length
//   in
//   let has_creation_ops (c : Canvas.canvas) =
//     List.map c.ops ~f:(fun (_, ops) ->
//         Canvas_clone.only_ops_since_last_savepoint ops
//         |> Tablecloth.List.any ~f:Canvas_clone.is_op_that_creates_toplevel)
//     |> Tablecloth.List.all ~f:(fun res -> res)
//   in
//   AT.check
//     AT.bool
//     "only_ops_since_last_savepoint retrieve latest ops from the last complete op"
//     true
//     (has_creation_ops !sample_canvas) ;
//   AT.check
//     AT.bool
//     "fewer ops means we removed old history"
//     true
//     (canvas_ops_length !cloned_canvas < canvas_ops_length !sample_canvas) ;
//   AT.check
//     AT.bool
//     "Same DBs when loading from db"
//     true
//     (Toplevel.equal_toplevels !sample_canvas.dbs !cloned_canvas.dbs) ;
//   AT.check
//     AT.string
//     "Same handlers when loading from db, except that string with url got properly munged from sample-gettingstarted... to clone-gettingstarted...,"
//     ( !sample_canvas.handlers
//     |> Toplevel.toplevels_to_yojson
//     |> Yojson.Safe.to_string
//     |> fun s ->
//     Libexecution.Util.string_replace
//       "http://sample-gettingstarted.builtwithdark.localhost"
//       "http://clone-gettingstarted.builtwithdark.localhost"
//       s )
//     ( !cloned_canvas.handlers
//     |> Toplevel.toplevels_to_yojson
//     |> Yojson.Safe.to_string ) ;
//   AT.check
//     AT.bool
//     "Same DBs when loading from cache"
//     true
//     (Toplevel.equal_toplevels !sample_canvas.dbs !cloned_canvas_from_cache.dbs) ;
//   AT.check
//     AT.string
//     "Same handlers when loading from cache, except that string with url got properly munged from sample-gettingstarted... to clone-gettingstarted...,"
//     ( !sample_canvas.handlers
//     |> Toplevel.toplevels_to_yojson
//     |> Yojson.Safe.to_string
//     |> fun s ->
//     Libexecution.Util.string_replace
//       "http://sample-gettingstarted.builtwithdark.localhost"
//       "http://clone-gettingstarted.builtwithdark.localhost"
//       s )
//     ( !cloned_canvas_from_cache.handlers
//     |> Toplevel.toplevels_to_yojson
//     |> Yojson.Safe.to_string )


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
      testSetFunctionAfterDelete ]
