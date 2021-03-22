module Tests.Undo

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils

module Canvas = LibBackend.Canvas
module PT = LibBackend.ProgramTypes
module S = PT.Shortcuts

let hop (h : PT.Handler.T) = PT.SetHandler(h.tlid, h.pos, h)


let testUndoFns : Test =
  testTask "test undo functions" {
    let handler code = testHttpRouteHandler "" "GET" (FSharpToExpr.parsePTExpr code)
    let tlid = 7UL
    let n1 = PT.TLSavepoint tlid
    let n2 = hop (handler "blank - blank")
    let n3 = hop (handler "blank - 3")
    let n4 = hop (handler "3 - 4")
    let u = PT.UndoTL tlid
    let ops = [ n1; n1; n1; n1; n2; n3; n4; u; u; u ]
    Expect.equal (LibBackend.Undo.undoCount ops tlid) 3 "undocount"
  }

//
// let t_undo () =
//   clear_test_data () ;
//   let ha ast = hop (handler ast) in
//   let sp = TLSavepoint tlid in
//   let u = UndoTL tlid in
//   let r = RedoTL tlid in
//   let o1 = int 1 in
//   let o2 = int 2 in
//   let o3 = int 3 in
//   let o4 = int 4 in
//   let o5 = int 5 in
//   let ops = [sp; ha o1; sp; ha o2; sp; ha o3; sp; ha o4; sp; ha o5] in
//   (* Check assumptions *)
//   execute_ops ops |> check_dval "t_undo_1" (Dval.dint 5) ;
//   (* First undo *)
//   execute_ops (List.concat [ops; [u]]) |> check_dval "t_undo_3" (Dval.dint 4) ;
//   (* Second undo *)
//   execute_ops (List.concat [ops; [u; u]]) |> check_dval "t_undo_4" (Dval.dint 3) ;
//   (* First redo *)
//   execute_ops (List.concat [ops; [u; u; r]])
//   |> check_dval "t_undo_5" (Dval.dint 4) ;
//   (* Second redo *)
//   execute_ops (List.concat [ops; [u; u; r; r]])
//   |> check_dval "t_undo_6" (Dval.dint 5) ;
//   (* Another undo *)
//   execute_ops (List.concat [ops; [u; u; r; r; u]])
//   |> check_dval "t_undo_7" (Dval.dint 4) ;
//   (* Another redo *)
//   execute_ops (List.concat [ops; [u; u; r; r; u; r]])
//   |> check_dval "t_undo_8" (Dval.dint 5)
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

let tests = testList "canvas" [ testUndoFns ]
