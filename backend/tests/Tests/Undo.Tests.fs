module Tests.Undo

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils.TestUtils

module Canvas = LibBackend.Canvas
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution

let setHandler (h : PT.Handler.T) = PT.SetHandler(h.tlid, h)

let handler code = testHttpRouteHandler "" "GET" (Parser.parsePTExpr code)

let testUndoCount : Test =
  // Creates several save points, (at least as many undos as we will do),
  // sets the handler to 3 different versions, then performs an 'undo' 3 times.
  //
  // This test just ensures that 'undo'
  // was done 3 times. Subsequent tests check that the right AST and such
  // are bound to the handler, when undos take place.
  test "test undo functions" {
    let tlid = 7UL
    let n1 = PT.TLSavepoint tlid
    let n2 = setHandler (handler "blank - blank")
    let n3 = setHandler (handler "blank - 3")
    let n4 = setHandler (handler "3 - 4")
    let u = PT.UndoTL tlid
    let ops = [ n1; n1; n1; n1; n2; n3; n4; u; u; u ]
    Expect.equal (LibBackend.Undo.undoCount ops tlid) 3 "undocount"
  }


let testUndo : Test =
  testTask "test undo" {
    let! meta = initializeTestCanvas (Randomized "undo")
    let tlid = 7UL
    let ha code = setHandler ({ handler code with tlid = tlid })
    let sp = PT.TLSavepoint tlid
    let u = PT.UndoTL tlid
    let r = PT.RedoTL tlid
    let ops = [ sp; ha "1"; sp; ha "2"; sp; ha "3"; sp; ha "4"; sp; ha "5" ]

    let exe (ops : PT.Oplist) =
      task {
        let c = Canvas.fromOplist meta [] ops
        let! state = executionStateFor meta Map.empty Map.empty
        let h =
          Map.get tlid c.handlers
          |> Exception.unwrapOptionInternal "missing handler" [ "tlid", tlid ]
        return! Exe.executeExpr state Map.empty (PT2RT.Expr.toRT h.ast)
      }

    let! v = exe ops
    Expect.equal v (RT.DInt 5L) "check assumptions"

    let! v = exe (ops @ [ u ])
    Expect.equal v (RT.DInt 4L) "first undo"

    let! v = exe (ops @ [ u; u ])
    Expect.equal v (RT.DInt 3L) "second undo"

    let! v = exe (ops @ [ u; u; r ])
    Expect.equal v (RT.DInt 4L) "2 undos and a redo"

    let! v = exe (ops @ [ u; u; r; r ])
    Expect.equal v (RT.DInt 5L) "2 undos and 2 redos"

    let! v = exe (ops @ [ u; u; r; r; u ])
    Expect.equal v (RT.DInt 4L) "2 undos and 2 redos, then another undo"

    let! v = exe (ops @ [ u; u; r; r; u; r ])
    Expect.equal v (RT.DInt 5L) "2 undos and 2 redos, then another undo + redo"
  }

let testCanvasVerificationUndoRenameDupedName : Test =
  testTask "verification triggers in undo/redo case" {
    let dbID = gid ()
    let nameID = gid ()
    let dbID2 = gid ()
    let nameID2 = gid ()
    let! meta = createTestCanvas (Randomized "undo-verification")

    let ops1 =
      [ PT.CreateDBWithBlankOr(dbID, nameID, "Books")
        PT.TLSavepoint dbID
        PT.DeleteTL dbID
        PT.CreateDBWithBlankOr(dbID2, nameID2, "Books") ]

    Canvas.fromOplist meta [] ops1 |> ignore<Canvas.T>

    try
      Canvas.fromOplist meta ops1 [ PT.UndoTL dbID ] |> ignore<Canvas.T>
      Expect.isFalse true "should fail to verify"
    with
    | _ ->
      // Expected
      ()
  }

let tests =
  testList
    "undo"
    [ testUndoCount; testUndo; testCanvasVerificationUndoRenameDupedName ]
