module Tests.Analysis

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils

open LibExecution.RuntimeTypes
open LibExecution.Shortcuts

module Analysis = LibBackend.Analysis
module Canvas = LibBackend.Canvas
module AT = LibExecution.AnalysisTypes
module PT = LibBackend.ProgramTypes
module TI = LibBackend.TraceInputs
module S = PT.Shortcuts


let testFilterSlash : Test =
  testTask "test that a request which doesnt match doesnt end up in the traces" {
    do! clearCanvasData (CanvasName.create "test-filter_slash")
    let route = "/:rest"
    let handler = testHttpRouteHandler route "GET" (PT.EBlank 0UL)
    let tlid = handler.tlid
    let oplist = [ PT.SetHandler(tlid, { x = 0; y = 0 }, handler) ]
    let! meta = testCanvasInfo "test-filter_slash"
    let! c = Canvas.fromOplist meta oplist

    let d = Canvas.NotDeleted
    do! Canvas.saveTLIDs meta [ tlid, oplist, PT.TLHandler handler, d ]

    let t1 = System.Guid.NewGuid()
    let desc = ("HTTP", "/", "GET")
    let! (_d : System.DateTime) = TI.storeEvent meta.id t1 desc (DStr "1")
    let! loaded = Analysis.traceIDsForHandler c handler
    Expect.equal loaded [ Analysis.traceIDofTLID tlid ] "ids is the default"

    return ()
  }

let tests = testList "Analysis" [ testFilterSlash ]
