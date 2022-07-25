open Tester
open! Tc
open Prelude
module B = BlankOr
module D = Defaults

open AppTypes.Page

let defaultHTTPSpec = PT.Handler.Spec.newHTTP("/", "GET")

let defaultREPLSpec = PT.Handler.Spec.newREPL("adjectiveNoun")

let defaultCronSpec = PT.Handler.Spec.newCron("daily", Some(PT.Handler.Spec.CronInterval.EveryDay))

let defaultWorkerSpec = PT.Handler.Spec.newWorker("sink")

let defaultSpec = defaultHTTPSpec

let defaultTLID = gtlid()

let defaultFluidExpr = ProgramTypes.Expr.EBlank(gid())

let aHandler = (
  ~tlid=defaultTLID,
  ~expr=defaultFluidExpr,
  ~pos=Pos.center,
  ~spec=defaultSpec,
  (),
): toplevel => {
  TLHandler({ast: FluidAST.ofExpr(expr), spec: spec, tlid: tlid, pos: pos})
}

let run = () => {
  describe("calculatePanOffset", () => {
    let m = AppTypes.Model.default
    let tl = aHandler(~pos={x: 500, y: 500}, ())
    test("do not update canvasProps if center=false", () => {
      let page = FocusedHandler(defaultTLID, None, false)
      let newM = Page.calculatePanOffset(m, tl, page)
      expect(m.canvasProps.offset == newM.canvasProps.offset) |> toEqual(true)
    })
    test("update canvasProps if center=true", () => {
      let page = FocusedHandler(defaultTLID, None, true)
      let newM = Page.calculatePanOffset(m, tl, page)
      expect(m.canvasProps.offset == newM.canvasProps.offset) |> toEqual(false)
    })
    ()
  })
  ()
}
