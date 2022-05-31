open Tester
open! Tc
open Prelude
module B = BlankOr
module D = Defaults

let defaultTLID = gtlid()

let defaultExpr = B.new_()

let defaultFluidExpr = FluidExpression.EBlank(gid())

let defaultPos = {x: 0, y: 0}

let aHandler = (
  ~tlid=defaultTLID,
  ~expr=defaultFluidExpr,
  ~pos=defaultPos,
  ~space: option<string>=None,
  (),
): toplevel => {
  let space = switch space {
  | None => B.new_()
  | Some(name) => B.newF(name)
  }
  let spec = {space: space, name: B.new_(), modifier: B.new_()}
  TLHandler({ast: FluidAST.ofExpr(expr), spec: spec, hTLID: tlid, pos: pos})
}

let run = () => {
  describe("calculatePanOffset", () => {
    let m = D.defaultModel
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
