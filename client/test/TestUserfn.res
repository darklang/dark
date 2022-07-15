open Tester
open! Tc
open Prelude
open UserFunctions
module B = BlankOr
module D = Defaults

let defaultTLID = gtlid()

let defaultFluidExpr = ProgramTypes.Expr.EBlank(gid())

let defaultPos = {x: 0, y: 0}

let defaultFnName = "myFun"

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

let aFn = (
  ~tlid=defaultTLID,
  ~name=defaultFnName,
  ~params=list{},
  ~expr=defaultFluidExpr,
  (),
): toplevel => TLFunc({
  tlid: tlid,
  metadata: {
    name: F(gid(), name),
    parameters: params,
    description: "",
    returnType: F(gid(), TAny),
    infix: false,
  },
  ast: FluidAST.ofExpr(expr),
})

let run = () => {
  describe("canDelete", () => {
    test("can delete a function with no used-in references", () =>
      expect(canDelete(list{}, defaultTLID)) |> toEqual(true)
    )
    test("cannot delete a function with used-in references from elsewhere", () => {
      let caller = aHandler(~tlid=TLID.fromInt(1), ())
      expect(canDelete(list{caller}, defaultTLID)) |> toEqual(false)
    })
    test("can delete if only used-in references are itself", () => {
      let fn = aFn()
      expect(canDelete(list{fn}, defaultTLID)) |> toEqual(true)
    })
    test("cannot delete if any one of the used-in references is from elsewhere", () => {
      let fn = aFn()
      let caller = aHandler(~tlid=TLID.fromInt(1), ())
      expect(canDelete(list{fn, caller}, defaultTLID)) |> toEqual(false)
    })
    ()
  })
  ()
}
