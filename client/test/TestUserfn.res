open Tester
open! Tc
open Prelude

open UserFunctions
module B = BlankOr
module D = Defaults

let defaultTLID = gtlid()

let defaultFluidExpr = ProgramTypes.Expr.EBlank(gid())

let defaultFnName = "myFun"

let aHandler = (~tlid=defaultTLID, ()): toplevel => {
  let spec = PT.Handler.Spec.HTTP("", "", {moduleID: gid(), modifierID: gid(), nameID: gid()})
  TLHandler({ast: FluidAST.ofExpr(defaultFluidExpr), spec: spec, tlid: tlid, pos: Pos.center})
}

let aFn = (
  ~tlid=defaultTLID,
  ~name=defaultFnName,
  ~params=list{},
  ~expr=defaultFluidExpr,
  (),
): toplevel => TLFunc({
  tlid: tlid,
  name: name,
  nameID: gid(),
  parameters: params,
  description: "",
  returnType: TVariable("x"),
  returnTypeID: gid(),
  infix: false,
  body: FluidAST.ofExpr(expr),
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
