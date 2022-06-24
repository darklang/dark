// Tests for functions in Fluid.ml that does not conform well with the tests in fluid-test.ml

open Tester
open Prelude
open Fluid
open FluidTestData
open FluidShortcuts
open ProgramTypes.Expr

let run = () => {
  describe("getSelectedExprID", () => {
    let aiFor = (ast, s) => ASTInfo.make(defaultTestProps, FluidAST.ofExpr(ast), s)
    test("nothing selected", () => {
      let s = {...defaultTestState, newPos: 2}
      let ast = plainIf
      expect(getSelectedExprID(aiFor(ast, s))) |> toEqual(None)
    })
    test("select atomic expression", () => {
      let ast = let'("a", EInteger(ID("letVal"), "1999"), b)
      let s = {
        ...defaultTestState,
        oldPos: 8,
        newPos: 12,
        selectionStart: Some(8),
      }

      expect(getSelectedExprID(aiFor(ast, s))) |> toEqual(Some(ID.fromString("letVal")))
    })
    test("select larger expressions", () => {
      let ast = EFnCall(ID("fn"), "+", list{int(1), int(2)}, NoRail)
      let s = {
        ...defaultTestState,
        oldPos: 0,
        newPos: 4,
        selectionStart: Some(0),
      }

      expect(getSelectedExprID(aiFor(ast, s))) |> toEqual(Some(ID.fromString("fn")))
    })
    test("selects part of AST", () => {
      let ast = let'("a", EFnCall(ID("fn"), "+", list{int(1), int(2)}, NoRail), b)

      let s = {
        ...defaultTestState,
        oldPos: 8,
        newPos: 12,
        selectionStart: Some(8),
      }

      expect(getSelectedExprID(aiFor(ast, s))) |> toEqual(Some(ID.fromString("fn")))
    })
    ()
  })
  ()
}
