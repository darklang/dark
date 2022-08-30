// Tests for functions in Fluid.ml that does not conform well with the tests in fluid-test.ml

open Tester
open Prelude
open Fluid
open FluidTestData
open FluidShortcuts
open ProgramTypes.Expr

let run = () => {
  describe("getSelectedExprID", () => {
    let aiFor = (ast, s) => ASTInfo.make(FluidAST.ofExpr(ast), s)
    test("nothing selected", () => {
      let s = {...defaultTestState, newPos: 2}
      let ast = plainIf
      expect(getSelectedExprID(aiFor(ast, s))) |> toEqual(None)
    })
    test("select atomic expression", () => {
      let id = gid()
      let ast = let'("a", EInteger(id, 1999L), b)
      let s = {
        ...defaultTestState,
        oldPos: 8,
        newPos: 12,
        selectionStart: Some(8),
      }

      expect(getSelectedExprID(aiFor(ast, s))) |> toEqual(Some(id))
    })
    test("select larger expressions", () => {
      let id = gid()
      let ast = fn(~id, "+", list{int(1), int(2)})
      let s = {
        ...defaultTestState,
        oldPos: 0,
        newPos: 4,
        selectionStart: Some(0),
      }

      expect(getSelectedExprID(aiFor(ast, s))) |> toEqual(Some(id))
    })
    test("selects part of AST", () => {
      let id = gid()
      let ast = let'("a", fn(~id, "+", list{int(1), int(2)}), b)

      let s = {
        ...defaultTestState,
        oldPos: 8,
        newPos: 12,
        selectionStart: Some(8),
      }

      expect(getSelectedExprID(aiFor(ast, s))) |> toEqual(Some(id))
    })
    ()
  })
  ()
}
