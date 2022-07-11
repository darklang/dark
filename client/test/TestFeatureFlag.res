open Prelude
open Tester
open FluidTestData
open FluidShortcuts
module FF = FeatureFlags

@ocaml.doc(" [testWrap name fn expected] tests FeatureFlag.wrap.
 *
 * Recall that FeatureFlag.wrap is called with [wrap ast id].
 *
 * The [fn] here is given the [id] and expected to return the [ast]
 * (as a FluidExpression to make life easier).
 *
 * That means that in order to target a specific expression to be wrapped, use
 * the passed [id] as the ID of that expression when constructing it. For
 * example, to wrap a simple string:
 *   (fun id -> str ~id \"a\")
 *
 * The [expected] value is compared with testEqualIgnoringIds, so should be
 * constructed with FluidShortcuts helpers.
 *
 * [testUnwrap] works the same way, but has two expectations: the result when
 * keeping the old code and the result when keeping the new code. ")
let testWrap = (
  name: string,
  ~state=defaultTestState,
  exprFn: id => FluidExpression.t,
  expected: FluidExpression.t,
) =>
  test(name, () => {
    let id = Prelude.gid()
    let ast = FluidAST.ofExpr(exprFn(id))
    let (_flagId, newAST) = FF.wrap(state, ast, id)
    expect(FluidAST.toExpr(newAST))
    |> withEquality(FluidExpression.testEqualIgnoringIds)
    |> toEqual(expected)
  })

let testUnwrap = (
  name: string,
  exprFn: id => FluidExpression.t,
  ~keepOld: FluidExpression.t,
  ~keepNew: FluidExpression.t,
) => {
  let id = Prelude.gid()
  let ast = FluidAST.ofExpr(exprFn(id))
  test(name ++ "- KeepOld", () => {
    let actualOld = FF.unwrap(FF.KeepOld, ast, id) |> Option.unwrapUnsafe
    expect(FluidAST.toExpr(actualOld))
    |> withEquality(FluidExpression.testEqualIgnoringIds)
    |> toEqual(keepOld)
  })
  test(name ++ "- KeepNew", () => {
    let actualNew = FF.unwrap(FF.KeepNew, ast, id) |> Option.unwrapUnsafe
    expect(FluidAST.toExpr(actualNew))
    |> withEquality(FluidExpression.testEqualIgnoringIds)
    |> toEqual(keepNew)
  })
}

let run = () => {
  describe("FeatureFlag.wrap", () => {
    testWrap(
      "wrapping a simple expression puts expression in the old code",
      id => str(~id, "a"),
      flag(blank(), str("a"), blank()),
    )
    testWrap(
      "wrapping a let puts the RHS in the old code",
      id => let'(~id, "a", int(1), var("a")),
      let'("a", flag(blank(), int(1), blank()), var("a")),
    )
    testWrap(
      "wrapping a select-all wraps entire thing",
      ~state={...defaultTestState, newPos: 0, selectionStart: Some(12)},
      id => let'(~id, "a", int(1), var("a")),
      let'("a", flag(blank(), int(1), blank()), var("a")),
    )
    testWrap(
      "does not wrap an expr inside FF condition",
      id => flag(bool(~id, true), str("old"), str("new")),
      flag(bool(true), str("old"), str("new")),
    )
    testWrap(
      "does not wrap an expr inside FF oldCode",
      id => flag(bool(true), str(~id, "old"), str("new")),
      flag(bool(true), str("old"), str("new")),
    )
    testWrap(
      "does not wrap an expr inside FF oldCode",
      id => flag(bool(true), str("old"), str(~id, "new")),
      flag(bool(true), str("old"), str("new")),
    )
    ()
  })
  describe("FeatureFlag.unwrap", () => {
    testUnwrap(
      "unwrapping a simple expression leaves old code",
      id => flag(~id, blank(), str("old"), str("new")),
      ~keepOld=str("old"),
      ~keepNew=str("new"),
    )
    testUnwrap(
      "unwrapping from within the old leaves the old code",
      id => flag(blank(), str(~id, "old"), str("new")),
      ~keepOld=str("old"),
      ~keepNew=str("new"),
    )
    testUnwrap(
      "unwrapping from within the new code leaves the old code",
      id => flag(blank(), str("old"), str(~id, "new")),
      ~keepOld=str("old"),
      ~keepNew=str("new"),
    )
    ()
  })
}
