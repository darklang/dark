open Prelude
open Tester
open FluidShortcuts
module E = FluidExpression

let gid = Shared.gid

let run = () => {
  describe("decendants", () => {
    /* [t f] helps test the decendants function
     *
     * It expects a single function [f], which should return the expression on
     * which [decendants] is called.
     *
     * [f] is passed a function (unit -> ID.t) that should be called to
     * generate an ID for each decendant expression that is expected in the
     * result. Each time this generator function is called, the generated ID
     * is tracked. The test assertion is that all generated IDs appear in the
     * expression's [decendants] list. */
    let t = (name: string, f: (unit => ID.t) => E.t) => {
      let generatedIDs = ref(ID.Set.empty)
      let idGenerator = () => {
        let id = gid()
        generatedIDs := Set.add(generatedIDs.contents, ~value=id)
        id
      }

      let expr = f(idGenerator)
      let decendantIDs = E.decendants(expr) |> ID.Set.fromList
      test(name, () =>
        expect(decendantIDs) |> withEquality(Belt.Set.eq) |> toEqual(generatedIDs.contents)
      )
    }

    t("simple expression", gid => int(~id=gid(), 5))
    t("complex expression", gid => binop(~id=gid(), "+", int(~id=gid(), 2), int(~id=gid(), 3)))
    t("let record", gid =>
      let'(
        ~id=gid(),
        "r",
        record(~id=gid(), list{("one", int(~id=gid(), 1)), ("two", int(~id=gid(), 2))}),
        var(~id=gid(), "r"),
      )
    )
    ()
  })
  describe("testEqualIgnoringIds", () => {
    let t = (expected: bool, name: string, e1: E.t, e2: E.t): unit =>
      test(name, () => expect(E.testEqualIgnoringIds(e1, e2)) |> toEqual(expected))

    let eq = t(true)
    let neq = t(false)
    eq("int with same values", int(1), int(1))
    neq("int with diff values", int(1), int(2))
    eq("float with same values", float'(1, 0), float'(1, 0))
    neq("float with diff values", float'(1, 0), float'(2, 0))
    eq("string with same values", str("a"), str("a"))
    neq("string with diff values", str("a"), str("b"))
    eq("bool with same values", bool(true), bool(true))
    neq("bool with diff values", bool(true), bool(false))
    eq("nulls are equal", ENull(gid()), ENull(gid()))
    eq("blanks are equal", blank(), blank())
    eq("list with same values", list(list{int(1), str("a")}), list(list{int(1), str("a")}))
    neq("list and empty list", list(list{}), list(list{int(2), str("a")}))
    neq("list with diff values", list(list{int(1), str("a")}), list(list{int(2), str("a")}))
    eq("let with same values", let'("a", int(1), var("a")), let'("a", int(1), var("a")))
    eq("ifs with same values", if'(bool(true), int(1), int(2)), if'(bool(true), int(1), int(2)))
    neq(
      "ifs with diff condition",
      if'(bool(true), int(1), int(2)),
      if'(bool(false), int(1), int(2)),
    )
    neq("ifs with diff then", if'(bool(true), int(1), int(2)), if'(bool(true), int(2), int(2)))
    neq("ifs with diff else", if'(bool(true), int(1), int(2)), if'(bool(true), int(1), int(3)))
    eq("fncall with same values", fn("add", list{int(1), int(2)}), fn("add", list{int(1), int(2)}))
    neq("fncall with diff name", fn("add", list{int(1), int(2)}), fn("sub", list{int(1), int(2)}))
    neq("fncall with diff args", fn("add", list{int(1), int(2)}), fn("add", list{int(1), int(3)}))
    neq(
      "fncall with diff toRail",
      fn("add", ~ster=NoRail, list{int(1), int(2)}),
      fn("add", ~ster=Rail, list{int(1), int(2)}),
    )
    eq("binop with same values", binop("+", int(1), int(2)), binop("+", int(1), int(2)))
    neq("binop with diff name", binop("+", int(1), int(2)), binop("-", int(1), int(2)))
    neq("binop with diff args", binop("+", int(1), int(2)), binop("+", int(1), int(3)))
    neq(
      "binop with diff toRail",
      binop("+", ~ster=NoRail, int(1), int(2)),
      binop("+", ~ster=Rail, int(1), int(2)),
    )
    eq(
      "record with same values",
      record(list{("a", int(1)), ("b", int(2))}),
      record(list{("a", int(1)), ("b", int(2))}),
    )
    eq(
      "record with same values in different order",
      record(list{("a", int(1)), ("b", int(2))}),
      record(list{("b", int(2)), ("a", int(1))}),
    )
    neq(
      "record with different values",
      record(list{("a", int(1)), ("b", int(2))}),
      record(list{("a", int(2)), ("b", int(2))}),
    )
    eq(
      "field access of same field/expression",
      fieldAccess(record(list{("a", int(1))}), "a"),
      fieldAccess(record(list{("a", int(1))}), "a"),
    )
    neq(
      "field access of diff expr",
      fieldAccess(record(list{("a", int(1))}), "a"),
      fieldAccess(record(list{("b", int(2))}), "a"),
    )
    neq(
      "field access of diff field",
      fieldAccess(record(list{("a", int(1))}), "a"),
      fieldAccess(record(list{("a", int(1))}), "b"),
    )
    eq(
      "pipe with same list",
      pipe(list(list{}), list{fn("reverse", list{pipeTarget})}),
      pipe(list(list{}), list{fn("reverse", list{pipeTarget})}),
    )
    neq(
      "pipe with diff first",
      pipe(list(list{}), list{fn("reverse", list{pipeTarget})}),
      pipe(list(list{int(1)}), list{fn("reverse", list{pipeTarget})}),
    )
    neq(
      "pipe with diff arg",
      pipe(list(list{}), list{fn("reverse", list{pipeTarget})}),
      pipe(list(list{}), list{fn("length", list{pipeTarget})}),
    )
    eq("flag with same values", flag(bool(true), int(1), int(2)), flag(bool(true), int(1), int(2)))
    eq /* we don't care about flag names right now */(
      "flag with diff names",
      flag(~name="flag-1", bool(true), int(1), int(2)),
      flag(~name="flag-2", bool(true), int(1), int(2)),
    )
    neq(
      "flag with diff condition",
      flag(bool(true), int(1), int(2)),
      flag(bool(false), int(1), int(2)),
    )
    neq(
      "flag with diff old code",
      flag(bool(true), int(1), int(2)),
      flag(bool(true), int(2), int(2)),
    )
    neq(
      "flag with diff new code",
      flag(bool(true), int(1), int(2)),
      flag(bool(true), int(1), int(3)),
    )
    eq("constructor nothings", nothing(), nothing())
    eq("constructor same justs", just(int(1)), just(int(1)))
    eq("nested same constructors", ok(just(int(1))), ok(just(int(1))))
    neq("diff constructors", nothing(), just(int(1)))
    neq("constructor diff justs", just(int(1)), just(int(2)))
    eq(
      "partials with same values",
      partial("List::", fn("List::empty", list{})),
      partial("List::", fn("List::empty", list{})),
    )
    neq(
      "partials with diff strings",
      partial("List::", fn("List::empty", list{})),
      partial("Dict::", fn("List::empty", list{})),
    )
    neq(
      "partials with diff exprs",
      partial("List::", fn("List::empty", list{})),
      partial("Dict::", fn("List::singleton", list{int(1)})),
    )
    eq(
      "left partials with same values",
      leftPartial("List::", fn("List::empty", list{})),
      leftPartial("List::", fn("List::empty", list{})),
    )
    neq(
      "left partials with diff strings",
      leftPartial("List::", fn("List::empty", list{})),
      leftPartial("Dict::", fn("List::empty", list{})),
    )
    neq(
      "left partials with diff exprs",
      leftPartial("List::", fn("List::empty", list{})),
      leftPartial("List::", fn("List::singleton", list{int(1)})),
    )
    eq(
      "right partials with same values",
      rightPartial("++", str("foo")),
      rightPartial("++", str("foo")),
    )
    neq(
      "right partials with diff strings",
      rightPartial("=", str("foo")),
      rightPartial("++", str("foo")),
    )
    neq(
      "right partials with diff exprs",
      rightPartial("++", str("foo")),
      rightPartial("++", str("bar")),
    )
    describe("renameVariableUses", () => {
      test("with variable used in right hand side", () => {
        let exprAst = let'("a", var("x"), E.newB())
        expect(
          E.renameVariableUses(~oldName="x", ~newName="x1", exprAst) |> FluidPrinter.eToTestString,
        ) |> toEqual("let a = x1\n___")
      })
      test("with rebound let expression", () => {
        let exprAst = let'("x", int(6), let'("a", var("x"), E.newB()))
        expect(
          E.renameVariableUses(~oldName="x", ~newName="x1", exprAst) |> FluidPrinter.eToTestString,
        ) |> toEqual("let x = 6\nlet a = x\n___")
      })
      test("ith rebound let expression and used as variable in right hand side", () => {
        let exprAst = let'("x", fn("Int::add", list{var("x"), int(6)}), E.newB())

        expect(
          E.renameVariableUses(~oldName="x", ~newName="x1", exprAst) |> FluidPrinter.eToTestString,
        ) |> toEqual("let x = Int::add x1 6\n___")
      })
    })
    ()
  })
}
