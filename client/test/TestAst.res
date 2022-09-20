open Tester
open Prelude
open AST
module B = BlankOr
open ProgramTypes.Expr
open ProgramTypes.MatchPattern
open FluidShortcuts

type transformation_test_result<'a, 'b> =
  | Pass
  | Fail('a, 'b)

let run = () => {
  describe("freeVariables", () => {
    let id1 = ID.fromInt(5)
    let id2 = ID.fromInt(10)
    let id3 = ID.fromInt(11)
    let id4 = ID.fromInt(12)
    let id5 = ID.fromInt(15)
    test("lambda var is not free", () =>
      expect(
        freeVariables(ELambda(id1, list{(id2, "var")}, EVariable(id3, "var"))),
      ) |> toEqual(list{})
    )
    test("match pattern is not free", () => {
      let e = EConstructor(id2, "Just", list{EVariable(id4, "request")})
      let pats = list{
        (MPConstructor(id1, "Just", list{MPVariable(id1, "anything")}), EVariable(id5, "anything")),
      }

      expect(freeVariables(EMatch(id1, e, pats))) |> toEqual(list{(id4, "request")})
    })
  })
  describe("getArguments", () => {
    test("getArguments of binop works", () => {
      let arg1 = int(4)
      let arg2 = str("asf")
      let e = binop("+", arg1, arg2)
      let ast = FluidAST.ofExpr(e)
      expect(getArguments(E.toID(e), ast)) |> toEqual(list{arg1, arg2})
    })
    test("getArguments of fncall works", () => {
      let arg1 = int(4)
      let arg2 = str("asf")
      let e = fn(~mod="Int", "add", list{arg1, arg2})
      let ast = FluidAST.ofExpr(e)
      expect(getArguments(E.toID(e), ast)) |> toEqual(list{arg1, arg2})
    })
    test("getArguments of pipe works", () => {
      let id = gid()
      let arg1 = int(4)
      let arg2 = str("asf")
      let e = pipe(arg1, fn(~id, "Int::add", list{pipeTarget, arg2}), list{})
      let ast = FluidAST.ofExpr(e)
      expect(getArguments(id, ast)) |> toEqual(list{arg1, arg2})
    })
  })
  describe("getParamIndex", () => {
    test("getParamIndex of pipe works", () => {
      let id1 = gid()
      let id2 = gid()
      let e = pipe(
        str(~id=id1, "asd"),
        fn(~mod="String", "append", list{pipeTarget, EBlank(id2)}),
        list{},
      )

      let ast = FluidAST.ofExpr(e)
      expect((getParamIndex(id1, ast), getParamIndex(id2, ast))) |> toEqual((
        Some("String::append", 0),
        Some("String::append", 1),
      ))
    })
  })
  describe("variablesIn", () => {
    test("variablesIn correctly identifies available vars in let RHS with incomplete LHS", () => {
      let testID = ID.fromInt(923478769)
      let inner = ELet(gid(), "", EBlank(testID), E.newB())
      let outer = ELet(gid(), "variable", int(4), inner)
      let vars = variablesIn(outer)->Map.get(~key=testID)
      let varsFor = vars |> Option.map(~f=d => Map.keys(d))
      expect(varsFor) |> toEqual(Some(list{"variable"}))
    })
    test("variablesIn correctly gets rhs id of latest let definition", () => {
      let let1ID = ID.fromInt(45683422)
      let let2ID = ID.fromInt(2388325)
      let a1ID = ID.fromInt(92305834)
      let a1 = int(~id=a1ID, 4)
      let a2ID = ID.fromInt(23353463)
      let a2 = int(~id=a2ID, 9)
      let lastBlankID = ID.fromInt(93490346)
      let lastBlank = EBlank(lastBlankID)
      let ast = ELet(let1ID, "a", a1, ELet(let2ID, "a", a2, lastBlank))
      expect(
        variablesIn(ast)
        |> Map.get(~key=lastBlankID)
        |> Option.andThen(~f=d => Map.get(~key="a", d)),
      ) |> toEqual(Some(a2ID))
    })
    test("variablesIn correctly gets the id of a pattern variable", () => {
      let id1 = gid()
      let targetID = gid()
      let b1 = blank()
      let target = EBlank(targetID)
      let ast = match'(b1, list{(pConstructor("Just", list{pVar(~id=id1, "myvar")}), target)})

      expect(
        variablesIn(ast)
        |> Map.get(~key=targetID)
        |> Option.andThen(~f=d => Map.get(~key="myvar", d)),
      ) |> toEqual(Some(id1))
    })
  })
  describe("removePartials", () => {
    let b = () => EBlank(gid())
    test("No changes when blank", () => {
      let expr = b()
      expect(removePartials(expr)) |> toEqual(expr)
    })
    test("No changes when not-partial", () => {
      let expr = fn("+", list{EInteger(gid(), 3L), EInteger(gid(), 9L)})

      expect(removePartials(expr)) |> toEqual(expr)
    })
    test("Updates AST when there's a partial in fn args", () => {
      let fnid = gid()
      let argid = gid()
      let blank = b()
      let expr = fn(~id=fnid, "+", list{EInteger(argid, 3L), EPartial(gid(), "abc", blank)})

      expect(removePartials(expr)) |> toEqual(fn(~id=fnid, "+", list{EInteger(argid, 3L), blank}))
    })
    test("Updates AST when there's a fn rename partial", () => {
      let fnid = gid()
      let b1 = b()
      let b2 = b()
      let expr = ERightPartial(gid(), "Int::a", fn(~id=fnid, ~mod="Int", "add", list{b1, b2}))

      expect(removePartials(expr)) |> toEqual(fn(~id=fnid, ~mod="Int", "add", list{b1, b2}))
    })
    test("Updates AST when there is a left partial", () => {
      let str = EString(gid(), "a string")
      let expr = ELeftPartial(gid(), "if", str)
      expect(removePartials(expr)) |> toEqual(str)
    })
    ()
  })
}
