open Tester
open Prelude
open FluidTestData
open ProgramTypes.Expr

let makeTL = ast => TLHandler({
  ast: ast,
  spec: {
    space: Blank(gid()),
    name: Blank(gid()),
    modifier: Blank(gid()),
  },
  tlid: TLID.fromInt(7),
  pos: Defaults.origin,
})

let run = () =>
  describe("commandsFor", () => {
    let hasCmd = (name: string, expr: FluidExpression.t, ast: FluidAST.t) => {
      let tl = makeTL(ast)
      FluidCommands.commandsFor(defaultTestModel, tl, expr)
      |> List.find(~f=(c: Types.command) => c.commandName == name)
      |> Option.isSome
    }

    let expectCmd = (name, expr) =>
      expect(hasCmd(name, expr, FluidAST.ofExpr(expr))) |> toEqual(true)

    let expectNoCmd = (name, expr) =>
      expect(hasCmd(name, expr, FluidAST.ofExpr(expr))) |> toEqual(false)

    test("has put on rail for railable NoRail function", () =>
      expectCmd("put-function-on-rail", aRailableFnCall)
    )
    test("no put on rail for non-railable NoRail function", () =>
      expectNoCmd("put-function-on-rail", aFnCall)
    )
    test("has take off rail for Rail function", () =>
      expectCmd("take-function-off-rail", aOnRailFnCall)
    )
    test("no put on rail for Rail function", () =>
      expectNoCmd("put-function-on-rail", aOnRailFnCall)
    )
    test("no take off rail for NoRail function", () =>
      expectNoCmd("take-function-off-rail", aFnCall)
    )
    test("no copy as curl for normal function", () => expectNoCmd("copy-request-as-curl", aFnCall))
    test("has copy as curl for Http function", () => {
      let expr = EFnCall(gid(), "HttpClient::get", list{aStr, emptyRecord, emptyRecord}, Rail)

      expectCmd("copy-request-as-curl", expr)
    })
    test("has add flag for AST without flag", () => expectCmd("add-feature-flag", aFnCall))
    test("no add flag for AST with existing flag", () =>
      expectNoCmd("add-feature-flag", letWithflagBody)
    )
    test("has discard+commit flag for target expr inside flag", () => {
      let targetExpr = oneCharStr
      let ast = ELet(gid(), "a", aShortInt, flagOld(targetExpr)) |> FluidAST.ofExpr

      let hasDiscard = hasCmd("discard-feature-flag", targetExpr, ast)
      let hasCommit = hasCmd("commit-feature-flag", targetExpr, ast)
      expect((hasDiscard, hasCommit)) |> toEqual((true, true))
    })
    test("no discard or commit flag for target expr outside flag", () => {
      let targetExpr = aShortInt
      let ast = ELet(gid(), "a", targetExpr, flagOld(oneCharStr)) |> FluidAST.ofExpr

      let hasDiscard = hasCmd("discard-feature-flag", targetExpr, ast)
      let hasCommit = hasCmd("commit-feature-flag", targetExpr, ast)
      expect((hasDiscard, hasCommit)) |> toEqual((false, false))
    })
    test("has convert to if for if expression", () => expectCmd("convert-if-to-match", emptyIf))
    test("no convert to if for match expression", () =>
      expectNoCmd("convert-if-to-match", emptyMatch)
    )
    ()
  })
