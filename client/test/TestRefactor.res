open Prelude
open Tester
open FluidTestData
module B = BlankOr
module D = Defaults
module R = Refactor
module TL = Toplevel
module E = FluidExpression
open FluidShortcuts
open ProgramTypes.Expr

let sampleFunctions: list<RT.BuiltInFn.t> = {
  let par = (~description="", ~args=list{}, name, typ): RT.BuiltInFn.Param.t => {
    name: name,
    typ: typ,
    args: args,
    description: description,
  }

  list{
    {
      name: {module_: "Int", function: "add", version: 0},
      parameters: list{par("a", TInt), par("b", TInt)},
      returnType: TInt,
      description: "",
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "List", function: "getAt", version: 2},
      parameters: list{par("list", TList), par("index", TInt)},
      returnType: TOption,
      description: "",
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "Dict", function: "map", version: 2},
      parameters: list{par("dict", TObj), par("f", TBlock, ~args=list{"key", "value"})},
      returnType: TObj,
      description: "",
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "DB", function: "set", version: 1},
      parameters: list{par("val", TObj), par("key", TStr), par("table", TDB)},
      returnType: TObj,
      description: "",
      previewable: Impure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
  }
}

let defaultTLID = TLID.fromInt(7)

let defaultHandler = {
  hTLID: defaultTLID,
  pos: {x: 0, y: 0},
  ast: FluidAST.ofExpr(EBlank(gid())),
  spec: {space: B.newF("HTTP"), name: B.newF("/src"), modifier: B.newF("POST")},
}

let aFn = (name, expr): userFunction => {
  ufTLID: gtlid(),
  ufMetadata: {
    ufmName: F(gid(), name),
    ufmParameters: list{},
    ufmDescription: "",
    ufmReturnTipe: F(gid(), TAny),
    ufmInfix: false,
  },
  ufAST: FluidAST.ofExpr(expr),
}

let run = () => {
  describe("takeOffRail & putOnRail", () => {
    let f1: RT.BuiltInFn.t = {
      name: {module_: "Result", function: "resulty", version: 0},
      parameters: list{},
      description: "",
      returnType: TResult,
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    }

    let f2: RT.BuiltInFn.t = {
      name: {module_: "Int", function: "notResulty", version: 0},
      parameters: list{},
      description: "",
      returnType: TInt,
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    }

    let model = hs => {
      ...D.defaultModel,
      functions: Functions.empty |> Functions.setBuiltins(list{f1, f2}, defaultFunctionsProps),
      handlers: Handlers.fromList(hs),
    }

    let handlerWithPointer = (fnName, fnRail) => {
      let id = ID.fromInt(1231241)
      let ast = FluidAST.ofExpr(EFnCall(id, fnName, list{}, fnRail))
      ({...defaultHandler, ast: ast}, id)
    }

    let init = (fnName, fnRail) => {
      let (h, pd) = handlerWithPointer(fnName, fnRail)
      let m = model(list{h})
      (m, h, pd)
    }

    test("toggles any fncall off rail", () => {
      let (m, h, id) = init("Int::notResulty", Rail)
      let mod' = Refactor.takeOffRail(m, TLHandler(h), id)
      let res = switch mod' {
      | AddOps(list{SetHandler(_, _, h)}, _) =>
        switch FluidAST.toExpr(h.ast) {
        | EFnCall(_, "Int::notResulty", list{}, NoRail) => true
        | _ => false
        }
      | _ => false
      }

      expect(res) |> toEqual(true)
    })
    test("toggles any fncall off rail in a thread", () => {
      let fn = fn(~ster=Rail, "List::getAt_v2", list{pipeTarget, int(5)})
      let ast = pipe(emptyList, fn, list{}) |> FluidAST.ofExpr
      let h = {...defaultHandler, ast: ast}
      let m = model(list{h})
      let id = E.toID(fn)
      // this used to crash or just lose all its arguments
      let mod' = Refactor.takeOffRail(m, TLHandler(h), id)
      let res = switch mod' {
      | AddOps(list{SetHandler(_, _, h)}, _) =>
        switch FluidAST.toExpr(h.ast) {
        | EPipe(
            _,
            EList(_, list{}),
            EFnCall(_, "List::getAt_v2", list{EPipeTarget(_), EInteger(_, 5L)}, NoRail),
            list{},
          ) => true
        | _ => false
        }
      | _ => false
      }

      expect(res) |> toEqual(true)
    })
    test("toggles error-rail-y function onto rail", () => {
      let (m, h, pd) = init("Result::resulty", NoRail)
      let mod' = Refactor.putOnRail(m, TLHandler(h), pd)
      let res = switch mod' {
      | AddOps(list{SetHandler(_, _, h)}, _) =>
        switch FluidAST.toExpr(h.ast) {
        | EFnCall(_, "Result::resulty", list{}, Rail) => true
        | _ => false
        }
      | _ => false
      }

      expect(res) |> toEqual(true)
    })
    test("does not put non-error-rail-y function onto rail", () => {
      let (m, h, pd) = init("Int::notResulty", NoRail)
      let mod' = Refactor.putOnRail(m, TLHandler(h), pd)
      let res = switch mod' {
      | NoChange => true
      | _ => false
      }
      expect(res) |> toEqual(true)
    })
  })
  describe("renameDBReferences", () => {
    let db0: PT.DB.t = {
      tlid: gtlid(),
      name: B.newF("ElmCode"),
      cols: list{},
      version: 0,
      pos: {x: 0, y: 0},
    }

    test("datastore renamed, handler updates variable", () => {
      let h = {
        ast: FluidAST.ofExpr(EVariable(gid(), "ElmCode")),
        spec: {
          space: B.newF("HTTP"),
          name: B.newF("/src"),
          modifier: B.newF("POST"),
        },
        hTLID: TLID.fromInt(5),
        pos: {x: 0, y: 0},
      }

      let f = {
        ufTLID: TLID.fromInt(6),
        ufMetadata: {
          ufmName: B.newF("f-1"),
          ufmParameters: list{},
          ufmDescription: "",
          ufmReturnTipe: B.new_(),
          ufmInfix: false,
        },
        ufAST: FluidAST.ofExpr(EVariable(gid(), "ElmCode")),
      }

      let model = {
        ...D.defaultModel,
        dbs: DB.fromList(list{db0}),
        handlers: Handlers.fromList(list{h}),
        userFunctions: UserFunctions.fromList(list{f}),
      }

      let ops = R.renameDBReferences(model, "ElmCode", "WeirdCode")
      let res = switch List.sortBy(~f=Encoders.tlidOf, ops) {
      | list{SetHandler(_, _, h), SetFunction(f)} =>
        switch (FluidAST.toExpr(h.ast), FluidAST.toExpr(f.ufAST)) {
        | (EVariable(_, "WeirdCode"), EVariable(_, "WeirdCode")) => true
        | _ => false
        }
      | _ => false
      }

      expect(res) |> toEqual(true)
    })
    test("datastore renamed, handler does not change", () => {
      let h = {
        ast: FluidAST.ofExpr(EVariable(gid(), "request")),
        spec: {
          space: B.newF("HTTP"),
          name: B.newF("/src"),
          modifier: B.newF("POST"),
        },
        hTLID: defaultTLID,
        pos: {x: 0, y: 0},
      }

      let model = {
        ...D.defaultModel,
        dbs: DB.fromList(list{db0}),
        handlers: Handlers.fromList(list{h}),
      }

      let ops = R.renameDBReferences(model, "ElmCode", "WeirdCode")
      expect(ops) |> toEqual(list{})
    })
    ()
  })
  describe("generateUserType", () => {
    test("with None input", () =>
      expect(
        switch R.generateUserType(None) {
        | Ok(_) => false
        | Error(_) => true
        },
      ) |> toEqual(true)
    )
    test("with Some non-DObj input", () =>
      expect(
        switch R.generateUserType(Some(DStr("foo"))) {
        | Ok(_) => false
        | Error(_) => true
        },
      ) |> toEqual(true)
    )
    test("with Some DObj input", () => {
      let dobj = Dval.obj(list{
        ("str", DStr("foo")),
        ("int", DInt(1L)),
        ("float", DFloat(1.0)),
        ("obj", Dval.obj(list{})),
        ("date", DDate("2019-07-10T20:42:11Z")),
        ("datestr", DStr("2019-07-10T20:42:11Z")),
        ("uuid", DUuid("0a18ca77-9bae-4dfb-816f-0d12cb81c17b")),
        ("uuidstr", DStr("0a18ca77-9bae-4dfb-816f-0d12cb81c17b")),
      })

      let expectedFields =
        // Note: datestr and uuidstr are TDate and TUuid respectively, _not_ TStr
        list{
          ("str", DType.TStr),
          ("int", TInt),
          ("float", TFloat),
          ("obj", TObj),
          ("date", TDate),
          ("datestr", TStr),
          /* for now, TStr; in future, maybe we coerce to
           TDate */
          ("uuid", TUuid),
          ("uuidstr", TStr),
          /* for now, TStr; in future, maybe we coerce to
           TUuid */
        }
        |> List.map(~f=((k, v)) => (Some(k), Some(v)))
        |> /* sortBy here because the dobj gets sorted - not sure exactly
         where, but order doesn't matter except in this test */
        List.sortBy(~f=((k, _)) => k)

      let _ = (dobj, expectedFields)
      let tipe = R.generateUserType(Some(dobj))
      let fields = switch tipe {
      | Error(_) => list{}
      | Ok(ut) =>
        switch ut.definition {
        | PT.UserType.Definition.UTRecord(utr) =>
          utr |> List.map(~f=(urf: PT.UserType.RecordField.t) => (
            urf.name |> B.toOption,
            urf.typ |> B.toOption,
          ))
        }
      }

      expect(fields) |> toEqual(expectedFields)
    })
  })
  describe("extractVarInAst", () => {
    let modelAndTl = (ast: FluidAST.t) => {
      let hTLID = defaultTLID
      let tl = {
        hTLID: hTLID,
        ast: ast,
        pos: {x: 0, y: 0},
        spec: {
          space: B.newF("HTTP"),
          name: B.newF("/src"),
          modifier: B.newF("POST"),
        },
      }

      let m = {
        ...D.defaultModel,
        functions: Functions.empty |> Functions.setBuiltins(sampleFunctions, defaultFunctionsProps),
        handlers: list{(hTLID, tl)} |> TLID.Dict.fromList,
        fluidState: {...Defaults.defaultFluidState, ac: FluidAutocomplete.init},
      }

      (m, TLHandler(tl))
    }

    test("with sole expression", () => {
      let ast = FluidAST.ofExpr(int(4))
      let (m, tl) = modelAndTl(ast)
      expect(
        R.extractVarInAst(m, tl, FluidAST.toID(ast), "var", ast)
        |> FluidAST.toExpr
        |> FluidPrinter.eToTestString,
      ) |> toEqual("let var = 4\nvar")
    })
    test("with expression inside let", () => {
      let expr = fn("Int::add", list{var("b"), int(4)})
      let ast = FluidAST.ofExpr(let'("b", int(5), expr))
      let (m, tl) = modelAndTl(ast)
      expect(
        R.extractVarInAst(m, tl, E.toID(expr), "var", ast)
        |> FluidAST.toExpr
        |> FluidPrinter.eToTestString,
      ) |> toEqual("let b = 5\nlet var = Int::add b 4\nvar")
    })
    test("with expression inside thread inside let", () => {
      let expr = fn(
        "DB::set_v1",
        list{fieldAccess(var("request"), "body"), fn("toString", list{var("id")}), b},
      )

      let threadedExpr = fn("Dict::set", list{str("id"), var("id")})
      let exprInThread = pipe(expr, threadedExpr, list{})
      let ast = FluidAST.ofExpr(let'("id", fn("Uuid::generate", list{}), exprInThread))

      let (m, tl) = modelAndTl(ast)
      expect(
        R.extractVarInAst(m, tl, E.toID(expr), "var", ast)
        |> FluidAST.toExpr
        |> FluidPrinter.eToTestString,
      ) |> toEqual(
        "let id = Uuid::generate\nlet var = DB::setv1 request.body toString id ___________________\nvar\n|>Dict::set \"id\" id\n",
      )
    })
    ()
  })
  describe("reorderFnCallArgs", () => {
    let matchExpr = (a, e) =>
      expect(e |> FluidPrinter.eToTestString) |> toEqual(a |> FluidPrinter.eToTestString)

    test("simple example", () =>
      fn("myFn", list{int(1), int(2), int(3)})
      |> AST.reorderFnCallArgs("myFn", 0, 2)
      |> matchExpr(fn("myFn", list{int(2), int(1), int(3)}))
    )
    test("inside another function's arguments", () =>
      fn("anotherFn", list{int(0), fn("myFn", list{int(1), int(2), int(3)})})
      |> AST.reorderFnCallArgs("myFn", 0, 2)
      |> matchExpr(fn("anotherFn", list{int(0), fn("myFn", list{int(2), int(1), int(3)})}))
    )
    test("inside its own arguments", () =>
      fn("myFn", list{int(0), fn("myFn", list{int(1), int(2), int(3)}), int(4)})
      |> AST.reorderFnCallArgs("myFn", 1, 0)
      |> matchExpr(fn("myFn", list{fn("myFn", list{int(2), int(1), int(3)}), int(0), int(4)}))
    )
    test("simple pipe first argument not moved", () =>
      pipe(int(1), fn("myFn", list{pipeTarget, int(2), int(3)}), list{})
      |> AST.reorderFnCallArgs("myFn", 2, 1)
      |> matchExpr(pipe(int(1), fn("myFn", list{pipeTarget, int(3), int(2)}), list{}))
    )
    test("simple pipe first argument moved", () =>
      pipe(int(1), fn("myFn", list{pipeTarget, int(2), int(3)}), list{})
      |> AST.reorderFnCallArgs("myFn", 0, 3)
      |> matchExpr(
        pipe(int(1), lambdaWithBinding("x", fn("myFn", list{int(2), int(3), var("x")})), list{}),
      )
    )
    test("pipe but the fn is later", () =>
      pipe(
        int(1),
        fn("other1", list{pipeTarget}),
        list{
          fn("other2", list{pipeTarget}),
          fn("myFn", list{pipeTarget, int(2), int(3)}),
          fn("other3", list{pipeTarget}),
        },
      )
      |> AST.reorderFnCallArgs("myFn", 2, 1)
      |> matchExpr(
        pipe(
          int(1),
          fn("other1", list{pipeTarget}),
          list{
            fn("other2", list{pipeTarget}),
            fn("myFn", list{pipeTarget, int(3), int(2)}),
            fn("other3", list{pipeTarget}),
          },
        ),
      )
    )
    test("pipe and first argument moved", () =>
      pipe(
        int(1),
        fn("other1", list{pipeTarget}),
        list{
          fn("other2", list{pipeTarget}),
          fn("myFn", list{pipeTarget, int(2), int(3)}),
          fn("other3", list{pipeTarget}),
        },
      )
      |> AST.reorderFnCallArgs("myFn", 1, 0)
      |> matchExpr(
        pipe(
          int(1),
          fn("other1", list{pipeTarget}),
          list{
            fn("other2", list{pipeTarget}),
            lambdaWithBinding("x", fn("myFn", list{int(2), var("x"), int(3)})),
            fn("other3", list{pipeTarget}),
          },
        ),
      )
    )
    test("recurse into piped lambda exprs", () =>
      pipe(int(1), fn("myFn", list{pipeTarget, int(2), int(3)}), list{})
      |> AST.reorderFnCallArgs("myFn", 0, 1)
      |> AST.reorderFnCallArgs("myFn", 0, 1)
      |> matchExpr(
        pipe(int(1), lambdaWithBinding("x", fn("myFn", list{var("x"), int(2), int(3)})), list{}),
      )
    )
    test("inside another expression in a pipe", () =>
      pipe(
        int(0),
        fn("anotherFn", list{pipeTarget, int(1), fn("myFn", list{int(2), int(3), int(4)})}),
        list{},
      )
      |> AST.reorderFnCallArgs("myFn", 1, 0)
      |> matchExpr(
        pipe(
          int(0),
          fn("anotherFn", list{pipeTarget, int(1), fn("myFn", list{int(3), int(2), int(4)})}),
          list{},
        ),
      )
    )
    test("inside nested pipes", () =>
      pipe(
        int(1),
        lambdaWithBinding(
          "a",
          pipe(var("a"), fn("myFn", list{pipeTarget, int(2), int(3)}), list{}),
        ),
        list{},
      )
      |> AST.reorderFnCallArgs("myFn", 0, 3)
      |> matchExpr(
        pipe(
          int(1),
          lambdaWithBinding(
            "a",
            pipe(
              var("a"),
              lambdaWithBinding("x", fn("myFn", list{int(2), int(3), var("x")})),
              list{},
            ),
          ),
          list{},
        ),
      )
    )
    ()
  })
  describe("calculateUserUnsafeFunctions", () => {
    let userFunctions =
      list{
        aFn("callsUnsafeBuiltin", fn("DB::set_v1", list{})),
        aFn("callsSafeBuiltin", fn("List::getAt_v1", list{})),
        aFn("callsSafeUserfn", fn("callsSafeBuiltin", list{})),
        aFn("callsUnsafeUserfn", fn("callsUnsafeBuiltin", list{})),
      }
      |> List.map(~f=fn => (fn.ufTLID, fn))
      |> TLID.Dict.fromList

    test("simple example", () => {
      let props = {userFunctions: userFunctions, usedFns: Map.String.empty}
      expect(
        Functions.empty
        |> Functions.setBuiltins(sampleFunctions, props)
        |> Functions.testCalculateUnsafeUserFunctions(props)
        |> Set.toList
        |> List.sortWith(compare),
      ) |> toEqual(list{"callsUnsafeBuiltin", "callsUnsafeUserfn"})
    })
    ()
  })
  describe("convert-if-to-match", () => {
    let model = hs => {
      ...D.defaultModel,
      functions: Functions.empty,
      handlers: Handlers.fromList(hs),
    }

    let handlerWithPointer = cond => {
      let id = gid()
      let ast = FluidAST.ofExpr(EIf(id, cond, EBool(gid(), true), EBool(gid(), false)))

      ({...defaultHandler, ast: ast}, id)
    }

    let binOp = (which, lhs, rhs) => EBinOp(gid(), which, lhs, rhs, NoRail)

    let init = cond => {
      let (h, pd) = handlerWithPointer(cond)
      let m = model(list{h})
      (m, h, pd)
    }

    test("generic true false arms", () => {
      let (m, h, id) = init(binOp("<", int(3), int(4)))
      let mod' = IfToMatch.refactor(m, TLHandler(h), id)
      let res = switch mod' {
      | AddOps(list{SetHandler(_, _, h)}, _) =>
        switch FluidAST.toExpr(h.ast) {
        | EMatch(
            _,
            EBinOp(_, "<", _, _, _),
            list{(PBool(_, true), EBool(_, true)), (PBool(_, false), EBool(_, false))},
          ) => true
        | _ => false
        }
      | _ => false
      }

      expect(res) |> toEqual(true)
    })
    test("fallback true false arms", () => {
      let (m, h, id) = init(binOp("==", aFnCall, aFnCall))
      let mod' = IfToMatch.refactor(m, TLHandler(h), id)
      let res = switch mod' {
      | AddOps(list{SetHandler(_, _, h)}, _) =>
        switch FluidAST.toExpr(h.ast) {
        | EMatch(
            _,
            EBinOp(_, "==", _, _, _),
            list{(PBool(_, true), EBool(_, true)), (PBool(_, false), EBool(_, false))},
          ) => true
        | _ => false
        }
      | _ => false
      }

      expect(res) |> toEqual(true)
    })
    test("pattern in arm", () => {
      let (m, h, id) = init(binOp("==", int(3), aFnCall))
      let mod' = IfToMatch.refactor(m, TLHandler(h), id)
      let res = switch mod' {
      | AddOps(list{SetHandler(_, _, h)}, _) =>
        switch FluidAST.toExpr(h.ast) {
        | EMatch(
            _,
            EFnCall(_),
            list{(PInteger(_, 3L), EBool(_, true)), (PVariable(_, "_"), EBool(_, false))},
          ) => true
        | _ => false
        }
      | _ => false
      }

      expect(res) |> toEqual(true)
    })
    ()
  })
  ()
}
