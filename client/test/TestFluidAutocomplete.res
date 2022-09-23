open Tester
open Prelude
module AC = FluidAutocomplete
module B = BlankOr
module K = FluidKeyboard
module Printer = FluidTokenizer
module TL = Toplevel
open ProgramTypes.Expr
open ProgramTypes.MatchPattern
open FluidTestData
open FluidShortcuts

let defaultUnknownSpec = PT.Handler.Spec.newUnknown("", "")

let defaultHTTPSpec = PT.Handler.Spec.newHTTP("/", "GET")

let defaultREPLSpec = PT.Handler.Spec.newREPL("adjectiveNoun")

let defaultCronSpec = PT.Handler.Spec.newCron("daily", Some(PT.Handler.Spec.CronInterval.EveryDay))

let defaultWorkerSpec = PT.Handler.Spec.newWorker("sink")

let defaultSpec = defaultHTTPSpec

let sampleFunctions: list<RT.BuiltInFn.t> = list{
  ("Twit", "somefunc", 0, list{DType.TDict(DType.TVariable("abc"))}, DType.TVariable("xyz")),
  ("Twit", "someOtherFunc", 0, list{TDict(TVariable("efg"))}, TVariable("b")),
  ("Twit", "yetAnother", 0, list{TDict(TVariable("d"))}, TVariable("abc")),
  ("", "+", 0, list{TInt, TInt}, TInt),
  ("Int", "add", 0, list{TInt, TInt}, TInt),
  ("Dict", "keys", 0, list{TDict(DType.TVariable("a"))}, TList(TStr)),
  ("List", "head", 0, list{TList(DType.TVariable("xyz"))}, DType.TVariable("lmn")),
  ("", "withlower", 0, list{TDict(DType.TVariable("c"))}, TDict(DType.TVariable("abc"))),
  ("", "withLower", 0, list{TDict(DType.TVariable("b"))}, TDict(DType.TVariable("x"))),
  ("SomeModule", "withLower", 0, list{TDict(DType.TVariable("x"))}, TDict(DType.TVariable("xyz"))),
  (
    "SomeOtherModule",
    "withlower",
    0,
    list{TDict(DType.TVariable("abc"))},
    TDict(DType.TVariable("a")),
  ),
  ("HTTP", "post", 0, list{DType.TVariable("efg")}, DType.TVariable("c")),
  ("HTTP", "head", 0, list{DType.TVariable("x")}, DType.TVariable("abc")),
  ("HTTP", "get", 0, list{DType.TVariable("xyz")}, DType.TVariable("d")),
  ("HTTP", "options", 0, list{DType.TVariable("a")}, DType.TVariable("lmn")),
  ("Some", "deprecated", 0, list{DType.TVariable("xyz")}, DType.TVariable("a")),
  ("DB", "deleteAll", 0, list{TDB(DType.TVariable("c"))}, TNull),
  ("DB", "generateKey", 0, list{}, TStr),
  ("DB", "getAll", 2, list{TDB(DType.TVariable("x"))}, TList(DType.TVariable("xyz"))),
  ("DB", "query", 4, list{TDB(DType.TVariable("x"))}, TNull),
  ("DB", "getAll", 1, list{TDB(DType.TVariable("b"))}, TList(DType.TVariable("efg"))),
  // ordering is deliberate - we want the query to order s.t. get is before getAll
  ("DB", "get", 1, list{TDB(DType.TVariable("a"))}, TList(DType.TVariable("lmn"))),
  ("String", "append", 0, list{TStr, TStr}, TStr),
  (
    "List",
    "append",
    0,
    list{TList(DType.TVariable("xyz")), TList(DType.TVariable("c"))},
    TList(DType.TVariable("b")),
  ),
  ("String", "newline", 0, list{}, TStr),
  ("Option", "withDefault", 0, list{TOption(DType.TVariable("a"))}, DType.TVariable("d")),
  (
    "Result",
    "withDefault",
    0,
    list{TResult(DType.TVariable("x"), DType.TVariable("xyz"))},
    DType.TVariable("b"),
  ),
  ("InQuery", "whatever", 0, list{TDict(DType.TVariable("efg"))}, DType.TVariable("x")),
} |> List.map(~f=((module_, function, version, paramTipes, returnType)): RT.BuiltInFn.t => {
  name: {module_: module_, function: function, version: version},
  parameters: List.map(paramTipes, ~f=(paramType): RT.BuiltInFn.Param.t => {
    name: "x",
    typ: paramType,
    args: list{},
    description: "",
  }),
  returnType: returnType,
  previewable: Impure,
  description: "",
  isInfix: true,
  deprecated: if function == "deprecated" {
    DeprecatedBecause("")
  } else {
    NotDeprecated
  },
  sqlSpec: if module_ == "InQuery" {
    SqlUnaryOp("+")
  } else if module_ == "DB" && function == "query" {
    QueryFunction
  } else {
    NotQueryable
  },
})

let defaultTraceID = "94167980-f909-527e-a4af-bc3155f586d3"

let defaultID = gid()

let defaultID2 = gid()

let defaultExpr = ProgramTypes.Expr.EBlank(defaultID)

let defaultToplevel = TLHandler({
  ast: FluidAST.ofExpr(defaultExpr),
  spec: defaultSpec,
  tlid: defaultTLID,
  pos: Pos.origin,
})

let defaultTokenInfo: FluidToken.tokenInfo = {
  startRow: 0,
  startCol: 0,
  startPos: 0,
  endPos: 0,
  length: 0,
  token: TBlank(defaultID, defaultID, None),
}

let defaultFullQuery = (~tl=defaultToplevel, ac: AC.t, queryString: string): AC.fullQuery => {
  let ti = switch tl {
  | TLHandler({ast, _}) | TLFunc({body: ast, _}) =>
    ast
    |> FluidAST.toExpr
    |> Printer.tokenize
    |> List.head
    |> Option.unwrap(~default=defaultTokenInfo)
  | _ => defaultTokenInfo
  }

  let (_, ti) = ac.query |> Option.unwrap(~default=(TL.id(tl), ti))
  {tl: tl, ti: ti, fieldList: list{}, pipedDval: None, queryString: queryString}
}

let aHandler = (~tlid=defaultTLID, ~expr=defaultExpr, ~spec=defaultSpec, ()): PT.Handler.t => {
  {ast: FluidAST.ofExpr(expr), spec: spec, tlid: tlid, pos: {x: 0, y: 0}}
}

let aFunction = (~tlid=defaultTLID, ~expr=defaultExpr, ()): PT.UserFunction.t => {
  tlid: tlid,
  name: "myFunc",
  nameID: gid(),
  parameters: list{},
  description: "",
  returnType: DType.TStr,
  returnTypeID: gid(),
  infix: false,
  body: FluidAST.ofExpr(expr),
}

let aDB = (~tlid=defaultTLID, ~fieldid=defaultID, ~typeid=defaultID2, ()): PT.DB.t => {
  tlid: tlid,
  name: "MyDB",
  nameID: gid(),
  cols: list{{name: None, typ: None, nameID: fieldid, typeID: typeid}},
  version: 0,
  pos: {x: 0, y: 0},
}

// Sets the model with the appropriate toplevels
let defaultModel = (
  ~tlid=defaultTLID,
  ~analyses=list{},
  ~dbs=list{},
  ~handlers=list{aHandler()},
  ~userFunctions=list{},
  ~userTypes=list{},
  (),
): AppTypes.model => {
  let analyses =
    analyses
    |> List.map(~f=((id, value)) => (id, AnalysisTypes.ExecutionResult.ExecutedResult(value)))
    |> List.toArray
    |> ID.Map.fromArray

  let default = FluidTestData.defaultTestModel
  {
    ...default,
    handlers: Handlers.fromList(handlers),
    dbs: DB.fromList(dbs),
    userFunctions: UserFunctions.fromList(userFunctions),
    userTypes: UserTypes.fromList(userTypes),
    cursorState: FluidEntering(tlid),
    functions: {...Functions.empty, builtinFunctions: sampleFunctions} |> Functions.update(
      defaultFunctionsProps,
    ),
    analyses: Map.String.singleton(~key=defaultTraceID, ~value=Loadable.Success(analyses)),
  }
}

// AC targeting a tlid and pointer
let acFor = (~tlid=defaultTLID, ~pos=0, ~init=AC.init, m: AppTypes.model): AC.t => {
  let ti =
    TL.get(m, tlid)
    |> Option.andThen(~f=TL.getAST)
    |> Option.andThen(~f=ast =>
      Fluid.ASTInfo.make(ast, {...m.fluidState, newPos: pos}) |> Fluid.ASTInfo.getToken
    )
    |> Option.unwrap(~default=defaultTokenInfo)

  AC.regenerate(m, init, (tlid, ti))
}

let setQuery = (q: string, a: AC.t): AC.t => {
  let fullQ = defaultFullQuery(a, q)
  let props = defaultTestProps
  AC.refilter(
    {functions: props.functions},
    fullQ,
    a,
    List.map(~f=({item, _}) => item, a.completions),
  )
}

let filterValid = (a: AC.t): list<AC.item> =>
  List.filterMap(a.completions, ~f=x =>
    switch x {
    | {item, validity: FACItemValid} => Some(item)
    | _ => None
    }
  )

let filterInvalid = (a: AC.t): list<AC.item> =>
  List.filterMap(a.completions, ~f=x =>
    switch x {
    | {validity: FACItemValid, _} => None
    | {item, _} => Some(item)
    }
  )

let run = () => {
  describe("autocomplete", () => {
    open FluidTypes.AutoComplete
    describe("queryWhenEntering", () => {
      let m = defaultModel()
      let acForQueries = (qs: list<string>) =>
        List.fold(qs, ~initial=acFor(m), ~f=(acc, q) => setQuery(q, acc))
        |> filterValid
        |> List.map(~f=AC.asName)

      let acForQuery = (q: string) => acForQueries(list{q})
      test("empty autocomplete doesn't highlight", () =>
        expect(acFor(m) |> (x => x.index)) |> toEqual(None)
      )
      test("pressing a letter from the AC.selected entry does not keep the entry AC.selected", () =>
        expect(
          acFor(m)
          |> setQuery("Twit::somef")
          |> setQuery("Twit::someO")
          |> AC.highlighted
          |> Option.map(~f=AC.asName),
        ) |> toEqual(Some("Twit::someOtherFunc"))
      )
      test("Returning to empty unselects", () =>
        expect(acFor(m) |> setQuery("lis") |> setQuery("") |> AC.highlighted) |> toEqual(None)
      )
      test("resetting the query refilters", () =>
        expect(
          acFor(m)
          |> setQuery("Twit::somefunc")
          |> setQuery("Twit::some")
          |> AC.selectDown
          |> AC.highlighted
          |> Option.map(~f=AC.asName),
        ) |> toEqual(Some("Twit::someOtherFunc"))
      )
      test("deprecated functions are removed", () =>
        expect(acFor(m) |> setQuery("deprecated") |> AC.highlighted) |> toEqual(None)
      )
      test("sorts correctly without typing ::", () =>
        expect(acForQuery("dbget") |> List.head) |> toEqual(Some("DB::get_v1"))
      )
      test("lowercase search still finds uppercase results", () =>
        expect(acForQuery("listh")) |> toEqual(list{"List::head"})
      )
      test("DB::get_v1 occurs before DB::getAll_v1", () =>
        expect(acForQuery("DB::get") |> List.head) |> toEqual(Some("DB::get_v1"))
      )
      test("DB::getAll_v2 occurs before DB::getAll_v1", () =>
        expect(acForQuery("DB::getA") |> List.head) |> toEqual(Some("DB::getAll_v2"))
      )
      test("DB::getAll_v1 is reachable", () =>
        expect(acForQuery("DB::getA")) |> toEqual(list{"DB::getAll_v2", "DB::getAll_v1"})
      )
      test("search finds only prefixed", () =>
        expect(acForQuery("twit::y")) |> toEqual(list{"Twit::yetAnother"})
      )
      test("show results when the only option is the setQuery", () =>
        expect(acForQuery("List::head") |> List.length) |> toEqual(1)
      )
      test("scrolling down a bit works", () =>
        expect(
          acFor(m) |> setQuery("Twit") |> AC.selectDown |> AC.selectDown |> (x => x.index),
        ) |> toEqual(Some(2))
      )
      test("scrolling loops one way", () =>
        expect(
          acFor(m)
          |> setQuery("Twit:")
          |> AC.selectDown
          |> AC.selectDown
          |> AC.selectDown
          |> (x => x.index),
        ) |> toEqual(Some(0))
      )
      test("scrolling loops the other way", () =>
        expect(
          acFor(m)
          |> setQuery("Twit:")
          |> AC.selectDown
          |> AC.selectUp
          |> AC.selectUp
          |> (x => x.index),
        ) |> toEqual(Some(2))
      )
      test("scrolling loops the other way without going forward first", () =>
        expect(
          acFor(m) |> setQuery("Twit:") |> AC.selectUp |> AC.selectUp |> (x => x.index),
        ) |> toEqual(Some(1))
      )
      test("Don't highlight when the list is empty", () =>
        expect(
          acFor(m)
          |> setQuery("Twit")
          |> AC.selectDown
          |> AC.selectDown
          |> setQuery("Twit::1334xxx")
          |> (x => x.index),
        ) |> toEqual(None)
      )
      test("ordering = startsWith then case match then case insensitive match", () =>
        expect(
          acFor(m) |> setQuery("withLo") |> filterValid |> List.map(~f=AC.asName),
        ) |> toEqual(list{
          "withLower",
          "withlower",
          "SomeModule::withLower",
          "SomeOtherModule::withlower",
        })
      )
      test("a specific bug where `+` is interpreted as an FACLiteral", () =>
        expect(acFor(m) |> setQuery("+") |> AC.highlighted |> Option.map(~f=AC.asName)) |> toEqual(
          Some("+"),
        )
      )
      test("null works", () =>
        expect(acFor(m) |> setQuery("nu") |> AC.highlighted) |> toEqual(Some(FACLiteral(LNull)))
      )
      test("Ok works", () =>
        expect(acFor(m) |> setQuery("Ok") |> AC.highlighted) |> toEqual(
          Some(FACConstructorName("Ok", 1)),
        )
      )
      test("Error works", () =>
        expect(acFor(m) |> setQuery("Error") |> AC.highlighted) |> toEqual(
          Some(FACConstructorName("Error", 1)),
        )
      )
      test("true works", () =>
        expect(acFor(m) |> setQuery("tr") |> AC.highlighted) |> toEqual(
          Some(FACLiteral(LBool(true))),
        )
      )
      test("case insensitive true works", () =>
        expect(acFor(m) |> setQuery("tR") |> AC.highlighted) |> toEqual(
          Some(FACLiteral(LBool(true))),
        )
      )
      test("false works", () =>
        expect(acFor(m) |> setQuery("fa") |> AC.highlighted) |> toEqual(
          Some(FACLiteral(LBool(false))),
        )
      )
      test("if works", () =>
        expect(acFor(m) |> setQuery("if") |> AC.highlighted) |> toEqual(Some(FACKeyword(KIf)))
      )
      test("let works", () =>
        expect(acFor(m) |> setQuery("let") |> AC.highlighted) |> toEqual(Some(FACKeyword(KLet)))
      )
      test("Lambda works", () =>
        expect(acFor(m) |> setQuery("lambda") |> AC.highlighted) |> toEqual(
          Some(FACKeyword(KLambda)),
        )
      )
      test("http handlers have request", () => {
        let m = defaultModel(~handlers=list{aHandler(~spec=defaultHTTPSpec, ())}, ())
        expect(acFor(m) |> setQuery("request") |> filterValid) |> toEqual(list{
          FACVariable("request", None),
        })
      })
      test("handlers with no route have request and event", () =>
        expect({
          let m = defaultModel(~handlers=list{aHandler(~spec=defaultUnknownSpec, ())}, ())
          let ac = acFor(m)
          (ac |> setQuery("request") |> filterValid, ac |> setQuery("event") |> filterValid)
        }) |> toEqual((list{FACVariable("request", None)}, list{FACVariable("event", None)}))
      )
      test("functions have DB names in the autocomplete", () => {
        let blankid = gid()
        let dbNameBlank = EBlank(blankid)
        let fntlid = gtlid()
        let fn = aFunction(~tlid=fntlid, ~expr=fn(~mod="DB", "deleteAll", list{dbNameBlank}), ())

        let m = defaultModel(
          ~tlid=fntlid,
          ~dbs=list{aDB(~tlid=gtlid(), ())},
          ~userFunctions=list{fn},
          (),
        )

        let ac = acFor(~tlid=fntlid, ~pos=14, m)
        expect(ac |> setQuery("MyDB") |> filterValid) |> toEqual(list{FACDatastore("MyDB")})
      })
    })
    describe("filter", () => {
      let isConstructor = x =>
        switch x {
        | FACConstructorName(_) => true
        | _ => false
        }

      let isDatastore = x =>
        switch x {
        | FACDatastore(_) => true
        | _ => false
        }
      let isVariable = x =>
        switch x {
        | FACVariable(_) => true
        | _ => false
        }
      let filterFor = (m, ~pos) => {
        let ac = acFor(~pos, m)
        (filterValid(ac), filterInvalid(ac))
      }

      test("Cannot use DB variable when type of blank isn't TDB", () => {
        let id = gid()
        let expr = fn(~mod="Int", "add", list{EBlank(id), b})
        let m = defaultModel(
          ~analyses=list{(id, DDB("MyDB"))},
          ~dbs=list{aDB(~tlid=TLID.fromInt(23), ())},
          ~handlers=list{aHandler(~expr, ())},
          (),
        )

        let (_valid, invalid) = filterFor(m, ~pos=9)
        expect(List.filter(invalid, ~f=isDatastore)) |> toEqual(list{FACDatastore("MyDB")})
      })
      test("Constructors are available in Any expression", () => {
        let m = defaultModel()
        let (valid, _invalid) = filterFor(m, ~pos=0)
        expect(List.filter(valid, ~f=isConstructor)) |> toEqual(list{
          FACConstructorName("Just", 1),
          FACConstructorName("Nothing", 0),
          FACConstructorName("Ok", 1),
          FACConstructorName("Error", 1),
        })
      })
      test("Method argument filters by variable type", () => {
        let id = gid()
        let id2 = gid()
        let expr = let'(
          "mystr",
          str(~id, "asd"),
          let'("myint", int(~id=id2, 5), fn(~mod="String", "append", list{b, b})),
        )
        // remove `request` var from valid
        let m = defaultModel(
          ~analyses=list{(id, DStr("asd")), (id2, DInt(5L))},
          ~handlers=list{aHandler(~spec=defaultREPLSpec, ~expr, ())},
          (),
        )

        let (valid, invalid) = filterFor(m, ~pos=47)
        expect((
          List.filter(valid, ~f=isVariable),
          List.filter(invalid, ~f=isVariable),
        )) |> toEqual((
          list{FACVariable("mystr", Some(DStr("asd")))},
          list{FACVariable("myint", Some(DInt(5L)))},
        ))
      })
      test("Method argument filters by fn return type ", () => {
        let expr = fn(~mod="String", "append", list{b, b})
        let m = defaultModel(~handlers=list{aHandler(~expr, ())}, ())
        let (valid, invalid) = filterFor(m, ~pos=15)
        expect((
          valid |> List.map(~f=AC.asName) |> List.member(~value="String::append"),
          invalid |> List.map(~f=AC.asName) |> List.member(~value="Int::add"),
        )) |> toEqual((true, true))
      })
      test("Only Just and Nothing are allowed in Option-blank", () => {
        let expr = fn(~mod="Option", "withDefault", list{b})
        let m = defaultModel(~handlers=list{aHandler(~expr, ())}, ())
        let (valid, _invalid) = filterFor(m, ~pos=20)
        expect(valid |> List.filter(~f=isConstructor)) |> toEqual(list{
          FACConstructorName("Just", 1),
          FACConstructorName("Nothing", 0),
        })
      })
      test("Only Ok and Error are allowed in Result blank", () => {
        let expr = fn(~mod="Result", "withDefault", list{b})
        let m = defaultModel(~handlers=list{aHandler(~expr, ())}, ())
        let (valid, _invalid) = filterFor(m, ~pos=20)
        expect(valid |> List.filter(~f=isConstructor)) |> toEqual(list{
          FACConstructorName("Ok", 1),
          FACConstructorName("Error", 1),
        })
      })
      test("Use piped types", () => {
        let id = gid()
        let expr = pipe(str(~id, "asd"), partial("append", b), list{})
        let m = defaultModel(
          ~analyses=list{(id, DStr("asd"))},
          ~handlers=list{aHandler(~expr, ())},
          (),
        )

        let (valid, _invalid) = filterFor(m, ~pos=14)
        expect(valid |> List.filter(~f=AC.isFnCall) |> List.map(~f=AC.asName)) |> toEqual(list{
          "String::append",
        })
      })
      test("functions with no arguments are invalid when piping", () => {
        let id = gid()
        let expr = pipe(int(~id, 5), partial("string", b), list{})
        let m = defaultModel(
          ~analyses=list{(id, DInt(5L))},
          ~handlers=list{aHandler(~expr, ())},
          (),
        )

        let (_valid, invalid) = filterFor(m, ~pos=10)
        expect(
          invalid |> List.map(~f=AC.asName) |> List.filter(~f=\"="("String::newline")),
        ) |> toEqual(list{"String::newline"})
      })
      test("MP completions are available in MP blank", () => {
        let tlid = gtlid()
        let patID = gid()
        let expr = match'(b, list{(MPVariable(patID, "o"), b)})
        let m =
          defaultModel(~handlers=list{aHandler(~tlid, ~expr, ())}, ()) |> (
            m => {...m, functions: Functions.empty}
          )

        expect(acFor(~tlid, ~pos=13, m) |> filterValid |> List.map(~f=AC.asName)) |> toEqual(list{
          "o",
          "Ok",
          "Nothing",
          "Error",
        })
      })
      ()
    })
    describe("filter in DB::query", () => {
      let isFunction = x =>
        switch x {
        | FACFunction(_) => true
        | _ => false
        }
      let isVariable = x =>
        switch x {
        | FACVariable(_) => true
        | _ => false
        }
      let isKeyword = x =>
        switch x {
        | FACKeyword(_) => true
        | _ => false
        }
      let isConstructor = x =>
        switch x {
        | FACConstructorName(_) => true
        | _ => false
        }

      let isLiteral = x =>
        switch x {
        | FACLiteral(_) => true
        | _ => false
        }
      let tlid = gtlid()
      let expr = fn(
        ~mod="DB",
        "query",
        ~version=4,
        list{str("MyDB"), lambda(list{"value"}, blank())},
      )

      let m = defaultModel(
        ~tlid,
        ~dbs=list{aDB(~tlid=gtlid(), ())},
        ~handlers=list{aHandler(~expr, ())},
        (),
      )

      let ac = acFor(~pos=50, m)
      let valid = filterValid(ac)
      test("includes supported functions only", () =>
        expect(valid |> List.filter(~f=isFunction) |> List.map(~f=AC.asName)) |> toEqual(list{
          "InQuery::whatever",
        })
      )
      test("includes variables", () =>
        expect(
          valid
          |> List.filter(~f=isVariable)
          |> List.map(~f=AC.asName)
          |> List.filter(~f=n => n == "value"),
        ) |> toEqual(list{"value"})
      )
      test("includes variables", () =>
        expect(valid |> List.filter(~f=isLiteral) |> List.map(~f=AC.asName)) |> toEqual(list{
          "true",
          "false",
          "null",
        })
      )
      test("includes keywords", () =>
        expect(valid |> List.filter(~f=isKeyword) |> List.map(~f=AC.asName)) |> toEqual(list{
          "let",
          "|>",
        })
      )
      test("includes constructors", () =>
        expect(valid |> List.filter(~f=isConstructor) |> List.map(~f=AC.asName)) |> toEqual(list{})
      )
      ()
    })
    describe("regenerating match pattern completions", () => {
      open FluidTypes.AutoComplete

      let tlid = gtlid()
      let expr = match'(b, list{(MPVariable(gid(), "t"), b)})
      let m = defaultModel(~handlers=list{aHandler(~tlid, ~expr, ())}, ())

      test("maintains highlight index", () => {
        // generate initial autocomplete completions
        let initialAcResults = acFor(m, ~tlid, ~pos=12)
        let firstAcThirdResult =
          List.getAt(~index=2, initialAcResults.completions) |> Option.unwrapUnsafe |> (c => c.item)

        let isAJustPattern = switch firstAcThirdResult {
        | FACMatchPattern(_, FMPAConstructor("Just", list{FMPABlank})) => true
        | _ => false
        }

        // highlight the third result; regenerate autocomplete options
        let subsequentAcResults = acFor(
          m,
          ~tlid,
          ~pos=12, // where the blank pattern inside the `match` starts
          ~init={...initialAcResults, index: Some(2)},
        )

        // Expectations:
        // - the completion we've highlighted is "Just ___"
        // - the highlighted index is still at 2
        // - the AC result from the first generation matches the result from
        //   the subsequent generation; the item was reused (ids didn't change)
        let secondAcThirdResult =
          List.getAt(~index=2, subsequentAcResults.completions)
          |> Option.unwrapUnsafe
          |> (c => c.item)

        let expected = (isAJustPattern, Some(2), firstAcThirdResult)
        let actual = (true, subsequentAcResults.index, secondAcThirdResult)
        expect(expected) |> toEqual(actual)
      })
      ()
    })
    ()
  })
  ()
}
