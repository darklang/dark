open Tester
open Prelude
module E = FluidExpression
module AC = FluidAutocomplete
module B = BlankOr
module K = FluidKeyboard
module P = FluidPattern
module Printer = FluidTokenizer
module TL = Toplevel
open FluidExpression
open FluidTestData
open FluidShortcuts

let sampleFunctions: list<function_> = list{
  ("Twit::somefunc", list{TObj}, TAny),
  ("Twit::someOtherFunc", list{TObj}, TAny),
  ("Twit::yetAnother", list{TObj}, TAny),
  ("+", list{TInt, TInt}, TInt),
  ("Int::add", list{TInt, TInt}, TInt),
  ("Dict::keys", list{TObj}, TList),
  ("List::head", list{TList}, TAny),
  ("withlower", list{TObj}, TObj),
  ("withLower", list{TObj}, TObj),
  ("SomeModule::withLower", list{TObj}, TObj),
  ("SomeOtherModule::withlower", list{TObj}, TObj),
  ("HTTP::post", list{TAny}, TAny),
  ("HTTP::head", list{TAny}, TAny),
  ("HTTP::get", list{TAny}, TAny),
  ("HTTP::options", list{TAny}, TAny),
  ("Some::deprecated", list{TAny}, TAny),
  ("DB::deleteAll", list{TDB}, TNull),
  ("DB::generateKey", list{}, TStr),
  ("DB::getAll_v2", list{TDB}, TList),
  ("DB::getAll_v1", list{TDB}, TList),
  /* ordering is deliberate - we want the query to order s.t. get is before getAll */
  ("DB::get_v1", list{TDB}, TList),
  ("String::append", list{TStr, TStr}, TStr),
  ("List::append", list{TList, TList}, TList),
  ("String::newline", list{}, TStr),
  ("Option::withDefault", list{TOption}, TAny),
  ("Result::withDefault", list{TResult}, TAny),
  ("InQuery::whatever", list{TObj}, TAny),
} |> List.map(~f=((fnName, paramTipes, fnReturnTipe)) => {
  fnName: fnName,
  fnParameters: List.map(paramTipes, ~f=paramTipe => {
    paramName: "x",
    paramTipe: paramTipe,
    paramBlock_args: list{},
    paramOptional: false,
    paramDescription: "",
  }),
  fnReturnTipe: fnReturnTipe,
  fnPreviewSafety: Unsafe,
  fnDescription: "",
  fnInfix: true,
  fnDeprecated: fnName == "Some::deprecated",
  fnIsSupportedInQuery: fnName == "InQuery::whatever",
  fnOrigin: Builtin,
})

let defaultTraceID = "94167980-f909-527e-a4af-bc3155f586d3"

let defaultID = gid()

let defaultID2 = gid()

let defaultExpr = E.EBlank(defaultID)

let defaultToplevel = TLHandler({
  ast: FluidAST.ofExpr(defaultExpr),
  spec: {
    space: Blank(gid()),
    name: Blank(gid()),
    modifier: Blank(gid()),
  },
  hTLID: defaultTLID,
  pos: Defaults.origin,
})

let defaultTokenInfo = {
  startRow: 0,
  startCol: 0,
  startPos: 0,
  endPos: 0,
  length: 0,
  token: TBlank(defaultID, None),
}

let defaultFullQuery = (~tl=defaultToplevel, ac: AC.t, queryString: string): AC.fullQuery => {
  let ti = switch tl {
  | TLHandler({ast, _}) | TLFunc({ufAST: ast, _}) =>
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

let aHandler = (~tlid=defaultTLID, ~expr=defaultExpr, ~space: option<string>=None, ()): handler => {
  let space = switch space {
  | None => B.new_()
  | Some(name) => B.newF(name)
  }
  let spec = {space: space, name: B.new_(), modifier: B.new_()}
  {ast: FluidAST.ofExpr(expr), spec: spec, hTLID: tlid, pos: {x: 0, y: 0}}
}

let aFunction = (~tlid=defaultTLID, ~expr=defaultExpr, ()): userFunction => {
  ufTLID: tlid,
  ufMetadata: {
    ufmName: B.newF("myFunc"),
    ufmParameters: list{},
    ufmDescription: "",
    ufmReturnTipe: B.newF(TStr),
    ufmInfix: false,
  },
  ufAST: FluidAST.ofExpr(expr),
}

let aDB = (~tlid=defaultTLID, ~fieldid=defaultID, ~typeid=defaultID2, ()): db => {
  dbTLID: tlid,
  dbName: B.newF("MyDB"),
  cols: list{(Blank(fieldid), Blank(typeid))},
  version: 0,
  oldMigrations: list{},
  activeMigration: None,
  pos: {x: 0, y: 0},
}

/* Sets the model with the appropriate toplevels */
let defaultModel = (
  ~tlid=defaultTLID,
  ~analyses=list{},
  ~dbs=list{},
  ~handlers=list{aHandler()},
  ~userFunctions=list{},
  ~userTipes=list{},
  (),
): model => {
  let analyses =
    analyses
    |> List.map(~f=((id, value)) => (ID.toString(id), ExecutedResult(value)))
    |> List.toArray
    |> Belt.Map.String.fromArray

  let default = FluidTestData.defaultTestModel
  {
    ...default,
    handlers: Handlers.fromList(handlers),
    dbs: DB.fromList(dbs),
    userFunctions: UserFunctions.fromList(userFunctions),
    userTipes: UserTypes.fromList(userTipes),
    cursorState: FluidEntering(tlid),
    functions: {...Functions.empty, builtinFunctions: sampleFunctions} |> Functions.update(
      defaultFunctionsProps,
    ),
    analyses: Map.String.singleton(~key=defaultTraceID, ~value=LoadableSuccess(analyses)),
  }
}

/* AC targeting a tlid and pointer */
let acFor = (~tlid=defaultTLID, ~pos=0, m: model): AC.t => {
  let ti =
    TL.get(m, tlid)
    |> Option.andThen(~f=TL.getAST)
    |> Option.andThen(~f=ast =>
      Fluid.ASTInfo.make(
        defaultTestProps,
        ast,
        {...m.fluidState, newPos: pos},
      ) |> Fluid.ASTInfo.getToken
    )
    |> Option.unwrap(~default=defaultTokenInfo)

  AC.regenerate(m, AC.init, (tlid, ti))
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
        expect(acFor(m) |> setQuery("nu") |> AC.highlighted) |> toEqual(Some(FACLiteral("null")))
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
        expect(acFor(m) |> setQuery("tr") |> AC.highlighted) |> toEqual(Some(FACLiteral("true")))
      )
      test("case insensitive true works", () =>
        expect(acFor(m) |> setQuery("tR") |> AC.highlighted) |> toEqual(Some(FACLiteral("true")))
      )
      test("false works", () =>
        expect(acFor(m) |> setQuery("fa") |> AC.highlighted) |> toEqual(Some(FACLiteral("false")))
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
        let space = Some("HTTP")
        let m = defaultModel(~handlers=list{aHandler(~space, ())}, ())
        expect(acFor(m) |> setQuery("request") |> filterValid) |> toEqual(list{
          FACVariable("request", None),
        })
      })
      test("handlers with no route have request and event", () =>
        expect({
          let ac = acFor(m)
          (ac |> setQuery("request") |> filterValid, ac |> setQuery("event") |> filterValid)
        }) |> toEqual((list{FACVariable("request", None)}, list{FACVariable("event", None)}))
      )
      test("functions have DB names in the autocomplete", () => {
        let blankid = ID.fromString("123")
        let dbNameBlank = EBlank(blankid)
        let fntlid = TLID.fromString("fn123")
        let fn = aFunction(
          ~tlid=fntlid,
          ~expr=EFnCall(gid(), "DB::deleteAll", list{dbNameBlank}, NoRail),
          (),
        )

        let m = defaultModel(
          ~tlid=fntlid,
          ~dbs=list{aDB(~tlid=TLID.fromString("db123"), ())},
          ~userFunctions=list{fn},
          (),
        )

        let ac = acFor(~tlid=fntlid, ~pos=14, m)
        expect(ac |> setQuery("MyDB") |> filterValid) |> toEqual(list{
          FACVariable("MyDB", Some(DDB("MyDB"))),
        })
      })
      ()
    })
    describe("filter", () => {
      let isConstructor = x =>
        switch x {
        | FACConstructorName(_) => true
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
        let expr = fn("Int::add", list{EBlank(id), b})
        let m = defaultModel(
          ~analyses=list{(id, DDB("MyDB"))},
          ~dbs=list{aDB(~tlid=TLID.fromString("23"), ())},
          ~handlers=list{aHandler(~expr, ())},
          (),
        )

        let (_valid, invalid) = filterFor(m, ~pos=9)
        expect(List.filter(invalid, ~f=isVariable)) |> toEqual(list{
          FACVariable("MyDB", Some(DDB("MyDB"))),
        })
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
          let'("myint", int(~id=id2, 5), fn("String::append", list{b, b})),
        )

        let m = defaultModel(
          ~analyses=list{(id, DStr("asd")), (id2, DInt(5))},
          ~handlers=list{
            aHandler(~space=Some("REPL" /* remove `request` var from valid */), ~expr, ()),
          },
          (),
        )

        let (valid, invalid) = filterFor(m, ~pos=47)
        expect((
          List.filter(valid, ~f=isVariable),
          List.filter(invalid, ~f=isVariable),
        )) |> toEqual((
          list{FACVariable("mystr", Some(DStr("asd")))},
          list{FACVariable("myint", Some(DInt(5)))},
        ))
      })
      test("Method argument filters by fn return type ", () => {
        let expr = fn("String::append", list{b, b})
        let m = defaultModel(~handlers=list{aHandler(~expr, ())}, ())
        let (valid, invalid) = filterFor(m, ~pos=15)
        expect((
          valid |> List.map(~f=AC.asName) |> List.member(~value="String::append"),
          invalid |> List.map(~f=AC.asName) |> List.member(~value="Int::add"),
        )) |> toEqual((true, true))
      })
      test("Only Just and Nothing are allowed in Option-blank", () => {
        let expr = fn("Option::withDefault", list{b})
        let m = defaultModel(~handlers=list{aHandler(~expr, ())}, ())
        let (valid, _invalid) = filterFor(m, ~pos=20)
        expect(valid |> List.filter(~f=isConstructor)) |> toEqual(list{
          FACConstructorName("Just", 1),
          FACConstructorName("Nothing", 0),
        })
      })
      test("Only Ok and Error are allowed in Result blank", () => {
        let expr = fn("Result::withDefault", list{b})
        let m = defaultModel(~handlers=list{aHandler(~expr, ())}, ())
        let (valid, _invalid) = filterFor(m, ~pos=20)
        expect(valid |> List.filter(~f=isConstructor)) |> toEqual(list{
          FACConstructorName("Ok", 1),
          FACConstructorName("Error", 1),
        })
      })
      test("Use piped types", () => {
        let id = gid()
        let expr = pipe(str(~id, "asd"), list{partial("append", b)})
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
        let expr = pipe(int(~id, 5), list{partial("string", b)})
        let m = defaultModel(~analyses=list{(id, DInt(5))}, ~handlers=list{aHandler(~expr, ())}, ())

        let (_valid, invalid) = filterFor(m, ~pos=10)
        expect(
          invalid |> List.map(~f=AC.asName) |> List.filter(~f=\"="("String::newline")),
        ) |> toEqual(list{"String::newline"})
      })
      test("Pattern expressions are available in pattern blank", () => {
        let tlid = TLID.fromString("789")
        let mID = ID.fromString("1234")
        let patID = ID.fromString("456")
        let pattern = P.FPVariable(mID, patID, "o")
        let expr = match'(b, list{(pattern, b)})
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
      let tlid = TLID.fromString("123")
      let expr = fn("DB::query_v4", list{str("MyDB"), lambda(list{"value"}, blank())})

      let m = defaultModel(
        ~tlid,
        ~dbs=list{aDB(~tlid=TLID.fromString("db123"), ())},
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
    describe("filter in other DB::queryWithKey", () => {
      let isFunction = x =>
        switch x {
        | FACFunction(_) => true
        | _ => false
        }
      let tlid = TLID.fromString("123")
      let expr = fn("DB::queryWithKey_v3", list{str("MyDB"), lambda(list{"value"}, blank())})

      let m = defaultModel(
        ~tlid,
        ~dbs=list{aDB(~tlid=TLID.fromString("db123"), ())},
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
      ()
    })
    describe("filter in other DB::queryOne", () => {
      let isFunction = x =>
        switch x {
        | FACFunction(_) => true
        | _ => false
        }
      let tlid = TLID.fromString("123")
      let expr = fn("DB::queryOne_v4", list{str("MyDB"), lambda(list{"value"}, blank())})

      let m = defaultModel(
        ~tlid,
        ~dbs=list{aDB(~tlid=TLID.fromString("db123"), ())},
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
      ()
    })
    describe("filter in other DB::queryOneWithKey", () => {
      let isFunction = x =>
        switch x {
        | FACFunction(_) => true
        | _ => false
        }
      let tlid = TLID.fromString("123")
      let expr = fn("DB::queryOneWithKey_v3", list{str("MyDB"), lambda(list{"value"}, blank())})

      let m = defaultModel(
        ~tlid,
        ~dbs=list{aDB(~tlid=TLID.fromString("db123"), ())},
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
      ()
    })
    describe("filter in other DB::queryCount", () => {
      let isFunction = x =>
        switch x {
        | FACFunction(_) => true
        | _ => false
        }
      let tlid = TLID.fromString("123")
      let expr = fn("DB::queryCount", list{str("MyDB"), lambda(list{"value"}, blank())})

      let m = defaultModel(
        ~tlid,
        ~dbs=list{aDB(~tlid=TLID.fromString("db123"), ())},
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
      ()
    })
    ()
  })
  ()
}
