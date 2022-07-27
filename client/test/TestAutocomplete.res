open Tester
open Autocomplete
open Prelude
module B = BlankOr

module AC = AppTypes.AutoComplete

type model = AppTypes.model

let defaultTLID = gtlid()

let defaultID = gid()

let defaultID2 = gid()

let defaultBlankOr = B.Blank(defaultID)

let defaultExpr = ProgramTypes.Expr.EBlank(defaultID)

let enteringCS = (~tlid=defaultTLID, ~id=defaultID, ()): AppTypes.CursorState.t => Entering(
  tlid,
  id,
)

let defaultHTTPSpec = PT.Handler.Spec.newHTTP("/", "GET")

let defaultREPLSpec = PT.Handler.Spec.newREPL("adjectiveNoun")

let defaultCronSpec = PT.Handler.Spec.newCron("daily", Some(PT.Handler.Spec.CronInterval.EveryDay))

let defaultWorkerSpec = PT.Handler.Spec.newWorker("sink")

let defaultSpec = defaultHTTPSpec

let omniboxCS: AppTypes.CursorState.t = Omnibox(None)

// Sets the model with the appropriate toplevels
let defaultModel = (
  ~dbs=list{},
  ~handlers=list{},
  ~userFunctions=list{},
  ~userTypes=list{},
  ~cursorState,
  (),
): model => {
  {
    ...AppTypes.Model.default,
    handlers: Handlers.fromList(handlers),
    dbs: DB.fromList(dbs),
    userFunctions: UserFunctions.fromList(userFunctions),
    userTypes: UserTypes.fromList(userTypes),
    cursorState: cursorState,
    fluidState: FluidTypes.State.default,
  }
}

let aHandler = (~tlid=defaultTLID, ~expr=defaultExpr, ~spec=defaultSpec, ()): PT.Handler.t => {
  {ast: FluidAST.ofExpr(expr), spec: spec, tlid: tlid, pos: {x: 0, y: 0}}
}

let aFunction = (
  ~tlid=defaultTLID,
  ~expr=defaultExpr,
  ~params=list{},
  ~name="myFunc",
  (),
): PT.UserFunction.t => {
  tlid: tlid,
  metadata: {
    name: B.newF(name),
    parameters: params,
    description: "",
    returnType: B.newF(DType.TStr),
    infix: false,
  },
  ast: FluidAST.ofExpr(expr),
}

let aDB = (
  ~tlid=defaultTLID,
  ~fieldid=defaultID,
  ~typeid=defaultID2,
  ~name="MyDB",
  (),
): PT.DB.t => {
  tlid: tlid,
  name: name,
  nameID: gid(),
  cols: list{{name: None, typ: None, nameID: fieldid, typeID: typeid}},
  version: 0,
  pos: {x: 0, y: 0},
}

let enteringFunction = (
  ~dbs=list{},
  ~handlers=list{},
  ~userFunctions=list{},
  ~userTypes=list{},
  (),
): model =>
  defaultModel(
    ~cursorState=enteringCS(),
    ~dbs,
    ~handlers,
    ~userTypes,
    ~userFunctions=list{aFunction(), ...userFunctions},
    (),
  )

let enteringDBField = (
  ~dbs=list{},
  ~handlers=list{},
  ~userFunctions=list{},
  ~userTypes=list{},
  (),
): model =>
  defaultModel(
    ~cursorState=enteringCS(),
    ~dbs=list{aDB(), ...dbs},
    ~handlers,
    ~userTypes,
    ~userFunctions,
    (),
  )

let enteringDBType = (
  ~dbs=list{},
  ~handlers=list{},
  ~userFunctions=list{},
  ~userTypes=list{},
  (),
): model =>
  defaultModel(
    ~cursorState=enteringCS(),
    ~dbs=list{aDB(~fieldid=defaultID2, ~typeid=defaultID, ()), ...dbs},
    ~handlers,
    ~userTypes,
    ~userFunctions,
    (),
  )

let enteringHandler = (~spec=defaultSpec, ()): model =>
  defaultModel(~cursorState=enteringCS(), ~handlers=list{aHandler(~spec, ())}, ())

let enteringEventNameHandler = (~spec=defaultSpec, ()): model => {
  let handler = aHandler(~spec, ())
  let id = PT.Handler.Spec.ids(spec).nameID
  defaultModel(~cursorState=enteringCS(~id, ()), ~handlers=list{handler}, ())
}

let creatingOmni: model = {
  ...AppTypes.Model.default,
  cursorState: Omnibox(None),
}

// AC targeting a tlid and pointer
let acFor = (~target=Some(defaultTLID, PDBColType(defaultBlankOr)), m: model): AC.t =>
  switch m.cursorState {
  | Omnibox(_) => init(m) |> setTarget(m, None)
  | Entering(_) => init(m) |> setTarget(m, target)
  | _ => init(m) |> setTarget(m, target)
  }

let acForDB = (): AC.t =>
  enteringDBType() |> acFor(~target=Some(defaultTLID, PDBColType(Blank(defaultID))))

let itemPresent = (aci: AC.item, ac: AC.t): bool => List.member(~value=aci, ac.completions)

let run = () => {
  describe("autocomplete", () => {
    describe("generation", () => {
      test("invalidated cursor state/acFor still produces a valid autocomplete", () =>
        try defaultModel(~cursorState=enteringCS(), ()) |> (
          x => {
            acFor(x) |> ignore
            pass()
          }
        ) catch {
        | _ => fail()
        }
      )
      ()
    })
    describe("validate httpName varnames", () => {
      let tl = TLHandler(aHandler())
      let pd = PEventName(B.F(ID.fromInt(0), "foo"))
      test("/foo/bar is valid, no variables", () => {
        let value = "/foo/bar"
        expect(Entry.validate(tl, pd, value)) |> toEqual(None)
      })
      test("/:some/:variableNames/:here_1 is valid", () => {
        let value = "/:some/:variableNames/:here_1"
        expect(Entry.validate(tl, pd, value)) |> toEqual(None)
      })
      test("/:here-1 is not valid, no hyphens allowed in varnames", () => {
        let value = "/:here-1"
        expect(Entry.validate(tl, pd, value)) |> toEqual(
          Some("route variables must match /[a-z_][a-zA-Z0-9_]*/"),
        )
      })
    })
    describe("validate Worker names", () => {
      let tl = TLHandler(aHandler(~spec=defaultWorkerSpec, ()))
      let pd = PEventName(B.F(ID.fromInt(0), "foo"))
      test("foo is valid", () => {
        let value = "/foo/bar"
        expect(Entry.validate(tl, pd, value)) |> toEqual(None)
      })
      test("\"foo\" is not valid, no double quotes allowed in worker names", () => {
        let value = "\"foo\""
        expect(Entry.validate(tl, pd, value)) |> toEqual(
          Some("event name must match /[-a-zA-Z0-9$_@.&!*'(),%/:]+/"),
        )
      })
    })
    describe("validate CRON intervals", () => {
      let tl = TLHandler(aHandler(~spec=defaultCronSpec, ()))
      let pd = PEventModifier(B.F(ID.fromInt(0), "5mins"))
      test("Every 1hr is valid", () => {
        let value = "Every 1hr"
        expect(Entry.validate(tl, pd, value)) |> toEqual(None)
      })
      test("Every 5mins is not valid", () => {
        let value = "Every 5mins"
        expect(Entry.validate(tl, pd, value)) |> toEqual(
          Some("Every 5mins is an invalid CRON interval"),
        )
      })
      ()
    })
    describe("validate functions", () => {
      let pn1 = B.newF("title")
      let pn2 = B.newF("author")
      let fnAsTL = aFunction(
        ~params=list{
          {
            name: pn1,
            typ: B.newF(DType.TStr),
            args: list{},
            optional: false,
            description: "",
          },
          {
            name: pn2,
            typ: B.newF(DType.TStr),
            args: list{},
            optional: false,
            description: "",
          },
        },
        (),
      ) |> TL.ufToTL

      test("don't allow duplicate param names", () =>
        expect(validateFnParamNameFree(fnAsTL, B.new_(), "title")) |> toEqual(
          Some("`title` is already declared. Use another name."),
        )
      )
      test("allow unused names", () =>
        expect(validateFnParamNameFree(fnAsTL, B.new_(), "rating")) |> toEqual(None)
      )
      test("allow param name to be renamed the same", () =>
        expect(validateFnParamNameFree(fnAsTL, pn2, "author")) |> toEqual(None)
      )
    })
    describe("queryWhenEntering", () => {
      let m = enteringHandler()
      test("empty autocomplete doesn't highlight", () =>
        expect(acFor(m) |> (x => x.index)) |> toEqual(-1)
      )
      test("pressing a letter from the selected entry resets the entry selected", () =>
        expect(
          acForDB()
          |> setQuery(m, "String")
          |> setQuery(m, "String]")
          |> highlighted
          |> Option.map(~f=asName),
        ) |> toEqual(Some("[String]"))
      )
      test("Returning to empty unselects", () =>
        expect(acFor(m) |> setQuery(m, "String") |> setQuery(m, "") |> highlighted) |> toEqual(None)
      )
      test("lowercase search still finds uppercase results", () =>
        expect(
          acForDB() |> setQuery(m, "uuid") |> (x => x.completions) |> List.map(~f=asName),
        ) |> toEqual(list{"UUID", "[UUID]"})
      )
      test("search works anywhere in term", () =>
        expect(
          acForDB()
          |> setQuery(m, "assw")
          |> (x => x.completions)
          |> List.filter(~f=isStaticItem)
          |> List.map(~f=asName),
        ) |> toEqual(list{"Password", "[Password]"})
      )
      test("show results when the only option is the setQuery m", () =>
        expect(
          acForDB()
          |> setQuery(m, "[String]")
          |> (x => x.completions)
          |> List.filter(~f=isStaticItem)
          |> List.map(~f=asName)
          |> List.length,
        ) |> toEqual(1)
      )
      test("scrolling down a bit works", () =>
        expect(
          acForDB() |> setQuery(m, "i") |> selectDown |> selectDown |> (x => x.index),
        ) |> toEqual(2)
      )
      test("scrolling loops one way", () =>
        expect(
          acForDB() |> setQuery(m, "f") |> selectDown |> selectDown |> (x => x.index),
        ) |> toEqual(0)
      )
      test("scrolling loops the other way", () =>
        expect(
          acForDB() |> setQuery(m, "f") |> selectDown |> selectUp |> selectUp |> (x => x.index),
        ) |> toEqual(1)
      )
      test("scrolling loops the other way without going forward first", () =>
        expect(acForDB() |> setQuery(m, "f") |> selectUp |> (x => x.index)) |> toEqual(1)
      )
      test("scrolling backward works if we haven't searched yet", () =>
        expect(acForDB() |> selectUp |> selectUp |> (x => x.index)) |> toEqual(14)
      )
      test("Don't highlight when the list is empty", () =>
        expect(
          acForDB()
          |> setQuery(m, "Twit")
          |> selectDown
          |> selectDown
          |> setQuery(m, "Twit::1334xxx")
          |> (x => x.index),
        ) |> toEqual(-1)
      )
      test("By default the list shows results", () =>
        expect(
          acForDB() |> setQuery(m, "") |> (x => x.completions) |> List.length |> \"<>"(0),
        ) |> toEqual(true)
      )
      test("ordering = startsWith then case match then case insensitive match", () =>
        expect(
          acForDB()
          |> setQuery(m, "S")
          |> (x => x.completions)
          |> List.filter(~f=isStaticItem)
          |> List.map(~f=asName),
        ) |> toEqual(list{"String", "[String]", "Password", "[Password]"})
      )
      test("autocomplete does not have slash when handler is not HTTP", () => {
        let m = enteringEventNameHandler(~spec=defaultWorkerSpec, ())
        expect(acFor(m) |> setQuery(m, "") |> itemPresent(ACHTTPRoute("/")) |> not) |> toEqual(true)
      })
      test("autocomplete supports password type", () => {
        let m = enteringDBType()
        expect(acFor(m) |> setQuery(m, "Pass") |> itemPresent(ACDBColType("Password"))) |> toEqual(
          true,
        )
      })
      ()
    })
    describe("omnibox completion", () => {
      let m = creatingOmni
      test("entering a DB name that used to be invalid works", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "HTTP")
          |> itemPresent(ACOmniAction(NewDB(Some("HTTP")))),
        ) |> toEqual(true)
      )
      test("entering an invalid DB name works", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, ":[]'/31234myDB[]")
          |> itemPresent(ACOmniAction(NewDB(Some("MyDB")))),
        ) |> toEqual(true)
      )
      test("entering a DB name works", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "Mydbname")
          |> itemPresent(ACOmniAction(NewDB(Some("Mydbname")))),
        ) |> toEqual(true)
      )
      test("entering a short DB name works", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "me")
          |> itemPresent(ACOmniAction(NewDB(Some("Me")))),
        ) |> toEqual(true)
      )
      test("db names can be multicase", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "MyDBnaMe")
          |> itemPresent(ACOmniAction(NewDB(Some("MyDBnaMe")))),
        ) |> toEqual(true)
      )
      test("alphabetical only DB names #1", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "dbname1234::")
          |> itemPresent(ACOmniAction(NewDB(Some("Dbname1234")))),
        ) |> toEqual(true)
      )
      test("alphabetical only DB names #2", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "db_name::")
          |> itemPresent(ACOmniAction(NewDB(Some("Db_name")))),
        ) |> toEqual(true)
      )
      test("add capital for DB names", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "mydbname")
          |> itemPresent(ACOmniAction(NewDB(Some("Mydbname")))),
        ) |> toEqual(true)
      )
      test("General HTTP handler", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "asdkkasd")
          |> itemPresent(ACOmniAction(NewHTTPHandler(Some("/asdkkasd")))),
        ) |> toEqual(true)
      )
      test("can create routes #1 (base case)", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "/")
          |> itemPresent(ACOmniAction(NewHTTPHandler(Some("/")))),
        ) |> toEqual(true)
      )
      test("can create routes #2 (normal)", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "/asasdasd")
          |> itemPresent(ACOmniAction(NewHTTPHandler(Some("/asasdasd")))),
        ) |> toEqual(true)
      )
      test("can create routes #3 (parameterized)", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "/user/:userid/card/:cardid")
          |> itemPresent(ACOmniAction(NewHTTPHandler(Some("/user/:userid/card/:cardid")))),
        ) |> toEqual(true)
      )
      test("entering an invalid route name works", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "[]/31234myDB[]")
          |> itemPresent(ACOmniAction(NewHTTPHandler(Some("/31234myDB")))),
        ) |> toEqual(true)
      )
      test("fix names for routes", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "asasdasd")
          |> itemPresent(ACOmniAction(NewHTTPHandler(Some("/asasdasd")))),
        ) |> toEqual(true)
      )
      test("fix slashes for routes", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "//12//////345/6789//12/")
          |> itemPresent(ACOmniAction(NewHTTPHandler(Some("/12/345/6789/12")))),
        ) |> toEqual(true)
      )
      test("fix route name ", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "^hello/[]world")
          |> itemPresent(ACOmniAction(NewHTTPHandler(Some("/hello/world")))),
        ) |> toEqual(true)
      )
      test("create DB from route name", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "/route")
          |> itemPresent(ACOmniAction(NewDB(Some("Route")))),
        ) |> toEqual(true)
      )
      test("entering an invalid function name works", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, ":[]'/31234MyFn[]")
          |> itemPresent(ACOmniAction(NewFunction(Some("myFn")))),
        ) |> toEqual(true)
      )
      test("new worker option available by default", () =>
        expect(
          acFor(~target=None, m) |> itemPresent(ACOmniAction(NewWorkerHandler(None))),
        ) |> toEqual(true)
      )
      test("new repl option available by default", () =>
        expect(
          acFor(~target=None, m) |> itemPresent(ACOmniAction(NewReplHandler(None))),
        ) |> toEqual(true)
      )
      test("new cron option available by default", () =>
        expect(
          acFor(~target=None, m) |> itemPresent(ACOmniAction(NewCronHandler(None))),
        ) |> toEqual(true)
      )
      test("new function option available by default", () =>
        expect(acFor(~target=None, m) |> itemPresent(ACOmniAction(NewFunction(None)))) |> toEqual(
          true,
        )
      )
      test("new HTTP option available by default", () =>
        expect(
          acFor(~target=None, m) |> itemPresent(ACOmniAction(NewHTTPHandler(None))),
        ) |> toEqual(true)
      )
      test("can create function with name from query", () =>
        expect(
          acFor(~target=None, m)
          |> setQuery(m, "myfunction")
          |> itemPresent(ACOmniAction(NewFunction(Some("myfunction")))),
        ) |> toEqual(true)
      )
      ()
    })
    describe("code search", () => {
      let http = aHandler(
        ~tlid=TLID.fromInt(123),
        ~spec=PT.Handler.Spec.newHTTP("/hello", "GET"),
        ~expr=EFieldAccess(gid(), EVariable(gid(), "request"), "queryParams"),
        (),
      )

      let repl = aHandler(
        ~tlid=TLID.fromInt(456),
        ~spec=PT.Handler.Spec.newREPL("findingDori"),
        ~expr=FluidShortcuts.fn(~mod="Int", "add", list{}),
        (),
      )

      let fn = aFunction(
        ~tlid=TLID.fromInt(789),
        ~name="fn1",
        ~expr=ELet(gid(), "bunny", EInteger(gid(), 9L), EString(gid(), "\"hello\"")),
        (),
      )

      let cursorState = omniboxCS
      let m = defaultModel(~handlers=list{http, repl}, ~userFunctions=list{fn}, ~cursorState, ())

      let searchCache =
        m.searchCache
        |> Map.add(
          ~key=http.tlid,
          ~value=http.ast |> FluidAST.toExpr |> FluidPrinter.eToHumanString,
        )
        |> Map.add(
          ~key=repl.tlid,
          ~value=repl.ast |> FluidAST.toExpr |> FluidPrinter.eToHumanString,
        )
        |> Map.add(~key=fn.tlid, ~value=fn.ast |> FluidAST.toExpr |> FluidPrinter.eToHumanString)

      let m = {...m, searchCache: searchCache}
      test("find variable", () => {
        let foundActions = switch qSearch(m, "bunny") {
        | list{Goto(FocusedFn(_), tlid, "Found in function fn1", true)} if tlid == fn.tlid => true
        | _ => false
        }

        expect(foundActions) |> toEqual(true)
      })
      test("find string literal", () => {
        let foundActions = switch qSearch(m, "hello") {
        | list{Goto(FocusedFn(_), tlid, "Found in function fn1", true)} if tlid == fn.tlid => true
        | _ => false
        }

        expect(foundActions) |> toEqual(true)
      })
      test("find field access", () => {
        let foundActions = switch qSearch(m, "request.query") {
        | list{Goto(FocusedHandler(_), tlid, "Found in HTTP::/hello - GET", true)}
          if tlid == http.tlid => true
        | _ => false
        }

        expect(foundActions) |> toEqual(true)
      })
      test("find function call", () => {
        let foundActions = switch qSearch(m, "Int::add") {
        | list{Goto(FocusedHandler(_), tlid, "Found in REPL::findingDori", true)}
          if tlid == repl.tlid => true
        | _ => false
        }

        expect(foundActions) |> toEqual(true)
      })
    })
    ()
  })
  ()
}
