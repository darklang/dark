open Tester
open Prelude
open Fluid
module K = FluidKeyboard
module TL = Toplevel
open ProgramTypes.MatchPattern
open ProgramTypes.Expr
open FluidTestData
open FluidShortcuts

// These tests should be synced with the subset of tests in TestFluid.res that
// makes sense for patterns. See the extensive docs there for how this all
// works.

let eToStructure = Printer.eToStructure

let eToTestString = Printer.eToTestString

let pToString = Printer.pToString

let h = (expr): PT.Handler.t => {
  ast: FluidAST.ofExpr(expr),
  tlid: TLID.fromInt(7),
  spec: PT.Handler.Spec.newHTTP("/test", "GET"),
  pos: {x: 0, y: 0},
}

let mID = gid()

module TestCase = {
  type t = {
    ast: ProgramTypes.Expr.t,
    state: AppTypes.fluidState,
    pos: int,
    pat: FluidPattern.t,
    debug: bool,
  }

  let init = (~debug=false, ~pos=0, originalPat: FluidPattern.t): t => {
    let ast = EMatch(mID, EBlank(gid()), list{(originalPat, EBlank(gid()))})

    let state = {
      {
        ...defaultTestState,
        oldPos: pos,
        newPos: pos,
      }
    }

    {
      pat: originalPat,
      pos: pos,
      ast: ast,
      state: state,
      debug: debug,
    }
  }
}

module TestResult = {
  type t = (string, int)
}

let run = () => {
  let aStr = PString(gid(), "some string")

  let emptyStr = PString(gid(), "")
  let oneCharStr = PString(gid(), "c")
  let aShortInt = PInteger(gid(), 1L)
  let anInt = PInteger(gid(), 12345L)
  let aHugeInt = PInteger(gid(), 3000000000000000000L)
  let aFloat = PFloat(gid(), Positive, "123", "456")
  let aHugeFloat = PFloat(gid(), Positive, "123456789", "123456789")
  let aShortFloat = PFloat(gid(), Positive, "1", "2")
  let aPartialFloat = PFloat(gid(), Positive, "1", "")
  let trueBool = PBool(gid(), true)
  let falseBool = PBool(gid(), false)
  let aNull = PNull(gid())
  let five = PInteger(gid(), 5L)
  // let fiftySix = PInteger (mID, gid (), 56)
  // let seventyEight = PInteger (gid (), 78)
  let b = () => PBlank(gid())
  //let aPartialVar = FPPartial (gid (), "req")
  let aVar = PVariable(gid(), "variable")
  let aShortVar = PVariable(gid(), "v")
  let aConstructor = PConstructor(gid(), "Just", list{b()})

  let process = (inputs: list<FluidTypes.Msg.inputEvent>, tc: TestCase.t): TestResult.t => {
    // the extra chars caused by the `match` that wraps the pattern under test
    let extra = 12
    let pos = tc.pos + extra
    let s = {
      ...FluidTestData.defaultTestState,
      ac: AC.init,
      oldPos: pos,
      newPos: pos,
    }

    if tc.debug {
      Js.log2("state before ", FluidUtils.debugState(s))
      Js.log2("pattern before", eToStructure(tc.ast))
    }
    let astInfo = Fluid.ASTInfo.make(FluidAST.ofExpr(tc.ast), s)

    let result = {
      let h = FluidUtils.h(FluidAST.toExpr(astInfo.ast))
      let m = {...FluidTestData.defaultTestModel, handlers: Handlers.fromList(list{h})}

      let astInfo = Fluid.updateAutocomplete(m, TLID.fromInt(7), astInfo)
      List.fold(inputs, ~initial=astInfo, ~f=(astInfo: ASTInfo.t, input) =>
        Fluid.updateMsg'(m, h.tlid, astInfo, FluidInputEvent(input))
      )
    }

    let resultPat = switch FluidAST.toExpr(result.ast) {
    | EMatch(_, _, list{(pat, _)}) => pat
    | _ => failwith("can't match: " ++ eToTestString(FluidAST.toExpr(result.ast)))
    }

    if tc.debug {
      Js.log2("state after", FluidUtils.debugState(result.state))
      Js.log2("pattern after", eToStructure(FluidAST.toExpr(result.ast)))
    }
    (pToString(resultPat), max(0, result.state.newPos - extra))
  }

  let render = (case: TestCase.t): TestResult.t => process(list{}, case)

  let keypress = (key: K.key): FluidTypes.Msg.inputEvent => Keypress({
    key: key,
    shiftKey: false,
    altKey: false,
    metaKey: false,
    ctrlKey: false,
  })

  let del = (case: TestCase.t): TestResult.t => process(list{DeleteContentForward}, case)

  let bs = (case: TestCase.t): TestResult.t => process(list{DeleteContentBackward}, case)

  let press = (key: K.key, case: TestCase.t): TestResult.t => process(list{keypress(key)}, case)

  let space = (case: TestCase.t): TestResult.t => process(list{keypress(K.Space)}, case)

  // let tab = (case: TestCase.t): TestResult.t => process(list{keypress(K.Tab)}, case)

  // let shiftTab = (case: TestCase.t): TestResult.t => process(list{keypress(K.ShiftTab)}, case)

  let inputs = (inputs: list<FluidTypes.Msg.inputEvent>, case: TestCase.t): TestResult.t =>
    process(inputs, case)

  let insert = (s: string, case: TestCase.t): TestResult.t => process(list{InsertText(s)}, case)

  let insertMany = (strings: list<string>, case: TestCase.t): TestResult.t =>
    process(List.map(strings, ~f=s => FluidTypes.Msg.InsertText(s)), case)

  let blank = "***"

  let t = (
    ~pos=0,
    ~debug=false,
    name: string,
    pat: fluidPattern,
    fn: TestCase.t => TestResult.t,
    expected: TestResult.t,
  ) => {
    let testName =
      name ++
      (" - `" ++
      ((pToString(pat) |> Regex.replace(~re=Regex.regex("\n"), ~repl=" ")) ++ "`"))

    let case = TestCase.init(~pos, ~debug, pat)

    test(testName, () => expect(fn(case)) |> toEqual(expected))
  }

  describe("Strings", () => {
    t("render string", aStr, render, ("\"some string\"", 0))
    t("insert mid string", aStr, ~pos=3, insert("c"), ("\"socme string\"", 4))
    t("del mid string", aStr, ~pos=3, del, ("\"soe string\"", 3))
    t("bs mid string", ~pos=4, aStr, bs, ("\"soe string\"", 3))
    t("insert empty string", ~pos=1, emptyStr, insert("c"), ("\"c\"", 2))
    t("del empty string", ~pos=1, emptyStr, del, ("\"\"", 1))
    t("del empty string from outside", emptyStr, del, (blank, 0))
    t("bs empty string", emptyStr, ~pos=1, bs, (blank, 0))
    t("bs outside empty string", emptyStr, ~pos=2, bs, ("\"\"", 1))
    t("bs near-empty string", oneCharStr, ~pos=2, bs, ("\"\"", 1))
    t("del near-empty string", oneCharStr, ~pos=1, del, ("\"\"", 1))
    t("insert outside string", aStr, insert("c"), ("\"some string\"", 0))
    t("del outside string", aStr, del, ("\"some string\"", 0))
    t("bs outside string", aStr, bs, ("\"some string\"", 0))
    t("insert start of string", aStr, ~pos=1, insert("c"), ("\"csome string\"", 2))
    t("del start of string", aStr, ~pos=1, del, ("\"ome string\"", 1))
    t("bs start of string", aStr, ~pos=1, bs, ("\"some string\"", 0))
    t("insert end of string", aStr, ~pos=12, insert("c"), ("\"some stringc\"", 13))
    t("del end of string", aStr, ~pos=12, del, ("\"some string\"", 12))
    t("bs end of string", aStr, ~pos=12, bs, ("\"some strin\"", 11))
    t("insert after end", aStr, ~pos=13, insert("c"), ("\"some string\"", 13))
    t("del after end of string", aStr, ~pos=13, del, ("\"some string\"", 13))
    t("bs after end", aStr, ~pos=13, bs, ("\"some string\"", 12))
    t("insert space in string", aStr, ~pos=3, space, ("\"so me string\"", 4))
    t("del space in string", aStr, ~pos=5, del, ("\"somestring\"", 5))
    t("bs space in string", aStr, ~pos=6, bs, ("\"somestring\"", 5))
    t("final quote is swallowed", aStr, ~pos=12, insert("\""), ("\"some string\"", 13))
    ()
  })
  describe("Integers", () => {
    t("render int", anInt, render, ("12345", 0))
    t("insert 0 at front ", anInt, insert("0"), ("12345", 0))
    t("insert at end of short", aShortInt, ~pos=1, insert("2"), ("12", 2))
    t("insert not a number", anInt, insert("c"), ("12345", 0))
    t("insert start of number", anInt, insert("5"), ("512345", 1))
    t("del start of number", anInt, del, ("2345", 0))
    t("bs start of number", anInt, bs, ("12345", 0))
    t("insert end of number", anInt, ~pos=5, insert("0"), ("123450", 6))
    t("del end of number", anInt, ~pos=5, del, ("12345", 5))
    t("bs end of number", anInt, ~pos=5, bs, ("1234", 4))
    t("insert number at scale", aHugeInt, ~pos=5, insert("9"), ("3000090000000000000", 6))
    t("insert number at scale", aHugeInt, insert("9"), ("930000000000000000", 1))
    t("insert number at scale", aHugeInt, ~pos=19, insert("9"), ("3000000000000000000", 19))
    let oneShorterThanMax63BitInt = PInteger(gid(), 922337203685477580L)

    t(
      "insert number at scale",
      oneShorterThanMax63BitInt,
      ~pos=18,
      insert("7"),
      ("9223372036854775807", 19),
    )
    t(
      "insert number at scale",
      oneShorterThanMax63BitInt,
      ~pos=18,
      insert("9"),
      ("922337203685477580", 18),
    )
    ()
  })
  describe("Floats", () => {
    t("insert . converts to float - end", anInt, ~pos=5, insert("."), ("12345.", 6))
    t("insert . converts to float - middle", anInt, ~pos=3, insert("."), ("123.45", 4))
    t("insert . converts to float - start", anInt, insert("."), (".12345", 1))
    t("insert . converts to float - short", aShortInt, ~pos=1, insert("."), ("1.", 2))
    t("continue after adding dot", aPartialFloat, ~pos=2, insert("2"), ("1.2", 3))
    t("insert zero in whole - start", aFloat, insert("0"), ("123.456", 0))
    t("insert int in whole - start", aFloat, insert("9"), ("9123.456", 1))
    t("insert int in whole - middle", aFloat, ~pos=1, insert("0"), ("1023.456", 2))
    t("insert int in whole - end", aFloat, ~pos=3, insert("0"), ("1230.456", 4))
    t("insert int in fraction - start", aFloat, ~pos=4, insert("0"), ("123.0456", 5))
    t("insert int in fraction - middle", aFloat, ~pos=6, insert("0"), ("123.4506", 7))
    t("insert int in fraction - end", aFloat, ~pos=7, insert("0"), ("123.4560", 8))
    t("insert non-int in whole", aFloat, ~pos=2, insert("c"), ("123.456", 2))
    t("insert non-int in fraction", aFloat, ~pos=6, insert("c"), ("123.456", 6))
    t("del dot", aFloat, ~pos=3, del, ("123456", 3))
    t("del dot at scale", aHugeFloat, ~pos=9, del, ("123456789123456789", 9))

    let maxPosIntWithDot = PFloat(gid(), Positive, "9223372036854775", "807")
    let maxPosIntPlus1WithDot = PFloat(gid(), Positive, "9223372036854775", "808")

    t("del dot at limit1", maxPosIntWithDot, ~pos=16, del, ("9223372036854775807", 16))
    t("del dot at limit2", maxPosIntPlus1WithDot, ~pos=16, del, ("922337203685477580", 16))
    t("del start of whole", aFloat, del, ("23.456", 0))
    t("del middle of whole", aFloat, ~pos=1, del, ("13.456", 1))
    t("del end of whole", aFloat, ~pos=2, del, ("12.456", 2))
    t("del start of fraction", aFloat, ~pos=4, del, ("123.56", 4))
    t("del middle of fraction", aFloat, ~pos=5, del, ("123.46", 5))
    t("del end of fraction", aFloat, ~pos=6, del, ("123.45", 6))
    t("del dot converts to int", aFloat, ~pos=3, del, ("123456", 3))
    t("del dot converts to int, no fraction", aPartialFloat, ~pos=1, del, ("1", 1))
    t("bs dot", aFloat, ~pos=4, bs, ("123456", 3))
    t("bs frac of float", aShortFloat, ~pos=3, bs, ("1.", 2))
    t("bs whole of float", aShortFloat, ~pos=1, bs, (".2", 0))
    t("bs dot at scale", aHugeFloat, ~pos=10, bs, ("123456789123456789", 9))
    t("bs dot at limit1", maxPosIntWithDot, ~pos=17, bs, ("9223372036854775807", 16))
    t("bs dot at limit2", maxPosIntPlus1WithDot, ~pos=17, bs, ("922337203685477580", 16))
    t("bs start of whole", aFloat, ~pos=1, bs, ("23.456", 0))
    t("bs middle of whole", aFloat, ~pos=2, bs, ("13.456", 1))
    t("bs end of whole", aFloat, ~pos=3, bs, ("12.456", 2))
    t("bs start of fraction", aFloat, ~pos=5, bs, ("123.56", 4))
    t("bs middle of fraction", aFloat, ~pos=6, bs, ("123.46", 5))
    t("bs end of fraction", aFloat, ~pos=7, bs, ("123.45", 6))
    t("bs dot converts to int", aFloat, ~pos=4, bs, ("123456", 3))
    t("bs dot converts to int, no fraction", aPartialFloat, ~pos=2, bs, ("1", 1))
    t("continue after adding dot", aPartialFloat, ~pos=2, insert("2"), ("1.2", 3))
    ()
  })
  describe("Bools", () => {
    t("insert start of true", trueBool, insert("c"), ("ctrue", 1))
    t("del start of true", trueBool, del, ("rue", 0))
    t("bs start of true", trueBool, bs, ("true", 0))
    t("insert end of true", trueBool, ~pos=4, insert("0"), ("true0", 5))
    t("del end of true", trueBool, ~pos=4, del, ("true", 4))
    t("bs end of true", trueBool, ~pos=4, bs, ("tru", 3))
    t("insert middle of true", trueBool, ~pos=2, insert("0"), ("tr0ue", 3))
    t("del middle of true", trueBool, ~pos=2, del, ("tre", 2))
    t("bs middle of true", trueBool, ~pos=2, bs, ("tue", 1))
    t("insert start of false", falseBool, insert("c"), ("cfalse", 1))
    t("del start of false", falseBool, del, ("alse", 0))
    t("bs start of false", falseBool, bs, ("false", 0))
    t("insert end of false", falseBool, ~pos=5, insert("0"), ("false0", 6))
    t("del end of false", falseBool, ~pos=5, del, ("false", 5))
    t("bs end of false", falseBool, ~pos=5, bs, ("fals", 4))
    t("insert middle of false", falseBool, ~pos=2, insert("0"), ("fa0lse", 3))
    t("del middle of false", falseBool, ~pos=2, del, ("fase", 2))
    t("bs middle of false", falseBool, ~pos=2, bs, ("flse", 1))
    ()
  })
  describe("Nulls", () => {
    t("insert start of null", aNull, insert("c"), ("cnull", 1))
    t("del start of null", aNull, del, ("ull", 0))
    t("bs start of null", aNull, bs, ("null", 0))
    t("insert end of null", aNull, ~pos=4, insert("0"), ("null0", 5))
    t("del end of null", aNull, ~pos=4, del, ("null", 4))
    t("bs end of null", aNull, ~pos=4, bs, ("nul", 3))
    t("insert middle of null", aNull, ~pos=2, insert("0"), ("nu0ll", 3))
    t("del middle of null", aNull, ~pos=2, del, ("nul", 2))
    t("bs middle of null", aNull, ~pos=2, bs, ("nll", 1))
    ()
  })
  describe("Blanks", () => {
    t("insert middle of blank->string", b(), ~pos=3, insert("\""), ("\"\"", 1))
    t("del middle of blank->blank", b(), ~pos=3, del, (blank, 3))
    t("bs middle of blank->blank", b(), ~pos=3, bs, (blank, 0))
    t("insert blank->string", b(), insert("\""), ("\"\"", 1))
    t("del blank->string", emptyStr, del, (blank, 0))
    t("bs blank->string", emptyStr, ~pos=1, bs, (blank, 0))
    t("insert blank->int", b(), insert("5"), ("5", 1))
    t("insert blank->int", b(), insert("0"), ("0", 1))
    t("del int->blank ", five, del, (blank, 0))
    t("bs int->blank ", five, ~pos=1, bs, (blank, 0))
    t("insert end of blank->int", b(), ~pos=1, insert("5"), ("5", 1))
    t("insert partial", b(), insert("t"), ("t", 1))
    t(
      "backspacing your way through a partial finishes",
      trueBool,
      ~pos=4,
      inputs(list{
        DeleteContentBackward,
        DeleteContentBackward,
        DeleteContentBackward,
        DeleteContentBackward,
        keypress(K.Left),
      }),
      ("***", 0),
    )
    t("insert blank->space", b(), press(K.Space), (blank, 0))
    ()
  })
  describe("Variables", () => {
    t("insert middle of variable", aVar, ~pos=5, insert("c"), ("variacble", 6))
    t("del middle of variable", aVar, ~pos=5, del, ("variale", 5))
    t("insert capital works", aVar, ~pos=5, insert("A"), ("variaAble", 6))
    t("can't insert invalid", aVar, ~pos=5, insert("$"), ("variable", 5))
    t("del variable", aShortVar, del, (blank, 0))
    t("del long variable", aVar, del, ("ariable", 0))
    t("del mid variable", aVar, ~pos=6, del, ("variabe", 6))
    t("bs variable", aShortVar, ~pos=1, bs, (blank, 0))
    t("bs mid variable", aVar, ~pos=8, bs, ("variabl", 7))
    t("bs mid variable", aVar, ~pos=6, bs, ("variale", 5))
    ()
  })
  describe("Constructors", () => {
    t("arguments work in constructors", aConstructor, ~pos=5, insert("t"), ("Just t", 6))
    t("int arguments work in constructors", aConstructor, ~pos=5, insert("5"), ("Just 5", 6))
    t("bs on a constructor deletes", aConstructor, ~pos=4, bs, ("Jus", 3))
    t("del on a constructor deletes", aConstructor, del, ("ust", 0))
    t("space on a constructor blank does nothing", aConstructor, ~pos=5, space, ("Just ***", 5))
    /* TODO: test renaming constructors.
     * It's not too useful yet because there's only 4 constructors and,
     * hence, unlikely that anyone will rename them this way.
     * Also, the names of the temporary variables used to store the old arguments of a changed
     * constructor are randomly generated and would be hard to test */
    ()
  })
  describe("Tuples", () => {
    describe("render", () => {
      t("blank tuple pattern", tuplePattern2WithBothBlank, render, ("(***,***)", 0))
      t("simple tuple pattern", tuplePattern2WithNoBlank, render, ("(56,78)", 0))
      // t( //TUPLETODO
      //   "a very long tuple pattern wraps",
      //   match'(b, list{(tuplePatternHuge, b)}),
      //   render,
      //   "~match ___\n (56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n 78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,\n 56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n 78,56,78,56,78,56,78,56,78,56,78) -> ___\n",
      // )
      // t(
      //   "a tuple of long floats does not break upon wrap",
      //   match'(
      //     b,
      //     list{
      //       (
      //         pTuple(
      //           PFloat(
      //             gid(),
      //             Positive,
      //             "4611686018427387",
      //             "12345678901234567989048290381902830912830912830912830912309128901234567890123456789",
      //           ),
      //           PFloat(
      //             gid(),
      //             Positive,
      //             "4611686018427387",
      //             "1234567890183918309183091809183091283019832345678901234567890123456789",
      //           ),
      //           list{PFloat(gid(), Positive, "4611686018427387", "123456")},
      //         ),
      //         b,
      //       ),
      //     },
      //   ),
      //   render,
      //   "~match ___\n (4611686018427387.12345678901234567989048290381902830912830912830912830912309128901234567890123456789,\n 4611686018427387.1234567890183918309183091809183091283019832345678901234567890123456789,\n 4611686018427387.123456) -> ___\n",
      // )
      ()
    })

    describe("navigate", () => {
      // t( // TUPLETODO we need some special code for this. it works with tuple Exprs
      //   "ctrl+left at the beginning of tuple pattern item moves to beginning of next tuple item",
      //   match'(
      //     b,
      //     list{
      //       (
      //         pTuple(
      //           fiftySixPat,
      //           seventyEightPat,
      //           list{fiftySixPat, seventyEightPat, fiftySixPat, seventyEightPat},
      //         ),
      //         b,
      //       ),
      //     },
      //   ),
      //   ~pos=22, // after the third comma, before the 2nd 78
      //   ctrlLeft,
      //   "match ___\n  (56,78,~56,78,56,78) -> ___\n",
      // )
      // t(
      //   "ctrl+right at the end of tuple item moves to end of next tuple item",
      //   match'(b, list{(tuplePattern6, b)}),
      //   ~pos=16,
      //   ctrlRight,
      //   "match ___\n  (56,78,56~,78,56,78) -> ___\n",
      // )
      ()
    })

    if defaultTestProps.settings.allowTuples {
      describe("create", () => {
        t("create tuple pattern", bPat(), insert("("), ("(***,***)", 1))

        t(
          "create and fill in tuple pattern",
          bPat(),
          insertMany(list{"(", "1", ",", "2", ")"}),
          ("(1,2)", 5),
        )
        ()
      })
    }

    describe("insert", () => {
      t(
        "insert into empty tuple pattern inserts",
        tuplePattern2WithBothBlank,
        ~pos=1,
        insert("5"),
        ("(5,***)", 2),
      )
      // TUPLETODO discrepency between pat and expr behaviour
      // t(
      //   "inserting before a tuple is no-op",
      //   tuplePattern2WithBothBlank,
      //
      //   insert("5"),
      //   ("(***,***)", 0),
      // )
      t(
        "insert space into tuple does nothing",
        tuplePattern2WithNoBlank,
        ~pos=6, // right before closing )
        press(K.Space),
        ("(56,78)", 6),
      )
      t(
        "insert separator after opening parens creates blank",
        pTuple(pInt(1), pInt(2), list{pInt(3)}),
        ~pos=1, // after the (
        insert(","),
        ("(***,1,2,3)", 1),
      )
      t(
        "insert separator before closing parens creates blank",
        pTuple(pInt(1), pInt(2), list{pInt(3)}), // (1,2,3)
        ~pos=6, // before the )
        insert(","),
        ("(1,2,3,***)", 7),
      )
      t(
        "insert separator after separator creates blank",
        pTuple(pInt(1), pInt(2), list{pInt(3)}), // (1,2,3)
        ~pos=5, // after the second comma
        insert(","),
        ("(1,2,***,3)", 5),
      )
      t(
        "inserting space into simple tuple does nothing",
        tuplePattern2WithNoBlank,
        ~pos=3,
        press(K.Space),
        ("(56,78)", 3),
      )
      t(
        "insert separator before item creates blank",
        tuplePattern2WithNoBlank,
        ~pos=1,
        insert(","),
        ("(***,56,78)", 1),
      )
      t(
        "insert separator after item creates blank",
        tuplePattern2WithNoBlank,
        ~pos=6, // after 78
        insert(","),
        ("(56,78,***)", 7),
      )
      t(
        "insert , in string in tuple types ,",
        pTuple(pString("01234567890123456789012345678901234567890"), fiftySixPat, list{}),
        ~pos=42, // right before the last 0
        insert(","),
        ("(\"0123456789012345678901234567890123456789,0\",56)", 43),
      )

      t(
        "insert separator just before another skips over it",
        tuplePattern2WithNoBlank,
        ~pos=3, // before the first comma
        insert(","),
        ("(56,78)", 4),
      )
      t(
        "insert separator just after another creates blank",
        tuplePattern2WithNoBlank,
        ~pos=4, // just after the comma
        insert(","),
        ("(56,***,78)", 4),
      )

      t(
        "insert separator mid int does nothing special ",
        tuplePattern2WithNoBlank,
        ~pos=2, // halfway through `56`
        insert(","),
        ("(56,78)", 2),
      )
      t(
        "insert separator mid string does nothing special ",
        tuplePattern3WithStrs,
        ~pos=3, // in between the a and b of the first str
        insert(","),
        ("(\"a,b\",\"cd\",\"ef\")", 4),
      )
      t(
        "close bracket at end of tuple is swallowed",
        tuplePattern2WithNoBlank,
        ~pos=6, // right before closing )
        insert(")"),
        ("(56,78)", 7),
      )
      t(
        "trying to write over a pattern with another type does nothing",
        tuplePattern3WithStrs,
        ~pos=1, // in between the a and b of the first str
        insert("1"),
        ("(\"ab\",\"cd\",\"ef\")", 1),
      )
      ()
    })

    describe("delete", () => {
      // 2-tuple, no blanks
      t(
        "deleting ( from a filled 2-tuple does nothing",
        tuplePattern2WithNoBlank,
        del,
        ("(56,78)", 0),
      )
      t(
        "deleting ) from a filled 2-tuple just moves cursor left",
        tuplePattern2WithNoBlank,
        ~pos=7,
        bs,
        ("(56,78)", 6),
      )
      t(
        "deleting , from a filled 2-tuple leaves only the first item",
        tuplePattern2WithNoBlank, // (56,78)
        ~pos=4, // at the ,
        bs,
        ("56", 2),
      )

      // 2-tuple, first blank
      t(
        "deleting ( from a 2-tuple with first value blank converts to the non-blank value",
        tuplePattern2WithFirstBlank,
        del,
        ("78", 0),
      )
      t(
        "deleting ) from a 2-tuple with first value blank just moves the cursor to left of )",
        tuplePattern2WithFirstBlank,
        ~pos=8, // just after )
        bs,
        ("(***,78)", 7),
      )
      t(
        "deleting , from a 2-tuple with first value blank converts to a blank",
        tuplePattern2WithFirstBlank,
        ~pos=4, // just after ,
        del,
        ("***", 0),
      )

      // 2-tuple, second blank
      t(
        "deleting ( from a 2-tuple with second value blank converts to the non-blank value",
        tuplePattern2WithSecondBlank,
        del,
        ("56", 0),
      )
      t(
        "deleting ) from a 2-tuple with second value blank just moves the cursor left",
        tuplePattern2WithSecondBlank,
        ~pos=7, // just before )
        del,
        ("(56,***)", 7),
      )
      t(
        "deleting , from a 2-tuple with second value blank converts to the non-blank value",
        tuplePattern2WithSecondBlank,
        ~pos=3, // just before ,
        del,
        ("56", 2),
      )

      // 2-tuple, both blank
      t(
        "deleting ( from a blank 2-tuple converts to blank",
        tuplePattern2WithBothBlank,
        del,
        ("***", 0),
      )
      t(
        "deleting ) from a blank 2-tuple just moves the cursor left",
        tuplePattern2WithBothBlank,
        ~pos=9,
        bs,
        ("(***,***)", 8),
      )
      t(
        "deleting , from a blank 2-tuple pattern replaces the tuple with a blank",
        tuplePattern2WithBothBlank,
        ~pos=5,
        bs,
        ("***", 0),
      )

      // 3-tuple, no blanks
      t(
        "deleting ( from a filled 3-tuple does nothing",
        tuplePattern3WithNoBlanks,
        del,
        ("(56,78,56)", 0),
      )
      t(
        "deleting ) from a filled 3-tuple just moves cursor left",
        tuplePattern3WithNoBlanks,
        ~pos=10, // just after )
        bs,
        ("(56,78,56)", 9),
      )
      t(
        "deleting first , from a filled 3-tuple removes 2nd item",
        tuplePattern3WithNoBlanks,
        ~pos=3, // just before ,
        del,
        ("(56,56)", 3),
      )
      t(
        "deleting second , from a filled 3-tuple removes 3rd item",
        tuplePattern3WithNoBlanks,
        ~pos=6, // just before ,
        del,
        ("(56,78)", 6),
      )

      // 3-tuple, first blank
      t(
        "deleting ( from a 3-tuple with first item blank does nothing",
        tuplePattern3WithFirstBlank,
        del,
        ("(***,78,56)", 0),
      )
      t(
        "deleting first , from a 3-tuple with first item blank removes 2nd item",
        tuplePattern3WithFirstBlank,
        ~pos=4, // just before ,
        del,
        ("(***,56)", 1),
      )
      t(
        "deleting second , from a 3-tuple with first item blank removes 3rd item",
        tuplePattern3WithFirstBlank,
        ~pos=8, // just after ,
        bs,
        ("(***,78)", 7),
      )
      t(
        "deleting ) from a 3-tuple with first item blank just moves cursor left",
        tuplePattern3WithFirstBlank,
        ~pos=11, // just after )
        bs,
        ("(***,78,56)", 10),
      )

      // 3-tuple, second blank
      t(
        "deleting ( from a 3-tuple with the second item blank does nothing",
        tuplePattern3WithSecondBlank,
        del,
        ("(56,***,78)", 0),
      )
      t(
        "deleting first , from a 3-tuple with the second item blank removes the blank",
        tuplePattern3WithSecondBlank,
        ~pos=3, // just before ,
        del,
        ("(56,78)", 3),
      )
      t(
        "deleting second , from a 3-tuple with the second item blank removes 3rd item",
        tuplePattern3WithSecondBlank, // (56,***,78)
        ~pos=7, // just before ,
        del,
        ("(56,***)", 4),
      )
      t(
        "deleting ) from a 3-tuple with the second item blank just moves cursor left",
        tuplePattern3WithSecondBlank,
        ~pos=11, // just after )
        bs,
        ("(56,***,78)", 10),
      )

      // 3-tuple, third blank `(56,78,***)`
      t(
        "deleting ( from a 3-tuple with the third item blank does nothing",
        tuplePattern3WithThirdBlank,
        del,
        ("(56,78,***)", 0),
      )
      t(
        "deleting first , from a 3-tuple with the third item blank removes the second item",
        tuplePattern3WithThirdBlank,
        ~pos=3, // just before ,
        del,
        ("(56,***)", 3), // or maybe 1char to the right of this
      )
      t(
        "deleting second , from a 3-tuple with the third item blank removes the blank",
        tuplePattern3WithThirdBlank,
        ~pos=7, // just after ,
        bs,
        ("(56,78)", 6),
      )
      t(
        "deleting ) from a 3-tuple with the third item blank just moves the cursor left",
        tuplePattern3WithThirdBlank,
        ~pos=11, // just after )
        bs,
        ("(56,78,***)", 10),
      )

      // 3-tuple, first non-blank `(56,***,***)`
      t(
        "deleting ( from a 3-tuple with only first item replaces it with that item",
        tuplePattern3WithFirstFilled,
        del,
        ("56", 0),
      )
      t(
        "deleting first , from a 3-tuple with only first item filled ***",
        tuplePattern3WithFirstFilled,
        ~pos=3, // just before ,
        del,
        ("(56,***)", 3),
      )
      t(
        "deleting second , from a 3-tuple with only first item filled ***",
        tuplePattern3WithFirstFilled,
        ~pos=8, // just after ,
        bs,
        ("(56,***)", 4),
      )
      t(
        "deleting ) from a 3-tuple with only first item filled just moves cursor left",
        tuplePattern3WithFirstFilled,
        ~pos=12, // just after )
        bs,
        ("(56,***,***)", 11),
      )

      // 3-tuple, second non-blank `(***,56,***)`
      t(
        "deleting ( from a 3-tuple with only second item filled replaces the tuple with the item",
        tuplePattern3WithSecondFilled,
        del,
        ("56", 0),
      )
      t(
        "deleting first , from a 3-tuple with only second item filled removes the second item",
        tuplePattern3WithSecondFilled,
        ~pos=4, // just before ,
        del,
        ("(***,***)", 1),
      )
      t(
        "deleting second , from a 3-tuple with only second item filled removes the ending blank",
        tuplePattern3WithSecondFilled,
        ~pos=8, // just after ,
        bs,
        ("(***,56)", 7),
      )
      t(
        "deleting ) from a 3-tuple with only second item filled just moves the cursor left",
        tuplePattern3WithSecondFilled,
        ~pos=12, // just after )
        bs,
        ("(***,56,***)", 11),
      )

      // 3-tuple, third non-blank `(***,***,56)`
      t(
        "deleting ( from a 3-tuple with only third item filled replaces the tuple with the item",
        tuplePattern3WithThirdFilled,
        del,
        ("56", 0),
      )
      t(
        "deleting first , from a 3-tuple with only third item filled removes the second blank",
        tuplePattern3WithThirdFilled,
        ~pos=4, // just before ,
        del,
        ("(***,56)", 1),
      )
      t(
        "deleting second , from a 3-tuple with only third item filled removes the filled item",
        tuplePattern3WithThirdFilled,
        ~pos=9, // just after ,
        bs,
        ("(***,***)", 5),
      )
      t(
        "deleting ) from a 3-tuple with only third item filled just moves the cursor left",
        tuplePattern3WithThirdFilled,
        ~pos=12, // just after )
        bs,
        ("(***,***,56)", 11),
      )

      // 3-tuple, all blank `(***,***,***)`
      t(
        "deleting ( from a 3-tuple of all blanks replaces the tuple with a blank",
        tuplePattern3WithAllBlank,
        del,
        ("***", 0),
      )
      t(
        "deleting first , from a 3-tuple of all blanks removes the second blank",
        tuplePattern3WithAllBlank,
        ~pos=4, // just before ,
        del,
        ("(***,***)", 1),
      )
      t(
        "deleting second , from a 3-tuple of all blanks removes the third blank",
        tuplePattern3WithAllBlank,
        ~pos=9, // just after ,
        bs,
        ("(***,***)", 5),
      )
      t(
        "deleting ) from a 3-tuple of all blanks just moves left",
        tuplePattern3WithAllBlank,
        ~pos=13, // just after )
        bs,
        ("(***,***,***)", 12), // I could see this instead turning into `~***`
      )
      ()
    })
    ()
  })
  ()
}
