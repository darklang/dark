open Prelude
open ProgramTypes.Expr
module MP = ProgramTypes.MatchPattern

// ----------------
// Shortcuts
// ----------------
let b = FluidExpression.newB()
let bPat = () => ProgramTypes.MatchPattern.MPBlank(gid())

open FluidShortcuts

// ----------------
// test data
// ----------------

// ----------------
// String
// ----------------
let aStr = EString(gid(), "some string")

let aStrEscape = EPartial(gid(), "so\\me string", EString(gid(), "some string"))

let emptyStr = EString(gid(), "")

let oneCharStr = EString(gid(), "c")

let numSegment = "123456789_"

let letterSegment = "abcdefghi,"

let mlSegment = numSegment ++ letterSegment ++ numSegment ++ letterSegment

let mlStr = str(mlSegment ++ mlSegment ++ numSegment)

let mlStrWSpace = str(
  mlSegment ++
  " " ++
  numSegment ++
  " " ++
  letterSegment ++
  " " ++
  numSegment ++
  " " ++
  letterSegment,
)

// ----------------
// Ints
// ----------------
let aShortInt = EInteger(gid(), 1L)

let anInt = EInteger(gid(), 12345L)

let aHugeInt = EInteger(gid(), 3000000000000000000L)

let max63BitInt = EInteger(gid(), 9223372036854775808L)

let oneShorterThanMax63BitInt = EInteger(gid(), 922337203685477580L)

let five = EInteger(gid(), 5L)

let six = EInteger(gid(), 6L)

let fiftySix = EInteger(gid(), 56L)

let seventyEight = EInteger(gid(), 78L)

// ----------------
// Floats
// ----------------
let aFloat = EFloat(gid(), Positive, "123", "456")

let negFloat = EFloat(gid(), Negative, "123", "456")

let aFloatWithoutWhole = EFloat(gid(), Positive, "", "1")

let aHugeFloat = EFloat(gid(), Positive, "123456789", "123456789")

let aPartialFloat = EFloat(gid(), Positive, "1", "")

let maxPosIntWithDot = floatStr(Positive, "9223372036854775", "807")

let maxPosIntPlus1WithDot = floatStr(Positive, "9223372036854775", "808")

// ----------------
// Bools
// ----------------
let trueBool = EBool(gid(), true)

let falseBool = EBool(gid(), false)

// ----------------
// Null
// ----------------
let aNull = ENull(gid())

// ----------------
// Partials
// ----------------
let aPartialVar = EPartial(gid(), "req", b)

// ----------------
// Lets
// ----------------
let completelyEmptyLet = ELet(gid(), "", b, b)

// let *** = ___\n5
let emptyLet = ELet(gid(), "", b, EInteger(gid(), 5L))

// let *** = 6\n___
let nonEmptyLetWithBlankEnd = ELet(gid(), "", EInteger(gid(), 6L), b)

let nonEmptyLet = ELet(gid(), "", EInteger(gid(), 6L), EInteger(gid(), 5L))

let twoLets = ELet(
  gid(),
  "x",
  EInteger(gid(), 5L),
  ELet(gid(), "y", EInteger(gid(), 6L), EInteger(gid(), 7L)),
)

let longLets = ELet(
  gid(),
  "firstLetName",
  EString(gid(), "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
  ELet(gid(), "secondLetName", EString(gid(), "0123456789"), EString(gid(), "RESULT")),
)

let letWithLhs = ELet(gid(), "n", EInteger(gid(), 6L), EInteger(gid(), 5L))

let letWithBinding = (bindingName: string, expr: t) => ELet(
  gid(),
  bindingName,
  EInteger(gid(), 6L),
  expr,
)

let letWithUsedBinding = (bindingName: string) =>
  letWithBinding(bindingName, EVariable(gid(), bindingName))

// ----------------
// Match
// ----------------
let emptyMatch = {
  let mID = gid()
  EMatch(mID, b, list{(MPBlank(gid()), b)})
}

let matchWithCond = (cond: t) => {
  let mID = gid()
  EMatch(mID, cond, list{(MPBlank(gid()), b)})
}

let emptyMatchWithTwoPatterns = {
  let mID = gid()
  EMatch(mID, b, list{(MPBlank(gid()), b), (MPBlank(gid()), b)})
}

let matchWithTwoPatterns = {
  let mID = gid()
  EMatch(mID, b, list{(MPInteger(gid(), 3L), b), (MPInteger(gid(), 4L), b)})
}

let matchWithPattern = {
  let mID = gid()
  EMatch(mID, b, list{(MPInteger(gid(), 3L), b)})
}

let matchWithConstructorPattern = {
  let mID = gid()
  EMatch(mID, b, list{(MPConstructor(gid(), "Just", list{}), b)})
}

let matchWithOneExpr = (expr: t) => {
  let mID = gid()
  EMatch(mID, b, list{(MPBlank(gid()), expr)})
}

let matchWithBinding = (bindingName: string, expr: t) => {
  let mID = gid()
  EMatch(mID, b, list{(MPVariable(gid(), bindingName), expr)})
}

let matchWithTwoBindings = (bindingName1: string, expr1: t, bindingName2: string, expr2: t) => {
  let mID = gid()
  EMatch(
    mID,
    b,
    list{(MPVariable(gid(), bindingName1), expr1), (MPVariable(gid(), bindingName2), expr2)},
  )
}

let matchWithConstructorBinding = (bindingName: string, expr: t) => {
  let mID = gid()
  EMatch(mID, b, list{(MPConstructor(gid(), "Ok", list{MPVariable(gid(), bindingName)}), expr)})
}

let matchWithTwoLets = {
  let mID = gid()
  EMatch(
    mID,
    b,
    list{
      (
        MPBlank(gid()),
        ELet(gid(), "x", EInteger(gid(), 5L), ELet(gid(), "y", EInteger(gid(), 6L), EBlank(gid()))),
      ),
    },
  )
}

let nestedMatch = {
  let mID = gid()
  EMatch(mID, b, list{(MPBlank(gid()), emptyMatch)})
}

// ----------------
// Match _Patterns_
// ----------------
let fiftySixMP = MP.MPInteger(gid(), 56L)
let seventyMP = MP.MPInteger(gid(), 78L)

let tupleMP2WithNoBlank = mpTuple(fiftySixMP, seventyMP, list{})
let tupleMP2WithBothBlank = mpTuple(bPat(), bPat(), list{})
let tupleMP2WithFirstBlank = mpTuple(bPat(), seventyMP, list{})
let tupleMP2WithSecondBlank = mpTuple(fiftySixMP, bPat(), list{})

let tupleMP3WithNoBlanks = mpTuple(fiftySixMP, seventyMP, list{fiftySixMP})
let tupleMP3WithAllBlank = mpTuple(bPat(), bPat(), list{bPat()})

let tupleMP3WithFirstBlank = mpTuple(bPat(), seventyMP, list{fiftySixMP})
let tupleMP3WithSecondBlank = mpTuple(fiftySixMP, bPat(), list{seventyMP})
let tupleMP3WithThirdBlank = mpTuple(fiftySixMP, seventyMP, list{bPat()})

let tupleMP3WithFirstFilled = mpTuple(fiftySixMP, bPat(), list{bPat()})
let tupleMP3WithSecondFilled = mpTuple(bPat(), fiftySixMP, list{bPat()})
let tupleMP3WithThirdFilled = mpTuple(bPat(), bPat(), list{fiftySixMP})

let tupleMP6 = mpTuple(fiftySixMP, seventyMP, list{fiftySixMP, seventyMP, fiftySixMP, seventyMP})

let tupleMP3WithStrs = mpTuple(mpString("ab"), mpString("cd"), list{mpString("ef")})

let tupleMPHuge = mpTuple(
  fiftySixMP,
  seventyMP,
  list{
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
    seventyMP,
    fiftySixMP,
  },
)

// ----------------
// Variables
// ----------------

let aVar = EVariable(gid(), "variable")

let aShortVar = EVariable(gid(), "v")

// ----------------
// Ifs
// ----------------
let emptyIf = EIf(gid(), b, b, b)

let ifOnlyCond = EIf(gid(), five, b, b)
let ifOnlyThen = EIf(gid(), b, five, b)
let ifOnlyElse = EIf(gid(), b, b, five)

let plainIf = EIf(gid(), EInteger(gid(), 5L), EInteger(gid(), 6L), EInteger(gid(), 7L))

let nestedIf = EIf(
  gid(),
  EInteger(gid(), 5L),
  EIf(gid(), EInteger(gid(), 5L), EInteger(gid(), 6L), EInteger(gid(), 7L)),
  EInteger(gid(), 7L),
)

let indentedIfElse = ELet(
  gid(),
  "var",
  EIf(gid(), b, EInteger(gid(), 6L), EInteger(gid(), 7L)),
  EVariable(gid(), "var"),
)

// ----------------
// Lambdas
// ----------------
let aLambda = ELambda(gid(), list{(gid(), "")}, b)

let nonEmptyLambda = ELambda(gid(), list{(gid(), "")}, five)

let lambdaWithBinding = (bindingName: string, expr: t) => ELambda(
  gid(),
  list{(gid(), bindingName)},
  expr,
)

let lambdaWithTwoBindings = ELambda(gid(), list{(gid(), "x"), (gid(), "y")}, b)

let lambdaWithUsedBinding = (bindingName: string) =>
  lambdaWithBinding(bindingName, EVariable(gid(), bindingName))

let lambdaWithUsed2ndBinding = (bindingName: string) => ELambda(
  gid(),
  list{(gid(), "somevar"), (gid(), bindingName)},
  EVariable(gid(), bindingName),
)

let lambdaWith3UsedBindings = {
  let b1 = "aVar"
  let b2 = "bVar"
  let b3 = "cVar"
  ELambda(
    gid(),
    list{(gid(), b1), (gid(), b2), (gid(), b3)},
    binop("+", var(b1), binop("*", var(b3), var(b2))),
  )
}

// ----------------
// Functions
// ----------------
let aFnCall = fn(~mod="Int", "add", list{five, b})

let aFullFnCall = fn(~id=gid(), "Int::add", list{int(5), int(5)})

let aFnCallWithVersion = fn(~mod="DB", "getAll", ~version=1, list{b})

let aFnCallWithZeroArgs = fn(~mod="List", "empty", list{})

let aFnCallWithZeroArgsAndVersion = fn(~mod="List", "empty", ~version=1, list{})

let aFnCallWithBlockArg = fn(~mod="Dict", "map", list{b, b})

let anInfixFn = binop("==", b, b)

let aFullInfixFn = binop("++", var("myvar"), five)

let aOnRailFnCall = fn(~mod="HttpClient", "get", ~version=3, list{b, b, b}, ~ster=Rail)

let aRailableFnCall = fn(~mod="HttpClient", "get", ~version=3, list{b, b, b}, ~ster=NoRail)

// ----------------
// And/Or
// ----------------

let binOp = or'(b, b)

let aFullBinOp = and'(var("myvar"), trueBool)

// ----------------
// Constructors
// ----------------
let aConstructor = EConstructor(gid(), "Just", list{b})

// ----------------
// Records
// ----------------
let emptyRow = list{("", b)}

let recordRow1 = ("f1", fiftySix)

let recordRow2 = ("f2", seventyEight)

let singleRowRecord = ERecord(gid(), list{recordRow1})

let multiRowRecord = ERecord(gid(), list{recordRow1, recordRow2})

let emptyRowRecord = ERecord(gid(), emptyRow)

let emptyRecord = ERecord(gid(), list{})

let functionWrappedEmptyRecord = fn(
  ~mod="HttpClient",
  "get",
  ~version=4,
  list{emptyStr, emptyRecord, emptyRecord},
)

// ----------------
// Lists
// ----------------
let emptyList = list(list{})

let singleElementList = list(list{fiftySix})

let multiList = list(list{fiftySix, seventyEight})

let listWithStr = list(list{str("ab")})

let longList = list(list{fiftySix, seventyEight, fiftySix, seventyEight, fiftySix, seventyEight})

let veryLongList = list(list{
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
  fiftySix,
  seventyEight,
})

let listWithBlank = list(list{fiftySix, seventyEight, b, fiftySix})

let listWithBlankAtStart = list(list{b, fiftySix, seventyEight, fiftySix})

let listWithJustABlank = list(list{b})

let listWithRecord = list(list{emptyRecord})

let listWithMultiStrs = list(list{str("ab"), str("cd"), str("ef")})

// ----------------
// Tuples
// ----------------

let tuple2WithNoBlank = tuple(fiftySix, seventyEight, list{})
let tuple2WithBothBlank = tuple(b, b, list{})
let tuple2WithFirstBlank = tuple(b, seventyEight, list{})
let tuple2WithSecondBlank = tuple(fiftySix, b, list{})

let tuple3WithNoBlanks = tuple(fiftySix, seventyEight, list{fiftySix})
let tuple3WithAllBlank = tuple(b, b, list{b})

let tuple3WithFirstBlank = tuple(b, seventyEight, list{fiftySix})
let tuple3WithSecondBlank = tuple(fiftySix, b, list{seventyEight})
let tuple3WithThirdBlank = tuple(fiftySix, seventyEight, list{b})

let tuple3WithFirstFilled = tuple(fiftySix, b, list{b})
let tuple3WithSecondFilled = tuple(b, fiftySix, list{b})
let tuple3WithThirdFilled = tuple(b, b, list{fiftySix})

let tuple6 = tuple(fiftySix, seventyEight, list{fiftySix, seventyEight, fiftySix, seventyEight})

let tupleWithRecord = tuple(emptyRecord, b, list{})

let tuple3WithStrs = tuple(str("ab"), str("cd"), list{str("ef")})

let tupleHuge = tuple(
  fiftySix,
  seventyEight,
  list{
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
    fiftySix,
    seventyEight,
  },
)

// ----------------
// Fields
// ----------------
let aField = EFieldAccess(gid(), EVariable(gid(), "obj"), "field")

let aNestedField = EFieldAccess(
  gid(),
  EFieldAccess(gid(), EVariable(gid(), "obj"), "field"),
  "field2",
)

let aShortField = EFieldAccess(gid(), EVariable(gid(), "obj"), "f")

let aBlankField = EFieldAccess(gid(), EVariable(gid(), "obj"), "")

let aPartialField = EPartial(gid(), "", EFieldAccess(gid(), EVariable(gid(), "obj"), ""))

// ----------------
// Pipes
// ----------------
let aList5 = list(list{five})

let aList6 = list(list{six})

let aListNum = n => list(list{int(n)})

let listFn = args => fn(~mod="List", "append", list{pipeTarget, ...args})

let aPipe = pipe(list(list{}), listFn(list{aList5}), list{listFn(list{aList5})})

let emptyPipe = pipe(b, b, list{})

let aLongPipe = pipe(
  list(list{}),
  listFn(list{aListNum(2)}),
  list{listFn(list{aListNum(3)}), listFn(list{aListNum(4)}), listFn(list{aListNum(5)})},
)

let aBinopPipe = pipe(b, binop("++", pipeTarget, str("asd")), list{})

let aBinopPlusPipe = pipe(b, binop("+", pipeTarget, int(10)), list{})

let aPipeInsideIf = if'(b, aLongPipe, b)

let aNestedPipe = pipe(
  list(list{}),
  listFn(list{pipe(aList5, listFn(list{aList6}), list{})}),
  list{},
)

let aPipeWithFilledFunction = pipe(
  str("hello"),
  fn(~mod="String", "length", ~version=1, list{pipeTarget}),
  list{},
)

// -------------
// Feature Flags
// -------------

let flagOld = oldCode => EFeatureFlag(gid(), "flag-name", falseBool, oldCode, b)

let flagNew = newCode => EFeatureFlag(gid(), "flag-name", trueBool, b, newCode)

let letWithflagBody = let'("a", aShortInt, flagOld(oneCharStr))

// ----------------
// Complex
// ----------------

let compoundExpr = if'(
  binop(
    "||",
    binop(
      "==",
      fieldAccess(fieldAccess(var("request"), "headers"), "origin"),
      str("https://usealtitude.com"),
    ),
    binop(
      "==",
      fieldAccess(fieldAccess(var("request"), "headers"), "origin"),
      str("https://localhost:3000"),
    ),
  ),
  let'("", b, fn(~mod="Http", "Forbidden", list{int(403)})),
  fn(~mod="Http", "Forbidden", list{}),
)

/// When updating this, also update SerializationTests.Tests.Values.testExpr in the
/// backend
let complexExpr = {
  let e = int(-5)
  let'(
    "x1",
    int(5),
    let'(
      "x2",
      int64(9223372036854775807L),
      let'(
        "bool",
        bool(true),
        let'(
          "bool",
          bool(false),
          let'(
            "str",
            str("a string"),
            let'(
              "char",
              str("a"),
              let'(
                "float",
                float'(Negative, 6, 5),
                let'(
                  "n",
                  null,
                  let'(
                    "b",
                    blank(),
                    let'(
                      "i",
                      if'(
                        fn(~mod="Bool", "isError", list{int(6)}, ~ster=Rail),
                        if'(
                          binop("!=", int(5), int(6)),
                          binop("+", int(5), int(2)),
                          lambda(list{"y"}, binop("+", int(2), var("y"))),
                        ),
                        binop(
                          "+",
                          binop(
                            "+",
                            fieldAccess(var("x"), "y"),
                            fn(~mod="Int", "add", list{int(6), int(2)}),
                          ),
                          list(list{int(5), int(6), int(7)}),
                        ),
                      ),
                      let'(
                        "r",
                        record(list{
                          ("field", pipe(int(5), binop("+", pipeTarget, int(2)), list{})),
                          (
                            "constructor",
                            constructor(
                              "Ok",
                              list{
                                constructor(
                                  "Error",
                                  list{constructor("Just", list{constructor("Nothing", list{})})},
                                ),
                              },
                            ),
                          ),
                        }),
                        let'(
                          "m",
                          match'(
                            fn(~mod="Mod", "function", ~version=2, list{}),
                            list{
                              (mpConstructor("Ok", list{mpVar("x")}), var("v")),
                              (mpInt(5), int64(-9223372036854775808L)),
                              (mpBool(true), int(7)),
                              // (pChar("c"), char("c")),
                              (mpString("string"), str("string")),
                              (mpNull(), null),
                              (mpVar("var"), binop("+", int(6), var("var"))),
                              (mpFloat(Positive, 5, 6), float'(Positive, 5, 6)),
                              (mpBlank(), int(6)),
                            },
                          ),
                          let'(
                            "f",
                            flag(~name="test", bool(true), int(5), int(6)),
                            let'(
                              "partials",
                              list(list{
                                partial("some 🤬 string", e),
                                rightPartial("some 😭 string", e),
                                leftPartial("some 👨‍👩‍👧‍👦 string", e),
                              }),
                              let'("tuples", tuple(e, e, list{e}), e),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    ),
  )
}

// ----------------
// Some useful defaults
// ----------------
let defaultTLID = TLID.fromInt(7)

let defaultTestFunctions: list<RT.BuiltInFn.t> = {
  let fnParam = (name: string, ~args=list{}, typ: DType.t): RT.BuiltInFn.Param.t => {
    name: name,
    typ: typ,
    args: args,
    description: "",
  }

  let infixFn = (op, typ, rtType): RT.BuiltInFn.t => {
    name: {module_: "", function: op, version: 0},
    parameters: list{fnParam("a", typ), fnParam("b", typ)},
    returnType: rtType,
    description: "Some binop",
    previewable: Pure,
    deprecated: NotDeprecated,
    isInfix: true,
    sqlSpec: NotQueryable,
  }

  list{
    infixFn("<", TInt, TBool),
    infixFn("+", TInt, TInt),
    infixFn("++", TStr, TStr),
    infixFn("==", TVariable("a"), TBool),
    infixFn("<=", TInt, TBool),
    infixFn(">=", TInt, TBool),
    // these are deprecated but adding them here keeps things working more like
    // production, which has these functions. This is important because of &&/||
    // operators
    infixFn("||", TBool, TBool),
    infixFn("&&", TBool, TBool),
    {
      name: {module_: "Int", function: "add", version: 0},
      parameters: list{fnParam("a", TInt), fnParam("b", TInt)},
      returnType: TInt,
      description: "Add two ints",
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "Int", function: "sqrt", version: 0},
      parameters: list{fnParam("a", TInt)},
      returnType: TInt,
      description: "Get the square root of an Int",
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "Bool", function: "and", version: 0},
      parameters: list{fnParam("a", TBool), fnParam("b", TBool)},
      returnType: TBool,
      description: "Return true if both are true",
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "HttpClient", function: "post", version: 4},
      parameters: list{
        fnParam("url", TStr),
        fnParam("body", DType.any),
        fnParam("query", TDict(DType.any)),
        fnParam("headers", TDict(DType.any)),
      },
      returnType: TResult(DType.any, DType.any),
      description: "Make blocking HTTP POST call to `uri`.",
      previewable: Impure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "HttpClient", function: "get", version: 3},
      parameters: list{
        fnParam("url", TStr),
        fnParam("query", TDict(DType.any)),
        fnParam("headers", TDict(DType.any)),
      },
      returnType: TResult(DType.any, DType.any),
      description: "Make blocking HTTP GET call to `uri`.",
      previewable: Impure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "DB", function: "getAll", version: 1},
      parameters: list{fnParam("table", TDB(DType.any))},
      returnType: TList(DType.any),
      description: "get all",
      previewable: Impure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "Dict", function: "map", version: 0},
      parameters: list{
        fnParam("dict", TDict(DType.any)),
        fnParam("f", TFn(list{TStr, DType.any}, DType.any), ~args=list{"key", "value"}),
      },
      returnType: TDict(DType.any),
      description: "Iterates each `key` and `value` in Dictionary `dict` and mutates it according to the provided lambda",
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "List", function: "append", version: 0},
      parameters: list{fnParam("l1", TList(DType.any)), fnParam("l2", TList(DType.any))},
      returnType: TList(DType.any),
      description: "append list",
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
    {
      name: {module_: "List", function: "empty", version: 0},
      parameters: list{},
      returnType: TList(DType.any),
      description: "empty list",
      previewable: Pure,
      deprecated: NotDeprecated,
      isInfix: false,
      sqlSpec: NotQueryable,
    },
  }
}

let defaultTestState = {...FluidTypes.State.default, activeEditor: MainEditor(defaultTLID)}

let defaultFunctionsProps: Functions.props = {
  usedFns: Map.String.empty,
  userFunctions: TLID.Dict.empty,
  allowTuples: false,
}

let defaultTestProps: FluidTypes.Props.t = {
  functions: Functions.empty |> Functions.setBuiltins(defaultTestFunctions, defaultFunctionsProps),
  settings: {
    allowTuples: SettingsContributing.InProgressFeatures.default.allowTuples,
  },
}

let fakeID1 = ID.fromInt(77777771)
let fakeID2 = ID.fromInt(77777772)
let fakeID3 = ID.fromInt(77777773)

let defaultTestModel: AppTypes.Model.t = {
  ...AppTypes.Model.default,
  functions: defaultTestProps.functions,
  analyses: Map.String.fromList(list{
    (
      "94167980-f909-527e-a4af-bc3155f586d3", // The default traceID for TLID 7
      (
        0,
        Loadable.Success(
          ID.Map.fromArray([
            (
              fakeID1,
              AnalysisTypes.ExecutionResult.ExecutedResult(
                RT.Dval.obj(list{("body", DNull), ("formBody", DNull)}),
              ),
            ),
            (
              fakeID2,
              AnalysisTypes.ExecutionResult.ExecutedResult(
                RT.Dval.obj(list{("title", DNull), ("author", DNull)}),
              ),
            ),
            (
              fakeID3,
              AnalysisTypes.ExecutionResult.ExecutedResult(RT.Dval.obj(list{("body", DInt(5L))})),
            ),
          ]),
        ),
      ),
    ),
  }),
  fluidState: defaultTestState,
  settings: {
    ...AppTypes.Model.default.settings,
    contributingSettings: {
      ...AppTypes.Model.default.settings.contributingSettings,
      inProgressFeatures: {
        ...AppTypes.Model.default.settings.contributingSettings.inProgressFeatures,
        allowShortCircuitingBinops: true,
      },
    },
  },
}
