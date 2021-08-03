open Prelude
open FluidExpression

/* ---------------- */
/* Shortcuts */
/* ---------------- */
let b = newB()

open FluidShortcuts

/* ---------------- */
/* test data */
/* ---------------- */

/* ---------------- */
/* String */
/* ---------------- */
let aStr = EString(gid(), "some string")

let aStrEscape = EPartial(gid(), "so\\me string", EString(gid(), "some string"))

let emptyStr = EString(gid(), "")

let oneCharStr = EString(gid(), "c")

let numSegment = "123456789_"

let letterSegment = "abcdefghi,"

let mlSegment = numSegment ++ (letterSegment ++ (numSegment ++ letterSegment))

let mlStr = str(mlSegment ++ (mlSegment ++ numSegment))

let mlStrWSpace = str(
  mlSegment ++
  (" " ++
  (numSegment ++ (" " ++ (letterSegment ++ (" " ++ (numSegment ++ (" " ++ letterSegment))))))),
)

/* ---------------- */
/* Ints */
/* ---------------- */
let aShortInt = EInteger(gid(), "1")

let anInt = EInteger(gid(), "12345")

let aHugeInt = EInteger(gid(), "2000000000000000000")

let max62BitInt = intStr("4611686018427387903")

let oneShorterThanMax62BitInt = intStr("461168601842738790")

let five = EInteger(gid(), "5")

let six = EInteger(gid(), "6")

let fiftySix = EInteger(gid(), "56")

let seventyEight = EInteger(gid(), "78")

/* ---------------- */
/* Floats */
/* ---------------- */
let aFloat = EFloat(gid(), "123", "456")

let aFloatWithoutWhole = EFloat(gid(), "", "1")

let aHugeFloat = EFloat(gid(), "123456789", "123456789")

let aPartialFloat = EFloat(gid(), "1", "")

let maxPosIntWithDot = floatStr("4611686018427387", "903")

let maxPosIntPlus1WithDot = floatStr("4611686018427387", "904")

/* ---------------- */
/* Bools */
/* ---------------- */
let trueBool = EBool(gid(), true)

let falseBool = EBool(gid(), false)

/* ---------------- */
/* Null */
/* ---------------- */
let aNull = ENull(gid())

/* ---------------- */
/* Partials */
/* ---------------- */
let aPartialVar = EPartial(gid(), "req", b)

/* ---------------- */
/* Lets */
/* ---------------- */
let completelyEmptyLet = ELet(gid(), "", b, b)

/* let *** = ___\n5 */
let emptyLet = ELet(gid(), "", b, EInteger(gid(), "5"))

/* let *** = 6\n___ */
let nonEmptyLetWithBlankEnd = ELet(gid(), "", EInteger(gid(), "6"), b)

let nonEmptyLet = ELet(gid(), "", EInteger(gid(), "6"), EInteger(gid(), "5"))

let twoLets = ELet(
  gid(),
  "x",
  EInteger(gid(), "5"),
  ELet(gid(), "y", EInteger(gid(), "6"), EInteger(gid(), "7")),
)

let longLets = ELet(
  gid(),
  "firstLetName",
  EString(gid(), "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
  ELet(gid(), "secondLetName", EString(gid(), "0123456789"), EString(gid(), "RESULT")),
)

let letWithLhs = ELet(gid(), "n", EInteger(gid(), "6"), EInteger(gid(), "5"))

let letWithBinding = (bindingName: string, expr: t) => ELet(
  gid(),
  bindingName,
  EInteger(gid(), "6"),
  expr,
)

let letWithUsedBinding = (bindingName: string) =>
  letWithBinding(bindingName, EVariable(gid(), bindingName))

/* ---------------- */
/* Match */
/* ---------------- */
let emptyMatch = {
  let mID = gid()
  EMatch(mID, b, list{(FPBlank(mID, gid()), b)})
}

let emptyMatchWithTwoPatterns = {
  let mID = gid()
  EMatch(mID, b, list{(FPBlank(mID, gid()), b), (FPBlank(mID, gid()), b)})
}

let matchWithPatterns = {
  let mID = gid()
  EMatch(mID, b, list{(FPInteger(mID, gid(), "3"), b)})
}

let matchWithConstructorPattern = {
  let mID = gid()
  EMatch(mID, b, list{(FPConstructor(mID, gid(), "Just", list{}), b)})
}

let matchWithBinding = (bindingName: string, expr: t) => {
  let mID = gid()
  EMatch(mID, b, list{(FPVariable(mID, gid(), bindingName), expr)})
}

let matchWithTwoBindings = (bindingName1: string, expr1: t, bindingName2: string, expr2: t) => {
  let mID = gid()
  EMatch(
    mID,
    b,
    list{
      (FPVariable(mID, gid(), bindingName1), expr1),
      (FPVariable(mID, gid(), bindingName2), expr2),
    },
  )
}

let matchWithConstructorBinding = (bindingName: string, expr: t) => {
  let mID = gid()
  EMatch(
    mID,
    b,
    list{(FPConstructor(mID, gid(), "Ok", list{FPVariable(mID, gid(), bindingName)}), expr)},
  )
}

let matchWithTwoLets = {
  let mID = gid()
  EMatch(
    mID,
    b,
    list{
      (
        FPBlank(mID, gid()),
        ELet(
          gid(),
          "x",
          EInteger(gid(), "5"),
          ELet(gid(), "y", EInteger(gid(), "6"), EBlank(gid())),
        ),
      ),
    },
  )
}

let nestedMatch = {
  let mID = gid()
  EMatch(mID, b, list{(FPBlank(mID, gid()), emptyMatch)})
}

/* ---------------- */
/* Variables */
/* ---------------- */

let aVar = EVariable(gid(), "variable")

let aShortVar = EVariable(gid(), "v")

/* ---------------- */
/* Ifs */
/* ---------------- */
let emptyIf = EIf(gid(), b, b, b)

let plainIf = EIf(gid(), EInteger(gid(), "5"), EInteger(gid(), "6"), EInteger(gid(), "7"))

let nestedIf = EIf(
  gid(),
  EInteger(gid(), "5"),
  EIf(gid(), EInteger(gid(), "5"), EInteger(gid(), "6"), EInteger(gid(), "7")),
  EInteger(gid(), "7"),
)

let indentedIfElse = ELet(
  gid(),
  "var",
  EIf(gid(), b, EInteger(gid(), "6"), EInteger(gid(), "7")),
  EVariable(gid(), "var"),
)

/* ---------------- */
/* Lambdas */
/* ---------------- */
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

/* ---------------- */
/* Functions */
/* ---------------- */
let aFnCall = EFnCall(gid(), "Int::add", list{five, b}, NoRail)

let aFullFnCall = fn(~id=gid(), "Int::add", list{int(5), int(5)})

let aFnCallWithVersion = EFnCall(gid(), "DB::getAll_v1", list{b}, NoRail)

let aFnCallWithZeroArgs = EFnCall(gid(), "List::empty", list{}, NoRail)

let aFnCallWithZeroArgsAndVersion = EFnCall(gid(), "List::empty_v1", list{}, NoRail)

let aFnCallWithBlockArg = EFnCall(gid(), "Dict::map", list{b, b}, NoRail)

let aBinOp = EBinOp(gid(), "==", b, b, NoRail)

let aFullBinOp = binop("||", var("myvar"), five)

let aOnRailFnCall = EFnCall(gid(), "HttpClient::get_v3", list{b, b, b}, Rail)

let aRailableFnCall = EFnCall(gid(), "HttpClient::get_v3", list{b, b, b}, NoRail)

/* ---------------- */
/* Constructors */
/* ---------------- */
let aConstructor = EConstructor(gid(), "Just", list{b})

/* ---------------- */
/* Records */
/* ---------------- */
let emptyRow = list{("", b)}

let recordRow1 = ("f1", fiftySix)

let recordRow2 = ("f2", seventyEight)

let singleRowRecord = ERecord(gid(), list{recordRow1})

let multiRowRecord = ERecord(gid(), list{recordRow1, recordRow2})

let emptyRowRecord = ERecord(gid(), emptyRow)

let emptyRecord = ERecord(gid(), list{})

let functionWrappedEmptyRecord = fn("HttpClient::get_v4", list{emptyStr, emptyRecord, emptyRecord})

/* ---------------- */
/* Lists */
/* ---------------- */
let emptyList = list(list{})

let single = list(list{fiftySix})

let multi = list(list{fiftySix, seventyEight})

let withStr = list(list{str("ab")})

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

let multiWithStrs = list(list{str("ab"), str("cd"), str("ef")})

/* ---------------- */
/* Fields */
/* ---------------- */
let aField = EFieldAccess(gid(), EVariable(gid(), "obj"), "field")

let aNestedField = EFieldAccess(
  gid(),
  EFieldAccess(gid(), EVariable(gid(), "obj"), "field"),
  "field2",
)

let aShortField = EFieldAccess(gid(), EVariable(gid(), "obj"), "f")

let aBlankField = EFieldAccess(gid(), EVariable(gid(), "obj"), "")

let aPartialField = EPartial(gid(), "", EFieldAccess(gid(), EVariable(gid(), "obj"), ""))

/* ---------------- */
/* Pipes */
/* ---------------- */
let aList5 = list(list{five})

let aList6 = list(list{six})

let aListNum = n => list(list{int(n)})

let listFn = args => fn("List::append", list{pipeTarget, ...args})

let aPipe = pipe(list(list{}), list{listFn(list{aList5}), listFn(list{aList5})})

let emptyPipe = pipe(b, list{b})

let aLongPipe = pipe(
  list(list{}),
  list{
    listFn(list{aListNum(2)}),
    listFn(list{aListNum(3)}),
    listFn(list{aListNum(4)}),
    listFn(list{aListNum(5)}),
  },
)

let aBinopPipe = pipe(b, list{binop("++", pipeTarget, str("asd"))})

let aBinopPlusPipe = pipe(b, list{binop("+", pipeTarget, int(10))})

let aPipeInsideIf = if'(b, aLongPipe, b)

let aNestedPipe = pipe(list(list{}), list{listFn(list{pipe(aList5, list{listFn(list{aList6})})})})

let aPipeWithFilledFunction = pipe(str("hello"), list{fn("String::length_v1", list{pipeTarget})})

/* ------------- */
/* Feature Flags */
/* ------------- */

let flagOld = oldCode => EFeatureFlag(gid(), "flag-name", falseBool, oldCode, b)

let flagNew = newCode => EFeatureFlag(gid(), "flag-name", trueBool, b, newCode)

let letWithflagBody = let'("a", aShortInt, flagOld(oneCharStr))

/* ---------------- */
/* Complex */
/* ---------------- */

let complexExpr = if'(
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
  let'("", b, fn("Http::Forbidden", list{int(403)})),
  fn("Http::Forbidden", list{}),
)

/* ---------------- */
/* Some useful defaults */
/* ---------------- */
let defaultTLID = TLID.fromString("7")

let defaultTestFunctions = {
  let fnParam = (name: string, t: tipe, ~blockArgs=list{}, opt: bool): Types.parameter => {
    paramName: name,
    paramTipe: t,
    paramBlock_args: blockArgs,
    paramOptional: opt,
    paramDescription: "",
  }

  let infixFn = (op, tipe, rtTipe) => {
    fnName: op,
    fnParameters: list{fnParam("a", tipe, false), fnParam("b", tipe, false)},
    fnReturnTipe: rtTipe,
    fnDescription: "Some infix function",
    fnPreviewSafety: Safe,
    fnDeprecated: false,
    fnInfix: true,
    fnIsSupportedInQuery: false,
    fnOrigin: Builtin,
  }

  list{
    infixFn("<", TInt, TBool),
    infixFn("+", TInt, TInt),
    infixFn("++", TStr, TStr),
    infixFn("==", TAny, TBool),
    infixFn("<=", TInt, TBool),
    infixFn("||", TBool, TBool),
    {
      fnName: "Int::add",
      fnParameters: list{fnParam("a", TInt, false), fnParam("b", TInt, false)},
      fnReturnTipe: TInt,
      fnDescription: "Add two ints",
      fnPreviewSafety: Safe,
      fnDeprecated: false,
      fnInfix: false,
      fnIsSupportedInQuery: false,
      fnOrigin: Builtin,
    },
    {
      fnName: "Int::sqrt",
      fnParameters: list{fnParam("a", TInt, false)},
      fnReturnTipe: TInt,
      fnDescription: "Get the square root of an Int",
      fnPreviewSafety: Safe,
      fnDeprecated: false,
      fnInfix: false,
      fnIsSupportedInQuery: false,
      fnOrigin: Builtin,
    },
    {
      fnName: "HttpClient::post_v4",
      fnParameters: list{
        fnParam("url", TStr, false),
        fnParam("body", TAny, false),
        fnParam("query", TObj, false),
        fnParam("headers", TObj, false),
      },
      fnReturnTipe: TResult,
      fnDescription: "Make blocking HTTP POST call to `uri`.",
      fnPreviewSafety: Unsafe,
      fnDeprecated: false,
      fnInfix: false,
      fnIsSupportedInQuery: false,
      fnOrigin: Builtin,
    },
    {
      fnName: "HttpClient::get_v3",
      fnParameters: list{
        fnParam("url", TStr, false),
        fnParam("query", TObj, false),
        fnParam("headers", TObj, false),
      },
      fnReturnTipe: TResult,
      fnDescription: "Make blocking HTTP GET call to `uri`.",
      fnPreviewSafety: Unsafe,
      fnDeprecated: false,
      fnInfix: false,
      fnIsSupportedInQuery: false,
      fnOrigin: Builtin,
    },
    {
      fnName: "DB::getAll_v1",
      fnParameters: list{fnParam("table", TDB, false)},
      fnReturnTipe: TList,
      fnDescription: "get all",
      fnPreviewSafety: Unsafe,
      fnDeprecated: false,
      fnInfix: false,
      fnIsSupportedInQuery: false,
      fnOrigin: Builtin,
    },
    {
      fnName: "Dict::map",
      fnParameters: list{
        fnParam("dict", TObj, false),
        fnParam("f", TBlock, false, ~blockArgs=list{"key", "value"}),
      },
      fnReturnTipe: TObj,
      fnDescription: "Iterates each `key` and `value` in Dictionary `dict` and mutates it according to the provided lambda",
      fnPreviewSafety: Safe,
      fnDeprecated: false,
      fnInfix: false,
      fnIsSupportedInQuery: false,
      fnOrigin: Builtin,
    },
    {
      fnName: "List::append",
      fnParameters: list{fnParam("l1", TList, false), fnParam("l2", TList, false)},
      fnReturnTipe: TList,
      fnDescription: "append list",
      fnPreviewSafety: Safe,
      fnDeprecated: false,
      fnInfix: false,
      fnIsSupportedInQuery: false,
      fnOrigin: Builtin,
    },
    {
      fnName: "List::empty",
      fnParameters: list{},
      fnReturnTipe: TList,
      fnDescription: "empty list",
      fnPreviewSafety: Safe,
      fnDeprecated: false,
      fnInfix: false,
      fnIsSupportedInQuery: false,
      fnOrigin: Builtin,
    },
  }
}

let defaultTestState = {...Defaults.defaultFluidState, activeEditor: MainEditor(defaultTLID)}

let defaultFunctionsProps = {usedFns: Map.String.empty, userFunctions: TLIDDict.empty}

let defaultTestProps: Types.fluidProps = {
  functions: Functions.empty |> Functions.setBuiltins(defaultTestFunctions, defaultFunctionsProps),
  variants: list{LeftPartialVariant},
}

let defaultTestModel = {
  ...Defaults.defaultModel,
  tests: defaultTestProps.variants,
  functions: defaultTestProps.functions,
  analyses: Map.String.fromList /* The default traceID for TLID 7 */(list{
    (
      "94167980-f909-527e-a4af-bc3155f586d3",
      LoadableSuccess(
        Belt.Map.String.fromArray([
          ("fake-acdata1", ExecutedResult(Dval.obj(list{("body", DNull), ("formBody", DNull)}))),
          ("fake-acdata2", ExecutedResult(Dval.obj(list{("title", DNull), ("author", DNull)}))),
          ("fake-acdata3", ExecutedResult(Dval.obj(list{("body", DInt(5))}))),
        ]),
      ),
    ),
  }),
  fluidState: defaultTestState,
}
