open Prelude
open FluidExpression

(* ---------------- *)
(* Shortcuts *)
(* ---------------- *)
let b = newB ()

open FluidShortcuts

(* ---------------- *)
(* test data *)
(* ---------------- *)

(* ---------------- *)
(* String *)
(* ---------------- *)
let aStr = EString (gid (), "some string")

let aStrEscape =
  EPartial (gid (), "so\\me string", EString (gid (), "some string"))


let emptyStr = EString (gid (), "")

let oneCharStr = EString (gid (), "c")

let numSegment = "123456789_"

let letterSegment = "abcdefghi,"

let mlSegment = numSegment ^ letterSegment ^ numSegment ^ letterSegment

let mlStr = str (mlSegment ^ mlSegment ^ numSegment)

let mlStrWSpace =
  str
    ( mlSegment
    ^ " "
    ^ numSegment
    ^ " "
    ^ letterSegment
    ^ " "
    ^ numSegment
    ^ " "
    ^ letterSegment )


(* ---------------- *)
(* Ints *)
(* ---------------- *)
let aShortInt = EInteger (gid (), "1")

let anInt = EInteger (gid (), "12345")

let aHugeInt = EInteger (gid (), "2000000000000000000")

let max62BitInt = intStr "4611686018427387903"

let oneShorterThanMax62BitInt = intStr "461168601842738790"

let five = EInteger (gid (), "5")

let six = EInteger (gid (), "6")

let fiftySix = EInteger (gid (), "56")

let seventyEight = EInteger (gid (), "78")

(* ---------------- *)
(* Floats *)
(* ---------------- *)
let aFloat = EFloat (gid (), "123", "456")

let aFloatWithoutWhole = EFloat (gid (), "", "1")

let aHugeFloat = EFloat (gid (), "123456789", "123456789")

let aPartialFloat = EFloat (gid (), "1", "")

let maxPosIntWithDot = floatStr "4611686018427387" "903"

let maxPosIntPlus1WithDot = floatStr "4611686018427387" "904"

(* ---------------- *)
(* Bools *)
(* ---------------- *)
let trueBool = EBool (gid (), true)

let falseBool = EBool (gid (), false)

(* ---------------- *)
(* Null *)
(* ---------------- *)
let aNull = ENull (gid ())

(* ---------------- *)
(* Partials *)
(* ---------------- *)
let aPartialVar = EPartial (gid (), "req", b)

(* ---------------- *)
(* Lets *)
(* ---------------- *)
let completelyEmptyLet = ELet (gid (), "", b, b)

(* let *** = ___\n5 *)
let emptyLet = ELet (gid (), "", b, EInteger (gid (), "5"))

(* let *** = 6\n___ *)
let nonEmptyLetWithBlankEnd = ELet (gid (), "", EInteger (gid (), "6"), b)

let nonEmptyLet =
  ELet (gid (), "", EInteger (gid (), "6"), EInteger (gid (), "5"))


let twoLets =
  ELet
    ( gid ()
    , "x"
    , EInteger (gid (), "5")
    , ELet (gid (), "y", EInteger (gid (), "6"), EInteger (gid (), "7")) )


let longLets =
  ELet
    ( gid ()
    , "firstLetName"
    , EString (gid (), "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    , ELet
        ( gid ()
        , "secondLetName"
        , EString (gid (), "0123456789")
        , EString (gid (), "RESULT") ) )


let letWithLhs =
  ELet (gid (), "n", EInteger (gid (), "6"), EInteger (gid (), "5"))


let letWithBinding (bindingName : string) (expr : t) =
  ELet (gid (), bindingName, EInteger (gid (), "6"), expr)


let letWithUsedBinding (bindingName : string) =
  letWithBinding bindingName (EVariable (gid (), bindingName))


(* ---------------- *)
(* Match *)
(* ---------------- *)
let emptyMatch =
  let mID = gid () in
  EMatch (mID, b, [(FPBlank (mID, gid ()), b)])


let emptyMatchWithTwoPatterns =
  let mID = gid () in
  EMatch (mID, b, [(FPBlank (mID, gid ()), b); (FPBlank (mID, gid ()), b)])


let matchWithPatterns =
  let mID = gid () in
  EMatch (mID, b, [(FPInteger (mID, gid (), "3"), b)])


let matchWithConstructorPattern =
  let mID = gid () in
  EMatch (mID, b, [(FPConstructor (mID, gid (), "Just", []), b)])


let matchWithBinding (bindingName : string) (expr : t) =
  let mID = gid () in
  EMatch (mID, b, [(FPVariable (mID, gid (), bindingName), expr)])


let matchWithTwoBindings
    (bindingName1 : string) (expr1 : t) (bindingName2 : string) (expr2 : t) =
  let mID = gid () in
  EMatch
    ( mID
    , b
    , [ (FPVariable (mID, gid (), bindingName1), expr1)
      ; (FPVariable (mID, gid (), bindingName2), expr2) ] )


let matchWithConstructorBinding (bindingName : string) (expr : t) =
  let mID = gid () in
  EMatch
    ( mID
    , b
    , [ ( FPConstructor
            (mID, gid (), "Ok", [FPVariable (mID, gid (), bindingName)])
        , expr ) ] )


let matchWithTwoLets =
  let mID = gid () in
  EMatch
    ( mID
    , b
    , [ ( FPBlank (mID, gid ())
        , ELet
            ( gid ()
            , "x"
            , EInteger (gid (), "5")
            , ELet (gid (), "y", EInteger (gid (), "6"), EBlank (gid ())) ) ) ]
    )


let nestedMatch =
  let mID = gid () in
  EMatch (mID, b, [(FPBlank (mID, gid ()), emptyMatch)])


(* ---------------- *)
(* Variables *)
(* ---------------- *)

let aVar = EVariable (gid (), "variable")

let aShortVar = EVariable (gid (), "v")

(* ---------------- *)
(* Ifs *)
(* ---------------- *)
let emptyIf = EIf (gid (), b, b, b)

let plainIf =
  EIf
    ( gid ()
    , EInteger (gid (), "5")
    , EInteger (gid (), "6")
    , EInteger (gid (), "7") )


let nestedIf =
  EIf
    ( gid ()
    , EInteger (gid (), "5")
    , EIf
        ( gid ()
        , EInteger (gid (), "5")
        , EInteger (gid (), "6")
        , EInteger (gid (), "7") )
    , EInteger (gid (), "7") )


let indentedIfElse =
  ELet
    ( gid ()
    , "var"
    , EIf (gid (), b, EInteger (gid (), "6"), EInteger (gid (), "7"))
    , EVariable (gid (), "var") )


(* ---------------- *)
(* Lambdas *)
(* ---------------- *)
let aLambda = ELambda (gid (), [(gid (), "")], b)

let nonEmptyLambda = ELambda (gid (), [(gid (), "")], five)

let lambdaWithBinding (bindingName : string) (expr : t) =
  ELambda (gid (), [(gid (), bindingName)], expr)


let lambdaWithTwoBindings = ELambda (gid (), [(gid (), "x"); (gid (), "y")], b)

let lambdaWithUsedBinding (bindingName : string) =
  lambdaWithBinding bindingName (EVariable (gid (), bindingName))


let lambdaWithUsed2ndBinding (bindingName : string) =
  ELambda
    ( gid ()
    , [(gid (), "somevar"); (gid (), bindingName)]
    , EVariable (gid (), bindingName) )


let lambdaWith3UsedBindings =
  let b1 = "aVar" in
  let b2 = "bVar" in
  let b3 = "cVar" in
  ELambda
    ( gid ()
    , [(gid (), b1); (gid (), b2); (gid (), b3)]
    , binop "+" (var b1) (binop "*" (var b3) (var b2)) )


(* ---------------- *)
(* Functions *)
(* ---------------- *)
let aFnCall = EFnCall (gid (), "Int::add", [five; b], NoRail)

let aFullFnCall = fn ~id:(gid ()) "Int::add" [int 5; int 5]

let aFnCallWithVersion = EFnCall (gid (), "DB::getAll_v1", [b], NoRail)

let aFnCallWithZeroArgs = EFnCall (gid (), "List::empty", [], NoRail)

let aFnCallWithZeroArgsAndVersion =
  EFnCall (gid (), "List::empty_v1", [], NoRail)


let aFnCallWithBlockArg = EFnCall (gid (), "Dict::map", [b; b], NoRail)

let aBinOp = EBinOp (gid (), "==", b, b, NoRail)

let aFullBinOp = binop "||" (var "myvar") five

let aOnRailFnCall = EFnCall (gid (), "HttpClient::get_v3", [b; b; b], Rail)

let aRailableFnCall = EFnCall (gid (), "HttpClient::get_v3", [b; b; b], NoRail)

(* ---------------- *)
(* Constructors *)
(* ---------------- *)
let aConstructor = EConstructor (gid (), "Just", [b])

(* ---------------- *)
(* Records *)
(* ---------------- *)
let emptyRow = [("", b)]

let recordRow1 = ("f1", fiftySix)

let recordRow2 = ("f2", seventyEight)

let singleRowRecord = ERecord (gid (), [recordRow1])

let multiRowRecord = ERecord (gid (), [recordRow1; recordRow2])

let emptyRowRecord = ERecord (gid (), emptyRow)

let emptyRecord = ERecord (gid (), [])

let functionWrappedEmptyRecord =
  fn "HttpClient::get_v4" [emptyStr; emptyRecord; emptyRecord]


(* ---------------- *)
(* Lists *)
(* ---------------- *)
let emptyList = list []

let single = list [fiftySix]

let multi = list [fiftySix; seventyEight]

let withStr = list [str "ab"]

let longList =
  list [fiftySix; seventyEight; fiftySix; seventyEight; fiftySix; seventyEight]


let veryLongList =
  list
    [ fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight
    ; fiftySix
    ; seventyEight ]


let listWithBlank = list [fiftySix; seventyEight; b; fiftySix]

let listWithBlankAtStart = list [b; fiftySix; seventyEight; fiftySix]

let listWithJustABlank = list [b]

let listWithRecord = list [emptyRecord]

let multiWithStrs = list [str "ab"; str "cd"; str "ef"]

(* ---------------- *)
(* Fields *)
(* ---------------- *)
let aField = EFieldAccess (gid (), EVariable (gid (), "obj"), "field")

let aNestedField =
  EFieldAccess
    (gid (), EFieldAccess (gid (), EVariable (gid (), "obj"), "field"), "field2")


let aShortField = EFieldAccess (gid (), EVariable (gid (), "obj"), "f")

let aBlankField = EFieldAccess (gid (), EVariable (gid (), "obj"), "")

let aPartialField =
  EPartial (gid (), "", EFieldAccess (gid (), EVariable (gid (), "obj"), ""))


(* ---------------- *)
(* Pipes *)
(* ---------------- *)
let aList5 = list [five]

let aList6 = list [six]

let aListNum n = list [int n]

let listFn args = fn "List::append" (pipeTarget :: args)

let aPipe = pipe (list []) [listFn [aList5]; listFn [aList5]]

let emptyPipe = pipe b [b]

let aLongPipe =
  pipe
    (list [])
    [ listFn [aListNum 2]
    ; listFn [aListNum 3]
    ; listFn [aListNum 4]
    ; listFn [aListNum 5] ]


let aBinopPipe = pipe b [binop "++" pipeTarget (str "asd")]

let aBinopPlusPipe = pipe b [binop "+" pipeTarget (int 10)]

let aPipeInsideIf = if' b aLongPipe b

let aNestedPipe = pipe (list []) [listFn [pipe aList5 [listFn [aList6]]]]

let aPipeWithFilledFunction =
  pipe (str "hello") [fn "String::length_v1" [pipeTarget]]


(* ------------- *)
(* Feature Flags *)
(* ------------- *)

let flagOld oldCode = EFeatureFlag (gid (), "flag-name", falseBool, oldCode, b)

let flagNew newCode = EFeatureFlag (gid (), "flag-name", trueBool, b, newCode)

let letWithflagBody = let' "a" aShortInt (flagOld oneCharStr)

(* ---------------- *)
(* Complex *)
(* ---------------- *)

let complexExpr =
  if'
    (binop
       "||"
       (binop
          "=="
          (fieldAccess (fieldAccess (var "request") "headers") "origin")
          (str "https://usealtitude.com"))
       (binop
          "=="
          (fieldAccess (fieldAccess (var "request") "headers") "origin")
          (str "https://localhost:3000")))
    (let' "" b (fn "Http::Forbidden" [int 403]))
    (fn "Http::Forbidden" [])


(* ---------------- *)
(* Some useful defaults *)
(* ---------------- *)
let defaultTLID = TLID.fromString "7"

let defaultTestFunctions =
  let fnParam (name : string) (t : tipe) ?(blockArgs = []) (opt : bool) :
      Types.parameter =
    { paramName = name
    ; paramTipe = t
    ; paramBlock_args = blockArgs
    ; paramOptional = opt
    ; paramDescription = "" }
  in
  let infixFn op tipe rtTipe =
    { fnName = op
    ; fnParameters = [fnParam "a" tipe false; fnParam "b" tipe false]
    ; fnReturnTipe = rtTipe
    ; fnDescription = "Some infix function"
    ; fnPreviewSafety = Safe
    ; fnDeprecated = false
    ; fnInfix = true
    ; fnIsSupportedInQuery = false
    ; fnOrigin = Builtin }
  in
  [ infixFn "<" TInt TBool
  ; infixFn "+" TInt TInt
  ; infixFn "++" TStr TStr
  ; infixFn "==" TAny TBool
  ; infixFn "<=" TInt TBool
  ; infixFn "||" TBool TBool
  ; { fnName = "Int::add"
    ; fnParameters = [fnParam "a" TInt false; fnParam "b" TInt false]
    ; fnReturnTipe = TInt
    ; fnDescription = "Add two ints"
    ; fnPreviewSafety = Safe
    ; fnDeprecated = false
    ; fnInfix = false
    ; fnIsSupportedInQuery = false
    ; fnOrigin = Builtin }
  ; { fnName = "Int::sqrt"
    ; fnParameters = [fnParam "a" TInt false]
    ; fnReturnTipe = TInt
    ; fnDescription = "Get the square root of an Int"
    ; fnPreviewSafety = Safe
    ; fnDeprecated = false
    ; fnInfix = false
    ; fnIsSupportedInQuery = false
    ; fnOrigin = Builtin }
  ; { fnName = "HttpClient::post_v4"
    ; fnParameters =
        [ fnParam "url" TStr false
        ; fnParam "body" TAny false
        ; fnParam "query" TObj false
        ; fnParam "headers" TObj false ]
    ; fnReturnTipe = TResult
    ; fnDescription = "Make blocking HTTP POST call to `uri`."
    ; fnPreviewSafety = Unsafe
    ; fnDeprecated = false
    ; fnInfix = false
    ; fnIsSupportedInQuery = false
    ; fnOrigin = Builtin }
  ; { fnName = "HttpClient::get_v3"
    ; fnParameters =
        [ fnParam "url" TStr false
        ; fnParam "query" TObj false
        ; fnParam "headers" TObj false ]
    ; fnReturnTipe = TResult
    ; fnDescription = "Make blocking HTTP GET call to `uri`."
    ; fnPreviewSafety = Unsafe
    ; fnDeprecated = false
    ; fnInfix = false
    ; fnIsSupportedInQuery = false
    ; fnOrigin = Builtin }
  ; { fnName = "DB::getAll_v1"
    ; fnParameters = [fnParam "table" TDB false]
    ; fnReturnTipe = TList
    ; fnDescription = "get all"
    ; fnPreviewSafety = Unsafe
    ; fnDeprecated = false
    ; fnInfix = false
    ; fnIsSupportedInQuery = false
    ; fnOrigin = Builtin }
  ; { fnName = "Dict::map"
    ; fnParameters =
        [ fnParam "dict" TObj false
        ; fnParam "f" TBlock false ~blockArgs:["key"; "value"] ]
    ; fnReturnTipe = TObj
    ; fnDescription =
        "Iterates each `key` and `value` in Dictionary `dict` and mutates it according to the provided lambda"
    ; fnPreviewSafety = Safe
    ; fnDeprecated = false
    ; fnInfix = false
    ; fnIsSupportedInQuery = false
    ; fnOrigin = Builtin }
  ; { fnName = "List::append"
    ; fnParameters = [fnParam "l1" TList false; fnParam "l2" TList false]
    ; fnReturnTipe = TList
    ; fnDescription = "append list"
    ; fnPreviewSafety = Safe
    ; fnDeprecated = false
    ; fnInfix = false
    ; fnIsSupportedInQuery = false
    ; fnOrigin = Builtin }
  ; { fnName = "List::empty"
    ; fnParameters = []
    ; fnReturnTipe = TList
    ; fnDescription = "empty list"
    ; fnPreviewSafety = Safe
    ; fnDeprecated = false
    ; fnInfix = false
    ; fnIsSupportedInQuery = false
    ; fnOrigin = Builtin } ]


let defaultTestState =
  {Defaults.defaultFluidState with activeEditor = MainEditor defaultTLID}


let defaultFunctionsProps =
  {usedFns = Map.String.empty; userFunctions = TLIDDict.empty}


let defaultTestProps : Types.fluidProps =
  { functions =
      Functions.empty
      |> Functions.setBuiltins defaultTestFunctions defaultFunctionsProps
  ; variants = [LeftPartialVariant] }


let defaultTestModel =
  { Defaults.defaultModel with
    tests = defaultTestProps.variants
  ; functions = defaultTestProps.functions
  ; analyses =
      Map.String.singleton (* The default traceID for TLID 7 *)
        ~key:"94167980-f909-527e-a4af-bc3155f586d3"
        ~value:
          (LoadableSuccess
             (Map.String.fromList
                [ ( "fake-acdata1"
                  , ExecutedResult
                      (DObj
                         (Map.String.fromList
                            [("body", DNull); ("formBody", DNull)])) )
                ; ( "fake-acdata2"
                  , ExecutedResult
                      (DObj
                         (Map.String.fromList
                            [("title", DNull); ("author", DNull)])) )
                ; ( "fake-acdata3"
                  , ExecutedResult
                      (DObj (Map.String.fromList [("body", DInt 5)])) ) ]))
  ; fluidState = defaultTestState }
