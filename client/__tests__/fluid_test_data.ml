open Tc
open Types
open Prelude

(* ---------------- *)
(* Shortcuts *)
(* ---------------- *)
let b = Fluid.newB ()

let str (str : string) : fluidExpr = EString (gid (), str)

let int (int : string) : fluidExpr = EInteger (gid (), int)

let bool (b : bool) : fluidExpr = EBool (gid (), b)

let float' (whole : string) (fraction : string) : fluidExpr =
  EFloat (gid (), whole, fraction)


let record (rows : (string * fluidExpr) list) : fluidExpr =
  ERecord (gid (), List.map rows ~f:(fun (k, v) -> (gid (), k, v)))


let list (elems : fluidExpr list) : fluidExpr = EList (gid (), elems)

let pipeTarget = EPipeTarget (gid ())

let fn ?(ster = NoRail) (name : string) (args : fluidExpr list) =
  EFnCall (gid (), name, args, ster)


let binop
    ?(ster = NoRail) (name : string) (arg0 : fluidExpr) (arg1 : fluidExpr) =
  EBinOp (gid (), name, arg0, arg1, ster)


let partial (str : string) (e : fluidExpr) : fluidExpr =
  EPartial (gid (), str, e)


let rightPartial (str : string) (e : fluidExpr) : fluidExpr =
  ERightPartial (gid (), str, e)


let var (name : string) : fluidExpr = EVariable (gid (), name)

let fieldAccess (expr : fluidExpr) (fieldName : string) : fluidExpr =
  EFieldAccess (gid (), expr, gid (), fieldName)


let if' (cond : fluidExpr) (then' : fluidExpr) (else' : fluidExpr) : fluidExpr
    =
  EIf (gid (), cond, then', else')


let let' (varName : string) (rhs : fluidExpr) (body : fluidExpr) : fluidExpr =
  ELet (gid (), gid (), varName, rhs, body)


let match' (cond : fluidExpr) (matches : (fluidPattern * fluidExpr) list) :
    fluidExpr =
  EMatch (gid (), cond, matches)


let pInt (int : string) : fluidPattern = FPInteger (gid (), gid (), int)

let pVar (name : string) : fluidPattern = FPVariable (gid (), gid (), name)

let lambda (varNames : string list) (body : fluidExpr) : fluidExpr =
  ELambda (gid (), List.map varNames ~f:(fun name -> (gid (), name)), body)


let pipe (first : fluidExpr) (rest : fluidExpr list) : fluidExpr =
  EPipe (gid (), first :: rest)


(* ---------------- *)
(* test data *)
(* ---------------- *)

(* ---------------- *)
(* String *)
(* ---------------- *)
let aStr = EString (gid (), "some string")

let emptyStr = EString (gid (), "")

let oneCharStr = EString (gid (), "c")

let numSegment = "123456789_"

let letterSegment = "abcdefghi,"

let mlSegment = numSegment ^ letterSegment ^ numSegment ^ letterSegment

let mlStr = str (mlSegment ^ mlSegment ^ numSegment)

(* ---------------- *)
(* Ints *)
(* ---------------- *)
let aShortInt = EInteger (gid (), "1")

let anInt = EInteger (gid (), "12345")

let aHugeInt = EInteger (gid (), "2000000000000000000")

let max62BitInt = int "4611686018427387903"

let oneShorterThanMax62BitInt = int "461168601842738790"

let five = EInteger (gid (), "5")

let six = EInteger (gid (), "6")

let fiftySix = EInteger (gid (), "56")

let seventyEight = EInteger (gid (), "78")

(* ---------------- *)
(* Floats *)
(* ---------------- *)
let aFloat = EFloat (gid (), "123", "456")

let aHugeFloat = EFloat (gid (), "123456789", "123456789")

let aPartialFloat = EFloat (gid (), "1", "")

let maxPosIntWithDot = float' "4611686018427387" "903"

let maxPosIntPlus1WithDot = float' "4611686018427387" "904"

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
let completelyEmptyLet = ELet (gid (), gid (), "", b, b)

let emptyLet = ELet (gid (), gid (), "", b, EInteger (gid (), "5"))

let nonEmptyLetWithBlankEnd =
  ELet (gid (), gid (), "", EInteger (gid (), "6"), b)


let nonEmptyLet =
  ELet (gid (), gid (), "", EInteger (gid (), "6"), EInteger (gid (), "5"))


let twoLets =
  ELet
    ( gid ()
    , gid ()
    , "x"
    , EInteger (gid (), "5")
    , ELet (gid (), gid (), "y", EInteger (gid (), "6"), EInteger (gid (), "7"))
    )


let longLets =
  ELet
    ( gid ()
    , gid ()
    , "firstLetName"
    , EString (gid (), "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    , ELet
        ( gid ()
        , gid ()
        , "secondLetName"
        , EString (gid (), "0123456789")
        , EString (gid (), "RESULT") ) )


let letWithLhs =
  ELet (gid (), gid (), "n", EInteger (gid (), "6"), EInteger (gid (), "5"))


let letWithBinding (bindingName : string) (expr : fluidExpr) =
  ELet (gid (), gid (), bindingName, EInteger (gid (), "6"), expr)


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


let matchWithBinding (bindingName : string) (expr : fluidExpr) =
  let mID = gid () in
  EMatch (mID, b, [(FPVariable (mID, gid (), bindingName), expr)])


let matchWithConstructorBinding (bindingName : string) (expr : fluidExpr) =
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
            , gid ()
            , "x"
            , EInteger (gid (), "5")
            , ELet
                (gid (), gid (), "y", EInteger (gid (), "6"), EBlank (gid ()))
            ) ) ] )


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
    , gid ()
    , "var"
    , EIf (gid (), b, EInteger (gid (), "6"), EInteger (gid (), "7"))
    , EVariable (gid (), "var") )


(* ---------------- *)
(* Lambdas *)
(* ---------------- *)
let aLambda = ELambda (gid (), [(gid (), "")], b)

let nonEmptyLambda = ELambda (gid (), [(gid (), "")], five)

let lambdaWithBinding (bindingName : string) (expr : fluidExpr) =
  ELambda (gid (), [(gid (), bindingName)], expr)


let lambdaWithTwoBindings = ELambda (gid (), [(gid (), "x"); (gid (), "y")], b)

let lambdaWithUsedBinding (bindingName : string) =
  lambdaWithBinding bindingName (EVariable (gid (), bindingName))


let lambdaWithUsed2ndBinding (bindingName : string) =
  ELambda
    ( gid ()
    , [(gid (), "somevar"); (gid (), bindingName)]
    , EVariable (gid (), bindingName) )


(* ---------------- *)
(* Functions *)
(* ---------------- *)
let aFnCall = EFnCall (gid (), "Int::add", [five; b], NoRail)

let aFnCallWithVersion = EFnCall (gid (), "DB::getAll_v1", [b], NoRail)

let aFnCallWithBlockArg = EFnCall (gid (), "Dict::map", [b; b], NoRail)

let aBinOp = EBinOp (gid (), "==", b, b, NoRail)

let aFullBinOp = binop "||" (var "myvar") (int "5")

(* ---------------- *)
(* Constructors *)
(* ---------------- *)
let aConstructor = EConstructor (gid (), gid (), "Just", [b])

(* ---------------- *)
(* Records *)
(* ---------------- *)
let emptyRow = [(gid (), "", b)]

let recordRow1 = (gid (), "f1", fiftySix)

let recordRow2 = (gid (), "f2", seventyEight)

let singleRowRecord = ERecord (gid (), [recordRow1])

let multiRowRecord = ERecord (gid (), [recordRow1; recordRow2])

let emptyRowRecord = ERecord (gid (), emptyRow)

let emptyRecord = ERecord (gid (), [])

(* ---------------- *)
(* Lists *)
(* ---------------- *)
let emptyList = list []

let single = list [fiftySix]

let multi = list [fiftySix; seventyEight]

let withStr = list [str "ab"]

let longList =
  list [fiftySix; seventyEight; fiftySix; seventyEight; fiftySix; seventyEight]


let listWithBlank = list [fiftySix; seventyEight; b; fiftySix]

let multiWithStrs = list [str "ab"; str "cd"; str "ef"]

(* ---------------- *)
(* Fields *)
(* ---------------- *)
let aField = EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "field")

let aNestedField =
  EFieldAccess
    ( gid ()
    , EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "field")
    , gid ()
    , "field2" )


let aShortField = EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "f")

let aBlankField = EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "")

let () =
  let open Jest in
  let open Expect in
  test "empty" (fun _ -> expect () |> toEqual ())


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
    [ listFn [aListNum "2"]
    ; listFn [aListNum "3"]
    ; listFn [aListNum "4"]
    ; listFn [aListNum "5"] ]


let aBinopPipe = pipe b [binop "++" pipeTarget (str "asd")]

let aPipeInsideIf = if' b aLongPipe b

let aNestedPipe = pipe (list []) [listFn [pipe aList5 [listFn [aList6]]]]

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
    (let' "" b (fn "Http::Forbidden" [int "403"]))
    (fn "Http::Forbidden" [])
