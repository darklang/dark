open Tc
open Types
open Prelude

(* Shortcuts *)
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


let lambda (varNames : string list) (body : fluidExpr) : fluidExpr =
  ELambda (gid (), List.map varNames ~f:(fun name -> (gid (), name)), body)


let pipe (first : fluidExpr) (rest : fluidExpr list) : fluidExpr =
  EPipe (gid (), first :: rest)


(* test data *)

let aStr = EString (gid (), "some string")

let emptyStr = EString (gid (), "")

let oneCharStr = EString (gid (), "c")

let aShortInt = EInteger (gid (), "1")

let anInt = EInteger (gid (), "12345")

let aHugeInt = EInteger (gid (), "2000000000000000000")

let aFloat = EFloat (gid (), "123", "456")

let aHugeFloat = EFloat (gid (), "123456789", "123456789")

let aPartialFloat = EFloat (gid (), "1", "")

let trueBool = EBool (gid (), true)

let falseBool = EBool (gid (), false)

let aNull = ENull (gid ())

let five = EInteger (gid (), "5")

let six = EInteger (gid (), "6")

let fiftySix = EInteger (gid (), "56")

let seventyEight = EInteger (gid (), "78")

let aPartialVar = EPartial (gid (), "req", b)

let completelyEmptyLet = ELet (gid (), gid (), "", b, b)

let emptyLet = ELet (gid (), gid (), "", b, EInteger (gid (), "5"))

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


let aVar = EVariable (gid (), "variable")

let aShortVar = EVariable (gid (), "v")

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


let aFnCall = EFnCall (gid (), "Int::add", [five; b], NoRail)

let aFnCallWithVersion = EFnCall (gid (), "DB::getAll_v1", [b], NoRail)

let aFnCallWithBlockArg = EFnCall (gid (), "Dict::map", [b; b], NoRail)

let aBinOp = EBinOp (gid (), "==", b, b, NoRail)

(* let aFullBinOp = *)
(*   EBinOp *)
(*     (gid (), "==", EVariable (gid (), "myvar"), EInteger (gid (), 5), NoRail) *)
(* in *)
let aConstructor = EConstructor (gid (), gid (), "Just", [b])

let emptyRow = [(gid (), "", b)]

let recordRow1 = (gid (), "f1", fiftySix)

let recordRow2 = (gid (), "f2", seventyEight)

let singleRowRecord = ERecord (gid (), [recordRow1])

let multiRowRecord = ERecord (gid (), [recordRow1; recordRow2])

let emptyRowRecord = ERecord (gid (), emptyRow)

let emptyRecord = ERecord (gid (), [])

let aField = EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "field")

let aNestedField =
  EFieldAccess
    ( gid ()
    , EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "field")
    , gid ()
    , "field2" )


let aShortField = EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "f")

let aBlankField = EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "")

let indentedIfElse =
  ELet
    ( gid ()
    , gid ()
    , "var"
    , EIf (gid (), b, EInteger (gid (), "6"), EInteger (gid (), "7"))
    , EVariable (gid (), "var") )


let () =
  let open Jest in
  let open Expect in
  test "empty" (fun _ -> expect () |> toEqual ())
