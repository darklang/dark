open Jest
open Expect
open Tc
open Types
open Prelude
open Fluid
module B = Blank
module K = FluidKeyboard

(*
 * These tests are all written in a common style: t "delete end of whole"
 * aFloat (delete 2) ("12.456", 2) ;
 *
 * This is a test that takes the fluidExpr called aFloat, and does a delete on
 * it in position 2. The stringified result is "12.456" and the cursor should
 * be in position 2.
 *
 * There are a handful of functions you can call, including press, presses,
 * insert, backspace, delete, tab, shiftTab, and render.
 *
 * There are a few different ways of running a test:
 *  - t vs tp:
 *    - a test case is created by calling t. This also asserts that the result
 *      does not include a partial.
 *    - you can also call tp, which asserts that the result _does_ include a
 *      partial.
 *  - wrap:
 *      When I started writing these tests, I discovered that they kept passing
 *      despite there being a bug. Whenever the cursor went over the end, it
 *      would stay in the last place, giving a false positive. To avoid this,
 *      I wrapped all test cases:
 *         ```
 *         let request = expression-I-actually-want-to-test
 *         request
 *         ```
 *      There is a downside to this though: the indentation gets screwed up.
 *      This affects match, if, records - basically anything that applies it's
 *      own indent. The outcome is that the final position is wrong if it's not
 *      on the same line. The solution to this is to set ~wrap:false on the
 *      test.
 *  - debug:
 *      When you need more information about a single test, set the ~debug:true
 *      flag.
 * There are also certain conventions in the display of text in the output:
 *  - TBlanks are displayed as `___`
 *  - TPlaceHolders are displayed as normal but with underscores: for example
 *    `_a_:_Int_`
 *  - TPartials are displayed as text - to detect their presence,
 *    see "t vs tp" above.
 *  - TGhostPartials are displayed as multiple @ signs
 *  - Other blanks (see FluidToken.isBlank) are displayed as `***` This is
 *    controlled by FluidToken.toTestText
 *
 *  There are more tests in fluid_pattern_tests for match patterns.
 *)

let complexExpr =
  EIf
    ( gid ()
    , EBinOp
        ( gid ()
        , "||"
        , EBinOp
            ( gid ()
            , "=="
            , EFieldAccess
                ( gid ()
                , EFieldAccess
                    (gid (), EVariable (gid (), "request"), gid (), "headers")
                , gid ()
                , "origin" )
            , EString (gid (), "https://usealtitude.com")
            , NoRail )
        , EBinOp
            ( gid ()
            , "=="
            , EFieldAccess
                ( gid ()
                , EFieldAccess
                    (gid (), EVariable (gid (), "request"), gid (), "headers")
                , gid ()
                , "origin" )
            , EString (gid (), "https://localhost:3000")
            , NoRail )
        , NoRail )
    , ELet
        ( gid ()
        , gid ()
        , ""
        , newB ()
        , EFnCall (gid (), "Http::Forbidden", [EInteger (gid (), 403)], NoRail)
        )
    , EFnCall (gid (), "Http::Forbidden", [], NoRail) )


let tl ast =
  { id = TLID "7"
  ; pos = {x = 0; y = 0}
  ; data =
      TLHandler
        { ast = toExpr ast
        ; tlid = TLID "7"
        ; spec =
            { space = Blank.newF "HTTP"
            ; name = Blank.newF "/test"
            ; modifier = Blank.newF "GET" } } }


type testResult = (string * int) * bool

let () =
  let aStr = EString (gid (), "some string") in
  let emptyStr = EString (gid (), "") in
  let oneCharStr = EString (gid (), "c") in
  let aShortInt = EInteger (gid (), 1) in
  let anInt = EInteger (gid (), 12345) in
  let aHugeInt = EInteger (gid (), 2000000000) in
  let aFloat = EFloat (gid (), "123", "456") in
  let aHugeFloat = EFloat (gid (), "123456789", "123456789") in
  let aPartialFloat = EFloat (gid (), "1", "") in
  let trueBool = EBool (gid (), true) in
  let falseBool = EBool (gid (), false) in
  let aNull = ENull (gid ()) in
  let five = EInteger (gid (), 5) in
  let fiftySix = EInteger (gid (), 56) in
  let seventyEight = EInteger (gid (), 78) in
  let blank = EBlank (gid ()) in
  let aPartialVar = EPartial (gid (), "req", EBlank (gid ())) in
  let completelyEmptyLet =
    ELet (gid (), gid (), "", EBlank (gid ()), EBlank (gid ()))
  in
  let emptyLet =
    ELet (gid (), gid (), "", EBlank (gid ()), EInteger (gid (), 5))
  in
  let emptyMatch =
    let mID = gid () in
    EMatch (mID, EBlank (gid ()), [(FPBlank (mID, gid ()), EBlank (gid ()))])
  in
  let emptyMatchWithTwoPatterns =
    let mID = gid () in
    EMatch
      ( mID
      , EBlank (gid ())
      , [ (FPBlank (mID, gid ()), EBlank (gid ()))
        ; (FPBlank (mID, gid ()), EBlank (gid ())) ] )
  in
  let matchWithPatterns =
    let mID = gid () in
    EMatch
      (mID, EBlank (gid ()), [(FPInteger (mID, gid (), 3), EBlank (gid ()))])
  in
  let matchWithConstructorPattern =
    let mID = gid () in
    EMatch
      ( mID
      , EBlank (gid ())
      , [(FPConstructor (mID, gid (), "Just", []), EBlank (gid ()))] )
  in
  let matchWithBinding (bindingName : string) (expr : fluidExpr) =
    let mID = gid () in
    EMatch (mID, blank, [(FPVariable (mID, gid (), bindingName), expr)])
  in
  let matchWithConstructorBinding (bindingName : string) (expr : fluidExpr) =
    let mID = gid () in
    EMatch
      ( mID
      , blank
      , [ ( FPConstructor
              (mID, gid (), "Ok", [FPVariable (mID, gid (), bindingName)])
          , expr ) ] )
  in
  let nonEmptyLet =
    ELet (gid (), gid (), "", EInteger (gid (), 6), EInteger (gid (), 5))
  in
  let letWithLhs =
    ELet (gid (), gid (), "n", EInteger (gid (), 6), EInteger (gid (), 5))
  in
  let letWithBinding (bindingName : string) (expr : fluidExpr) =
    ELet (gid (), gid (), bindingName, EInteger (gid (), 6), expr)
  in
  let letWithUsedBinding (bindingName : string) =
    letWithBinding bindingName (EVariable (gid (), bindingName))
  in
  let aVar = EVariable (gid (), "variable") in
  let aShortVar = EVariable (gid (), "v") in
  let emptyIf = EIf (gid (), newB (), newB (), newB ()) in
  let plainIf =
    EIf
      (gid (), EInteger (gid (), 5), EInteger (gid (), 6), EInteger (gid (), 7))
  in
  let nestedIf =
    EIf
      ( gid ()
      , EInteger (gid (), 5)
      , EIf
          ( gid ()
          , EInteger (gid (), 5)
          , EInteger (gid (), 6)
          , EInteger (gid (), 7) )
      , EInteger (gid (), 7) )
  in
  let aLambda = ELambda (gid (), [(gid (), "")], blank) in
  let nonEmptyLambda = ELambda (gid (), [(gid (), "")], five) in
  let lambdaWithBinding (bindingName : string) (expr : fluidExpr) =
    ELambda (gid (), [(gid (), bindingName)], expr)
  in
  let lambdaWithUsedBinding (bindingName : string) =
    lambdaWithBinding bindingName (EVariable (gid (), bindingName))
  in
  let lambdaWithUsed2ndBinding (bindingName : string) =
    ELambda
      ( gid ()
      , [(gid (), "somevar"); (gid (), bindingName)]
      , EVariable (gid (), bindingName) )
  in
  let aFnCall = EFnCall (gid (), "Int::add", [five; blank], NoRail) in
  let aBinOp =
    EBinOp (gid (), "==", EBlank (gid ()), EBlank (gid ()), NoRail)
  in
  (* let aFullBinOp = *)
  (*   EBinOp *)
  (*     (gid (), "==", EVariable (gid (), "myvar"), EInteger (gid (), 5), NoRail) *)
  (* in *)
  let aConstructor =
    EConstructor (gid (), gid (), "Just", [EBlank (gid ())])
  in
  let aField =
    EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "field")
  in
  let aNestedField =
    EFieldAccess
      ( gid ()
      , EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "field")
      , gid ()
      , "field2" )
  in
  let aShortField =
    EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "f")
  in
  let aBlankField =
    EFieldAccess (gid (), EVariable (gid (), "obj"), gid (), "")
  in
  let indentedIfElse =
    ELet
      ( gid ()
      , gid ()
      , "var"
      , EIf
          (gid (), EBlank (gid ()), EInteger (gid (), 6), EInteger (gid (), 7))
      , EVariable (gid (), "var") )
  in
  let m =
    let fnParam (name : string) (t : tipe) (opt : bool) : Types.parameter =
      { paramName = name
      ; paramTipe = t
      ; paramBlock_args = []
      ; paramOptional = opt
      ; paramDescription = "" }
    in
    let infixFn op tipe rtTipe =
      { fnName = op
      ; fnParameters = [fnParam "a" tipe false; fnParam "b" tipe false]
      ; fnReturnTipe = rtTipe
      ; fnDescription = "Some infix function"
      ; fnPreviewExecutionSafe = true
      ; fnDeprecated = false
      ; fnInfix = true }
    in
    { Defaults.defaultModel with
      builtInFunctions =
        [ infixFn "<" TInt TBool
        ; infixFn "+" TInt TInt
        ; infixFn "==" TAny TBool
        ; infixFn "<=" TInt TBool
        ; infixFn "||" TBool TBool
        ; { fnName = "Int::add"
          ; fnParameters = [fnParam "a" TAny false; fnParam "b" TAny false]
          ; fnReturnTipe = TInt
          ; fnDescription = "Add two ints"
          ; fnPreviewExecutionSafe = true
          ; fnDeprecated = false
          ; fnInfix = false }
        ; { fnName = "Int::sqrt"
          ; fnParameters = [fnParam "a" TAny false]
          ; fnReturnTipe = TInt
          ; fnDescription = "Get the square root of an Int"
          ; fnPreviewExecutionSafe = true
          ; fnDeprecated = false
          ; fnInfix = false } ] }
  in
  let process
      ~(debug : bool)
      ~(wrap : bool)
      (keys : K.key list)
      (pos : int)
      (ast : ast) : testResult =
    (* we wrap it so that there's something before and after the expr (esp
     * after it), which catches more bugs that ending the text area
     * immediately. Unfortunately, it doesn't work for well nested exprs, like
     * ifs. *)
    let ast =
      if wrap
      then ELet (gid (), gid (), "request", ast, EVariable (gid (), "request"))
      else ast
    in
    let extra = if wrap then 14 else 0 in
    let pos = pos + extra in
    let s =
      { Defaults.defaultFluidState with
        ac = AC.reset m; oldPos = pos; newPos = pos }
    in
    let newAST, newState =
      let tl = tl ast in
      let m = {m with toplevels = [tl]} in
      List.foldl keys ~init:(ast, s) ~f:(fun key (ast, s) ->
          updateMsg
            m
            tl.id
            ast
            (FluidKeyPress
               { key
               ; shiftKey = false
               ; altKey = false
               ; metaKey = false
               ; ctrlKey = false })
            s )
    in
    let result =
      match newAST with
      | expr when not wrap ->
          expr
      | ELet (_, _, _, expr, _) ->
          expr
      | expr ->
          impossible ("not wrapped and not a let: " ^ eToString s expr)
    in
    let partialsFound =
      List.any (toTokens newState result) ~f:(fun ti ->
          match ti.token with
          | TRightPartial _ | TPartial _ ->
              true
          | _ ->
              false )
    in
    if debug
    then (
      Js.log2
        "state"
        (show_fluidState
           { newState with
             (* remove the things that take a lot of space and provide little
            * value. *)
             ac = {newState.ac with functions = []; allCompletions = []} }) ;
      Js.log2 "expr" (eToStructure s result) ) ;
    ((eToString s result, max 0 (newState.newPos - extra)), partialsFound)
  in
  let render (expr : fluidExpr) : testResult =
    process ~debug:false ~wrap:true [] 0 expr
  in
  let delete ?(debug = false) ?(wrap = true) (pos : int) (expr : fluidExpr) :
      testResult =
    process ~debug ~wrap [K.Delete] pos expr
  in
  let backspace ?(debug = false) ?(wrap = true) (pos : int) (expr : fluidExpr)
      : testResult =
    process ~debug ~wrap [K.Backspace] pos expr
  in
  let tab ?(debug = false) ?(wrap = true) (pos : int) (expr : fluidExpr) :
      testResult =
    process ~debug ~wrap [K.Tab] pos expr
  in
  let shiftTab ?(debug = false) ?(wrap = true) (pos : int) (expr : fluidExpr) :
      testResult =
    process ~debug ~wrap [K.ShiftTab] pos expr
  in
  let press
      ?(debug = false)
      ?(wrap = true)
      (key : K.key)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~debug ~wrap [key] pos expr
  in
  let presses
      ?(debug = false)
      ?(wrap = true)
      (keys : K.key list)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~debug ~wrap keys pos expr
  in
  let insert
      ?(debug = false)
      ?(wrap = true)
      (char : char)
      (pos : int)
      (expr : fluidExpr) : testResult =
    let key = K.fromChar char in
    process ~debug ~wrap [key] pos expr
  in
  let b = "___" in
  let t
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      (expected : string * int) =
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () -> expect (fn initial) |> toEqual (expected, false))
  in
  let tp
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      (expected : string * int) =
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () -> expect (fn initial) |> toEqual (expected, true))
  in
  describe "Strings" (fun () ->
      t "insert mid string" aStr (insert 'c' 3) ("\"socme string\"", 4) ;
      t "delete mid string" aStr (delete 3) ("\"soe string\"", 3) ;
      t "backspace mid string" aStr (backspace 4) ("\"soe string\"", 3) ;
      t "insert empty string" emptyStr (insert 'c' 1) ("\"c\"", 2) ;
      t "delete empty string" emptyStr (delete 1) (b, 0) ;
      t "delete empty string from outside" emptyStr (delete 0) (b, 0) ;
      t "backspace empty string" emptyStr (backspace 1) (b, 0) ;
      t
        "backspace empty string from outside goes in"
        emptyStr
        (backspace 2)
        ("\"\"", 1) ;
      t "backspace near-empty string" oneCharStr (backspace 2) ("\"\"", 1) ;
      t "delete near-empty string" oneCharStr (delete 1) ("\"\"", 1) ;
      t "insert outside string" aStr (insert 'c' 0) ("\"some string\"", 0) ;
      t "delete outside string" aStr (delete 0) ("\"some string\"", 0) ;
      t "backspace outside string" aStr (backspace 0) ("\"some string\"", 0) ;
      t "insert start of string" aStr (insert 'c' 1) ("\"csome string\"", 2) ;
      t "delete start of string" aStr (delete 1) ("\"ome string\"", 1) ;
      t "backspace start of string" aStr (backspace 1) ("\"some string\"", 0) ;
      t "insert end of string" aStr (insert 'c' 12) ("\"some stringc\"", 13) ;
      t "delete end of string" aStr (delete 12) ("\"some string\"", 12) ;
      t "backspace end of string" aStr (backspace 12) ("\"some strin\"", 11) ;
      t
        "insert after end of string"
        aStr
        (insert 'c' 13)
        ("\"some string\"", 13) ;
      t "delete after end of string" aStr (delete 13) ("\"some string\"", 13) ;
      t
        "backspace after end of string"
        aStr
        (backspace 13)
        ("\"some string\"", 12) ;
      t "insert space in string" aStr (insert ' ' 3) ("\"so me string\"", 4) ;
      t "delete space in string" aStr (delete 5) ("\"somestring\"", 5) ;
      t "backspace space in string" aStr (backspace 6) ("\"somestring\"", 5) ;
      t "final quote is swallowed" aStr (insert '"' 12) ("\"some string\"", 13) ;
      () ) ;
  describe "Integers" (fun () ->
      t "insert 0 at front " anInt (insert '0' 0) ("12345", 0) ;
      t "insert at end of short" aShortInt (insert '2' 1) ("12", 2) ;
      t "insert not a number" anInt (insert 'c' 0) ("12345", 0) ;
      t "insert start of number" anInt (insert '5' 0) ("512345", 1) ;
      t "delete start of number" anInt (delete 0) ("2345", 0) ;
      t "backspace start of number" anInt (backspace 0) ("12345", 0) ;
      t "insert end of number" anInt (insert '0' 5) ("123450", 6) ;
      t "delete end of number" anInt (delete 5) ("12345", 5) ;
      t "backspace end of number" anInt (backspace 5) ("1234", 4) ;
      t "insert number at scale" aHugeInt (insert '9' 5) ("2000090000", 6) ;
      t "insert number at scale" aHugeInt (insert '9' 0) ("920000000", 1) ;
      t "insert number at scale" aHugeInt (insert '9' 10) ("2000000000", 10) ;
      () ) ;
  describe "Floats" (fun () ->
      t "insert . converts to float - end" anInt (insert '.' 5) ("12345.", 6) ;
      t "insert . converts to float - middle" anInt (insert '.' 3) ("123.45", 4) ;
      t "insert . converts to float - start" anInt (insert '.' 0) ("12345", 0) ;
      t "insert . converts to float - short" aShortInt (insert '.' 1) ("1.", 2) ;
      t "insert zero in whole - start" aFloat (insert '0' 0) ("123.456", 0) ;
      t "insert int in whole - start" aFloat (insert '9' 0) ("9123.456", 1) ;
      t "insert int in whole - middle" aFloat (insert '0' 1) ("1023.456", 2) ;
      t "insert int in whole - end" aFloat (insert '0' 3) ("1230.456", 4) ;
      t "insert int in fraction - start" aFloat (insert '0' 4) ("123.0456", 5) ;
      t "insert int in fraction - middle" aFloat (insert '0' 6) ("123.4506", 7) ;
      t "insert int in fraction - end" aFloat (insert '0' 7) ("123.4560", 8) ;
      t "insert non-int in whole" aFloat (insert 'c' 2) ("123.456", 2) ;
      t "insert non-int in fraction" aFloat (insert 'c' 6) ("123.456", 6) ;
      t "delete dot" aFloat (delete 3) ("123456", 3) ;
      t "delete dot at scale" aHugeFloat (delete 9) ("1234567891", 9) ;
      t "backspace dot" aFloat (backspace 4) ("123456", 3) ;
      t "backspace dot at scale" aHugeFloat (backspace 10) ("1234567891", 9) ;
      t "delete start of whole" aFloat (delete 0) ("23.456", 0) ;
      t "delete middle of whole" aFloat (delete 1) ("13.456", 1) ;
      t "delete end of whole" aFloat (delete 2) ("12.456", 2) ;
      t "delete start of fraction" aFloat (delete 4) ("123.56", 4) ;
      t "delete middle of fraction" aFloat (delete 5) ("123.46", 5) ;
      t "delete end of fraction" aFloat (delete 6) ("123.45", 6) ;
      t "delete dot converts to int" aFloat (delete 3) ("123456", 3) ;
      t
        "delete dot converts to int, no fraction"
        aPartialFloat
        (delete 1)
        ("1", 1) ;
      t "backspace start of whole" aFloat (backspace 1) ("23.456", 0) ;
      t "backspace middle of whole" aFloat (backspace 2) ("13.456", 1) ;
      t "backspace end of whole" aFloat (backspace 3) ("12.456", 2) ;
      t "backspace start of fraction" aFloat (backspace 5) ("123.56", 4) ;
      t "backspace middle of fraction" aFloat (backspace 6) ("123.46", 5) ;
      t "backspace end of fraction" aFloat (backspace 7) ("123.45", 6) ;
      t "backspace dot converts to int" aFloat (backspace 4) ("123456", 3) ;
      t
        "backspace dot converts to int, no fraction"
        aPartialFloat
        (backspace 2)
        ("1", 1) ;
      t "continue after adding dot" aPartialFloat (insert '2' 2) ("1.2", 3) ;
      () ) ;
  describe "Bools" (fun () ->
      tp "insert start of true" trueBool (insert 'c' 0) ("ctrue", 1) ;
      tp "delete start of true" trueBool (delete 0) ("rue", 0) ;
      t "backspace start of true" trueBool (backspace 0) ("true", 0) ;
      tp "insert end of true" trueBool (insert '0' 4) ("true0", 5) ;
      t "delete end of true" trueBool (delete 4) ("true", 4) ;
      tp "backspace end of true" trueBool (backspace 4) ("tru", 3) ;
      tp "insert middle of true" trueBool (insert '0' 2) ("tr0ue", 3) ;
      tp "delete middle of true" trueBool (delete 2) ("tre", 2) ;
      tp "backspace middle of true" trueBool (backspace 2) ("tue", 1) ;
      tp "insert start of false" falseBool (insert 'c' 0) ("cfalse", 1) ;
      tp "delete start of false" falseBool (delete 0) ("alse", 0) ;
      t "backspace start of false" falseBool (backspace 0) ("false", 0) ;
      tp "insert end of false" falseBool (insert '0' 5) ("false0", 6) ;
      t "delete end of false" falseBool (delete 5) ("false", 5) ;
      tp "backspace end of false" falseBool (backspace 5) ("fals", 4) ;
      tp "insert middle of false" falseBool (insert '0' 2) ("fa0lse", 3) ;
      tp "delete middle of false" falseBool (delete 2) ("fase", 2) ;
      tp "backspace middle of false" falseBool (backspace 2) ("flse", 1) ;
      () ) ;
  describe "Nulls" (fun () ->
      tp "insert start of null" aNull (insert 'c' 0) ("cnull", 1) ;
      tp "delete start of null" aNull (delete 0) ("ull", 0) ;
      t "backspace start of null" aNull (backspace 0) ("null", 0) ;
      tp "insert end of null" aNull (insert '0' 4) ("null0", 5) ;
      t "delete end of null" aNull (delete 4) ("null", 4) ;
      tp "backspace end of null" aNull (backspace 4) ("nul", 3) ;
      tp "insert middle of null" aNull (insert '0' 2) ("nu0ll", 3) ;
      tp "delete middle of null" aNull (delete 2) ("nul", 2) ;
      tp "backspace middle of null" aNull (backspace 2) ("nll", 1) ;
      () ) ;
  describe "Blanks" (fun () ->
      t "insert middle of blank->string" blank (insert '"' 3) ("\"\"", 1) ;
      t "delete middle of blank->blank" blank (delete 3) (b, 3) ;
      t "backspace middle of blank->blank" blank (backspace 3) (b, 2) ;
      t "insert blank->string" blank (insert '"' 0) ("\"\"", 1) ;
      t "delete blank->string" emptyStr (delete 0) (b, 0) ;
      t "backspace blank->string" emptyStr (backspace 1) (b, 0) ;
      t "insert blank->int" blank (insert '5' 0) ("5", 1) ;
      t "insert blank->int" blank (insert '0' 0) ("0", 1) ;
      t "delete int->blank " five (delete 0) (b, 0) ;
      t "backspace int->blank " five (backspace 1) (b, 0) ;
      t "insert end of blank->int" blank (insert '5' 1) ("5", 1) ;
      tp "insert partial" blank (insert 't' 0) ("t", 1) ;
      t
        "backspacing your way through a partial finishes"
        trueBool
        (presses [K.Backspace; K.Backspace; K.Backspace; K.Backspace; K.Left] 4)
        ("___", 0) ;
      t "insert blank->space" blank (press K.Space 0) (b, 0) ;
      () ) ;
  describe "Fields" (fun () ->
      t "insert middle of fieldname" aField (insert 'c' 5) ("obj.fcield", 6) ;
      t
        "cant insert invalid chars fieldname"
        aField
        (insert '$' 5)
        ("obj.field", 5) ;
      t "delete middle of fieldname" aField (delete 5) ("obj.feld", 5) ;
      t "delete fieldname" aShortField (delete 4) ("obj.***", 4) ;
      t "backspace fieldname" aShortField (backspace 5) ("obj.***", 4) ;
      t "insert end of fieldname" aField (insert 'c' 9) ("obj.fieldc", 10) ;
      tp "insert end of varname" aField (insert 'c' 3) ("objc.field", 4) ;
      t "insert start of fieldname" aField (insert 'c' 4) ("obj.cfield", 5) ;
      t "insert blank fieldname" aBlankField (insert 'c' 4) ("obj.c", 5) ;
      t "delete fieldop with name" aShortField (delete 3) ("obj", 3) ;
      t "backspace fieldop with name" aShortField (backspace 4) ("obj", 3) ;
      t "delete fieldop with blank" aBlankField (delete 3) ("obj", 3) ;
      t "backspace fieldop with blank" aBlankField (backspace 4) ("obj", 3) ;
      t "delete fieldop in nested" aNestedField (delete 3) ("obj.field2", 3) ;
      t
        "backspace fieldop in nested"
        aNestedField
        (backspace 4)
        ("obj.field2", 3) ;
      t
        "adding a dot after a completed variable goes into a fieldaccess"
        aVar
        (insert '.' 8)
        ("variable.***", 9) ;
      t
        "adding a dot after a partial goes into a fieldaccess"
        aPartialVar
        (insert '.' 3)
        ("request.***", 8) ;
      t
        "adding a dot after a completed field goes into a fieldaccess"
        aField
        (insert '.' 9)
        ("obj.field.***", 10) ;
      t "insert space in blank " aBlankField (press K.Space 4) ("obj.***", 4) ;
      () ) ;
  describe "Functions" (fun () ->
      t
        "space on a sep goes to next arg"
        aFnCall
        (press K.Space 10)
        ("Int::add 5 _________", 11) ;
      tp
        "backspace on a function converts to partial for renaming"
        aFnCall
        (press K.Backspace 8)
        ("Int::ad@ 5 _________", 7) ;
      tp
        "deleting on a function converts to partial for renaming"
        aFnCall
        (press K.Delete 7)
        ("Int::ad@ 5 _________", 7) ;
      t
        "renaming a function maintains unaligned params in let scope"
        (EPartial
           (gid (), "Int::", EFnCall (gid (), "Int::add", [five; five], NoRail)))
        (presses ~wrap:false [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        ("let b = 5\nInt::sqrt 5", 10) ;
      t
        "renaming a function doesn't maintain unaligned params if they're already set to variables"
        (EPartial
           ( gid ()
           , "Int::"
           , EFnCall
               ( gid ()
               , "Int::add"
               , [EVariable (gid (), "a"); EVariable (gid (), "b")]
               , NoRail ) ))
        (presses ~wrap:false [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        ("Int::sqrt a", 10) ;
      t
        "renaming a function doesn't maintain unaligned params if they're not set (blanks)"
        (EPartial
           ( gid ()
           , "Int::"
           , EFnCall
               (gid (), "Int::add", [EBlank (gid ()); EBlank (gid ())], NoRail)
           ))
        (presses ~wrap:false [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        ("Int::sqrt _________", 10) ;
      (* TODO: functions are not implemented fully. I deleted backspace and
       * delete because we were switching to partials, but this isn't
       * implemented. Some tests we need:
         * myFunc arg1 arg2, 6 => Backspace => myFun arg1 arg2, with a ghost and a partial.
         * same with delete *)
      () ) ;
  describe "Binops" (fun () ->
      tp "pipe key starts partial" trueBool (press K.Pipe 4) ("true |", 6) ;
      t
        "pressing enter completes partial"
        trueBool
        (presses [K.Pipe; K.Down; K.Enter] 4)
        ("true || __________", 7) ;
      t
        "pressing space completes partial"
        trueBool
        (presses [K.Pipe; K.Down; K.Space] 4)
        ("true || __________", 8) ;
      tp
        "pressing plus key starts partial"
        trueBool
        (press K.Plus 4)
        ("true +", 6) ;
      tp
        "pressing caret key starts partial"
        anInt
        (press K.Caret 5)
        ("12345 ^", 7) ;
      t
        "pressing pipe twice then space completes partial"
        trueBool
        (presses [K.Pipe; K.Pipe; K.Space] 4)
        ("true || __________", 8) ;
      t
        "piping into newline creates thread"
        trueBool
        (presses [K.Pipe; K.GreaterThan; K.Space] 4)
        ("true\n|>___", 8) ;
      t
        "pressing backspace to clear partial reverts for blank rhs"
        (EPartial (gid (), "|", EBinOp (gid (), "||", anInt, blank, NoRail)))
        (press K.Backspace 7)
        ("12345", 5) ;
      t
        "pressing backspace to clear partial reverts for blank rhs, check lhs pos goes to start"
        (EPartial (gid (), "|", EBinOp (gid (), "||", newB (), blank, NoRail)))
        (press K.Backspace 12)
        ("___", 0) ;
      t
        "pressing delete to clear partial reverts for blank rhs"
        (EPartial (gid (), "|", EBinOp (gid (), "||", anInt, blank, NoRail)))
        (press K.Delete 6)
        ("12345", 5) ;
      t
        "pressing delete to clear partial reverts for blank rhs, check lhs pos goes to start"
        (EPartial (gid (), "|", EBinOp (gid (), "||", newB (), blank, NoRail)))
        (press K.Delete 11)
        ("___", 0) ;
      t
        "using backspace to remove an infix with a placeholder goes to right place"
        (EPartial (gid (), "|", EBinOp (gid (), "||", newB (), newB (), NoRail)))
        (press K.Backspace 12)
        ("___", 0) ;
      t
        "using backspace to remove an infix with a placeholder goes to right place 2"
        (EPartial (gid (), "|", EBinOp (gid (), "||", five, newB (), NoRail)))
        (press K.Backspace 3)
        ("5", 1) ;
      t
        "pressing backspace to clear rightpartial reverts for blank rhs"
        (ERightPartial (gid (), "|", blank))
        (press K.Backspace 5)
        ("___", 0) ;
      t
        "pressing backspace on single digit binop leaves lhs"
        (EBinOp (gid (), "+", anInt, anInt, NoRail))
        (press K.Backspace 7)
        ("12345", 5) ;
      t
        "using delete to remove an infix with a placeholder goes to right place"
        (EPartial (gid (), "|", EBinOp (gid (), "||", newB (), newB (), NoRail)))
        (press K.Delete 11)
        ("___", 0) ;
      t
        "pressing delete to clear rightpartial reverts for blank rhs"
        (ERightPartial (gid (), "|", blank))
        (press K.Delete 4)
        ("___", 0) ;
      t
        "pressing delete on single digit binop leaves lhs"
        (EBinOp (gid (), "+", anInt, anInt, NoRail))
        (press K.Delete 6)
        ("12345", 5) ;
      t
        "pressing letters and numbers on a partial completes it"
        blank
        (presses [K.Number '5'; K.Plus; K.Number '5'] 0)
        ("5 + 5", 5) ;
      tp
        "pressing pipe while editing a partial works properly"
        (EPartial (gid (), "|", EBinOp (gid (), "||", anInt, anInt, NoRail)))
        (press K.Pipe 7)
        ("12345 || 12345", 8) ;
      tp
        "pressing = after < should go to partial"
        (EBinOp (gid (), "<", anInt, anInt, NoRail))
        (press K.Equals 7)
        ("12345 <= 12345", 8) ;
      t
        "changing binop should work"
        (EBinOp (gid (), "<", anInt, anInt, NoRail))
        (presses [K.Equals; K.Enter] 7)
        ("12345 <= 12345", 8) ;
      tp
        "adding binop in `if` works"
        (EIf (gid (), EBlank (gid ()), EBlank (gid ()), EBlank (gid ())))
        (press K.Percent 3)
        ("if %\nthen\n  ___\nelse\n  ___", 4) ;
      let aFullBinOp =
        EBinOp
          ( gid ()
          , "||"
          , EVariable (gid (), "myvar")
          , EInteger (gid (), 5)
          , NoRail )
      in
      tp "show ghost partial" aFullBinOp (backspace 8) ("myvar |@ 5", 7) ;
      (* TODO backspace on empty partial does something *)
      (* TODO support delete on all the backspace commands *)
      (* TODO pressing enter at the end of the partialGhost *)
      () ) ;
  describe "Constructors" (fun () ->
      tp
        "backspace on a constructor converts it to a partial with ghost"
        aConstructor
        (press K.Backspace 4)
        ("Jus@ ___", 3) ;
      tp
        "backspace on a constructor converts it to a partial with ghost"
        aConstructor
        (press K.Delete 0)
        ("ust@ ___", 0) ;
      t
        "space on a constructor blank does nothing"
        aConstructor
        (press K.Space 5)
        ("Just ___", 5) ;
      (* TODO: test renaming constructors.
       * It's not too useful yet because there's only 4 constructors and,
       * hence, unlikely that anyone will rename them this way.
       * Also, the names of the temporary variables used to store the old arguments of a changed
       * constructor are randomly generated and would be hard to test *)
      () ) ;
  describe "Lambdas" (fun () ->
      t "backspace over lambda symbol" aLambda (backspace 1) ("___", 0) ;
      t "insert space in lambda" aLambda (press K.Space 1) ("\\*** -> ___", 1) ;
      t
        "backspace non-empty lambda symbol"
        nonEmptyLambda
        (backspace 1)
        ("\\*** -> 5", 1) ;
      t "delete lambda symbol" aLambda (delete 0) ("___", 0) ;
      t
        "delete non-empty lambda symbol"
        nonEmptyLambda
        (delete 0)
        ("\\*** -> 5", 0) ;
      t
        "insert changes occurence of binding var"
        (lambdaWithUsedBinding "binding")
        (insert 'c' 8)
        ("\\bindingc -> bindingc", 9) ;
      t
        "insert changes occurence of binding 2nd var"
        (lambdaWithUsed2ndBinding "binding")
        (insert 'c' 17)
        ("\\somevar, bindingc -> bindingc", 18) ;
      t
        "dont jump in lambdavars with infix chars"
        aLambda
        (press K.Plus 1)
        ("\\*** -> ___", 1) ;
      t
        "dont allow name to start with a number"
        aLambda
        (insert '5' 1)
        ("\\*** -> ___", 1) ;
      t
        "dont allow name to start with a number, pt 2"
        (lambdaWithBinding "test" five)
        (insert '2' 1)
        ("\\test -> 5", 1) ;
      t
        "dont allow name to start with a number, pt 3"
        aLambda
        (insert '5' 3)
        ("\\*** -> ___", 3) ;
      () ) ;
  describe "Variables" (fun () ->
      tp "insert middle of variable" aVar (insert 'c' 5) ("variacble", 6) ;
      tp "delete middle of variable" aVar (delete 5) ("variale", 5) ;
      tp "insert capital works" aVar (press (K.Letter 'A') 5) ("variaAble", 6) ;
      t "can't insert invalid" aVar (press K.Dollar 5) ("variable", 5) ;
      t "delete variable" aShortVar (delete 0) (b, 0) ;
      tp "delete long variable" aVar (delete 0) ("ariable", 0) ;
      tp "delete mid variable" aVar (delete 6) ("variabe", 6) ;
      t "backspace variable" aShortVar (backspace 1) (b, 0) ;
      tp "backspace mid variable" aVar (backspace 8) ("variabl", 7) ;
      tp "backspace mid variable" aVar (backspace 6) ("variale", 5) ;
      t
        "variable doesn't override if"
        (ELet
           (gid (), gid (), "i", blank, EPartial (gid (), "i", EBlank (gid ()))))
        (presses ~wrap:false [K.Letter 'f'; K.Enter] 13)
        ("let i = ___\nif ___\nthen\n  ___\nelse\n  ___", 15) ;
      () ) ;
  describe "Match" (fun () ->
      t
        "move to the front of match"
        emptyMatch
        (press K.GoToStartOfLine 6)
        ("match ___\n  *** -> ___", 0) ;
      t
        "move to the end of match"
        emptyMatch
        (press K.GoToEndOfLine 0)
        ("match ___\n  *** -> ___", 9) ;
      t
        "move to the front of match on line 2"
        emptyMatch
        (press ~wrap:false K.GoToStartOfLine 15)
        ("match ___\n  *** -> ___", 12) ;
      t
        "move to the end of match on line 2"
        emptyMatch
        (press ~wrap:false K.GoToEndOfLine 12)
        ("match ___\n  *** -> ___", 22) ;
      t
        "move back over match"
        emptyMatch
        (press K.Left 6)
        ("match ___\n  *** -> ___", 0) ;
      t
        "move forward over match"
        emptyMatch
        (press K.Right 0)
        ("match ___\n  *** -> ___", 6) ;
      t "backspace over empty match" emptyMatch (backspace 6) ("___", 0) ;
      t
        "backspace over empty match with 2 patterns"
        emptyMatchWithTwoPatterns
        (backspace 6)
        ("___", 0) ;
      t
        "backspace over match with 2 patterns"
        matchWithPatterns
        (backspace 6)
        ("match ___\n  3 -> ___", 6) ;
      t "delete over empty match" emptyMatch (delete 0) ("___", 0) ;
      t
        "delete over empty match with 2 patterns"
        emptyMatchWithTwoPatterns
        (delete 0)
        ("___", 0) ;
      t
        "delete over match with 2 patterns"
        matchWithPatterns
        (delete 0)
        ("match ___\n  3 -> ___", 0) ;
      t
        "delete constructor in match pattern"
        matchWithConstructorPattern
        (delete ~wrap:false 12)
        ("match ___\n  ust -> ___", 12) ;
      t
        "backspace constructor in match pattern"
        matchWithConstructorPattern
        (backspace ~wrap:false 16)
        ("match ___\n  Jus -> ___", 15) ;
      t
        "insert changes occurence of non-shadowed var in case"
        (matchWithBinding "binding" (EVariable (gid (), "binding")))
        (insert ~wrap:false 'c' 19)
        ("match ___\n  bindingc -> bindingc", 20) ;
      t
        "insert changes occurence of non-shadowed var in case constructor"
        (matchWithConstructorBinding "binding" (EVariable (gid (), "binding")))
        (insert ~wrap:false 'c' 22)
        ("match ___\n  Ok bindingc -> bindingc", 23) ;
      t
        "insert space in blank match"
        emptyMatch
        (press K.Space 6)
        ("match ___\n  *** -> ___", 6) ;
      t
        "insert space in blank match on line 2"
        emptyMatch
        (press ~wrap:false K.Space 12)
        ("match ___\n  *** -> ___", 12) ;
      () ) ;
  describe "Lets" (fun () ->
      t
        "move to the front of let"
        emptyLet
        (press K.GoToStartOfLine 4)
        ("let *** = ___\n5", 0) ;
      t
        "move to the end of let"
        emptyLet
        (press K.GoToEndOfLine 4)
        ("let *** = ___\n5", 13) ;
      t "move back over let" emptyLet (press K.Left 4) ("let *** = ___\n5", 0) ;
      t
        "move forward over let"
        emptyLet
        (press K.Right 0)
        ("let *** = ___\n5", 4) ;
      t "backspace over empty let" emptyLet (backspace 3) ("5", 0) ;
      t "delete empty let" emptyLet (delete 0) ("5", 0) ;
      t
        "backspace over non-empty let"
        nonEmptyLet
        (backspace 3)
        ("let *** = 6\n5", 3) ;
      t "delete non-empty let" nonEmptyLet (delete 0) ("let *** = 6\n5", 0) ;
      t
        "insert space on blank let"
        emptyLet
        (press K.Space 4)
        ("let *** = ___\n5", 4) ;
      t "lhs on empty" emptyLet (insert 'c' 4) ("let c = ___\n5", 5) ;
      t "middle of blank" emptyLet (insert 'c' 5) ("let c = ___\n5", 5) ;
      t "backspace letlhs" letWithLhs (backspace 5) ("let *** = 6\n5", 4) ;
      t "delete letlhs" letWithLhs (delete 4) ("let *** = 6\n5", 4) ;
      t
        "equals skips over assignment"
        emptyLet
        (presses [K.Letter 'c'; K.Equals] 4)
        ("let c = ___\n5", 8) ;
      t
        "equals skips over assignment 1"
        emptyLet
        (press K.Equals 7)
        ("let *** = ___\n5", 10) ;
      t
        "equals skips over assignment 2"
        emptyLet
        (press K.Equals 8)
        ("let *** = ___\n5", 10) ;
      t
        "equals skips over assignment 3"
        emptyLet
        (press K.Equals 9)
        ("let *** = ___\n5", 10) ;
      t
        "backspace changes occurence of binding var"
        (letWithUsedBinding "binding")
        (backspace 11)
        ("let bindin = 6\nbindin", 10) ;
      t
        "insert changes occurence of binding var"
        (letWithUsedBinding "binding")
        (insert 'c' 11)
        ("let bindingc = 6\nbindingc", 12) ;
      t
        "insert changes occurence of binding in match nested expr"
        (letWithBinding
           "binding"
           (EMatch
              ( gid ()
              , blank
              , [ ( FPVariable (gid (), gid (), "binding")
                  , EVariable (gid (), "binding") )
                ; (FPInteger (gid (), gid (), 5), EVariable (gid (), "binding"))
                ] )))
        (insert 'c' 11)
        ( "let bindingc = 6\nmatch ___\n  binding -> binding\n  5 -> bindingc"
        , 12 ) ;
      t
        "insert doesn't change occurence of binding in shadowed lambda expr"
        (letWithBinding
           "binding"
           (ELambda
              (gid (), [(gid (), "binding")], EVariable (gid (), "binding"))))
        (insert 'c' 11)
        ("let bindingc = 6\n\\binding -> binding", 12) ;
      t
        "insert changes occurence of binding in lambda expr"
        (letWithBinding
           "binding"
           (ELambda
              (gid (), [(gid (), "somevar")], EVariable (gid (), "binding"))))
        (insert 'c' 11)
        ("let bindingc = 6\n\\somevar -> bindingc", 12) ;
      t
        "dont jump in letlhs with infix chars"
        emptyLet
        (press K.Plus 4)
        ("let *** = ___\n5", 4) ;
      t
        "dont allow letlhs to start with a number"
        emptyLet
        (insert '5' 4)
        ("let *** = ___\n5", 4) ;
      t
        "dont allow letlhs to start with a number, pt 2"
        letWithLhs
        (insert '2' 4)
        ("let n = 6\n5", 4) ;
      t
        "dont allow letlhs to start with a number, pt 3"
        emptyLet
        (insert '5' 6)
        ("let *** = ___\n5", 6) ;
      () ) ;
  describe "Threads" (fun () ->
      let threadOn expr fns = EThread (gid (), expr :: fns) in
      let emptyList = EList (gid (), []) in
      let aList5 = EList (gid (), [five]) in
      let aListNum n = EList (gid (), [EInteger (gid (), n)]) in
      let listFn args =
        EFnCall (gid (), "List::append", EThreadTarget (gid ()) :: args, NoRail)
      in
      let aThread = threadOn emptyList [listFn [aList5]; listFn [aList5]] in
      let emptyThread = EThread (gid (), [newB (); newB ()]) in
      let aLongThread =
        threadOn
          emptyList
          [ listFn [aListNum 2]
          ; listFn [aListNum 3]
          ; listFn [aListNum 4]
          ; listFn [aListNum 5] ]
      in
      let aThreadInsideIf = EIf (gid (), newB (), aLongThread, newB ()) in
      (* TODO: add tests for clicking in the middle of a thread pipe (or blank) *)
      t
        "move to the front of thread on line 1"
        aThread
        (press K.GoToStartOfLine 2)
        ("[]\n|>List::append [5]\n|>List::append [5]", 0) ;
      t
        "move to the end of thread on line 1"
        aThread
        (press K.GoToEndOfLine 0)
        ("[]\n|>List::append [5]\n|>List::append [5]", 2) ;
      t
        "move to the front of thread on line 2"
        aThread
        (press ~wrap:false K.GoToStartOfLine 8)
        ("[]\n|>List::append [5]\n|>List::append [5]", 5) ;
      t
        "move to the end of thread on line 2"
        aThread
        (press ~wrap:false K.GoToEndOfLine 5)
        ("[]\n|>List::append [5]\n|>List::append [5]", 21) ;
      t
        "move to the front of thread on line 3"
        aThread
        (press ~wrap:false K.GoToStartOfLine 40)
        ("[]\n|>List::append [5]\n|>List::append [5]", 24) ;
      t
        "move to the end of thread on line 3"
        aThread
        (press ~wrap:false K.GoToEndOfLine 24)
        ("[]\n|>List::append [5]\n|>List::append [5]", 40) ;
      t
        "threads appear on new lines"
        aThread
        render
        ("[]\n|>List::append [5]\n|>List::append [5]", 0) ;
      let aNestedThread =
        threadOn emptyList [listFn [threadOn aList5 [listFn [aList5]]]]
      in
      t
        "nested threads will indent"
        aNestedThread
        render
        ("[]\n|>List::append [5]\n               |>List::append [5]", 0) ;
      t
        "backspacing a thread's first pipe works"
        aLongThread
        (backspace ~wrap:false 5)
        ("[]\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]", 2) ;
      t
        "deleting a thread's first pipe works"
        aLongThread
        (delete ~wrap:false 3)
        ("[]\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]", 3) ;
      t
        "backspacing a thread's second pipe works"
        aLongThread
        (backspace ~wrap:false 24)
        ("[]\n|>List::append [2]\n|>List::append [4]\n|>List::append [5]", 21) ;
      t
        "deleting a thread's second pipe works"
        aLongThread
        (delete ~wrap:false 22)
        ("[]\n|>List::append [2]\n|>List::append [4]\n|>List::append [5]", 22) ;
      t
        "backspacing a thread's third pipe works"
        aLongThread
        (backspace ~wrap:false 43)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [5]", 40) ;
      t
        "deleting a thread's third pipe works"
        aLongThread
        (delete ~wrap:false 41)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [5]", 41) ;
      t
        "backspacing a thread's last pipe works"
        aLongThread
        (backspace ~wrap:false 62)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]", 59) ;
      t
        "deleting a thread's last pipe works"
        aLongThread
        (delete ~wrap:false 60)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]", 60) ;
      t
        "backspacing a thread's first pipe that isn't in the first column works"
        aThreadInsideIf
        (backspace ~wrap:false 21)
        ( "if ___\nthen\n  []\n  |>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 16 ) ;
      t
        "deleting a thread's first pipe that isn't in the first column works"
        aThreadInsideIf
        (delete ~wrap:false 19)
        ( "if ___\nthen\n  []\n  |>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 19 ) ;
      t
        "backspacing a thread's second pipe that isn't in the first column works"
        aThreadInsideIf
        (backspace ~wrap:false 42)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 37 ) ;
      t
        "deleting a thread's second pipe that isn't in the first column works"
        aThreadInsideIf
        (delete ~wrap:false 40)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 40 ) ;
      t
        "backspacing a thread's third pipe that isn't in the first column works"
        aThreadInsideIf
        (backspace ~wrap:false 63)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [5]\nelse\n  ___"
        , 58 ) ;
      t
        "deleting a thread's third pipe that isn't in the first column works"
        aThreadInsideIf
        (delete ~wrap:false 61)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [5]\nelse\n  ___"
        , 61 ) ;
      t
        "backspacing a thread's fourth pipe that isn't in the first column works"
        aThreadInsideIf
        (backspace ~wrap:false 84)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]\nelse\n  ___"
        , 79 ) ;
      t
        "deleting a thread's fourth pipe that isn't in the first column works"
        aThreadInsideIf
        (delete ~wrap:false 82)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]\nelse\n  ___"
        , 82 ) ;
      t
        "adding infix functions adds the right number of blanks"
        emptyThread
        (presses ~wrap:false [K.Plus; K.Enter] 6)
        ("___\n|>+ _________", 8) ;
      (* TODO: test for prefix fns *)
      (* TODO: test for deleting threaded infix fns *)
      (* TODO: test for deleting threaded prefix fns *)
      () ) ;
  describe "Ifs" (fun () ->
      t
        "move over indent 1"
        plainIf
        (press ~wrap:false K.Left 12)
        ("if 5\nthen\n  6\nelse\n  7", 9) ;
      t
        "move over indent 2"
        plainIf
        (press ~wrap:false K.Left 21)
        ("if 5\nthen\n  6\nelse\n  7", 18) ;
      t
        "backspace over indent 1"
        plainIf
        (backspace ~wrap:false 12)
        ("if 5\nthen\n  6\nelse\n  7", 9) ;
      t
        "backspace over indent 2"
        plainIf
        (backspace ~wrap:false 21)
        ("if 5\nthen\n  6\nelse\n  7", 18) ;
      t "backspace over empty if" emptyIf (backspace 2) ("___", 0) ;
      t
        "move to front of line 1"
        plainIf
        (press ~wrap:false K.GoToStartOfLine 4)
        ("if 5\nthen\n  6\nelse\n  7", 0) ;
      t
        "move to end of line 1"
        plainIf
        (press ~wrap:false K.GoToEndOfLine 0)
        ("if 5\nthen\n  6\nelse\n  7", 4) ;
      t
        "move to front of line 3"
        plainIf
        (press ~wrap:false K.GoToStartOfLine 13)
        ("if 5\nthen\n  6\nelse\n  7", 12) ;
      t
        "move to end of line 3"
        plainIf
        (press ~wrap:false K.GoToEndOfLine 12)
        ("if 5\nthen\n  6\nelse\n  7", 13) ;
      t
        "move to front of line 5 in nested if"
        nestedIf
        (press ~wrap:false K.GoToStartOfLine 16)
        ("if 5\nthen\n  if 5\n  then\n    6\n  else\n    7\nelse\n  7", 12) ;
      t
        "move to end of line 5 in nested if"
        nestedIf
        (press ~wrap:false K.GoToEndOfLine 12)
        ("if 5\nthen\n  if 5\n  then\n    6\n  else\n    7\nelse\n  7", 16) ;
      t
        "try to insert space on blank"
        emptyIf
        (press ~wrap:false K.Space 3)
        ("if ___\nthen\n  ___\nelse\n  ___", 3) ;
      t
        "try to insert space on blank indent 2"
        emptyIf
        (press ~wrap:false K.Space 14)
        ("if ___\nthen\n  ___\nelse\n  ___", 14) ;
      () ) ;
  describe "Lists" (fun () ->
      let emptyList = EList (gid (), []) in
      let single = EList (gid (), [fiftySix]) in
      let multi = EList (gid (), [fiftySix; seventyEight]) in
      let withStr = EList (gid (), [EString (gid (), "ab")]) in
      let longList =
        EList
          ( gid ()
          , [ fiftySix
            ; seventyEight
            ; fiftySix
            ; seventyEight
            ; fiftySix
            ; seventyEight ] )
      in
      let listWithBlank =
        EList (gid (), [fiftySix; seventyEight; EBlank (gid ()); fiftySix])
      in
      let multiWithStrs =
        EList
          ( gid ()
          , [ EString (gid (), "ab")
            ; EString (gid (), "cd")
            ; EString (gid (), "ef") ] )
      in
      t "create list" blank (press K.LeftSquareBracket 0) ("[]", 1) ;
      t
        "inserting before the list does nothing"
        emptyList
        (insert '5' 0)
        ("[]", 0) ;
      t "insert space into multi list" multi (press K.Space 6) ("[56,78]", 6) ;
      t "insert space into single list" single (press K.Space 3) ("[56]", 3) ;
      t "insert into existing list item" single (insert '4' 1) ("[456]", 2) ;
      t
        "insert separator before item creates blank"
        single
        (insert ',' 1)
        ("[___,56]", 2) ;
      t
        "insert separator after item creates blank"
        single
        (insert ',' 3)
        ("[56,___]", 4) ;
      t
        "insert separator between items creates blank"
        multi
        (insert ',' 3)
        ("[56,___,78]", 4) ;
      (* t "insert separator mid integer makes two items" single (insert ',' 2) *)
      (*   ("[5,6]", 3) ; *)
      (* TODO: when on a separator in a nested list, pressing comma makes an entry outside the list. *)
      t
        "insert separator mid string does nothing special "
        withStr
        (insert ',' 3)
        ("[\"a,b\"]", 4) ;
      t
        "backspacing open bracket of empty list deletes list"
        emptyList
        (backspace 1)
        (b, 0) ;
      t
        "backspacing close bracket of empty list moves inside list"
        emptyList
        (backspace 2)
        ("[]", 1) ;
      t
        "deleting open bracket of empty list deletes list"
        emptyList
        (delete 0)
        (b, 0) ;
      t
        "close bracket at end of list is swallowed"
        emptyList
        (press K.RightSquareBracket 1)
        ("[]", 2) ;
      t
        "backspace on first separator between items deletes item after separator"
        multi
        (backspace 4)
        ("[56]", 3) ;
      t
        "delete before first separator between items deletes item after separator"
        multi
        (delete 3)
        ("[56]", 3) ;
      t
        "backspace on middle separator between items deletes item after separator"
        longList
        (backspace 10)
        ("[56,78,56,56,78]", 9) ;
      t
        "delete before middle separator between items deletes item after separator"
        longList
        (delete 9)
        ("[56,78,56,56,78]", 9) ;
      t
        "backspace on middle separator between items deletes blank after separator"
        listWithBlank
        (backspace 7)
        ("[56,78,56]", 6) ;
      t
        "delete before middle separator between items deletes blank after separator"
        listWithBlank
        (delete 6)
        ("[56,78,56]", 6) ;
      t
        "backspace on last separator between a blank and item deletes item after separator"
        listWithBlank
        (backspace 11)
        ("[56,78,___]", 10) ;
      t
        "delete before last separator between a blank and item deletes item after separator"
        listWithBlank
        (delete 10)
        ("[56,78,___]", 10) ;
      t
        "backspace on separator between string items deletes item after separator"
        multiWithStrs
        (backspace 6)
        ("[\"ab\",\"ef\"]", 5) ;
      t
        "delete before separator between string items deletes item after separator"
        multiWithStrs
        (delete 5)
        ("[\"ab\",\"ef\"]", 5) ;
      () ) ;
  describe "Record" (fun () ->
      let emptyRecord = ERecord (gid (), []) in
      let emptyRow = ERecord (gid (), [(gid (), "", blank)]) in
      let single = ERecord (gid (), [(gid (), "f1", fiftySix)]) in
      let multi =
        ERecord
          (gid (), [(gid (), "f1", fiftySix); (gid (), "f2", seventyEight)])
      in
      (* let withStr = EList (gid (), [EString (gid (), "ab")]) in *)
      t "create record" blank (press K.LeftCurlyBrace 0) ("{}", 1) ;
      t
        "inserting before the record does nothing"
        emptyRecord
        (insert '5' 0)
        ("{}", 0) ;
      t
        "inserting space between empty record does nothing"
        emptyRecord
        (press K.Space 1)
        ("{}", 1) ;
      t
        "inserting space in empty record field does nothing"
        emptyRow
        (press K.Space 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "inserting space in empty record value does nothing"
        emptyRow
        (press K.Space 10)
        ("{\n  *** : ___\n}", 10) ;
      t
        "pressing enter in an empty record adds a new line"
        emptyRecord
        (press ~wrap:false K.Enter 1)
        ("{\n  *** : ___\n}", 4) ;
      t
        "enter fieldname"
        emptyRow
        (insert ~wrap:false 'c' 4)
        ("{\n  c : ___\n}", 5) ;
      t
        "move to the front of an empty record"
        emptyRow
        (press K.GoToStartOfLine ~wrap:false 13)
        ("{\n  *** : ___\n}", 4) ;
      t
        "move to the end of an empty record"
        emptyRow
        (press K.GoToEndOfLine ~wrap:false 4)
        ("{\n  *** : ___\n}", 13) ;
      t
        "cant enter invalid fieldname"
        emptyRow
        (insert '^' 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "backspacing open brace of empty record deletes record"
        emptyRecord
        (backspace 1)
        (b, 0) ;
      t
        "backspacing close brace of empty record moves inside record"
        emptyRecord
        (backspace 2)
        ("{}", 1) ;
      t
        "deleting open brace of empty record deletes record"
        emptyRecord
        (delete 0)
        (b, 0) ;
      t
        "close brace at end of record is swallowed"
        emptyRecord
        (press K.RightCurlyBrace 1)
        ("{}", 2) ;
      t
        "backspacing empty record field clears entry"
        emptyRow
        (backspace ~wrap:false 4)
        (* TODO: shouldn't need to wrap *)
        ("{}", 1) ;
      t
        "appending to int in expr works"
        single
        (insert ~wrap:false '1' 11)
        ("{\n  f1 : 561\n}", 12) ;
      t
        "appending to int in expr works"
        multi
        (insert ~wrap:false '1' 21)
        ("{\n  f1 : 56\n  f2 : 781\n}", 22) ;
      t
        "move to the front of a record with multiple values"
        multi
        (press K.GoToStartOfLine ~wrap:false 21)
        ("{\n  f1 : 56\n  f2 : 78\n}", 14) ;
      t
        "move to the end of a record with multiple values"
        multi
        (press K.GoToEndOfLine ~wrap:false 14)
        ("{\n  f1 : 56\n  f2 : 78\n}", 21) ;
      t
        "inserting at the end of the key works"
        emptyRow
        (insert 'f' ~wrap:false 6)
        ("{\n  f : ___\n}", 5) ;
      t
        "pressing enter at start adds a row"
        multi
        (press K.Enter ~wrap:false 1)
        ("{\n  *** : ___\n  f1 : 56\n  f2 : 78\n}", 4) ;
      t
        "pressing enter at the back adds a row"
        multi
        (press K.Enter ~wrap:false 22)
        ("{\n  f1 : 56\n  f2 : 78\n  *** : ___\n}", 24) ;
      t
        "pressing enter at the start of a field adds a row"
        multi
        (press K.Enter ~wrap:false 14)
        ("{\n  f1 : 56\n  *** : ___\n  f2 : 78\n}", 14) ;
      t
        "dont allow weird chars in recordFields"
        emptyRow
        (press K.RightParens ~wrap:false 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "dont jump in recordFields with infix chars"
        emptyRow
        (press K.Plus ~wrap:false 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "dont jump in recordFields with infix chars, pt 2"
        single
        (press K.Plus ~wrap:false 6)
        ("{\n  f1 : 56\n}", 6) ;
      t
        "colon should skip over the record colon"
        emptyRow
        (press K.Colon ~wrap:false 7)
        ("{\n  *** : ___\n}", 10) ;
      t
        "dont allow key to start with a number"
        emptyRow
        (insert '5' ~wrap:false 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "dont allow key to start with a number, pt 2"
        single
        (insert '5' ~wrap:false 4)
        ("{\n  f1 : 56\n}", 4) ;
      t
        "dont allow key to start with a number, pt 3"
        emptyRow
        (insert '5' ~wrap:false 6)
        ("{\n  *** : ___\n}", 6) ;
      () ) ;
  describe "Autocomplete" (fun () ->
      t
        "space autocompletes correctly"
        (EPartial (gid (), "if", EBlank (gid ())))
        (press K.Space 2)
        ("if ___\nthen\n  ___\nelse\n  ___", 3) ;
      t
        "let moves to right place"
        (EPartial (gid (), "let", EBlank (gid ())))
        (press K.Enter 3)
        ("let *** = ___\n___", 4) ;
      t
        "autocomplete space moves forward by 1"
        aBinOp
        (presses [K.Letter 'r'; K.Space] 0)
        ("request == _________", 8) ;
      t
        "autocomplete enter moves to end of value"
        aBinOp
        (presses [K.Letter 'r'; K.Enter] 0)
        ("request == _________", 7) ;
      t "can tab to lambda blank" aLambda (tab 0) ("\\*** -> ___", 1) ;
      t
        "autocomplete tab moves to next blank"
        aBinOp
        (presses [K.Letter 'r'; K.Tab] 0)
        ("request == _________", 11) ;
      t
        "autocomplete enter on bin-op moves to start of first blank"
        (EBlank (gid ()))
        (presses [K.Equals; K.Enter] 0)
        ("_________ == _________", 0) ;
      t
        "autocomplete tab on bin-op moves to start of second blank"
        (EBlank (gid ()))
        (presses [K.Equals; K.Tab] 0)
        ("_________ == _________", 13) ;
      (* TODO: make autocomplete on space work consistently
      t
        "autocomplete space on bin-op moves to start of first blank"
        (EBlank (gid ()))
        (presses [K.Equals; K.Space] 0)
        ("_________ == _________", 0) ;
      *)
      t
        "variable moves to right place"
        (EPartial (gid (), "req", EBlank (gid ())))
        (press K.Enter 3)
        ("request", 7) ;
      t
        "thread moves to right place on blank"
        (EBlank (gid ()))
        (presses ~wrap:false [K.Letter '|'; K.Letter '>'; K.Enter] 2)
        ("___\n|>___", 6) ;
      t
        "thread moves to right place on placeholder"
        aFnCall
        (presses ~wrap:false [K.Letter '|'; K.Letter '>'; K.Enter] 11)
        ("Int::add 5 ___\n           |>___", 28) ;
      t
        "thread moves to right place in if then"
        emptyIf
        (presses ~wrap:false [K.Letter '|'; K.Letter '>'; K.Enter] 14)
        ("if ___\nthen\n  ___\n  |>___\nelse\n  ___", 22) ;
      t
        "thread moves to right place in lambda body"
        aLambda
        (presses ~wrap:false [K.Letter '|'; K.Letter '>'; K.Enter] 8)
        ("\\*** -> ___\n        |>___", 22) ;
      t
        "thread moves to right place in match body"
        emptyMatch
        (presses ~wrap:false [K.Letter '|'; K.Letter '>'; K.Enter] 19)
        ("match ___\n  *** -> ___\n         |>___", 34) ;
      t
        "autocomplete for Just"
        (EPartial (gid (), "Just", EBlank (gid ())))
        (press K.Enter 4)
        ("Just ___", 5) ;
      t
        "autocomplete for Ok"
        (EPartial (gid (), "Ok", EBlank (gid ())))
        (press K.Enter 2)
        ("Ok ___", 3) ;
      t
        "autocomplete for Nothing"
        (EPartial (gid (), "Nothing", EBlank (gid ())))
        (press K.Enter 7)
        (* TODO: this should be 7 *)
        ("Nothing", 8) ;
      t
        "autocomplete for Error"
        (EPartial (gid (), "Error", EBlank (gid ())))
        (press K.Enter 5)
        ("Error ___", 6) ;
      (* test "backspacing on variable reopens autocomplete" (fun () -> *)
      (*     expect (backspace (EVariable (5, "request"))). *)
      (*     gridFor ~pos:116 tokens) |> toEqual {row= 2; col= 2} ) ; *)
      () ) ;
  describe "Movement" (fun () ->
      let tokens = toTokens m.fluidState complexExpr in
      let len = tokens |> List.map ~f:(fun ti -> ti.token) |> length in
      let s = Defaults.defaultFluidState in
      let ast = complexExpr in
      test "gridFor - 1" (fun () ->
          expect (gridFor ~pos:116 tokens) |> toEqual {row = 2; col = 2} ) ;
      test "gridFor - 2" (fun () ->
          expect (gridFor ~pos:70 tokens) |> toEqual {row = 0; col = 70} ) ;
      test "gridFor - 3" (fun () ->
          expect (gridFor ~pos:129 tokens) |> toEqual {row = 2; col = 15} ) ;
      test "gridFor - start of line" (fun () ->
          expect (gridFor ~pos:130 tokens) |> toEqual {row = 3; col = 0} ) ;
      test "gridFor - in an indent" (fun () ->
          expect (gridFor ~pos:158 tokens) |> toEqual {row = 5; col = 1} ) ;
      test "gridFor - (reverse) in an indent" (fun () ->
          expect (posFor ~row:5 ~col:1 tokens) |> toEqual 158 ) ;
      test "gridFor - (reverse) in an indent" (fun () ->
          expect (posFor ~row:5 ~col:1 tokens) |> toEqual 158 ) ;
      test "gridFor roundtrips" (fun () ->
          let poses = List.range 0 len in
          let newPoses =
            List.map poses ~f:(fun pos ->
                let {row; col} = gridFor ~pos tokens in
                posFor ~row ~col tokens )
          in
          expect poses |> toEqual newPoses ) ;
      t
        "right skips over indent when in indent"
        emptyIf
        (press ~wrap:false K.Right 12)
        ("if ___\nthen\n  ___\nelse\n  ___", 17) ;
      t
        "left skips over indent when in indent"
        emptyIf
        (press ~wrap:false K.Left 13)
        ("if ___\nthen\n  ___\nelse\n  ___", 11) ;
      (* length *)
      test "up from first row is zero" (fun () ->
          expect (doUp ~pos:5 ast s |> fun s -> s.newPos) |> toEqual 0 ) ;
      test "down from first row is end of last row" (fun () ->
          expect (doDown ~pos:168 ast s |> fun s -> s.newPos) |> toEqual 174 ) ;
      (* end of short row *)
      test "up into shorter row goes to end of row" (fun () ->
          expect (doUp ~pos:172 ast s |> fun m -> m.newPos) |> toEqual 156 ) ;
      test "down into shorter row goes to end of row" (fun () ->
          expect (doDown ~pos:143 ast s |> fun m -> m.newPos) |> toEqual 156 ) ;
      (* start of indented row *)
      test "up into indented row goes to first token" (fun () ->
          expect (doUp ~pos:152 ast s |> fun m -> m.newPos) |> toEqual 130 ) ;
      test "down into indented row goes to first token" (fun () ->
          expect (doDown ~pos:109 ast s |> fun m -> m.newPos) |> toEqual 114 ) ;
      t
        "enter at the end of a line goes to start of next line"
        nonEmptyLet
        (press ~wrap:false K.Enter 11)
        ("let *** = 6\n5", 12) ;
      t
        "enter at the end of a line goes to first non-whitespace token"
        indentedIfElse
        (press ~wrap:false K.Enter 16)
        ( "let var = if ___\n"
          ^ "          then\n"
          ^ "            6\n"
          ^ "          else\n"
          ^ "            7\n"
          ^ "var"
        , 27 ) ;
      t
        "end of if-then blank goes up properly"
        emptyIf
        (presses ~wrap:false [K.Escape; K.Up] 17)
        ("if ___\nthen\n  ___\nelse\n  ___", 11) ;
      t
        "end of if-then blank goes up properly, twice"
        emptyIf
        (presses ~wrap:false [K.Escape; K.Up; K.Up] 17)
        ("if ___\nthen\n  ___\nelse\n  ___", 5) ;
      t
        "end of if-then blank goes down properly"
        emptyIf
        (presses ~wrap:false [K.Escape; K.Down] 5)
        ("if ___\nthen\n  ___\nelse\n  ___", 11) ;
      t
        "end of if-then blank goes down properly, twice"
        emptyIf
        (presses ~wrap:false [K.Escape; K.Down; K.Down] 5)
        ("if ___\nthen\n  ___\nelse\n  ___", 17) ;
      (* moving through the autocomplete *)
      test "up goes through the autocomplete" (fun () ->
          expect
            ( moveTo 143 s
            |> (fun s -> updateKey K.Up ast s)
            |> (fun (ast, s) -> updateKey K.Up ast s)
            |> (fun (ast, s) -> updateKey K.Up ast s)
            |> fun (_, s) -> s.newPos )
          |> toEqual 13 ) ;
      test "down goes through the autocomplete" (fun () ->
          expect
            ( moveTo 14 s
            |> (fun s -> updateKey K.Down ast s)
            |> (fun (ast, s) -> updateKey K.Down ast s)
            |> (fun (ast, s) -> updateKey K.Down ast s)
            |> fun (_, s) -> s.newPos )
          |> toEqual 144 ) ;
      test "clicking away from autocomplete commits" (fun () ->
          expect
            (let ast =
               ELet
                 ( gid ()
                 , gid ()
                 , "var"
                 , EPartial (gid (), "false", EBlank (gid ()))
                 , blank )
             in
             moveTo 14 s
             |> (fun s ->
                  let tl = tl ast in
                  let m = {m with toplevels = [tl]} in
                  updateAutocomplete m tl.id ast s )
             |> (fun s -> updateMouseClick 0 ast s)
             |> fun (ast, s) ->
             match ast with
             | ELet (_, _, _, EBool (_, false), _) ->
                 "success"
             | _ ->
                 eToStructure s ast)
          |> toEqual "success" ) ;
      t
        "moving right off a function autocompletes it anyway"
        (ELet
           ( gid ()
           , gid ()
           , "x"
           , EPartial (gid (), "Int::add", EBlank (gid ()))
           , blank ))
        (press ~wrap:false K.Right 16)
        ("let x = Int::add _________ _________\n___", 17) ;
      t
        "moving left off a function autocompletes it anyway"
        (ELet
           ( gid ()
           , gid ()
           , "x"
           , EPartial (gid (), "Int::add", EBlank (gid ()))
           , blank ))
        (press ~wrap:false K.Left 8)
        ("let x = Int::add _________ _________\n___", 7) ;
      test "escape hides autocomplete" (fun () ->
          expect
            (let ast = blank in
             moveTo 0 s
             |> (fun s -> updateKey (K.Letter 'r') ast s)
             |> (fun (ast, s) -> updateKey K.Escape ast s)
             |> fun (_, s) -> s.ac.index)
          |> toEqual None ) ;
      test "right/left brings back autocomplete" (fun () ->
          expect
            (let ast = blank in
             moveTo 0 s
             |> (fun s -> updateKey (K.Letter 'r') ast s)
             |> (fun (ast, s) -> updateKey K.Escape ast s)
             |> fun (_, s) -> s.ac.index)
          |> toEqual None ) ;
      () ) ;
  describe "Tabs" (fun () ->
      t
        "tab goes to first block in a let"
        emptyLet
        (tab 0)
        ("let *** = ___\n5", 4) ;
      t
        "tab goes when on blank"
        completelyEmptyLet
        (tab ~wrap:false 10)
        ("let *** = ___\n___", 14) ;
      t
        "tab goes to second block in a let"
        emptyLet
        (tab 4)
        ("let *** = ___\n5", 10) ;
      t
        "tab wraps second block in a let"
        emptyLet
        (tab 15)
        ("let *** = ___\n5", 4) ;
      t
        "shift tab goes to last block in a let"
        emptyLet
        (shiftTab 14)
        ("let *** = ___\n5", 10) ;
      t
        "shift tab goes to previous block in a let"
        emptyLet
        (shiftTab 10)
        ("let *** = ___\n5", 4) ;
      t
        "shift tab completes autocomplete"
        completelyEmptyLet
        (presses ~wrap:false [K.Letter 'i'; K.Letter 'f'; K.ShiftTab] 14)
        ("let *** = ___\nif ___\nthen\n  ___\nelse\n  ___", 10) ;
      t
        "shift-tab goes when on blank"
        completelyEmptyLet
        (shiftTab 14)
        ("let *** = ___\n___", 10) ;
      t
        "shift tab wraps from the start of a let"
        emptyLet
        (shiftTab 4)
        ("let *** = ___\n5", 10) ;
      t "cant tab to filled letLHS" letWithLhs (tab 0) ("let n = 6\n5", 0) ;
      t "can tab to lambda blank" aLambda (tab 0) ("\\*** -> ___", 1) ;
      t "can shift tab to field blank" aBlankField (shiftTab 0) ("obj.***", 4) ;
      () ) ;
  ()
