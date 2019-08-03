open Jest
open Expect
open Tc
open Types
open Prelude
open Fluid
module B = Blank
module K = FluidKeyboard

(*
 * These tests are all written in a common style: t "del end of whole"
 * aFloat (del 2) ("12.456", 2) ;
 *
 * This is a test that takes the fluidExpr called aFloat, and does a del on
 * it in position 2. The stringified result is "12.456" and the cursor should
 * be in position 2.
 *
 * There are a handful of functions you can call, including press, presses,
 * insert, bs, del, tab, shiftTab, and render.
 *
 * There are a few different ways of running a test:
 *  - t vs tp:
 *    - a test case is created by calling t. This also asserts that the result
 *      does not include a partial.
 *    - you can also call tp, which asserts that the result _does_ include a
 *      partial.
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
 *  - Wrap:
 *      When I started writing these tests, I discovered that they kept passing
 *      despite there being a bug. Whenever the cursor went over the end, it
 *      would stay in the last place, giving a false positive. To avoid this,
 *      I wrapped all test cases:
 *         ```
 *         if true
 *         then
 *           expression-I-actually-want-to-test
 *         else
 *           5
 *         ```
 *      We go to great efforts to fix the indentation afterwards. However, you
 *      may find that your test works without the wrapping and doesn't work
 *      with it. The solution is to try it wrapped in the editor and see where
 *      it goes wrong. *
 *  There are more tests in fluid_pattern_tests for match patterns.
 *)

let b () = newB ()

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
        , b ()
        , EFnCall (gid (), "Http::Forbidden", [EInteger (gid (), 403)], NoRail)
        )
    , EFnCall (gid (), "Http::Forbidden", [], NoRail) )


let debugState s =
  show_fluidState
    { s with
      (* remove the things that take a lot of space and provide little value. *)
      ac =
        { s.ac with
          functions = []
        ; allCompletions = []
        ; completions = (if s.ac.index = None then [] else s.ac.completions) }
    }


let h ast : handler =
  { ast = toExpr ast
  ; hTLID = TLID "7"
  ; pos = {x = 0; y = 0}
  ; spec =
      { space = Blank.newF "HTTP"
      ; name = Blank.newF "/test"
      ; modifier = Blank.newF "GET" } }


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
  let six = EInteger (gid (), 6) in
  let fiftySix = EInteger (gid (), 56) in
  let seventyEight = EInteger (gid (), 78) in
  let aPartialVar = EPartial (gid (), "req", b ()) in
  let completelyEmptyLet = ELet (gid (), gid (), "", b (), b ()) in
  let emptyLet = ELet (gid (), gid (), "", b (), EInteger (gid (), 5)) in
  let emptyMatch =
    let mID = gid () in
    EMatch (mID, b (), [(FPBlank (mID, gid ()), b ())])
  in
  let emptyMatchWithTwoPatterns =
    let mID = gid () in
    EMatch
      ( mID
      , b ()
      , [(FPBlank (mID, gid ()), b ()); (FPBlank (mID, gid ()), b ())] )
  in
  let matchWithPatterns =
    let mID = gid () in
    EMatch (mID, b (), [(FPInteger (mID, gid (), 3), b ())])
  in
  let matchWithConstructorPattern =
    let mID = gid () in
    EMatch (mID, b (), [(FPConstructor (mID, gid (), "Just", []), b ())])
  in
  let matchWithBinding (bindingName : string) (expr : fluidExpr) =
    let mID = gid () in
    EMatch (mID, b (), [(FPVariable (mID, gid (), bindingName), expr)])
  in
  let matchWithConstructorBinding (bindingName : string) (expr : fluidExpr) =
    let mID = gid () in
    EMatch
      ( mID
      , b ()
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
  let emptyIf = EIf (gid (), b (), b (), b ()) in
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
  let aLambda = ELambda (gid (), [(gid (), "")], b ()) in
  let nonEmptyLambda = ELambda (gid (), [(gid (), "")], five) in
  let lambdaWithBinding (bindingName : string) (expr : fluidExpr) =
    ELambda (gid (), [(gid (), bindingName)], expr)
  in
  let lambdaWithTwoBindings =
    ELambda (gid (), [(gid (), "x"); (gid (), "y")], b ())
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
  let aFnCall = EFnCall (gid (), "Int::add", [five; b ()], NoRail) in
  let aFnCallWithVersion = EFnCall (gid (), "DB::getAll_v1", [b ()], NoRail) in
  let aFnCallWithBlockArg =
    EFnCall (gid (), "Dict::map", [b (); b ()], NoRail)
  in
  let aBinOp = EBinOp (gid (), "==", b (), b (), NoRail) in
  (* let aFullBinOp = *)
  (*   EBinOp *)
  (*     (gid (), "==", EVariable (gid (), "myvar"), EInteger (gid (), 5), NoRail) *)
  (* in *)
  let aConstructor = EConstructor (gid (), gid (), "Just", [b ()]) in
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
      , EIf (gid (), b (), EInteger (gid (), 6), EInteger (gid (), 7))
      , EVariable (gid (), "var") )
  in
  let m =
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
      ; fnPreviewExecutionSafe = true
      ; fnDeprecated = false
      ; fnInfix = true }
    in
    { Defaults.defaultModel with
      analyses =
        StrDict.singleton (* The default traceID for TLID 7 *)
          ~key:"94167980-f909-527e-a4af-bc3155f586d3"
          ~value:
            { liveValues =
                StrDict.singleton
                  ~key:"12"
                  ~value:
                    (DObj
                       (StrDict.fromList [("body", DNull); ("formBody", DNull)]))
            }
    ; builtInFunctions =
        [ infixFn "<" TInt TBool
        ; infixFn "+" TInt TInt
        ; infixFn "==" TAny TBool
        ; infixFn "<=" TInt TBool
        ; infixFn "||" TBool TBool
        ; { fnName = "Int::add"
          ; fnParameters = [fnParam "a" TInt false; fnParam "b" TInt false]
          ; fnReturnTipe = TInt
          ; fnDescription = "Add two ints"
          ; fnPreviewExecutionSafe = true
          ; fnDeprecated = false
          ; fnInfix = false }
        ; { fnName = "Int::sqrt"
          ; fnParameters = [fnParam "a" TInt false]
          ; fnReturnTipe = TInt
          ; fnDescription = "Get the square root of an Int"
          ; fnPreviewExecutionSafe = true
          ; fnDeprecated = false
          ; fnInfix = false }
        ; { fnName = "DB::getAll_v1"
          ; fnParameters = [fnParam "table" TDB false]
          ; fnReturnTipe = TList
          ; fnDescription = "get all"
          ; fnPreviewExecutionSafe = false
          ; fnDeprecated = false
          ; fnInfix = false }
        ; { fnName = "Dict::map"
          ; fnParameters =
              [ fnParam "dict" TObj false
              ; fnParam "f" TBlock false ~blockArgs:["key"; "value"] ]
          ; fnReturnTipe = TObj
          ; fnDescription =
              "Iterates each `key` and `value` in Dictionary `dict` and mutates it according to the provided lambda"
          ; fnPreviewExecutionSafe = true
          ; fnDeprecated = false
          ; fnInfix = false } ] }
  in
  let process ~(debug : bool) (keys : K.key list) (pos : int) (ast : ast) :
      testResult =
    let s = {Defaults.defaultFluidState with ac = AC.reset m} in
    let newlinesBeforeStartPos =
      (* How many newlines occur before the pos, it'll be indented by 2 for
       * each newline, once the expr is wrapped in an if, so we need to add
       * 2*nl to get the pos in place. (Note: it's correct to just count them,
       * as opposed to the iterative approach we do later, because we're using
       * the old ast that has no newlines. *)
      ast
      |> toTokens {s with newPos = pos}
      |> List.filter ~f:(fun ti ->
             FluidToken.isNewline ti.token && ti.startPos < pos )
      |> List.length
    in
    let ast = EIf (gid (), EBool (gid (), true), ast, EInteger (gid (), 5)) in
    let wrapperOffset = 15 in
    let extra = wrapperOffset + (newlinesBeforeStartPos * 2) in
    let pos = pos + extra in
    let s = {s with oldPos = pos; newPos = pos} in
    if debug
    then (
      Js.log2 "state before " (debugState s) ;
      Js.log2 "expr before" (eToStructure s ast) ) ;
    let newAST, newState =
      let h = h ast in
      let m = {m with handlers = Handlers.fromList [h]} in
      List.foldl keys ~init:(ast, s) ~f:(fun key (ast, s) ->
          updateMsg
            m
            h.hTLID
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
      | EIf (_, _, expr, _) ->
          expr
      | expr ->
          impossible ("the wrapper is broken: " ^ eToString s expr)
    in
    let endPos = ref (newState.newPos - wrapperOffset) in
    (* Account for the newlines as we find them, or else we won't know our
     * position to find the newlines correctly. There'll be extra indentation,
     * so we need to subtract those to get the pos we expect. *)
    result
    |> toTokens newState
    |> List.iter ~f:(fun ti ->
           match ti.token with
           | TNewline _ when !endPos > ti.endPos ->
               endPos := !endPos - 2
           | _ ->
               () ) ;
    (* max 0 cause tests can bs past 0 and that's weird to test for *)
    let finalPos = max 0 !endPos in
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
      Js.log2 "state after" (debugState newState) ;
      Js.log2 "expr after" (eToStructure newState result) ) ;
    ((eToString s result, finalPos), partialsFound)
  in
  let render (expr : fluidExpr) : testResult =
    process ~debug:false [] 0 expr
  in
  let del ?(debug = false) (pos : int) (expr : fluidExpr) : testResult =
    process ~debug [K.Delete] pos expr
  in
  let bs ?(debug = false) (pos : int) (expr : fluidExpr) : testResult =
    process ~debug [K.Backspace] pos expr
  in
  let tab ?(debug = false) (pos : int) (expr : fluidExpr) : testResult =
    process ~debug [K.Tab] pos expr
  in
  let shiftTab ?(debug = false) (pos : int) (expr : fluidExpr) : testResult =
    process ~debug [K.ShiftTab] pos expr
  in
  let space ?(debug = false) (pos : int) (expr : fluidExpr) : testResult =
    process ~debug [K.Space] pos expr
  in
  let enter ?(debug = false) (pos : int) (expr : fluidExpr) : testResult =
    process ~debug [K.Enter] pos expr
  in
  let press ?(debug = false) (key : K.key) (pos : int) (expr : fluidExpr) :
      testResult =
    process ~debug [key] pos expr
  in
  let presses
      ?(debug = false) (keys : K.key list) (pos : int) (expr : fluidExpr) :
      testResult =
    process ~debug keys pos expr
  in
  let insert ?(debug = false) (char : char) (pos : int) (expr : fluidExpr) :
      testResult =
    let key = K.fromChar char in
    process ~debug [key] pos expr
  in
  let blank = "___" in
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
      t "del mid string" aStr (del 3) ("\"soe string\"", 3) ;
      t "bs mid string" aStr (bs 4) ("\"soe string\"", 3) ;
      t "insert empty string" emptyStr (insert 'c' 1) ("\"c\"", 2) ;
      t "del empty string" emptyStr (del 1) (blank, 0) ;
      t "del empty string from outside" emptyStr (del 0) (blank, 0) ;
      t "bs empty string" emptyStr (bs 1) (blank, 0) ;
      t "bs outside empty string" emptyStr (bs 2) ("\"\"", 1) ;
      t "bs near-empty string" oneCharStr (bs 2) ("\"\"", 1) ;
      t "del near-empty string" oneCharStr (del 1) ("\"\"", 1) ;
      t "insert outside string" aStr (insert 'c' 0) ("\"some string\"", 0) ;
      t "del outside string" aStr (del 0) ("\"some string\"", 0) ;
      t "bs outside string" aStr (bs 0) ("\"some string\"", 0) ;
      t "insert start of string" aStr (insert 'c' 1) ("\"csome string\"", 2) ;
      t "del start of string" aStr (del 1) ("\"ome string\"", 1) ;
      t "bs start of string" aStr (bs 1) ("\"some string\"", 0) ;
      t "insert end of string" aStr (insert 'c' 12) ("\"some stringc\"", 13) ;
      t "del end of string" aStr (del 12) ("\"some string\"", 12) ;
      t "bs end of string" aStr (bs 12) ("\"some strin\"", 11) ;
      t "insert after end" aStr (insert 'c' 13) ("\"some string\"", 13) ;
      t "del after end of string" aStr (del 13) ("\"some string\"", 13) ;
      t "bs after end" aStr (bs 13) ("\"some string\"", 12) ;
      t "insert space in string" aStr (insert ' ' 3) ("\"so me string\"", 4) ;
      t "del space in string" aStr (del 5) ("\"somestring\"", 5) ;
      t "bs space in string" aStr (bs 6) ("\"somestring\"", 5) ;
      t "final quote is swallowed" aStr (insert '"' 12) ("\"some string\"", 13) ;
      () ) ;
  describe "Integers" (fun () ->
      t "insert 0 at front " anInt (insert '0' 0) ("12345", 0) ;
      t "insert at end of short" aShortInt (insert '2' 1) ("12", 2) ;
      t "insert not a number" anInt (insert 'c' 0) ("12345", 0) ;
      t "insert start of number" anInt (insert '5' 0) ("512345", 1) ;
      t "del start of number" anInt (del 0) ("2345", 0) ;
      t "bs start of number" anInt (bs 0) ("12345", 0) ;
      t "insert end of number" anInt (insert '0' 5) ("123450", 6) ;
      t "del end of number" anInt (del 5) ("12345", 5) ;
      t "bs end of number" anInt (bs 5) ("1234", 4) ;
      t "insert number at scale" aHugeInt (insert '9' 5) ("2000090000", 6) ;
      t "insert number at scale" aHugeInt (insert '9' 0) ("920000000", 1) ;
      t "insert number at scale" aHugeInt (insert '9' 10) ("2000000000", 10) ;
      () ) ;
  describe "Floats" (fun () ->
      t "insert . converts to float - end" anInt (insert '.' 5) ("12345.", 6) ;
      t "insert . converts to float - middle" anInt (insert '.' 3) ("123.45", 4) ;
      t "insert . converts to float - start" anInt (insert '.' 0) ("12345", 0) ;
      t "insert . converts to float - short" aShortInt (insert '.' 1) ("1.", 2) ;
      t "continue after adding dot" aPartialFloat (insert '2' 2) ("1.2", 3) ;
      t "insert zero in whole - start" aFloat (insert '0' 0) ("123.456", 0) ;
      t "insert int in whole - start" aFloat (insert '9' 0) ("9123.456", 1) ;
      t "insert int in whole - middle" aFloat (insert '0' 1) ("1023.456", 2) ;
      t "insert int in whole - end" aFloat (insert '0' 3) ("1230.456", 4) ;
      t "insert int in fraction - start" aFloat (insert '0' 4) ("123.0456", 5) ;
      t "insert int in fraction - middle" aFloat (insert '0' 6) ("123.4506", 7) ;
      t "insert int in fraction - end" aFloat (insert '0' 7) ("123.4560", 8) ;
      t "insert non-int in whole" aFloat (insert 'c' 2) ("123.456", 2) ;
      t "insert non-int in fraction" aFloat (insert 'c' 6) ("123.456", 6) ;
      t "del dot" aFloat (del 3) ("123456", 3) ;
      t "del dot at scale" aHugeFloat (del 9) ("1234567891", 9) ;
      t "bs dot" aFloat (bs 4) ("123456", 3) ;
      t "bs dot at scale" aHugeFloat (bs 10) ("1234567891", 9) ;
      t "del start of whole" aFloat (del 0) ("23.456", 0) ;
      t "del middle of whole" aFloat (del 1) ("13.456", 1) ;
      t "del end of whole" aFloat (del 2) ("12.456", 2) ;
      t "del start of fraction" aFloat (del 4) ("123.56", 4) ;
      t "del middle of fraction" aFloat (del 5) ("123.46", 5) ;
      t "del end of fraction" aFloat (del 6) ("123.45", 6) ;
      t "del dot converts to int" aFloat (del 3) ("123456", 3) ;
      t "del dot converts to int, no fraction" aPartialFloat (del 1) ("1", 1) ;
      t "bs dot" aFloat (bs 4) ("123456", 3) ;
      t "bs dot at scale" aHugeFloat (bs 10) ("1234567891", 9) ;
      t "bs start of whole" aFloat (bs 1) ("23.456", 0) ;
      t "bs middle of whole" aFloat (bs 2) ("13.456", 1) ;
      t "bs end of whole" aFloat (bs 3) ("12.456", 2) ;
      t "bs start of fraction" aFloat (bs 5) ("123.56", 4) ;
      t "bs middle of fraction" aFloat (bs 6) ("123.46", 5) ;
      t "bs end of fraction" aFloat (bs 7) ("123.45", 6) ;
      t "bs dot converts to int" aFloat (bs 4) ("123456", 3) ;
      t "bs dot converts to int, no fraction" aPartialFloat (bs 2) ("1", 1) ;
      t "continue after adding dot" aPartialFloat (insert '2' 2) ("1.2", 3) ;
      () ) ;
  describe "Bools" (fun () ->
      tp "insert start of true" trueBool (insert 'c' 0) ("ctrue", 1) ;
      tp "del start of true" trueBool (del 0) ("rue", 0) ;
      t "bs start of true" trueBool (bs 0) ("true", 0) ;
      tp "insert end of true" trueBool (insert '0' 4) ("true0", 5) ;
      t "del end of true" trueBool (del 4) ("true", 4) ;
      tp "bs end of true" trueBool (bs 4) ("tru", 3) ;
      tp "insert middle of true" trueBool (insert '0' 2) ("tr0ue", 3) ;
      tp "del middle of true" trueBool (del 2) ("tre", 2) ;
      tp "bs middle of true" trueBool (bs 2) ("tue", 1) ;
      tp "insert start of false" falseBool (insert 'c' 0) ("cfalse", 1) ;
      tp "del start of false" falseBool (del 0) ("alse", 0) ;
      t "bs start of false" falseBool (bs 0) ("false", 0) ;
      tp "insert end of false" falseBool (insert '0' 5) ("false0", 6) ;
      t "del end of false" falseBool (del 5) ("false", 5) ;
      tp "bs end of false" falseBool (bs 5) ("fals", 4) ;
      tp "insert middle of false" falseBool (insert '0' 2) ("fa0lse", 3) ;
      tp "del middle of false" falseBool (del 2) ("fase", 2) ;
      tp "bs middle of false" falseBool (bs 2) ("flse", 1) ;
      () ) ;
  describe "Nulls" (fun () ->
      tp "insert start of null" aNull (insert 'c' 0) ("cnull", 1) ;
      tp "del start of null" aNull (del 0) ("ull", 0) ;
      t "bs start of null" aNull (bs 0) ("null", 0) ;
      tp "insert end of null" aNull (insert '0' 4) ("null0", 5) ;
      t "del end of null" aNull (del 4) ("null", 4) ;
      tp "bs end of null" aNull (bs 4) ("nul", 3) ;
      tp "insert middle of null" aNull (insert '0' 2) ("nu0ll", 3) ;
      tp "del middle of null" aNull (del 2) ("nul", 2) ;
      tp "bs middle of null" aNull (bs 2) ("nll", 1) ;
      () ) ;
  describe "Blanks" (fun () ->
      t "insert middle of blank->string" (b ()) (insert '"' 3) ("\"\"", 1) ;
      t "del middle of blank->blank" (b ()) (del 3) (blank, 3) ;
      t "bs middle of blank->blank" (b ()) (bs 3) (blank, 2) ;
      t "insert blank->string" (b ()) (insert '"' 0) ("\"\"", 1) ;
      t "del blank->string" emptyStr (del 0) (blank, 0) ;
      t "bs blank->string" emptyStr (bs 1) (blank, 0) ;
      t "insert blank->int" (b ()) (insert '5' 0) ("5", 1) ;
      t "insert blank->int" (b ()) (insert '0' 0) ("0", 1) ;
      t "del int->blank " five (del 0) (blank, 0) ;
      t "bs int->blank " five (bs 1) (blank, 0) ;
      t "insert end of blank->int" (b ()) (insert '5' 1) ("5", 1) ;
      tp "insert partial" (b ()) (insert 't' 0) ("t", 1) ;
      t
        "backspacing your way through a partial finishes"
        trueBool
        (presses [K.Backspace; K.Backspace; K.Backspace; K.Backspace; K.Left] 4)
        ("___", 0) ;
      t "insert blank->space" (b ()) (space 0) (blank, 0) ;
      () ) ;
  describe "Fields" (fun () ->
      t "insert middle of fieldname" aField (insert 'c' 5) ("obj.fcield", 6) ;
      t
        "cant insert invalid chars fieldname"
        aField
        (insert '$' 5)
        ("obj.field", 5) ;
      t "del middle of fieldname" aField (del 5) ("obj.feld", 5) ;
      t "del fieldname" aShortField (del 4) ("obj.***", 4) ;
      t "bs fieldname" aShortField (bs 5) ("obj.***", 4) ;
      t "insert end of fieldname" aField (insert 'c' 9) ("obj.fieldc", 10) ;
      tp "insert end of varname" aField (insert 'c' 3) ("objc.field", 4) ;
      t "insert start of fieldname" aField (insert 'c' 4) ("obj.cfield", 5) ;
      t "insert blank fieldname" aBlankField (insert 'c' 4) ("obj.c", 5) ;
      t "del fieldop with name" aShortField (del 3) ("obj", 3) ;
      t "bs fieldop with name" aShortField (bs 4) ("obj", 3) ;
      t "del fieldop with blank" aBlankField (del 3) ("obj", 3) ;
      t "bs fieldop with blank" aBlankField (bs 4) ("obj", 3) ;
      t "del fieldop in nested" aNestedField (del 3) ("obj.field2", 3) ;
      t "bs fieldop in nested" aNestedField (bs 4) ("obj.field2", 3) ;
      t "add dot after variable" aVar (insert '.' 8) ("variable.***", 9) ;
      t "add dot after partial " aPartialVar (insert '.' 3) ("request.***", 8) ;
      t "add dot after field" aField (insert '.' 9) ("obj.field.***", 10) ;
      t "insert space in blank " aBlankField (space 4) ("obj.***", 4) ;
      () ) ;
  describe "Functions" (fun () ->
      t
        "space on a sep goes to next arg"
        aFnCall
        (space 10)
        ("Int::add 5 _________", 11) ;
      tp "bs function renames" aFnCall (bs 8) ("Int::ad@ 5 _________", 7) ;
      tp
        "deleting a function renames"
        aFnCall
        (del 7)
        ("Int::ad@ 5 _________", 7) ;
      t
        "renaming a function maintains unaligned params in let scope"
        (EPartial
           (gid (), "Int::", EFnCall (gid (), "Int::add", [five; six], NoRail)))
        (presses [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        ("let b = 6\nInt::sqrt 5", 10) ;
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
        (presses [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        ("Int::sqrt a", 10) ;
      t
        "renaming a function doesn't maintain unaligned params if they're not set (blanks)"
        (EPartial
           (gid (), "Int::", EFnCall (gid (), "Int::add", [b (); b ()], NoRail)))
        (presses [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        ("Int::sqrt _________", 10) ;
      (* TODO: functions are not implemented fully. I deld bs and
       * del because we were switching to partials, but this isn't
       * implemented. Some tests we need:
         * myFunc arg1 arg2, 6 => Backspace => myFun arg1 arg2, with a ghost and a partial.
         * same with del *)
      tp
        "del on function with version"
        aFnCallWithVersion
        (del 11)
        ("DB::getAllv@ ___________________", 11) ;
      tp
        "bs on function with version"
        aFnCallWithVersion
        (bs 12)
        ("DB::getAllv@ ___________________", 11) ;
      tp
        "del on function with version in between the version and function name"
        aFnCallWithVersion
        (del 10)
        ("DB::getAll1@ ___________________", 10) ;
      tp
        "bs on function with version in between the version and function name"
        aFnCallWithVersion
        (bs 10)
        ("DB::getAlv1@ ___________________", 9) ;
      tp
        "del on function with version in function name"
        aFnCallWithVersion
        (del 7)
        ("DB::getllv1@ ___________________", 7) ;
      tp
        "bs on function with version in function name"
        aFnCallWithVersion
        (bs 8)
        ("DB::getllv1@ ___________________", 7) ;
      t
        "adding function with version goes to the right place"
        (b ())
        (presses [K.Letter 'd'; K.Letter 'b'; K.Enter] 0)
        ("DB::getAllv1 ___________________", 13) ;
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
        ("true\n|>___", 7) ;
      t
        "pressing bs to clear partial reverts for blank rhs"
        (EPartial (gid (), "|", EBinOp (gid (), "||", anInt, b (), NoRail)))
        (bs 7)
        ("12345", 5) ;
      t
        "pressing bs to clear partial reverts for blank rhs, check lhs pos goes to start"
        (EPartial (gid (), "|", EBinOp (gid (), "||", b (), b (), NoRail)))
        (bs 12)
        ("___", 0) ;
      t
        "pressing del to clear partial reverts for blank rhs"
        (EPartial (gid (), "|", EBinOp (gid (), "||", anInt, b (), NoRail)))
        (del 6)
        ("12345", 5) ;
      t
        "pressing del to clear partial reverts for blank rhs, check lhs pos goes to start"
        (EPartial (gid (), "|", EBinOp (gid (), "||", b (), b (), NoRail)))
        (del 11)
        ("___", 0) ;
      t
        "using bs to remove an infix with a placeholder goes to right place"
        (EPartial (gid (), "|", EBinOp (gid (), "||", b (), b (), NoRail)))
        (bs 12)
        ("___", 0) ;
      t
        "using bs to remove an infix with a placeholder goes to right place 2"
        (EPartial (gid (), "|", EBinOp (gid (), "||", five, b (), NoRail)))
        (bs 3)
        ("5", 1) ;
      t
        "pressing bs to clear rightpartial reverts for blank rhs"
        (ERightPartial (gid (), "|", b ()))
        (bs 5)
        ("___", 0) ;
      t
        "pressing bs on single digit binop leaves lhs"
        (EBinOp (gid (), "+", anInt, anInt, NoRail))
        (bs 7)
        ("12345", 5) ;
      t
        "using del to remove an infix with a placeholder goes to right place"
        (EPartial (gid (), "|", EBinOp (gid (), "||", b (), b (), NoRail)))
        (del 11)
        ("___", 0) ;
      t
        "pressing del to clear rightpartial reverts for blank rhs"
        (ERightPartial (gid (), "|", b ()))
        (del 4)
        ("___", 0) ;
      t
        "pressing del on single digit binop leaves lhs"
        (EBinOp (gid (), "+", anInt, anInt, NoRail))
        (del 6)
        ("12345", 5) ;
      t
        "pressing letters and numbers on a partial completes it"
        (b ())
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
        (EIf (gid (), b (), b (), b ()))
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
      tp "show ghost partial" aFullBinOp (bs 8) ("myvar |@ 5", 7) ;
      (* TODO bs on empty partial does something *)
      (* TODO support del on all the bs commands *)
      (* TODO pressing enter at the end of the partialGhost *)
      () ) ;
  describe "Constructors" (fun () ->
      tp
        "arguments work in constructors"
        aConstructor
        (insert 't' 5)
        ("Just t", 6) ;
      t
        "int arguments work in constructors"
        aConstructor
        (insert '5' 5)
        ("Just 5", 6) ;
      tp
        "bs on a constructor converts it to a partial with ghost"
        aConstructor
        (bs 4)
        ("Jus@ ___", 3) ;
      tp
        "del on a constructor converts it to a partial with ghost"
        aConstructor
        (del 0)
        ("ust@ ___", 0) ;
      t
        "space on a constructor blank does nothing"
        aConstructor
        (space 5)
        ("Just ___", 5) ;
      (* TODO: test renaming constructors.
       * It's not too useful yet because there's only 4 constructors and,
       * hence, unlikely that anyone will rename them this way.
       * Also, the names of the temporary variables used to store the old arguments of a changed
       * constructor are randomly generated and would be hard to test *)
      () ) ;
  describe "Lambdas" (fun () ->
      t "bs over lambda symbol" aLambda (bs 1) ("___", 0) ;
      t "insert space in lambda" aLambda (press K.Space 1) ("\\*** -> ___", 1) ;
      t "bs non-empty lambda symbol" nonEmptyLambda (bs 1) ("\\*** -> 5", 1) ;
      t "del lambda symbol" aLambda (del 0) ("___", 0) ;
      t "del non-empty lambda symbol" nonEmptyLambda (del 0) ("\\*** -> 5", 0) ;
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
      t
        "creating lambda in block placeholder should set arguments"
        aFnCallWithBlockArg
        (press (K.Letter '\\') 23)
        ("Dict::map ____________ \\key, value -> ___", 24) ;
      t
        "creating lambda in block placeholder should set arguments when wrapping expression is inside thread"
        (EThread (gid (), [b (); b ()]))
        (presses
           (* we have to insert the function with completion here
            * so the arguments are adjusted based on the thread *)
           [K.Letter 'm'; K.Letter 'a'; K.Letter 'p'; K.Enter; K.Letter '\\']
           6)
        ("___\n|>Dict::map \\key, value -> ___", 17) ;
      t
        "deleting a lambda argument should work"
        lambdaWithTwoBindings
        (del 2)
        ("\\x -> ___", 2) ;
      t
        "backspacing a lambda argument should work"
        lambdaWithTwoBindings
        (bs 3)
        ("\\x -> ___", 2) ;
      t
        "deleting a lambda argument should update used variable"
        (lambdaWithUsed2ndBinding "x")
        (del 8)
        ("\\somevar -> ___", 8) ;
      t
        "can add lambda arguments when blank"
        aLambda
        (insert ',' 4)
        ("\\***, *** -> ___", 6) ;
      t
        "can add lambda arguments to used binding"
        lambdaWithTwoBindings
        (insert ',' 5)
        ("\\x, y, *** -> ___", 7) ;
      t
        "can add lambda arguments in middle used binding"
        lambdaWithTwoBindings
        (insert ',' 2)
        ("\\x, ***, y -> ___", 4) ;
      t
        "can add lambda arguments in the front"
        lambdaWithTwoBindings
        (insert ',' 1)
        ("\\***, x, y -> ___", 1) ;
      t
        "can add lambda arguments in front of middle"
        lambdaWithTwoBindings
        (insert ',' 4)
        ("\\x, ***, y -> ___", 4) ;
      t
        "cant insert a blank from outside the lambda"
        lambdaWithTwoBindings
        (insert ',' 0)
        ("\\x, y -> ___", 0) ;
      t
        "cant bs a blank from the space in a lambda"
        lambdaWithTwoBindings
        (bs 4)
        ("\\x, y -> ___", 3) ;
      () ) ;
  describe "Variables" (fun () ->
      tp "insert middle of variable" aVar (insert 'c' 5) ("variacble", 6) ;
      tp "del middle of variable" aVar (del 5) ("variale", 5) ;
      tp "insert capital works" aVar (press (K.Letter 'A') 5) ("variaAble", 6) ;
      t "can't insert invalid" aVar (press K.Dollar 5) ("variable", 5) ;
      t "del variable" aShortVar (del 0) (blank, 0) ;
      tp "del long variable" aVar (del 0) ("ariable", 0) ;
      tp "del mid variable" aVar (del 6) ("variabe", 6) ;
      t "bs variable" aShortVar (bs 1) (blank, 0) ;
      tp "bs mid variable" aVar (bs 8) ("variabl", 7) ;
      tp "bs mid variable" aVar (bs 6) ("variale", 5) ;
      t
        "variable doesn't override if"
        (ELet (gid (), gid (), "i", b (), EPartial (gid (), "i", b ())))
        (presses [K.Letter 'f'; K.Enter] 13)
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
        (press K.GoToStartOfLine 15)
        ("match ___\n  *** -> ___", 12) ;
      t
        "move to the end of match on line 2"
        emptyMatch
        (press K.GoToEndOfLine 12)
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
      t "bs over empty match" emptyMatch (bs 6) ("___", 0) ;
      t
        "bs over empty match with 2 patterns"
        emptyMatchWithTwoPatterns
        (bs 6)
        ("___", 0) ;
      t
        "bs over match with 2 patterns"
        matchWithPatterns
        (bs 6)
        ("match ___\n  3 -> ___", 6) ;
      t "del over empty match" emptyMatch (del 0) ("___", 0) ;
      t
        "del over empty match with 2 patterns"
        emptyMatchWithTwoPatterns
        (del 0)
        ("___", 0) ;
      t
        "del over match with 2 patterns"
        matchWithPatterns
        (del 0)
        ("match ___\n  3 -> ___", 0) ;
      t
        "del constructor in match pattern"
        matchWithConstructorPattern
        (del 12)
        ("match ___\n  ust -> ___", 12) ;
      t
        "bs constructor in match pattern"
        matchWithConstructorPattern
        (bs 16)
        ("match ___\n  Jus -> ___", 15) ;
      t
        "insert changes occurence of non-shadowed var in case"
        (matchWithBinding "binding" (EVariable (gid (), "binding")))
        (insert 'c' 19)
        ("match ___\n  bindingc -> bindingc", 20) ;
      t
        "insert changes occurence of non-shadowed var in case constructor"
        (matchWithConstructorBinding "binding" (EVariable (gid (), "binding")))
        (insert 'c' 22)
        ("match ___\n  Ok bindingc -> bindingc", 23) ;
      t
        "insert space in blank match"
        emptyMatch
        (press K.Space 6)
        ("match ___\n  *** -> ___", 6) ;
      t
        "insert space in blank match on line 2"
        emptyMatch
        (press K.Space 12)
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
      t "bs over empty let" emptyLet (bs 3) ("5", 0) ;
      t "del empty let" emptyLet (del 0) ("5", 0) ;
      t "bs over non-empty let" nonEmptyLet (bs 3) ("let *** = 6\n5", 3) ;
      t "del non-empty let" nonEmptyLet (del 0) ("let *** = 6\n5", 0) ;
      t
        "insert space on blank let"
        emptyLet
        (press K.Space 4)
        ("let *** = ___\n5", 4) ;
      t "lhs on empty" emptyLet (insert 'c' 4) ("let c = ___\n5", 5) ;
      t "middle of blank" emptyLet (insert 'c' 5) ("let c = ___\n5", 5) ;
      t "bs letlhs" letWithLhs (bs 5) ("let *** = 6\n5", 4) ;
      t "del letlhs" letWithLhs (del 4) ("let *** = 6\n5", 4) ;
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
        "bs changes occurence of binding var"
        (letWithUsedBinding "binding")
        (bs 11)
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
              , b ()
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
      let emptyThread = EThread (gid (), [b (); b ()]) in
      let aLongThread =
        threadOn
          emptyList
          [ listFn [aListNum 2]
          ; listFn [aListNum 3]
          ; listFn [aListNum 4]
          ; listFn [aListNum 5] ]
      in
      let aThreadInsideIf = EIf (gid (), b (), aLongThread, b ()) in
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
        (press K.GoToStartOfLine 8)
        ("[]\n|>List::append [5]\n|>List::append [5]", 5) ;
      t
        "move to the end of thread on line 2"
        aThread
        (press K.GoToEndOfLine 5)
        ("[]\n|>List::append [5]\n|>List::append [5]", 21) ;
      t
        "move to the front of thread on line 3"
        aThread
        (press K.GoToStartOfLine 40)
        ("[]\n|>List::append [5]\n|>List::append [5]", 24) ;
      t
        "move to the end of thread on line 3"
        aThread
        (press K.GoToEndOfLine 24)
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
        (bs 5)
        ("[]\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]", 2) ;
      t
        "deleting a thread's first pipe works"
        aLongThread
        (del 3)
        ("[]\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]", 3) ;
      t
        "backspacing a thread's second pipe works"
        aLongThread
        (bs 24)
        ("[]\n|>List::append [2]\n|>List::append [4]\n|>List::append [5]", 21) ;
      t
        "deleting a thread's second pipe works"
        aLongThread
        (del 22)
        ("[]\n|>List::append [2]\n|>List::append [4]\n|>List::append [5]", 22) ;
      t
        "backspacing a thread's third pipe works"
        aLongThread
        (bs 43)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [5]", 40) ;
      t
        "deleting a thread's third pipe works"
        aLongThread
        (del 41)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [5]", 41) ;
      t
        "backspacing a thread's last pipe works"
        aLongThread
        (bs 62)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]", 59) ;
      t
        "deleting a thread's last pipe works"
        aLongThread
        (del 60)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]", 59) ;
      t
        "backspacing a thread's first pipe that isn't in the first column works"
        aThreadInsideIf
        (bs 21)
        ( "if ___\nthen\n  []\n  |>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 16 ) ;
      t
        "deleting a thread's first pipe that isn't in the first column works"
        aThreadInsideIf
        (del 19)
        ( "if ___\nthen\n  []\n  |>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 19 ) ;
      t
        "backspacing a thread's second pipe that isn't in the first column works"
        aThreadInsideIf
        (bs 42)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 37 ) ;
      t
        "deleting a thread's second pipe that isn't in the first column works"
        aThreadInsideIf
        (del 40)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 40 ) ;
      t
        "backspacing a thread's third pipe that isn't in the first column works"
        aThreadInsideIf
        (bs 63)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [5]\nelse\n  ___"
        , 58 ) ;
      t
        "deleting a thread's third pipe that isn't in the first column works"
        aThreadInsideIf
        (del 61)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [5]\nelse\n  ___"
        , 61 ) ;
      t
        "backspacing a thread's fourth pipe that isn't in the first column works"
        aThreadInsideIf
        (bs 84)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]\nelse\n  ___"
        , 79 ) ;
      t
        "deleting a thread's fourth pipe that isn't in the first column works"
        aThreadInsideIf
        (del 82)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]\nelse\n  ___"
        , 79 ) ;
      t
        "adding infix functions adds the right number of blanks"
        emptyThread
        (presses [K.Plus; K.Enter] 6)
        ("___\n|>+ _________", 8) ;
      (* TODO: test for prefix fns *)
      (* TODO: test for deleting threaded infix fns *)
      (* TODO: test for deleting threaded prefix fns *)
      () ) ;
  describe "Ifs" (fun () ->
      t
        "move over indent 1"
        plainIf
        (press K.Left 12)
        ("if 5\nthen\n  6\nelse\n  7", 9) ;
      t
        "move over indent 2"
        plainIf
        (press K.Left 21)
        ("if 5\nthen\n  6\nelse\n  7", 18) ;
      t "bs over indent 1" plainIf (bs 12) ("if 5\nthen\n  6\nelse\n  7", 9) ;
      t "bs over indent 2" plainIf (bs 21) ("if 5\nthen\n  6\nelse\n  7", 18) ;
      t "bs over empty if" emptyIf (bs 2) ("___", 0) ;
      t
        "move to front of line 1"
        plainIf
        (press K.GoToStartOfLine 4)
        ("if 5\nthen\n  6\nelse\n  7", 0) ;
      t
        "move to end of line 1"
        plainIf
        (press K.GoToEndOfLine 0)
        ("if 5\nthen\n  6\nelse\n  7", 4) ;
      t
        "move to front of line 3"
        plainIf
        (press K.GoToStartOfLine 13)
        ("if 5\nthen\n  6\nelse\n  7", 12) ;
      t
        "move to end of line 3"
        plainIf
        (press K.GoToEndOfLine 12)
        ("if 5\nthen\n  6\nelse\n  7", 13) ;
      t
        "move to front of line 5 in nested if"
        nestedIf
        (press K.GoToStartOfLine 16)
        ("if 5\nthen\n  if 5\n  then\n    6\n  else\n    7\nelse\n  7", 12) ;
      t
        "move to end of line 5 in nested if"
        nestedIf
        (press K.GoToEndOfLine 12)
        ("if 5\nthen\n  if 5\n  then\n    6\n  else\n    7\nelse\n  7", 16) ;
      t
        "try to insert space on blank"
        emptyIf
        (press K.Space 3)
        ("if ___\nthen\n  ___\nelse\n  ___", 3) ;
      t
        "try to insert space on blank indent 2"
        emptyIf
        (press K.Space 14)
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
        EList (gid (), [fiftySix; seventyEight; b (); fiftySix])
      in
      let multiWithStrs =
        EList
          ( gid ()
          , [ EString (gid (), "ab")
            ; EString (gid (), "cd")
            ; EString (gid (), "ef") ] )
      in
      t "create list" (b ()) (press K.LeftSquareBracket 0) ("[]", 1) ;
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
        "backspacing open bracket of empty list dels list"
        emptyList
        (bs 1)
        (blank, 0) ;
      t
        "backspacing close bracket of empty list moves inside list"
        emptyList
        (bs 2)
        ("[]", 1) ;
      t
        "deleting open bracket of empty list dels list"
        emptyList
        (del 0)
        (blank, 0) ;
      t
        "close bracket at end of list is swallowed"
        emptyList
        (press K.RightSquareBracket 1)
        ("[]", 2) ;
      t
        "bs on first separator between items dels item after separator"
        multi
        (bs 4)
        ("[56]", 3) ;
      t
        "del before first separator between items dels item after separator"
        multi
        (del 3)
        ("[56]", 3) ;
      t
        "bs on middle separator between items dels item after separator"
        longList
        (bs 10)
        ("[56,78,56,56,78]", 9) ;
      t
        "del before middle separator between items dels item after separator"
        longList
        (del 9)
        ("[56,78,56,56,78]", 9) ;
      t
        "bs on middle separator between items dels blank after separator"
        listWithBlank
        (bs 7)
        ("[56,78,56]", 6) ;
      t
        "del before middle separator between items dels blank after separator"
        listWithBlank
        (del 6)
        ("[56,78,56]", 6) ;
      t
        "bs on last separator between a blank and item dels item after separator"
        listWithBlank
        (bs 11)
        ("[56,78,___]", 10) ;
      t
        "del before last separator between a blank and item dels item after separator"
        listWithBlank
        (del 10)
        ("[56,78,___]", 10) ;
      t
        "bs on separator between string items dels item after separator"
        multiWithStrs
        (bs 6)
        ("[\"ab\",\"ef\"]", 5) ;
      t
        "del before separator between string items dels item after separator"
        multiWithStrs
        (del 5)
        ("[\"ab\",\"ef\"]", 5) ;
      () ) ;
  describe "Record" (fun () ->
      let emptyRecord = ERecord (gid (), []) in
      let emptyRow = ERecord (gid (), [(gid (), "", b ())]) in
      let single = ERecord (gid (), [(gid (), "f1", fiftySix)]) in
      let multi =
        ERecord
          (gid (), [(gid (), "f1", fiftySix); (gid (), "f2", seventyEight)])
      in
      (* let withStr = EList (gid (), [EString (gid (), "ab")]) in *)
      t "create record" (b ()) (press K.LeftCurlyBrace 0) ("{}", 1) ;
      t
        "inserting before the record does nothing"
        emptyRecord
        (insert '5' 0)
        ("{}", 0) ;
      t
        "inserting space between empty record does nothing"
        emptyRecord
        (space 1)
        ("{}", 1) ;
      t
        "inserting space in empty record field does nothing"
        emptyRow
        (space 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "inserting space in empty record value does nothing"
        emptyRow
        (space 10)
        ("{\n  *** : ___\n}", 10) ;
      t
        "pressing enter in an empty record adds a new line"
        emptyRecord
        (enter 1)
        ("{\n  *** : ___\n}", 4) ;
      t "enter fieldname" emptyRow (insert 'c' 4) ("{\n  c : ___\n}", 5) ;
      t
        "move to the front of an empty record"
        emptyRow
        (press K.GoToStartOfLine 13)
        ("{\n  *** : ___\n}", 4) ;
      t
        "move to the end of an empty record"
        emptyRow
        (press K.GoToEndOfLine 4)
        ("{\n  *** : ___\n}", 13) ;
      t
        "cant enter invalid fieldname"
        emptyRow
        (insert '^' 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "backspacing open brace of empty record dels record"
        emptyRecord
        (bs 1)
        (blank, 0) ;
      t
        "backspacing close brace of empty record moves inside record"
        emptyRecord
        (bs 2)
        ("{}", 1) ;
      t
        "deleting open brace of empty record dels record"
        emptyRecord
        (del 0)
        (blank, 0) ;
      t
        "close brace at end of record is swallowed"
        emptyRecord
        (press K.RightCurlyBrace 1)
        ("{}", 2) ;
      t
        "backspacing empty record field clears entry"
        emptyRow
        (bs 4)
        (* TODO: buggy. Should be 1 *)
        ("{}", 3) ;
      t
        "appending to int in expr works"
        single
        (insert '1' 11)
        ("{\n  f1 : 561\n}", 12) ;
      t
        "appending to int in expr works"
        multi
        (insert '1' 21)
        ("{\n  f1 : 56\n  f2 : 781\n}", 22) ;
      t
        "move to the front of a record with multiple values"
        multi
        (press K.GoToStartOfLine 21)
        ("{\n  f1 : 56\n  f2 : 78\n}", 14) ;
      t
        "move to the end of a record with multiple values"
        multi
        (press K.GoToEndOfLine 14)
        ("{\n  f1 : 56\n  f2 : 78\n}", 21) ;
      t
        "inserting at the end of the key works"
        emptyRow
        (insert 'f' 6)
        ("{\n  f : ___\n}", 5) ;
      t
        "pressing enter at start adds a row"
        multi
        (enter 1)
        ("{\n  *** : ___\n  f1 : 56\n  f2 : 78\n}", 4) ;
      t
        "pressing enter at the back adds a row"
        multi
        (enter 22)
        ("{\n  f1 : 56\n  f2 : 78\n  *** : ___\n}", 24) ;
      t
        "pressing enter at the start of a field adds a row"
        multi
        (enter 14)
        ("{\n  f1 : 56\n  *** : ___\n  f2 : 78\n}", 14) ;
      t
        "pressing enter at the end of row adds a row"
        multi
        (enter 11)
        ("{\n  f1 : 56\n  *** : ___\n  f2 : 78\n}", 14) ;
      t
        "dont allow weird chars in recordFields"
        emptyRow
        (press K.RightParens 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "dont jump in recordFields with infix chars"
        emptyRow
        (press K.Plus 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "dont jump in recordFields with infix chars, pt 2"
        single
        (press K.Plus 6)
        ("{\n  f1 : 56\n}", 6) ;
      t
        "colon should skip over the record colon"
        emptyRow
        (press K.Colon 7)
        ("{\n  *** : ___\n}", 10) ;
      t
        "dont allow key to start with a number"
        emptyRow
        (insert '5' 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "dont allow key to start with a number, pt 2"
        single
        (insert '5' 4)
        ("{\n  f1 : 56\n}", 4) ;
      t
        "dont allow key to start with a number, pt 3"
        emptyRow
        (insert '5' 6)
        ("{\n  *** : ___\n}", 6) ;
      () ) ;
  describe "Autocomplete" (fun () ->
      t
        "space autocompletes correctly"
        (EPartial (gid (), "if", b ()))
        (space 2)
        ("if ___\nthen\n  ___\nelse\n  ___", 3) ;
      t
        "let moves to right place"
        (EPartial (gid (), "let", b ()))
        (enter 3)
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
        (b ())
        (presses [K.Equals; K.Enter] 0)
        ("_________ == _________", 0) ;
      t
        "autocomplete tab on bin-op moves to start of second blank"
        (b ())
        (presses [K.Equals; K.Tab] 0)
        ("_________ == _________", 13) ;
      t
        "autocomplete space on bin-op moves to start of first blank"
        (b ())
        (presses [K.Equals; K.Space] 0)
        ("_________ == _________", 0) ;
      t
        "variable moves to right place"
        (EPartial (gid (), "req", b ()))
        (enter 3)
        ("request", 7) ;
      t
        "thread moves to right place on blank"
        (b ())
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 2)
        ("___\n|>___", 6) ;
      t
        "thread moves to right place on placeholder"
        aFnCall
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 11)
        ("Int::add 5 ___\n           |>___", 28) ;
      t
        "thread moves to right place in if then"
        emptyIf
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 14)
        ("if ___\nthen\n  ___\n  |>___\nelse\n  ___", 22) ;
      t
        "thread moves to right place in lambda body"
        aLambda
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 8)
        ("\\*** -> ___\n        |>___", 22) ;
      t
        "thread moves to right place in match body"
        emptyMatch
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 19)
        ("match ___\n  *** -> ___\n         |>___", 34) ;
      t
        "autocomplete for Just"
        (EPartial (gid (), "Just", b ()))
        (enter 4)
        ("Just ___", 5) ;
      t
        "autocomplete for Ok"
        (EPartial (gid (), "Ok", b ()))
        (enter 2)
        ("Ok ___", 3) ;
      t
        "autocomplete for Nothing"
        (EPartial (gid (), "Nothing", b ()))
        (enter 7)
        ("Nothing", 7) ;
      t
        "autocomplete for Nothing at end of a line"
        (EIf (gid (), b (), EPartial (gid (), "Nothing", b ()), b ()))
        (space 21)
        ("if ___\nthen\n  Nothing\nelse\n  ___", 21) ;
      t
        "autocomplete for Error"
        (EPartial (gid (), "Error", b ()))
        (enter 5)
        ("Error ___", 6) ;
      t
        "autocomplete for field"
        (EFieldAccess (gid (), EVariable (ID "12", "request"), gid (), "bo"))
        (enter 10)
        ("request.body", 12) ;
      (* test "backspacing on variable reopens autocomplete" (fun () -> *)
      (*     expect (bs (EVariable (5, "request"))). *)
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
        (press K.Right 12)
        ("if ___\nthen\n  ___\nelse\n  ___", 17) ;
      t
        "left skips over indent when in indent"
        emptyIf
        (press K.Left 13)
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
        (press K.Enter 11)
        ("let *** = 6\nlet *** = ___\n5", 16) ;
      t
        "enter at the end of a line goes to first non-whitespace token"
        indentedIfElse
        (enter 16)
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
        (presses [K.Escape; K.Up] 17)
        ("if ___\nthen\n  ___\nelse\n  ___", 11) ;
      t
        "end of if-then blank goes up properly, twice"
        emptyIf
        (presses [K.Escape; K.Up; K.Up] 17)
        ("if ___\nthen\n  ___\nelse\n  ___", 5) ;
      t
        "end of if-then blank goes down properly"
        emptyIf
        (presses [K.Escape; K.Down] 5)
        ("if ___\nthen\n  ___\nelse\n  ___", 11) ;
      t
        "end of if-then blank goes down properly, twice"
        emptyIf
        (presses [K.Escape; K.Down; K.Down] 5)
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
                 (gid (), gid (), "var", EPartial (gid (), "false", b ()), b ())
             in
             moveTo 14 s
             |> (fun s ->
                  let h = h ast in
                  let m = {m with handlers = Handlers.fromList [h]} in
                  updateAutocomplete m h.hTLID ast s )
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
        (ELet (gid (), gid (), "x", EPartial (gid (), "Int::add", b ()), b ()))
        (press K.Right 16)
        ("let x = Int::add _________ _________\n___", 17) ;
      tp
        "pressing an infix which could be valid doesn't commit"
        (b ())
        (presses [K.Pipe; K.Pipe] 0)
        ("||", 2) ;
      tp
        "pressing an infix after true commits it "
        (EPartial (gid (), "true", b ()))
        (press K.Plus 4)
        ("true +", 6) ;
      t
        "moving left off a function autocompletes it anyway"
        (ELet (gid (), gid (), "x", EPartial (gid (), "Int::add", b ()), b ()))
        (press K.Left 8)
        ("let x = Int::add _________ _________\n___", 7) ;
      test "escape hides autocomplete" (fun () ->
          expect
            (let ast = b () in
             moveTo 0 s
             |> (fun s -> updateKey (K.Letter 'r') ast s)
             |> (fun (ast, s) -> updateKey K.Escape ast s)
             |> fun (_, s) -> s.ac.index)
          |> toEqual None ) ;
      test "right/left brings back autocomplete" (fun () ->
          expect
            (let ast = b () in
             moveTo 0 s
             |> (fun s -> updateKey (K.Letter 'r') ast s)
             |> (fun (ast, s) -> updateKey K.Escape ast s)
             |> fun (_, s) -> s.ac.index)
          |> toEqual None ) ;
      () ) ;
  describe "Neighbours" (fun () ->
      test "with empty AST, have left neighbour" (fun () ->
          let id = ID "543" in
          expect
            (let ast = EString (id, "test") in
             let tokens = toTokens m.fluidState ast in
             Fluid.getNeighbours ~pos:3 tokens)
          |> toEqual
               (let token = TString (id, "test") in
                let ti =
                  { token
                  ; startRow = 0
                  ; startCol = 0
                  ; startPos = 0
                  ; endPos = 6
                  ; length = 6 }
                in
                (L (token, ti), R (token, ti), None)) ) ;
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
        (tab 10)
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
        (presses [K.Letter 'i'; K.Letter 'f'; K.ShiftTab] 14)
        ("let *** = ___\nif ___\nthen\n  ___\nelse\n  ___", 10) ;
      t
        "shift-tab goes when on blank"
        completelyEmptyLet
        (shiftTab 14)
        ("let *** = ___\n___", 10) ;
      t
        "shift tab wraps from start of let"
        emptyLet
        (shiftTab 4)
        ("let *** = ___\n5", 10) ;
      t "cant tab to filled letLHS" letWithLhs (tab 0) ("let n = 6\n5", 0) ;
      t "can tab to lambda blank" aLambda (tab 0) ("\\*** -> ___", 1) ;
      t "can shift tab to field blank" aBlankField (shiftTab 0) ("obj.***", 4) ;
      () ) ;
  ()
