open Jest
open Expect
open Tc
open Types
open Prelude
open Fluid
open Fluid_test_data
module Regex = Util.Regex
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
 *      would stay in the last place, giving a false pass. To avoid this,
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
        , b
        , EFnCall
            (gid (), "Http::Forbidden", [EInteger (gid (), "403")], NoRail) )
    , EFnCall (gid (), "Http::Forbidden", [], NoRail) )


let deOption msg v = match v with Some v -> v | None -> Debug.crash msg

type testResult = (string * (int option * int)) * bool

type shiftState =
  | ShiftHeld
  | ShiftNotHeld

let () =
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
            (LoadableSuccess
               (StrDict.singleton
                  ~key:"12"
                  ~value:
                    (DObj
                       (StrDict.fromList [("body", DNull); ("formBody", DNull)]))))
    ; builtInFunctions =
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
        ; { fnName = "HttpClient::post_v4"
          ; fnParameters =
              [ fnParam "url" TStr false
              ; fnParam "body" TAny false
              ; fnParam "query" TObj false
              ; fnParam "headers" TObj false ]
          ; fnReturnTipe = TResult
          ; fnDescription = "Make blocking HTTP POST call to `uri`."
          ; fnPreviewExecutionSafe = false
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
  let processMsg
      (keys : (K.key * shiftState) list) (s : fluidState) (ast : ast) :
      ast * fluidState =
    let h = Fluid_utils.h ast in
    let m = {m with handlers = Handlers.fromList [h]} in
    List.foldl keys ~init:(ast, s) ~f:(fun (key, shiftHeld) (ast, s) ->
        updateMsg
          m
          h.hTLID
          ast
          (FluidKeyPress
             { key
             ; shiftKey = shiftHeld = ShiftHeld
             ; altKey = false
             ; metaKey = false
             ; ctrlKey = false })
          s )
  in
  let process
      ~(debug : bool)
      ~(clone : bool)
      ~(wrap : bool)
      (keys : (K.key * shiftState) list)
      (selectionStart : int option)
      (pos : int)
      (ast : ast) : testResult =
    let s = {Defaults.defaultFluidState with ac = AC.reset m} in
    let ast = if clone then Fluid.clone ~state:s ast else ast in
    let newlinesBefore (pos : int) =
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
    let ast =
      if wrap
      then EIf (gid (), EBool (gid (), true), ast, EInteger (gid (), "5"))
      else ast
    in
    (* See the "Wrap" block comment at the top of the file for an explanation of this *)
    let wrapperOffset = 15 in
    let addWrapper pos =
      if wrap then pos + wrapperOffset + (newlinesBefore pos * 2) else pos
    in
    let pos = addWrapper pos in
    let selectionStart = Option.map selectionStart ~f:addWrapper in
    let s = {s with oldPos = pos; newPos = pos; selectionStart} in
    if debug
    then (
      Js.log2 "state before " (Fluid_utils.debugState s) ;
      Js.log2 "expr before" (eToStructure s ast) ) ;
    let newAST, newState = processMsg keys s ast in
    let result =
      match newAST with
      | EIf (_, _, expr, _) when wrap ->
          expr
      | expr when not wrap ->
          expr
      | expr ->
          Debug.crash ("the wrapper is broken: " ^ eToString s expr)
    in
    let removeWrapperFromCaretPos (p : int) : int =
      let endPos = ref (p - wrapperOffset) in
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
      let last =
        toTokens newState result
        |> List.last
        |> deOption "last"
        |> fun x -> x.endPos
      in
      (* even though the wrapper allows tests to go past the start and end, it's
          * weird to test for *)
      max 0 (min last !endPos)
    in
    let finalPos =
      if wrap
      then removeWrapperFromCaretPos newState.newPos
      else newState.newPos
    in
    let selPos =
      Option.map newState.selectionStart ~f:removeWrapperFromCaretPos
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
      Js.log2 "state after" (Fluid_utils.debugState newState) ;
      Js.log2 "expr after" (eToStructure newState result) ) ;
    ((eToString s result, (selPos, finalPos)), partialsFound)
  in
  let render (expr : fluidExpr) : testResult =
    process ~wrap:true ~clone:false ~debug:false [] None 0 expr
  in
  let del
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~wrap ~clone ~debug [(K.Delete, ShiftNotHeld)] None pos expr
  in
  let bs
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~wrap ~clone ~debug [(K.Backspace, ShiftNotHeld)] None pos expr
  in
  let tab
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~wrap ~clone ~debug [(K.Tab, ShiftNotHeld)] None pos expr
  in
  let shiftTab
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~wrap ~clone ~debug [(K.ShiftTab, ShiftNotHeld)] None pos expr
  in
  let space
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~wrap ~clone ~debug [(K.Space, ShiftNotHeld)] None pos expr
  in
  let enter
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~wrap ~clone ~debug [(K.Enter, ShiftNotHeld)] None pos expr
  in
  let press
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (key : K.key)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~wrap ~clone ~debug [(key, ShiftNotHeld)] None pos expr
  in
  let selectionPress
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (key : K.key)
      (selectionStart : int)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [(key, ShiftNotHeld)]
      (Some selectionStart)
      pos
      expr
  in
  let presses
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (keys : K.key list)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~debug
      ~clone
      (List.map ~f:(fun key -> (key, ShiftNotHeld)) keys)
      None
      pos
      expr
  in
  let modPresses
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (keys : (K.key * shiftState) list)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~wrap ~clone ~debug keys None pos expr
  in
  let insert
      ?(debug = false)
      ?(wrap = true)
      ?(clone = true)
      (char : char)
      (pos : int)
      (expr : fluidExpr) : testResult =
    let key = K.fromChar char in
    process ~wrap ~debug ~clone [(key, ShiftNotHeld)] None pos expr
  in
  let blank = "___" in
  (* Test expecting no partials found and an expected caret position but no selection *)
  let t
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      ((expectedStr, expectedPos) : string * int) =
    let insertCursor
        (((str, (selection, cursor)), res) :
          (string * (int option * int)) * bool) :
        (string * (int option * int)) * bool =
      let cursorString = "~" in
      match str |> String.splitAt ~index:cursor with a, b ->
        (([a; b] |> String.join ~sep:cursorString, (selection, cursor)), res)
    in
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () ->
        expect (fn initial |> insertCursor)
        |> toEqual (insertCursor ((expectedStr, (None, expectedPos)), false))
        )
  in
  let tc
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      (expectedStr : string) =
    let insertCursor
        (((str, (_selection, cursor)), res) :
          (string * (int option * int)) * bool) : string * bool =
      let cursorString = "~" in
      match str |> String.splitAt ~index:cursor with a, b ->
        ([a; b] |> String.join ~sep:cursorString, res)
    in
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () ->
        expect (fn initial |> insertCursor) |> toEqual (expectedStr, false) )
  in
  (* Test expecting partials found and an expected caret position but no selection *)
  let tp
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      ((expectedStr, expectedPos) : string * int) =
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () ->
        expect (fn initial)
        |> toEqual ((expectedStr, (None, expectedPos)), true) )
  in
  let tcp
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      (expectedStr : string) =
    let insertCursor
        (((str, (_selection, cursor)), res) :
          (string * (int option * int)) * bool) : string * bool =
      let cursorString = "~" in
      match str |> String.splitAt ~index:cursor with a, b ->
        ([a; b] |> String.join ~sep:cursorString, res)
    in
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () ->
        expect (fn initial |> insertCursor) |> toEqual (expectedStr, true) )
  in
  (* Test expecting no partials found and an expected resulting selection *)
  let ts
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      ((expectedString, (expectedSelStart, expectedPos)) :
        string * (int option * int)) =
    let expected = (expectedString, (expectedSelStart, expectedPos)) in
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () -> expect (fn initial) |> toEqual (expected, false))
  in
  describe "Strings" (fun () ->
      tc "insert mid string" aStr (insert 'c' 3) "\"soc~me string\"" ;
      tc "del mid string" aStr (del 3) "\"so~e string\"" ;
      tc "bs mid string" aStr (bs 4) "\"so~e string\"" ;
      tc "insert empty string" emptyStr (insert 'c' 1) "\"c~\"" ;
      tc "del empty string" emptyStr (del 1) "~___" ;
      tc "del empty string from outside" emptyStr (del 0) "~___" ;
      tc "bs empty string" emptyStr (bs 1) "~___" ;
      tc "bs outside empty string" emptyStr (bs 2) "\"~\"" ;
      tc "bs near-empty string" oneCharStr (bs 2) "\"~\"" ;
      tc "del near-empty string" oneCharStr (del 1) "\"~\"" ;
      tc "insert outside string" aStr (insert 'c' 0) "~\"some string\"" ;
      tc "del outside string" aStr (del 0) "~\"some string\"" ;
      tc "bs outside string" aStr (bs 0) "~\"some string\"" ;
      tc "insert start of string" aStr (insert 'c' 1) "\"c~some string\"" ;
      tc "del start of string" aStr (del 1) "\"~ome string\"" ;
      tc "bs start of string" aStr (bs 1) "~\"some string\"" ;
      tc "insert end of string" aStr (insert 'c' 12) "\"some stringc~\"" ;
      tc "del end of string" aStr (del 12) "\"some string~\"" ;
      tc "bs end of string" aStr (bs 12) "\"some strin~\"" ;
      tc "insert after end" aStr (insert 'c' 13) "\"some string\"~" ;
      tc "del after end of string" aStr (del 13) "\"some string\"~" ;
      tc "bs after end" aStr (bs 13) "\"some string~\"" ;
      tc "insert space in string" aStr (insert ' ' 3) "\"so ~me string\"" ;
      tc "del space in string" aStr (del 5) "\"some~string\"" ;
      tc "bs space in string" aStr (bs 6) "\"some~string\"" ;
      tc "final quote is swallowed" aStr (insert '"' 12) "\"some string\"~" ;
      () ) ;
  describe "Multi-line Strings" (fun () ->
      let nums = "123456789_" in
      let letters = "abcdefghi," in
      let segment = nums ^ letters ^ nums ^ letters in
      let mlStr = EString (gid (), segment ^ segment ^ nums) in
      let wrapIf e = EIf (gid (), e, newB (), newB ()) in
      tc
        "insert into start string"
        mlStr
        (insert 'c' 3)
        ( "\"12c~3456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      tc
        "insert into middle string"
        mlStr
        (insert 'c' 44 (* quote + 2 + newline *))
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12c~3456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      tc
        "insert into end string"
        mlStr
        (insert 'c' 85 (* quote + 2 + newline*2 *))
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12c~3456789_\"" ) ;
      tc
        "del mid start string"
        mlStr
        (del 3)
        ( "\"12~456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      tc
        "del mid middle string"
        mlStr
        (del 44)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12~456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      tc
        "del mid end string"
        mlStr
        (del 85)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12~456789_\"" ) ;
      tc
        "bs mid start string"
        mlStr
        (bs 4)
        ( "\"12~456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      tc
        "bs mid middle string"
        mlStr
        (bs 45)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12~456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      tc
        "bs mid end string"
        mlStr
        (bs 86)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12~456789_\"" ) ;
      tc
        "insert outside string"
        mlStr
        (insert 'c' 0)
        ( "~\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      tc
        "del outside string"
        mlStr
        (del 0)
        ( "~\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      tc
        "bs outside string"
        mlStr
        (bs 0)
        ( "~\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      tc
        "insert start of start string"
        mlStr
        (insert 'c' 1)
        ( "\"c~123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      tc
        "insert start of middle string"
        mlStr
        (insert 'c' 42)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "c~123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      tc
        "insert start of end string"
        mlStr
        (insert 'c' 83)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "c~123456789_\"" ) ;
      tc
        "del start of start string"
        mlStr
        (del 1)
        ( "\"~23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      tc
        "del start of middle string"
        mlStr
        (del 42)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "~23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      tc
        "del start of end string"
        mlStr
        (del 83)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "~23456789_\"" ) ;
      tc
        "bs start of start string"
        mlStr
        (bs 1)
        ( "~\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      tc
        "bs start of middle string"
        mlStr
        (bs 42)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      tc
        "bs start of end string"
        mlStr
        (bs 83)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ "123456789_\"" ) ;
      tc
        "insert end of start string"
        mlStr
        (insert 'c' 41)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\nc~"
        ^ "123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      tc
        "insert end of middle string"
        mlStr
        (insert 'c' 82)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\nc~"
        ^ "123456789_\"" ) ;
      tc
        "insert end of end string"
        mlStr
        (insert 'c' 93)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_c~\"" ) ;
      tc
        "string converts to ml string"
        (EString (gid (), segment))
        (insert 'c' 41)
        "\"123456789_abcdefghi,123456789_abcdefghi,\nc~\"" ;
      tc
        "indented string converts to ml string"
        (wrapIf (EString (gid (), segment)))
        (insert 'c' 44)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   c~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      tc
        "insert end of indented start string"
        (wrapIf (EString (gid (), segment ^ segment)))
        (insert 'c' 44)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   c~123456789_abcdefghi,123456789_abcdefghi\n"
        ^ "   ,\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      tc
        "insert end of indented end string"
        (wrapIf (EString (gid (), segment ^ segment)))
        (insert 'c' 88)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   c~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      tc
        "del end of start string"
        mlStr
        (del 41)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      tc
        "del end of middle string"
        mlStr
        (del 82)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ "123456789_\"" ) ;
      tc
        "del end of end string"
        mlStr
        (del 93)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_~\"" ) ;
      tc
        "bs end of start string"
        mlStr
        (bs 41)
        ( "\"123456789_abcdefghi,123456789_abcdefghi"
        ^ "~1\n23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      tc
        "bs end of middle string"
        mlStr
        (bs 82)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi"
        ^ "~1\n23456789_\"" ) ;
      tc
        "bs end of end string"
        mlStr
        (bs 93)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789~\"" ) ;
      tc
        "insert after end of end string"
        mlStr
        (insert 'c' 94)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"~" ) ;
      tc
        "del after end of end string"
        mlStr
        (del 94)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"~" ) ;
      tc
        "bs after end of end string"
        mlStr
        (bs 94)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_~\"" ) ;
      (* Skipped insert, del, bs of space, as it doesn't seem interesting *)
      tc
        "final quote is swallowed"
        mlStr
        (insert '"' 93)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"~" ) ;
      tc
        "bs, 3 lines to 2, end"
        (wrapIf (EString (gid (), segment ^ segment ^ "c")))
        (bs 93)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   123456789_abcdefghi,123456789_abcdefghi,~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      tc
        "bs, 2 lines to 1, end"
        (wrapIf (EString (gid (), segment ^ "c")))
        (bs 49)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      tc
        "del, 3 lines to 2, end"
        (wrapIf (EString (gid (), segment ^ segment ^ "c")))
        (del 92)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   123456789_abcdefghi,123456789_abcdefghi,~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      tc
        "del, 2 lines to 1, end"
        (wrapIf (EString (gid (), segment ^ "c")))
        (del 48)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      () ) ;
  describe "Integers" (fun () ->
      tc "insert 0 at front " anInt (insert '0' 0) "~12345" ;
      tc "insert at end of short" aShortInt (insert '2' 1) "12~" ;
      tc "insert not a number" anInt (insert 'c' 0) "~12345" ;
      tc "insert start of number" anInt (insert '5' 0) "5~12345" ;
      tc "del start of number" anInt (del 0) "~2345" ;
      tc "bs start of number" anInt (bs 0) "~12345" ;
      tc "insert end of number" anInt (insert '0' 5) "123450~" ;
      tc "del end of number" anInt (del 5) "12345~" ;
      tc "bs end of number" anInt (bs 5) "1234~" ;
      tc
        "insert number at scale"
        aHugeInt
        (insert '9' 5)
        "200009~0000000000000" ;
      tc "insert number at scale" aHugeInt (insert '9' 0) "9~20000000000000000" ;
      tc
        "insert number at scale"
        aHugeInt
        (insert '9' 19)
        "2000000000000000000~" ;
      (* let max62BitInt = EInteger (gid (), "4611686018427387903") in *)
      let oneShorterThanMax62BitInt =
        EInteger (gid (), "461168601842738790")
      in
      tc
        "insert number at scale"
        oneShorterThanMax62BitInt
        (insert '3' 18)
        "4611686018427387903~" ;
      tc
        "insert number at scale"
        oneShorterThanMax62BitInt
        (insert '4' 18)
        "461168601842738790~" ;
      () ) ;
  describe "Floats" (fun () ->
      tc "insert . converts to float - end" anInt (insert '.' 5) "12345.~" ;
      tc "insert . converts to float - middle" anInt (insert '.' 3) "123.~45" ;
      tc "insert . converts to float - start" anInt (insert '.' 0) "~12345" ;
      tc "insert . converts to float - short" aShortInt (insert '.' 1) "1.~" ;
      tc "continue after adding dot" aPartialFloat (insert '2' 2) "1.2~" ;
      tc "insert zero in whole - start" aFloat (insert '0' 0) "~123.456" ;
      tc "insert int in whole - start" aFloat (insert '9' 0) "9~123.456" ;
      tc "insert int in whole - middle" aFloat (insert '0' 1) "10~23.456" ;
      tc "insert int in whole - end" aFloat (insert '0' 3) "1230~.456" ;
      tc "insert int in fraction - start" aFloat (insert '0' 4) "123.0~456" ;
      tc "insert int in fraction - middle" aFloat (insert '0' 6) "123.450~6" ;
      tc "insert int in fraction - end" aFloat (insert '0' 7) "123.4560~" ;
      tc "insert non-int in whole" aFloat (insert 'c' 2) "12~3.456" ;
      tc "insert non-int in fraction" aFloat (insert 'c' 6) "123.45~6" ;
      tc "del dot" aFloat (del 3) "123~456" ;
      tc "del dot at scale" aHugeFloat (del 9) "123456789~123456789" ;
      let maxPosIntWithDot = EFloat (gid (), "4611686018427387", "903") in
      let maxPosIntPlus1WithDot = EFloat (gid (), "4611686018427387", "904") in
      tc "del dot at limit1" maxPosIntWithDot (del 16) "4611686018427387~903" ;
      tc
        "del dot at limit2"
        maxPosIntPlus1WithDot
        (del 16)
        "4611686018427387~90" ;
      tc "del start of whole" aFloat (del 0) "~23.456" ;
      tc "del middle of whole" aFloat (del 1) "1~3.456" ;
      tc "del end of whole" aFloat (del 2) "12~.456" ;
      tc "del start of fraction" aFloat (del 4) "123.~56" ;
      tc "del middle of fraction" aFloat (del 5) "123.4~6" ;
      tc "del end of fraction" aFloat (del 6) "123.45~" ;
      tc "del dot converts to int" aFloat (del 3) "123~456" ;
      tc "del dot converts to int, no fraction" aPartialFloat (del 1) "1~" ;
      tc "bs dot" aFloat (bs 4) "123~456" ;
      tc "bs dot at scale" aHugeFloat (bs 10) "123456789~123456789" ;
      tc "bs dot at limit1" maxPosIntWithDot (bs 17) "4611686018427387~903" ;
      tc "bs dot at limit2" maxPosIntPlus1WithDot (bs 17) "4611686018427387~90" ;
      tc "bs start of whole" aFloat (bs 1) "~23.456" ;
      tc "bs middle of whole" aFloat (bs 2) "1~3.456" ;
      tc "bs end of whole" aFloat (bs 3) "12~.456" ;
      tc "bs start of fraction" aFloat (bs 5) "123.~56" ;
      tc "bs middle of fraction" aFloat (bs 6) "123.4~6" ;
      tc "bs end of fraction" aFloat (bs 7) "123.45~" ;
      tc "bs dot converts to int" aFloat (bs 4) "123~456" ;
      tc "bs dot converts to int, no fraction" aPartialFloat (bs 2) "1~" ;
      tc "continue after adding dot" aPartialFloat (insert '2' 2) "1.2~" ;
      () ) ;
  describe "Bools" (fun () ->
      tcp "insert start of true" trueBool (insert 'c' 0) "c~true" ;
      tcp "del start of true" trueBool (del 0) "~rue" ;
      tc "bs start of true" trueBool (bs 0) "~true" ;
      tcp "insert end of true" trueBool (insert '0' 4) "true0~" ;
      tc "del end of true" trueBool (del 4) "true~" ;
      tcp "bs end of true" trueBool (bs 4) "tru~" ;
      tcp "insert middle of true" trueBool (insert '0' 2) "tr0~ue" ;
      tcp "del middle of true" trueBool (del 2) "tr~e" ;
      tcp "bs middle of true" trueBool (bs 2) "t~ue" ;
      tcp "insert start of false" falseBool (insert 'c' 0) "c~false" ;
      tcp "del start of false" falseBool (del 0) "~alse" ;
      tc "bs start of false" falseBool (bs 0) "~false" ;
      tcp "insert end of false" falseBool (insert '0' 5) "false0~" ;
      tc "del end of false" falseBool (del 5) "false~" ;
      tcp "bs end of false" falseBool (bs 5) "fals~" ;
      tcp "insert middle of false" falseBool (insert '0' 2) "fa0~lse" ;
      tcp "del middle of false" falseBool (del 2) "fa~se" ;
      tcp "bs middle of false" falseBool (bs 2) "f~lse" ;
      () ) ;
  describe "Nulls" (fun () ->
      tcp "insert start of null" aNull (insert 'c' 0) "c~null" ;
      tcp "del start of null" aNull (del 0) "~ull" ;
      tc "bs start of null" aNull (bs 0) "~null" ;
      tcp "insert end of null" aNull (insert '0' 4) "null0~" ;
      tc "del end of null" aNull (del 4) "null~" ;
      tcp "bs end of null" aNull (bs 4) "nul~" ;
      tcp "insert middle of null" aNull (insert '0' 2) "nu0~ll" ;
      tcp "del middle of null" aNull (del 2) "nu~l" ;
      tcp "bs middle of null" aNull (bs 2) "n~ll" ;
      () ) ;
  describe "Blanks" (fun () ->
      tc "insert middle of blank->string" b (insert '"' 3) "\"~\"" ;
      tc "del middle of blank->blank" b (del 3) "___~" ;
      tc "bs middle of blank->blank" b (bs 3) "~___" ;
      tc "insert blank->string" b (insert '"' 0) "\"~\"" ;
      tc "del blank->string" emptyStr (del 0) "~___" ;
      tc "bs blank->string" emptyStr (bs 1) "~___" ;
      tc "insert blank->int" b (insert '5' 0) "5~" ;
      tc "insert blank->int" b (insert '0' 0) "0~" ;
      tc "del int->blank " five (del 0) "~___" ;
      tc "bs int->blank " five (bs 1) "~___" ;
      tc "insert end of blank->int" b (insert '5' 1) "5~" ;
      tcp "insert partial" b (insert 't' 0) "t~" ;
      tc
        "backspacing your way through a partial finishes"
        trueBool
        (presses [K.Backspace; K.Backspace; K.Backspace; K.Backspace; K.Left] 4)
        "~___" ;
      tc "insert blank->space" b (space 0) "~___" ;
      () ) ;
  describe "Fields" (fun () ->
      tc "insert middle of fieldname" aField (insert 'c' 5) "obj.fc~ield" ;
      tc
        "cant insert invalid chars fieldname"
        aField
        (insert '$' 5)
        "obj.f~ield" ;
      tc "del middle of fieldname" aField (del 5) "obj.f~eld" ;
      tc "del fieldname" aShortField (del 4) "obj.~***" ;
      tc "bs fieldname" aShortField (bs 5) "obj.~***" ;
      tc "insert end of fieldname" aField (insert 'c' 9) "obj.fieldc~" ;
      tcp "insert end of varname" aField (insert 'c' 3) "objc~.field" ;
      tc "insert start of fieldname" aField (insert 'c' 4) "obj.c~field" ;
      tc "insert blank fieldname" aBlankField (insert 'c' 4) "obj.c~" ;
      tc "del fieldop with name" aShortField (del 3) "obj~" ;
      tc "bs fieldop with name" aShortField (bs 4) "obj~" ;
      tc "del fieldop with blank" aBlankField (del 3) "obj~" ;
      tc "bs fieldop with blank" aBlankField (bs 4) "obj~" ;
      tc "del fieldop in nested" aNestedField (del 3) "obj~.field2" ;
      tc "bs fieldop in nested" aNestedField (bs 4) "obj~.field2" ;
      tc "add dot after variable" aVar (insert '.' 8) "variable.~***" ;
      tc "add dot after partial " aPartialVar (insert '.' 3) "request.~***" ;
      tc "add dot after field" aField (insert '.' 9) "obj.field.~***" ;
      tc "insert space in blank " aBlankField (space 4) "obj.~***" ;
      () ) ;
  describe "Functions" (fun () ->
      tc
        "space on a sep goes to next arg"
        aFnCall
        (space 10)
        "Int::add 5 ~_________" ;
      tcp "bs function renames" aFnCall (bs 8) "Int::ad~@ 5 _________" ;
      tcp "deleting a function renames" aFnCall (del 7) "Int::ad~@ 5 _________" ;
      tc
        "renaming a function maintains unaligned params in let scope"
        (EPartial
           (gid (), "Int::", EFnCall (gid (), "Int::add", [five; six], NoRail)))
        (presses [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        "let b = 6\n~Int::sqrt 5" ;
      tc
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
        "Int::sqrt ~a" ;
      tc
        "renaming a function doesn't maintain unaligned params if they're not set (blanks)"
        (EPartial
           (gid (), "Int::", EFnCall (gid (), "Int::add", [b; b], NoRail)))
        (presses [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        "Int::sqrt ~_________" ;
      (* TODO: functions are not implemented fully. I deld bs and
       * del because we were switching to partials, but this isn't
       * implemented. Some tests we need:
         * myFunc arg1 arg2, 6 => Backspace => myFun arg1 arg2, with a ghost and a partial.
         * same with del *)
      tcp
        "del on function with version"
        aFnCallWithVersion
        (del 11)
        "DB::getAllv~@ ___________________" ;
      tcp
        "bs on function with version"
        aFnCallWithVersion
        (bs 12)
        "DB::getAllv~@ ___________________" ;
      tcp
        "del on function with version in between the version and function name"
        aFnCallWithVersion
        (del 10)
        "DB::getAll~1@ ___________________" ;
      tcp
        "bs on function with version in between the version and function name"
        aFnCallWithVersion
        (bs 10)
        "DB::getAl~v1@ ___________________" ;
      tcp
        "del on function with version in function name"
        aFnCallWithVersion
        (del 7)
        "DB::get~llv1@ ___________________" ;
      tcp
        "bs on function with version in function name"
        aFnCallWithVersion
        (bs 8)
        "DB::get~llv1@ ___________________" ;
      tc
        "adding function with version goes to the right place"
        b
        (presses [K.Letter 'd'; K.Letter 'b'; K.Enter] 0)
        "DB::getAllv1 ~___________________" ;
      let fn ?(ster = NoRail) name args = EFnCall (gid (), name, args, ster) in
      tc
        "backspacing a fn arg's separator goes to the right place"
        (fn "Int::add" [five; six])
        (bs 11)
        "Int::add 5~ 6" ;
      let string40 = "0123456789abcdefghij0123456789abcdefghij" in
      let string80 = string40 ^ string40 in
      let string160 = string80 ^ string80 in
      tc
        "reflows work for functions"
        (fn
           "HttpClient::post_v4"
           [ EString (gid (), string40)
           ; ERecord (gid (), [(gid (), string80, b)])
           ; emptyRecord
           ; emptyRecord ])
        render
        "~HttpClient::postv4\n  \"0123456789abcdefghij0123456789abcdefghij\"\n  {\n    0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij : ___\n  }\n  {}\n  {}" ;
      tc
        "reflows work for functions with long strings"
        (fn "HttpClient::post_v4" [EString (gid (), string160); b; b; b])
        render
        "~HttpClient::postv4\n  \"0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\"\n  ____________\n  ______________\n  ________________" ;
      tcp
        "reflows work for partials too "
        (EPartial
           ( gid ()
           , "TEST"
           , fn "HttpClient::post_v4" [EString (gid (), string160); b; b; b] ))
        render
        "~TEST@lient::postv@\n  \"0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\"\n  ____________\n  ______________\n  ________________" ;
      tc
        "reflows happen for functions whose arguments have newlines"
        (fn "HttpClient::post_v4" [emptyStr; emptyRowRecord; b; b])
        render
        "~HttpClient::postv4\n  \"\"\n  {\n    *** : ___\n  }\n  ______________\n  ________________" ;
      tc
        "reflows don't happen for functions whose only newline is in the last argument"
        (fn "HttpClient::post_v4" [emptyStr; b; b; emptyRowRecord])
        render
        "~HttpClient::postv4 \"\" ____________ ______________ {\n                                                    *** : ___\n                                                  }" ;
      tcp
        "reflows put the cursor in the right place on insert"
        (let justShortEnoughNotToReflow =
           "abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij01"
         in
         fn
           "HttpClient::post_v4"
           [ emptyStr
           ; emptyRecord
           ; emptyRecord
           ; EVariable (gid (), justShortEnoughNotToReflow) ])
        (insert ~wrap:false 'x' 120)
        "HttpClient::postv4\n  \"\"\n  {}\n  {}\n  abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcde~fghij01x"
      (* TODO: This should be 129, but reflow puts the cursor in the wrong
           * place for new partials *) ;
      tcp
        "reflows put the cursor in the right place on bs"
        (let justLongEnoughToReflow =
           "abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij012"
         in
         fn
           "HttpClient::post_v4"
           [ emptyStr
           ; emptyRecord
           ; emptyRecord
           ; EVariable (gid (), justLongEnoughToReflow) ])
        (bs ~wrap:false 129)
        "HttpClient::postv4 \"\" {} {} abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij01~"
      (* TODO: This should be 120, but reflow puts the cursor in the wrong
           * place for new partials *) ;
      () ) ;
  describe "Binops" (fun () ->
      tcp "pipe key starts partial" trueBool (press K.Pipe 4) "true |~" ;
      tc
        "pressing enter completes partial"
        trueBool
        (presses [K.Pipe; K.Down; K.Enter] 4)
        "true ||~ __________" ;
      tc
        "pressing space completes partial"
        trueBool
        (presses [K.Pipe; K.Down; K.Space] 4)
        "true || ~__________" ;
      tcp
        "pressing plus key starts partial"
        trueBool
        (press K.Plus 4)
        "true +~" ;
      tcp
        "pressing caret key starts partial"
        anInt
        (press K.Caret 5)
        "12345 ^~" ;
      tc
        "pressing pipe twice then space completes partial"
        trueBool
        (presses [K.Pipe; K.Pipe; K.Space] 4)
        "true || ~__________" ;
      tc
        "piping into newline creates pipe"
        trueBool
        (presses [K.Pipe; K.GreaterThan; K.Space] 4)
        "true\n|>~___\n" ;
      tc
        "pressing bs to clear partial reverts for blank rhs"
        (EPartial (gid (), "|", EBinOp (gid (), "||", anInt, b, NoRail)))
        (bs 7)
        "12345~" ;
      tc
        "pressing bs to clear partial reverts for blank rhs, check lhs pos goes to start"
        (EPartial (gid (), "|", EBinOp (gid (), "||", b, b, NoRail)))
        (bs 12)
        "~___" ;
      tc
        "pressing del to clear partial reverts for blank rhs"
        (EPartial (gid (), "|", EBinOp (gid (), "||", anInt, b, NoRail)))
        (del 6)
        "12345~" ;
      tc
        "pressing del to clear partial reverts for blank rhs, check lhs pos goes to start"
        (EPartial (gid (), "|", EBinOp (gid (), "||", b, b, NoRail)))
        (del 11)
        "~___" ;
      tc
        "using bs to remove an infix with a placeholder goes to right place"
        (EPartial (gid (), "|", EBinOp (gid (), "||", b, b, NoRail)))
        (bs 12)
        "~___" ;
      tc
        "using bs to remove an infix with a placeholder goes to right place 2"
        (EPartial (gid (), "|", EBinOp (gid (), "||", five, b, NoRail)))
        (bs 3)
        "5~" ;
      tc
        "pressing bs to clear rightpartial reverts for blank rhs"
        (ERightPartial (gid (), "|", b))
        (bs 5)
        "~___" ;
      tc
        "pressing bs on single digit binop leaves lhs"
        (EBinOp (gid (), "+", anInt, anInt, NoRail))
        (bs 7)
        "12345~" ;
      tc
        "using del to remove an infix with a placeholder goes to right place"
        (EPartial (gid (), "|", EBinOp (gid (), "||", b, b, NoRail)))
        (del 11)
        "~___" ;
      tc
        "pressing del to clear rightpartial reverts for blank rhs"
        (ERightPartial (gid (), "|", b))
        (del 4)
        "~___" ;
      tc
        "pressing del on single digit binop leaves lhs"
        (EBinOp (gid (), "+", anInt, anInt, NoRail))
        (del 6)
        "12345~" ;
      tc
        "pressing del to remove a string binop combines lhs and rhs"
        (EBinOp
           ( gid ()
           , "++"
           , EString (gid (), "five")
           , EString (gid (), "six")
           , NoRail ))
        (presses [K.Delete; K.Delete] 7)
        "\"five~six\"" ;
      tc
        "pressing backspace to remove a string binop combines lhs and rhs"
        (EBinOp
           ( gid ()
           , "++"
           , EString (gid (), "five")
           , EString (gid (), "six")
           , NoRail ))
        (presses [K.Backspace; K.Backspace] 9)
        "\"five~six\"" ;
      tc
        "pressing letters and numbers on a partial completes it"
        b
        (presses [K.Number '5'; K.Plus; K.Number '5'] 0)
        "5 + 5~" ;
      tcp
        "pressing pipe while editing a partial works properly"
        (EPartial (gid (), "|", EBinOp (gid (), "||", anInt, anInt, NoRail)))
        (press K.Pipe 7)
        "12345 ||~ 12345" ;
      tcp
        "pressing = after < should go to partial"
        (EBinOp (gid (), "<", anInt, anInt, NoRail))
        (press K.Equals 7)
        "12345 <=~ 12345" ;
      tc
        "changing binop to fn should work"
        (EPartial
           (gid (), "Int::add", EBinOp (gid (), "+", anInt, anInt, NoRail)))
        (presses [K.Enter] 14)
        "Int::add 12345 ~12345" ;
      tc
        "changing fn to binops should work"
        (EPartial
           (gid (), "+", EFnCall (gid (), "Int::add", [anInt; anInt], NoRail)))
        (presses [K.Enter] 1)
        "~12345 + 12345" ;
      tc
        "changing binop should work"
        (EBinOp (gid (), "<", anInt, anInt, NoRail))
        (presses [K.Equals; K.Enter] 7)
        "12345 <=~ 12345" ;
      tcp
        "adding binop in `if` works"
        (EIf (gid (), b, b, b))
        (press K.Percent 3)
        "if %~\nthen\n  ___\nelse\n  ___" ;
      let aFullBinOp =
        EBinOp
          ( gid ()
          , "||"
          , EVariable (gid (), "myvar")
          , EInteger (gid (), "5")
          , NoRail )
      in
      tcp "show ghost partial" aFullBinOp (bs 8) "myvar |~@ 5" ;
      (* TODO bs on empty partial does something *)
      (* TODO support del on all the bs commands *)
      (* TODO pressing enter at the end of the partialGhost *)
      () ) ;
  describe "Constructors" (fun () ->
      tcp
        "arguments work in constructors"
        aConstructor
        (insert 't' 5)
        "Just t~" ;
      tc
        "int arguments work in constructors"
        aConstructor
        (insert '5' 5)
        "Just 5~" ;
      tcp
        "bs on a constructor converts it to a partial with ghost"
        aConstructor
        (bs 4)
        "Jus~@ ___" ;
      tcp
        "del on a constructor converts it to a partial with ghost"
        aConstructor
        (del 0)
        "~ust@ ___" ;
      tc
        "space on a constructor blank does nothing"
        aConstructor
        (space 5)
        "Just ~___" ;
      (* TODO: test renaming constructors.
       * It's not too useful yet because there's only 4 constructors and,
       * hence, unlikely that anyone will rename them this way.
       * Also, the names of the temporary variables used to store the old arguments of a changed
       * constructor are randomly generated and would be hard to test *)
      () ) ;
  describe "Lambdas" (fun () ->
      (* type -> to move through a lambda *)
      tc
        "type - after a lambda var to move into a lambda arrow"
        aLambda
        (press Minus 4)
        "\\*** -~> ___" ;
      tc
        "type - before a lambda arrow to move into a lambda arrow"
        aLambda
        (press Minus 5)
        "\\*** -~> ___" ;
      tc
        "type > inside a lambda arrow to move past it"
        aLambda
        (press GreaterThan 6)
        "\\*** -> ~___" ;
      (* end type -> to move through a lambda *)
      tc "bs over lambda symbol" aLambda (bs 1) "~___" ;
      tc "insert space in lambda" aLambda (press K.Space 1) "\\~*** -> ___" ;
      tc "bs non-empty lambda symbol" nonEmptyLambda (bs 1) "\\~*** -> 5" ;
      tc "del lambda symbol" aLambda (del 0) "~___" ;
      tc "del non-empty lambda symbol" nonEmptyLambda (del 0) "~\\*** -> 5" ;
      tc
        "insert changes occurence of binding var"
        (lambdaWithUsedBinding "binding")
        (insert 'c' 8)
        "\\bindingc~ -> bindingc" ;
      tc
        "insert changes occurence of binding 2nd var"
        (lambdaWithUsed2ndBinding "binding")
        (insert 'c' 17)
        "\\somevar, bindingc~ -> bindingc" ;
      tc
        "dont jump in lambdavars with infix chars"
        aLambda
        (press K.Plus 1)
        "\\~*** -> ___" ;
      tc
        "dont allow name to start with a number"
        aLambda
        (insert '5' 1)
        "\\~*** -> ___" ;
      tc
        "dont allow name to start with a number, pt 2"
        (lambdaWithBinding "test" five)
        (insert '2' 1)
        "\\~test -> 5" ;
      tc
        "dont allow name to start with a number, pt 3"
        aLambda
        (insert '5' 3)
        (* TODO: this looks wrong *)
        "\\**~* -> ___" ;
      tc
        "creating lambda in block placeholder should set arguments"
        aFnCallWithBlockArg
        (press (K.Letter '\\') 24)
        "Dict::map _____________ \\~key, value -> ___" ;
      tc
        "creating lambda in block placeholder should set arguments when wrapping expression is inside pipe"
        (EPipe (gid (), [b; b]))
        (presses
           (* we have to insert the function with completion here
            * so the arguments are adjusted based on the pipe *)
           [K.Letter 'm'; K.Letter 'a'; K.Letter 'p'; K.Enter; K.Letter '\\']
           6)
        "___\n|>Dict::map \\~key, value -> ___\n" ;
      tc
        "deleting a lambda argument should work"
        lambdaWithTwoBindings
        (del 2)
        "\\x~ -> ___" ;
      tc
        "backspacing a lambda argument should work"
        lambdaWithTwoBindings
        (bs 3)
        "\\x~ -> ___" ;
      tc
        "deleting a lambda argument should update used variable"
        (lambdaWithUsed2ndBinding "x")
        (del 8)
        "\\somevar~ -> ___" ;
      tc
        "can add lambda arguments when blank"
        aLambda
        (insert ',' 4)
        "\\***, ~*** -> ___" ;
      tc
        "can add lambda arguments to used binding"
        lambdaWithTwoBindings
        (insert ',' 5)
        "\\x, y, ~*** -> ___" ;
      tc
        "can add lambda arguments in middle used binding"
        lambdaWithTwoBindings
        (insert ',' 2)
        "\\x, ~***, y -> ___" ;
      tc
        "can add lambda arguments in the front"
        lambdaWithTwoBindings
        (insert ',' 1)
        "\\~***, x, y -> ___" ;
      tc
        "can add lambda arguments in front of middle"
        lambdaWithTwoBindings
        (insert ',' 4)
        "\\x, ~***, y -> ___" ;
      tc
        "cant insert a blank from outside the lambda"
        lambdaWithTwoBindings
        (insert ',' 0)
        "~\\x, y -> ___" ;
      tc
        "cant bs a blank from the space in a lambda"
        lambdaWithTwoBindings
        (bs 4)
        "\\x,~ y -> ___" ;
      () ) ;
  describe "Variables" (fun () ->
      tcp "insert middle of variable" aVar (insert 'c' 5) "variac~ble" ;
      tcp "del middle of variable" aVar (del 5) "varia~le" ;
      tcp "insert capital works" aVar (press (K.Letter 'A') 5) "variaA~ble" ;
      tc "can't insert invalid" aVar (press K.Dollar 5) "varia~ble" ;
      tc "del variable" aShortVar (del 0) "~___" ;
      tcp "del long variable" aVar (del 0) "~ariable" ;
      tcp "del mid variable" aVar (del 6) "variab~e" ;
      tc "bs variable" aShortVar (bs 1) "~___" ;
      tcp "bs mid variable" aVar (bs 8) "variabl~" ;
      tcp "bs mid variable" aVar (bs 6) "varia~le" ;
      tc
        "variable doesn't override if"
        (ELet (gid (), gid (), "i", b, EPartial (gid (), "i", b)))
        (presses [K.Letter 'f'; K.Enter] 13)
        "let i = ___\nif ~___\nthen\n  ___\nelse\n  ___" ;
      () ) ;
  describe "Match" (fun () ->
      t
        "move to the front of match"
        emptyMatch
        (press K.GoToStartOfLine 6)
        ("match ___\n  *** -> ___\n", 0) ;
      t
        "move to the end of match"
        emptyMatch
        (press K.GoToEndOfLine 0)
        ("match ___\n  *** -> ___\n", 9) ;
      t
        "move to the front of match on line 2"
        emptyMatch
        (press K.GoToStartOfLine 15)
        ("match ___\n  *** -> ___\n", 12) ;
      t
        "move to the end of match on line 2"
        emptyMatch
        (press K.GoToEndOfLine 12)
        ("match ___\n  *** -> ___\n", 22) ;
      t
        "move back over match"
        emptyMatch
        (press K.Left 6)
        ("match ___\n  *** -> ___\n", 0) ;
      t
        "move forward over match"
        emptyMatch
        (press K.Right 0)
        ("match ___\n  *** -> ___\n", 6) ;
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
        ("match ___\n  3 -> ___\n", 6) ;
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
        ("match ___\n  3 -> ___\n", 0) ;
      t
        "del constructor in match pattern"
        matchWithConstructorPattern
        (del 12)
        ("match ___\n  ust -> ___\n", 12) ;
      t
        "bs constructor in match pattern"
        matchWithConstructorPattern
        (bs 16)
        ("match ___\n  Jus -> ___\n", 15) ;
      t
        "insert changes occurence of non-shadowed var in case"
        (matchWithBinding "binding" (EVariable (gid (), "binding")))
        (insert 'c' 19)
        ("match ___\n  bindingc -> bindingc\n", 20) ;
      t
        "insert changes occurence of non-shadowed var in case constructor"
        (matchWithConstructorBinding "binding" (EVariable (gid (), "binding")))
        (insert 'c' 22)
        ("match ___\n  Ok bindingc -> bindingc\n", 23) ;
      t
        "insert space in blank match"
        emptyMatch
        (press K.Space 6)
        ("match ___\n  *** -> ___\n", 6) ;
      t
        "insert space in blank match on line 2"
        emptyMatch
        (press K.Space 12)
        ("match ___\n  *** -> ___\n", 12) ;
      t
        "enter at the end of the cond creates a new row"
        matchWithPatterns
        (enter 9)
        ("match ___\n  *** -> ___\n  3 -> ___\n", 12) ;
      t
        "enter at the end of a row creates a new row"
        emptyMatchWithTwoPatterns
        (enter 22)
        ("match ___\n  *** -> ___\n  *** -> ___\n  *** -> ___\n", 25) ;
      t
        "enter at the end of the last row creates a new row"
        emptyMatchWithTwoPatterns
        (enter 35)
        ("match ___\n  *** -> ___\n  *** -> ___\n  *** -> ___\n", 38) ;
      t
        "enter at the end of the last row in nested match creates a new row"
        nestedMatch
        (enter 50)
        ( "match ___\n  *** -> match ___\n           *** -> ___\n           *** -> ___\n"
        , 62 ) ;
      t
        "enter at the start of a row creates a new row"
        matchWithPatterns
        (enter 12)
        ("match ___\n  *** -> ___\n  3 -> ___\n", 25) ;
      t
        "backspace first row deletes it"
        emptyMatchWithTwoPatterns
        (bs 12)
        ("match ___\n  *** -> ___\n", 9) ;
      t
        "backspace second row deletes it"
        emptyMatchWithTwoPatterns
        (bs 25)
        ("match ___\n  *** -> ___\n", 22) ;
      t
        "backspacing only row doesn't delete"
        emptyMatch
        (bs 12)
        ("match ___\n  *** -> ___\n", 9) ;
      (* delete row with delete *)
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
              , b
              , [ ( FPVariable (gid (), gid (), "binding")
                  , EVariable (gid (), "binding") )
                ; ( FPInteger (gid (), gid (), "5")
                  , EVariable (gid (), "binding") ) ] )))
        (insert 'c' 11)
        ( "let bindingc = 6\nmatch ___\n  binding -> binding\n  5 -> bindingc\n"
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
      t
        "enter on the end of let goes to blank"
        nonEmptyLetWithBlankEnd
        (enter 11)
        ("let *** = 6\n___", 12) ;
      t
        "enter at the end of a line inserts let if no blank is next"
        nonEmptyLet
        (enter 11)
        ("let *** = 6\nlet *** = ___\n5", 16) ;
      t
        "enter at the start of a let creates let above"
        twoLets
        (enter 10)
        ("let x = 5\nlet *** = ___\nlet y = 6\n7", 24) ;
      t
        "enter at the start of first let creates let above"
        nonEmptyLet
        (enter 0)
        ("let *** = ___\nlet *** = 6\n5", 14) ;
      t
        "enter at the end of a let with a let below inserts new let"
        twoLets
        (enter 9)
        ("let x = 5\nlet *** = ___\nlet y = 6\n7", 14) ;
      t
        "enter on the end of first let inserts new let"
        matchWithTwoLets
        (enter 28)
        ( "match ___\n  *** -> let x = 5\n         let *** = ___\n         let y = 6\n         ___\n"
        , 42 ) ;
      t
        "enter on the end of second let goes to blank"
        matchWithTwoLets
        (enter 47)
        ( "match ___\n  *** -> let x = 5\n         let y = 6\n         ___\n"
        , 57 ) ;
      t
        "enter at the start of a non-let also creates let above"
        anInt
        (enter 0)
        ("let *** = ___\n12345", 14) ;
      test "enter at the start of ast also creates let" (fun () ->
          (* Test doesn't work wrapped *)
          expect
            (let ast, state =
               processMsg
                 [(K.Enter, ShiftNotHeld)]
                 Defaults.defaultFluidState
                 anInt
             in
             (eToString state ast, state.newPos))
          |> toEqual ("let *** = ___\n12345", 14) ) ;
      () ) ;
  describe "Pipes" (fun () ->
      let pipeOn expr fns = EPipe (gid (), expr :: fns) in
      let emptyList = EList (gid (), []) in
      let aList5 = EList (gid (), [five]) in
      let aList6 = EList (gid (), [six]) in
      let aListNum n = EList (gid (), [EInteger (gid (), n)]) in
      let listFn args =
        EFnCall (gid (), "List::append", EPipeTarget (gid ()) :: args, NoRail)
      in
      let aPipe = pipeOn emptyList [listFn [aList5]; listFn [aList5]] in
      let emptyPipe = EPipe (gid (), [b; b]) in
      let aLongPipe =
        pipeOn
          emptyList
          [ listFn [aListNum "2"]
          ; listFn [aListNum "3"]
          ; listFn [aListNum "4"]
          ; listFn [aListNum "5"] ]
      in
      let aBinopPipe =
        pipeOn
          (newB ())
          [ EBinOp
              ( gid ()
              , "++"
              , EPipeTarget (gid ())
              , EString (gid (), "asd")
              , NoRail ) ]
      in
      let aPipeInsideIf = EIf (gid (), b, aLongPipe, b) in
      let aNestedPipe =
        pipeOn emptyList [listFn [pipeOn aList5 [listFn [aList6]]]]
      in
      (* TODO: add tests for clicking in the middle of a pipe (or blank) *)
      t
        "move to the front of pipe on line 1"
        aPipe
        (press K.GoToStartOfLine 2)
        ("[]\n|>List::append [5]\n|>List::append [5]\n", 0) ;
      t
        "move to the end of pipe on line 1"
        aPipe
        (press K.GoToEndOfLine 0)
        ("[]\n|>List::append [5]\n|>List::append [5]\n", 2) ;
      t
        "move to the front of pipe on line 2"
        aPipe
        (press K.GoToStartOfLine 8)
        ("[]\n|>List::append [5]\n|>List::append [5]\n", 5) ;
      t
        "move to the end of pipe on line 2"
        aPipe
        (press K.GoToEndOfLine 5)
        ("[]\n|>List::append [5]\n|>List::append [5]\n", 21) ;
      t
        "move to the front of pipe on line 3"
        aPipe
        (press K.GoToStartOfLine 40)
        ("[]\n|>List::append [5]\n|>List::append [5]\n", 24) ;
      t
        "move to the end of pipe on line 3"
        aPipe
        (press K.GoToEndOfLine 24)
        ("[]\n|>List::append [5]\n|>List::append [5]\n", 40) ;
      t
        "pipes appear on new lines"
        aPipe
        render
        ("[]\n|>List::append [5]\n|>List::append [5]\n", 0) ;
      t
        "nested pipes will indent"
        aNestedPipe
        render
        ("[]\n|>List::append [5]\n               |>List::append [6]\n", 0) ;
      t
        "backspacing a pipe's first pipe works"
        aLongPipe
        (bs 5)
        ("[]\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]\n", 2) ;
      t
        "deleting a pipe's first pipe works"
        aLongPipe
        (del 3)
        ("[]\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]\n", 3) ;
      t
        "backspacing a pipe's second pipe works"
        aLongPipe
        (bs 24)
        ("[]\n|>List::append [2]\n|>List::append [4]\n|>List::append [5]\n", 21) ;
      t
        "deleting a pipe's second pipe works"
        aLongPipe
        (del 22)
        ("[]\n|>List::append [2]\n|>List::append [4]\n|>List::append [5]\n", 22) ;
      t
        "backspacing a pipe's third pipe works"
        aLongPipe
        (bs 43)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [5]\n", 40) ;
      t
        "deleting a pipe's third pipe works"
        aLongPipe
        (del 41)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [5]\n", 41) ;
      t
        "backspacing a pipe's last pipe works"
        aLongPipe
        (bs 62)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]\n", 59) ;
      t
        "deleting a pipe's last pipe works"
        aLongPipe
        (del 60)
        ("[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]\n", 59) ;
      t
        "backspacing a pipe's first pipe that isn't in the first column works"
        aPipeInsideIf
        (bs 21)
        ( "if ___\nthen\n  []\n  |>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 16 ) ;
      t
        "deleting a pipe's first pipe that isn't in the first column works"
        aPipeInsideIf
        (del 19)
        ( "if ___\nthen\n  []\n  |>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 19 ) ;
      t
        "backspacing a pipe's second pipe that isn't in the first column works"
        aPipeInsideIf
        (bs 42)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 37 ) ;
      t
        "deleting a pipe's second pipe that isn't in the first column works"
        aPipeInsideIf
        (del 40)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___"
        , 40 ) ;
      t
        "backspacing a pipe's third pipe that isn't in the first column works"
        aPipeInsideIf
        (bs 63)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [5]\nelse\n  ___"
        , 58 ) ;
      t
        "deleting a pipe's third pipe that isn't in the first column works"
        aPipeInsideIf
        (del 61)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [5]\nelse\n  ___"
        , 61 ) ;
      t
        "backspacing a pipe's fourth pipe that isn't in the first column works"
        aPipeInsideIf
        (bs 84)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]\nelse\n  ___"
        , 79 ) ;
      t
        "deleting a pipe's fourth pipe that isn't in the first column works"
        aPipeInsideIf
        (del 82)
        ( "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]\nelse\n  ___"
        , 79 ) ;
      tp
        "backspacing a pipe's first fn works"
        aLongPipe
        (bs 17)
        ( "[]\n|>List::appen@ [2]\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]\n"
        , 16 ) ;
      tp
        "backspacing a pipe's first binop works"
        aBinopPipe
        (bs 8)
        ("___\n|>+@ \"asd\"\n", 7) ;
      t
        "adding infix functions adds the right number of blanks"
        emptyPipe
        (presses [K.Plus; K.Enter] 6)
        ("___\n|>+ _________\n", 8) ;
      t
        "creating a pipe from an fn via a partial works"
        (EPartial (gid (), "|>", aFnCall))
        (enter 2)
        (* This really should end in 18, but too much work for now *)
        ("Int::add 5 _________\n|>___\n", 11) ;
      t
        "enter at the end of a pipe expr creates a new entry"
        aPipe
        (enter 21)
        ("[]\n|>List::append [5]\n|>___\n|>List::append [5]\n", 24) ;
      t
        "enter at the end of the opening expr creates a new entry"
        aPipe
        (enter 2)
        ("[]\n|>___\n|>List::append [5]\n|>List::append [5]\n", 5) ;
      t
        "enter at the start of a line creates a new entry"
        aPipe
        (enter 3)
        ("[]\n|>___\n|>List::append [5]\n|>List::append [5]\n", 9) ;
      t
        "enter at start of blank (within pipe) creates a new entry"
        aPipe
        (enter 5)
        ("[]\n|>___\n|>List::append [5]\n|>List::append [5]\n", 11) ;
      t
        "enter at the end of the last expr creates a new entry"
        aPipe
        (enter 40)
        ("[]\n|>List::append [5]\n|>List::append [5]\n|>___\n", 43) ;
      t
        "enter at the end of the last nested expr creates a new entry"
        aNestedPipe
        (enter 55)
        ( "[]\n|>List::append [5]\n               |>List::append [6]\n               |>___\n"
        , 73 ) ;
      t
        "inserting a pipe into another pipe gives a single pipe1"
        (pipeOn five [listFn [ERightPartial (gid (), "|>", aList5)]])
        (enter 23)
        ("5\n|>List::append [5]\n|>___\n", 23) ;
      t
        "inserting a pipe into another pipe gives a single pipe2"
        (pipeOn five [listFn [aList5]])
        (press K.ShiftEnter 19)
        ("5\n|>List::append [5]\n|>___\n", 23) ;
      t
        "inserting a pipe into another pipe gives a single pipe3"
        five
        (press K.ShiftEnter 1)
        ("5\n|>___\n", 4) ;
      t
        "shift enter at a let's newline creates the pipe on the rhs"
        nonEmptyLet
        (press K.ShiftEnter 11)
        ("let *** = 6\n          |>___\n5", 24) ;
      t
        "shift enter in a record's newline creates the pipe in the expr, not the entire record"
        (ERecord
           (gid (), [(gid (), "f1", fiftySix); (gid (), "f2", seventyEight)]))
        (press K.ShiftEnter 11)
        ("{\n  f1 : 56\n       |>___\n  f2 : 78\n}", 21) ;
      (* TODO: test for prefix fns *)
      (* TODO: test for deleting pipeed infix fns *)
      (* TODO: test for deleting pipeed prefix fns *)
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
      t
        "enter in front of an if wraps in a let"
        plainIf
        (enter 0)
        ("let *** = ___\nif 5\nthen\n  6\nelse\n  7", 14) ;
      t
        "enter at end of if line does nothing"
        plainIf
        (enter 4)
        ("if 5\nthen\n  6\nelse\n  7", 5) ;
      t
        "enter at end of then line inserts let if no blank next "
        plainIf
        (enter 9)
        ("if 5\nthen\n  let *** = ___\n  6\nelse\n  7", 16) ;
      t
        "enter at end of then expr line does nothing"
        plainIf
        (enter 13)
        ("if 5\nthen\n  6\nelse\n  7", 14) ;
      t
        "enter at end of else line does inserts let if no blank next"
        (* TODO: This should probably do nothing, but right now it acts like
         * it's at the front of the line below. *)
        plainIf
        (enter 18)
        ("if 5\nthen\n  6\nelse\n  let *** = ___\n  7", 25) ;
      t
        "enter at end of else expr line does nothing"
        plainIf
        (enter 22)
        ("if 5\nthen\n  6\nelse\n  7", 22) ;
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
        EList (gid (), [fiftySix; seventyEight; b; fiftySix])
      in
      let multiWithStrs =
        EList
          ( gid ()
          , [ EString (gid (), "ab")
            ; EString (gid (), "cd")
            ; EString (gid (), "ef") ] )
      in
      t "create list" b (press K.LeftSquareBracket 0) ("[]", 1) ;
      t "insert into empty list inserts" emptyList (insert '5' 1) ("[5]", 2) ;
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
        ("[___,56]", 1) ;
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
      (* let withStr = EList (gid (), [EString (gid (), "ab")]) in *)
      t "create record" b (press K.LeftCurlyBrace 0) ("{}", 1) ;
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
        emptyRowRecord
        (space 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "inserting space in empty record value does nothing"
        emptyRowRecord
        (space 10)
        ("{\n  *** : ___\n}", 10) ;
      t
        "pressing enter in an the start of empty record adds a new line"
        emptyRecord
        (enter 1)
        ("{\n  *** : ___\n}", 4) ;
      t "enter fieldname" emptyRowRecord (insert 'c' 4) ("{\n  c : ___\n}", 5) ;
      t
        "move to the front of an empty record"
        emptyRowRecord
        (press K.GoToStartOfLine 13)
        ("{\n  *** : ___\n}", 4) ;
      t
        "move to the end of an empty record"
        emptyRowRecord
        (press K.GoToEndOfLine 4)
        ("{\n  *** : ___\n}", 13) ;
      t
        "cant enter invalid fieldname"
        emptyRowRecord
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
        emptyRowRecord
        (bs 4)
        (* TODO: buggy. Should be 1 *)
        ("{}", 2) ;
      t
        "appending to int in expr works"
        singleRowRecord
        (insert '1' 11)
        ("{\n  f1 : 561\n}", 12) ;
      t
        "appending to int in expr works"
        multiRowRecord
        (insert '1' 21)
        ("{\n  f1 : 56\n  f2 : 781\n}", 22) ;
      t
        "move to the front of a record with multiRowRecordple values"
        multiRowRecord
        (press K.GoToStartOfLine 21)
        ("{\n  f1 : 56\n  f2 : 78\n}", 14) ;
      t
        "move to the end of a record with multiRowRecordple values"
        multiRowRecord
        (press K.GoToEndOfLine 14)
        ("{\n  f1 : 56\n  f2 : 78\n}", 21) ;
      t
        "inserting at the end of the key works"
        emptyRowRecord
        (insert 'f' 6)
        ("{\n  f : ___\n}", 5) ;
      t
        "pressing enter at start adds a row"
        multiRowRecord
        (enter 1)
        ("{\n  *** : ___\n  f1 : 56\n  f2 : 78\n}", 4) ;
      t
        "pressing enter at the back adds a row"
        multiRowRecord
        (enter 22)
        ("{\n  f1 : 56\n  f2 : 78\n  *** : ___\n}", 24) ;
      t
        "pressing enter at the start of a field adds a row"
        multiRowRecord
        (enter 14)
        ("{\n  f1 : 56\n  *** : ___\n  f2 : 78\n}", 26) ;
      t
        "pressing enter at the end of row adds a row"
        multiRowRecord
        (enter 11)
        ("{\n  f1 : 56\n  *** : ___\n  f2 : 78\n}", 14) ;
      t
        "dont allow weird chars in recordFields"
        emptyRowRecord
        (press K.RightParens 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "dont jump in recordFields with infix chars"
        emptyRowRecord
        (press K.Plus 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "dont jump in recordFields with infix chars, pt 2"
        singleRowRecord
        (press K.Plus 6)
        ("{\n  f1 : 56\n}", 6) ;
      t
        "colon should skip over the record colon"
        emptyRowRecord
        (press K.Colon 7)
        ("{\n  *** : ___\n}", 10) ;
      t
        "dont allow key to start with a number"
        emptyRowRecord
        (insert '5' 4)
        ("{\n  *** : ___\n}", 4) ;
      t
        "dont allow key to start with a number, pt 2"
        singleRowRecord
        (insert '5' 4)
        ("{\n  f1 : 56\n}", 4) ;
      t
        "dont allow key to start with a number, pt 3"
        emptyRowRecord
        (insert '5' 6)
        ("{\n  *** : ___\n}", 6) ;
      () ) ;
  describe "Autocomplete" (fun () ->
      t
        "space autocompletes correctly"
        (EPartial (gid (), "if", b))
        (space 2)
        ("if ___\nthen\n  ___\nelse\n  ___", 3) ;
      t
        "let moves to right place"
        (EPartial (gid (), "let", b))
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
        b
        (presses [K.Equals; K.Enter] 0)
        ("_________ == _________", 0) ;
      t
        "autocomplete tab on bin-op moves to start of second blank"
        b
        (presses [K.Equals; K.Tab] 0)
        ("_________ == _________", 13) ;
      t
        "autocomplete space on bin-op moves to start of first blank"
        b
        (presses [K.Equals; K.Space] 0)
        ("_________ == _________", 0) ;
      t
        "variable moves to right place"
        (EPartial (gid (), "req", b))
        (enter 3)
        ("request", 7) ;
      t
        "pipe moves to right place on blank"
        b
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 2)
        ("___\n|>___\n", 6) ;
      t
        "pipe moves to right place on placeholder"
        aFnCall
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 11)
        ("Int::add 5 _________\n|>___\n", 23) ;
      t
        "pipe moves to right place in if then"
        emptyIf
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 14)
        ("if ___\nthen\n  ___\n  |>___\nelse\n  ___", 22) ;
      t
        "pipe moves to right place in lambda body"
        aLambda
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 8)
        ("\\*** -> ___\n        |>___\n", 22) ;
      t
        "pipe moves to right place in match body"
        emptyMatch
        (presses [K.Letter '|'; K.Letter '>'; K.Enter] 19)
        ("match ___\n  *** -> ___\n         |>___\n", 34) ;
      t
        "autocomplete for Just"
        (EPartial (gid (), "Just", b))
        (enter 4)
        ("Just ___", 5) ;
      t
        "autocomplete for Ok"
        (EPartial (gid (), "Ok", b))
        (enter 2)
        ("Ok ___", 3) ;
      t
        "autocomplete for Nothing"
        (EPartial (gid (), "Nothing", b))
        (enter 7)
        ("Nothing", 7) ;
      t
        "autocomplete for Nothing at end of a line"
        (EIf (gid (), b, EPartial (gid (), "Nothing", b), b))
        (space 21)
        ("if ___\nthen\n  Nothing\nelse\n  ___", 21) ;
      t
        "autocomplete for Error"
        (EPartial (gid (), "Error", b))
        (enter 5)
        ("Error ___", 6) ;
      t
        "autocomplete for field"
        (EFieldAccess (gid (), EVariable (ID "12", "request"), gid (), "bo"))
        (enter ~clone:false 10)
        ("request.body", 12) ;
      (* TODO: this doesn't work but should *)
      (* t *)
      (*   "autocomplete for field in body" *)
      (*   (EMatch *)
      (*      ( gid () *)
      (*      , EFieldAccess (gid (), EVariable (ID "12", "request"), gid (), "bo") *)
      (*      , [] )) *)
      (*   (enter 18) *)
      (*   ("match request.body", 18) ; *)
      (* test "backspacing on variable reopens autocomplete" (fun () -> *)
      (*     expect (bs (EVariable (5, "request"))). *)
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
               ELet (gid (), gid (), "var", EPartial (gid (), "false", b), b)
             in
             moveTo 14 s
             |> (fun s ->
                  let h = Fluid_utils.h ast in
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
        (ELet (gid (), gid (), "x", EPartial (gid (), "Int::add", b), b))
        (press K.Right 16)
        ("let x = Int::add _________ _________\n___", 17) ;
      tp
        "pressing an infix which could be valid doesn't commit"
        b
        (presses [K.Pipe; K.Pipe] 0)
        ("||", 2) ;
      tp
        "pressing an infix after true commits it "
        (EPartial (gid (), "true", b))
        (press K.Plus 4)
        ("true +", 6) ;
      t
        "moving left off a function autocompletes it anyway"
        (ELet (gid (), gid (), "x", EPartial (gid (), "Int::add", b), b))
        (press K.Left 8)
        ("let x = Int::add _________ _________\n___", 7) ;
      test "escape hides autocomplete" (fun () ->
          expect
            (let ast = b in
             moveTo 0 s
             |> (fun s -> updateKey (K.Letter 'r') ast s)
             |> (fun (ast, s) -> updateKey K.Escape ast s)
             |> fun (_, s) -> s.ac.index)
          |> toEqual None ) ;
      test "right/left brings back autocomplete" (fun () ->
          expect
            (let ast = b in
             moveTo 0 s
             |> (fun s -> updateKey (K.Letter 'r') ast s)
             |> (fun (ast, s) -> updateKey K.Escape ast s)
             |> fun (_, s) -> s.ac.index)
          |> toEqual None ) ;
      () ) ;
  describe "Selection Movement" (fun () ->
      ts
        "shift right selects"
        longLets
        (modPresses [(K.Right, ShiftHeld)] 0)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , (Some 0, 4) ) ;
      ts
        "shift down selects"
        longLets
        (modPresses [(K.Down, ShiftHeld)] 4)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , (Some 4, 52) ) ;
      ts
        "shift left selects"
        longLets
        (modPresses [(K.Left, ShiftHeld)] 52)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , (Some 52, 48) ) ;
      ts
        "keypress on selection drops selection"
        longLets
        (selectionPress K.Left 0 13)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , (None, 0) ) ;
      t
        "shiftless left aborts left-to-right selection on left"
        longLets
        (selectionPress K.Left 4 52)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , 4 ) ;
      t
        "shiftless left aborts right-to-left selection on left"
        longLets
        (selectionPress K.Left 52 4)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , 4 ) ;
      t
        "shiftless right aborts left-to-right selection on right"
        longLets
        (selectionPress K.Right 4 52)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , 52 ) ;
      t
        "shiftless right aborts right-to-left selection on right"
        longLets
        (selectionPress K.Right 52 4)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , 52 ) ;
      t
        "selecting an expression pipes from it 1"
        (EBinOp
           (gid (), "+", EInteger (gid (), "4"), EInteger (gid (), "5"), NoRail))
        (selectionPress K.ShiftEnter 4 5)
        ("4 + 5\n    |>___\n", 12) ;
      t
        "selecting an expression pipes from it 2"
        (EBinOp
           (gid (), "+", EInteger (gid (), "4"), EInteger (gid (), "5"), NoRail))
        (selectionPress K.ShiftEnter 5 4)
        ("4 + 5\n    |>___\n", 12) ;
      ts
        "cmd+a selects all"
        longLets
        (press K.SelectAll 4)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , (Some 0, 89) ) ;
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
