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
 * aFloat (del 2) ("12~.456") ;
 *
 * This is a test that takes the fluidExpr called aFloat, and does a del on
 * it in position 2. The stringified result is "12.456" and the caret should
 * be in position 2 (indicated by the tilde).
 *
 * There are a handful of functions you can call, including modKeys, key,
 * keys, insert, bs, del, tab, shiftTab, and render.
 *
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
 *  - TPlaceHolders are displayed as underscores of the original length:
 *  for example if the original is " a : Int " then we show
 *    `_________`
 *  - TPartials are displayed as text - to detect their presence,
 *    see "t vs tp" above.
 *  - TGhostPartials are displayed as multiple @ signs
 *  - Other blanks (see FluidToken.isBlank) are displayed as `***` This is
 *    controlled by FluidToken.toTestText
 *  - Wrap:
 *      When I started writing these tests, I discovered that they kept passing
 *      despite there being a bug. Whenever the caret went over the end, it
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

let deOption msg v = match v with Some v -> v | None -> failwith msg

type hasPartial =
  | NoPartial
  | ContainsPartial

type testResult = (string * (int option * int)) * hasPartial

type modifierKeys =
  { shiftKey : bool
  ; altKey : bool
  ; metaKey : bool
  ; ctrlKey : bool }

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
               (StrDict.fromList
                  [ ( "12"
                    , DObj
                        (StrDict.fromList [("body", DNull); ("formBody", DNull)])
                    )
                  ; ( "13"
                    , DObj
                        (StrDict.fromList [("title", DNull); ("author", DNull)])
                    ) ]))
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
          ; fnInfix = false }
        ; { fnName = "List::append"
          ; fnParameters = [fnParam "l1" TList false; fnParam "l2" TList false]
          ; fnReturnTipe = TList
          ; fnDescription = "append list"
          ; fnPreviewExecutionSafe = true
          ; fnDeprecated = false
          ; fnInfix = false }
        ; { fnName = "List::empty"
          ; fnParameters = []
          ; fnReturnTipe = TList
          ; fnDescription = "empty list"
          ; fnPreviewExecutionSafe = true
          ; fnDeprecated = false
          ; fnInfix = false } ] }
  in
  let processMsg
      (keys : (K.key * modifierKeys) list) (s : fluidState) (ast : ast) :
      ast * fluidState =
    let h = Fluid_utils.h ast in
    let m = {m with handlers = Handlers.fromList [h]} in
    List.foldl keys ~init:(ast, s) ~f:(fun (key, modifierKeys) (ast, s) ->
        updateMsg
          m
          h.hTLID
          ast
          (FluidKeyPress
             { key
             ; shiftKey = modifierKeys.shiftKey
             ; altKey = modifierKeys.altKey
             ; metaKey = modifierKeys.metaKey
             ; ctrlKey = modifierKeys.ctrlKey })
          s )
  in
  let process
      ~(debug : bool)
      ~(clone : bool)
      ~(wrap : bool)
      (keys : (K.key * modifierKeys) list)
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
    let ast = if wrap then if' (bool true) ast (int "5") else ast in
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
      Js.log2 "expr before" (eToStructure ~includeIDs:true s ast) ) ;
    let newAST, newState = processMsg keys s ast in
    let result =
      match newAST with
      | EIf (_, _, expr, _) when wrap ->
          expr
      | expr when not wrap ->
          expr
      | expr ->
          failwith ("the wrapper is broken: " ^ eToString s expr)
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
      if wrap
      then Option.map newState.selectionStart ~f:removeWrapperFromCaretPos
      else newState.selectionStart
    in
    let partialsFound =
      List.any (toTokens newState result) ~f:(fun ti ->
          match ti.token with
          | TRightPartial _ | TPartial _ | TFieldPartial _ ->
              true
          | _ ->
              false )
    in
    if debug
    then (
      Js.log2 "state after" (Fluid_utils.debugState newState) ;
      Js.log2 "expr after" (eToStructure ~includeIDs:true newState result) ) ;
    ( (eToString s result, (selPos, finalPos))
    , if partialsFound then ContainsPartial else NoPartial )
  in
  let render (expr : fluidExpr) : testResult =
    process ~wrap:true ~clone:false ~debug:false [] None 0 expr
  in
  let del
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( K.Delete
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  let bs
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( K.Backspace
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  let tab
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( K.Tab
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  let ctrlLeft
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( K.GoToStartOfWord
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  let ctrlRight
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( K.GoToEndOfWord
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  let shiftTab
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( K.ShiftTab
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  let space
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( K.Space
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  let enter
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( K.Enter
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  let key
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (key : K.key)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( key
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  let selectionPress
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (key : K.key)
      (selectionStart : int)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~clone
      ~debug
      [ ( key
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      (Some selectionStart)
      pos
      expr
  in
  let keys
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      ?(shiftHeld = false)
      (keys : K.key list)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process
      ~wrap
      ~debug
      ~clone
      (List.map
         ~f:(fun key ->
           ( key
           , { shiftKey = shiftHeld
             ; altKey = false
             ; metaKey = false
             ; ctrlKey = false } ) )
         keys)
      None
      pos
      expr
  in
  let modkeys
      ?(wrap = true)
      ?(debug = false)
      ?(clone = true)
      (keys : (K.key * modifierKeys) list)
      (pos : int)
      (expr : fluidExpr) : testResult =
    process ~wrap ~clone ~debug keys None pos expr
  in
  let ins
      ?(debug = false)
      ?(wrap = true)
      ?(clone = true)
      ?(shiftHeld = false)
      (char : char)
      (pos : int)
      (expr : fluidExpr) : testResult =
    let key = K.fromChar char in
    process
      ~wrap
      ~debug
      ~clone
      [ ( key
        , { shiftKey = shiftHeld
          ; altKey = false
          ; metaKey = false
          ; ctrlKey = false } ) ]
      None
      pos
      expr
  in
  (* Test expecting no partials found and an expected caret position but no selection *)
  let t
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      (expectedStr : string) =
    let insertCaret
        (((str, (_selection, caret)), res) :
          (string * (int option * int)) * hasPartial) : string * hasPartial =
      let caretString = "~" in
      match str |> String.splitAt ~index:caret with a, b ->
        ([a; b] |> String.join ~sep:caretString, res)
    in
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () ->
        expect (fn initial |> insertCaret) |> toEqual (expectedStr, NoPartial)
        )
  in
  (* Test expecting partials found and an expected caret position but no selection *)
  let tp
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      (expectedStr : string) =
    let insertCaret
        (((str, (_selection, caret)), res) :
          (string * (int option * int)) * hasPartial) : string * hasPartial =
      let caretString = "~" in
      match str |> String.splitAt ~index:caret with a, b ->
        ([a; b] |> String.join ~sep:caretString, res)
    in
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () ->
        expect (fn initial |> insertCaret)
        |> toEqual (expectedStr, ContainsPartial) )
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
      (fun () -> expect (fn initial) |> toEqual (expected, NoPartial))
  in
  describe "Strings" (fun () ->
      t "insert mid string" aStr (ins 'c' 3) "\"soc~me string\"" ;
      t "del mid string" aStr (del 3) "\"so~e string\"" ;
      t "bs mid string" aStr (bs 4) "\"so~e string\"" ;
      t "insert empty string" emptyStr (ins 'c' 1) "\"c~\"" ;
      t "del empty string" emptyStr (del 1) "~___" ;
      t "del empty string from outside" emptyStr (del 0) "~___" ;
      t "bs empty string" emptyStr (bs 1) "~___" ;
      t "bs outside empty string" emptyStr (bs 2) "\"~\"" ;
      t "bs near-empty string" oneCharStr (bs 2) "\"~\"" ;
      t "del near-empty string" oneCharStr (del 1) "\"~\"" ;
      t "insert outside string" aStr (ins 'c' 0) "~\"some string\"" ;
      t "del outside string" aStr (del 0) "~\"some string\"" ;
      t "bs outside string" aStr (bs 0) "~\"some string\"" ;
      t "insert start of string" aStr (ins 'c' 1) "\"c~some string\"" ;
      t "del start of string" aStr (del 1) "\"~ome string\"" ;
      t "bs start of string" aStr (bs 1) "~\"some string\"" ;
      t "insert end of string" aStr (ins 'c' 12) "\"some stringc~\"" ;
      t "del end of string" aStr (del 12) "\"some string~\"" ;
      t "bs end of string" aStr (bs 12) "\"some strin~\"" ;
      t "insert after end" aStr (ins 'c' 13) "\"some string\"~" ;
      t "del after end of string" aStr (del 13) "\"some string\"~" ;
      t "bs after end" aStr (bs 13) "\"some string~\"" ;
      t "insert space in string" aStr (ins ' ' 3) "\"so ~me string\"" ;
      t "del space in string" aStr (del 5) "\"some~string\"" ;
      t "bs space in string" aStr (bs 6) "\"some~string\"" ;
      t "final quote is swallowed" aStr (ins '"' 12) "\"some string\"~" ;
      t "insert backtick in string" aStr (key K.Backtick 1) "\"`~some string\"" ;
      t
        "ctrl+left from mid string goes front of word in string"
        aStr
        (ctrlLeft 12)
        "\"some ~string\"" ;
      t
        "ctrl+right from mid string goes end of word in string"
        aStr
        (ctrlRight 2)
        "\"some~ string\"" ;
      t
        "ctrl+left from beg of string goes front of next word in string"
        aStr
        (ctrlLeft 6)
        "\"~some string\"" ;
      t
        "ctrl+right goes end of word in string"
        aStr
        (ctrlRight 5)
        "\"some string~\"" ;
      t
        "DeletePrevWord at end of last word in string should only delete last word"
        aStr
        (key K.DeletePrevWord 12)
        "\"some ~\"" ;
      t
        "DeletePrevWord at beg of last word in string should delete first word"
        aStr
        (key K.DeletePrevWord 6)
        "\"~string\"" ;
      t
        "DeletePrevWord at beg of first word in string does nothing"
        aStr
        (key K.DeletePrevWord 1)
        "~\"some string\"" ;
      t
        "DeletePrevWord in the middle of first word in string only deletes in front of the cursor"
        aStr
        (key K.DeletePrevWord 3)
        "\"~me string\"" ;
      t
        "DeleteNextWord at end of last word in string moves cursor outside string"
        aStr
        (key K.DeleteNextWord 12)
        "\"some string\"~" ;
      t
        "DeleteNextWord at beg of last word in string should delete last word"
        aStr
        (key K.DeleteNextWord 6)
        "\"some ~\"" ;
      t
        "DeleteNextWord at beg of first word deletes first word"
        aStr
        (key K.DeleteNextWord 1)
        "\"~ string\"" ;
      t
        "DeleteNextWord in the middle of first word in string deletes to next whitespace"
        aStr
        (key K.DeleteNextWord 3)
        "\"so~ string\"" ;
      () ) ;
  describe "Multi-line Strings" (fun () ->
      t
        "insert into start string"
        mlStr
        (ins 'c' 3)
        ( "\"12c~3456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      t
        "insert into middle string"
        mlStr
        (ins 'c' 44 (* quote + 2 + newline *))
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12c~3456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      t
        "insert into end string"
        mlStr
        (ins 'c' 85 (* quote + 2 + newline*2 *))
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12c~3456789_\"" ) ;
      t
        "del mid start string"
        mlStr
        (del 3)
        ( "\"12~456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      t
        "del mid middle string"
        mlStr
        (del 44)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12~456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      t
        "del mid end string"
        mlStr
        (del 85)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12~456789_\"" ) ;
      t
        "bs mid start string"
        mlStr
        (bs 4)
        ( "\"12~456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      t
        "bs mid middle string"
        mlStr
        (bs 45)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12~456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      t
        "bs mid end string"
        mlStr
        (bs 86)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "12~456789_\"" ) ;
      t
        "insert outside string"
        mlStr
        (ins 'c' 0)
        ( "~\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      t
        "del outside string"
        mlStr
        (del 0)
        ( "~\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      t
        "bs outside string"
        mlStr
        (bs 0)
        ( "~\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      t
        "insert start of start string"
        mlStr
        (ins 'c' 1)
        ( "\"c~123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      t
        "insert start of middle string"
        mlStr
        (ins 'c' 42)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "c~123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      t
        "insert start of end string"
        mlStr
        (ins 'c' 83)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "c~123456789_\"" ) ;
      t
        "del start of start string"
        mlStr
        (del 1)
        ( "\"~23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      t
        "del start of middle string"
        mlStr
        (del 42)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "~23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      t
        "del start of end string"
        mlStr
        (del 83)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "~23456789_\"" ) ;
      t
        "bs start of start string"
        mlStr
        (bs 1)
        ( "~\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      t
        "bs start of middle string"
        mlStr
        (bs 42)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      t
        "bs start of end string"
        mlStr
        (bs 83)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ "123456789_\"" ) ;
      t
        "insert end of start string"
        mlStr
        (ins 'c' 41)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\nc~"
        ^ "123456789_abcdefghi,123456789_abcdefghi\n,"
        ^ "123456789_\"" ) ;
      t
        "insert end of middle string"
        mlStr
        (ins 'c' 82)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\nc~"
        ^ "123456789_\"" ) ;
      t
        "insert end of end string"
        mlStr
        (ins 'c' 93)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_c~\"" ) ;
      t
        "string converts to ml string"
        (str mlSegment)
        (ins 'c' 41)
        "\"123456789_abcdefghi,123456789_abcdefghi,\nc~\"" ;
      t
        "indented string converts to ml string"
        (if' (str mlSegment) b b)
        (ins 'c' 44)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   c~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      t
        "insert end of indented start string"
        (if' (str (mlSegment ^ mlSegment)) b b)
        (ins 'c' 44)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   c~123456789_abcdefghi,123456789_abcdefghi\n"
        ^ "   ,\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      t
        "insert end of indented end string"
        (if' (str (mlSegment ^ mlSegment)) b b)
        (ins 'c' 88)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   c~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      t
        "del end of start string"
        mlStr
        (del 41)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"" ) ;
      t
        "del end of middle string"
        mlStr
        (del 82)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ "123456789_\"" ) ;
      t
        "del end of end string"
        mlStr
        (del 93)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_~\"" ) ;
      t
        "bs end of start string"
        mlStr
        (bs 41)
        ( "\"123456789_abcdefghi,123456789_abcdefghi"
        ^ "~1\n23456789_abcdefghi,123456789_abcdefghi,"
        ^ "1\n23456789_\"" ) ;
      t
        "bs end of middle string"
        mlStr
        (bs 82)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi"
        ^ "~1\n23456789_\"" ) ;
      t
        "bs end of end string"
        mlStr
        (bs 93)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789~\"" ) ;
      t
        "insert after end of end string"
        mlStr
        (ins 'c' 94)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"~" ) ;
      t
        "del after end of end string"
        mlStr
        (del 94)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"~" ) ;
      t
        "bs after end of end string"
        mlStr
        (bs 94)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_~\"" ) ;
      (* Skipped insert, del, bs of space, as it doesn't seem interesting *)
      t
        "final quote is swallowed"
        mlStr
        (ins '"' 93)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "123456789_\"~" ) ;
      t
        "bs, 3 lines to 2, end"
        (if' (str (mlSegment ^ mlSegment ^ "c")) b b)
        (bs 93)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   123456789_abcdefghi,123456789_abcdefghi,~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      t
        "bs, 2 lines to 1, end"
        (if' (str (mlSegment ^ "c")) b b)
        (bs 49)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      t
        "del, 3 lines to 2, end"
        (if' (str (mlSegment ^ mlSegment ^ "c")) b b)
        (del 92)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "   123456789_abcdefghi,123456789_abcdefghi,~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      t
        "del, 2 lines to 1, end"
        (if' (str (mlSegment ^ "c")) b b)
        (del 48)
        ( "if \"123456789_abcdefghi,123456789_abcdefghi,~\"\n"
        ^ "then\n  ___\nelse\n  ___" ) ;
      t
        "ctrl+left at beg of start string moves to beg"
        mlStrWSpace
        (ctrlLeft 6)
        ( "\"~123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ " 123456789_ abcdefghi, 123456789_ abcdef\n"
        ^ "ghi,\"" ) ;
      t
        "ctrl+left at beg of middle string moves to beg"
        mlStrWSpace
        (ctrlLeft 54)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ " ~123456789_ abcdefghi, 123456789_ abcdef\n"
        ^ "ghi,\"" ) ;
      t
        "ctrl+left at beg of end string moves to beg"
        mlStrWSpace
        (ctrlLeft 76)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ " 123456789_ abcdefghi, ~123456789_ abcdef\n"
        ^ "ghi,\"" ) ;
      t
        "ctrl+right at beg of start string moves to end"
        mlStrWSpace
        (ctrlRight 0)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ " 123456789_ abcdefghi, 123456789_ abcdef\n"
        ^ "ghi,\"" ) ;
      t
        "ctrl+right at beg of middle string moves to end"
        mlStrWSpace
        (ctrlRight 46)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ " 123456789_~ abcdefghi, 123456789_ abcdef\n"
        ^ "ghi,\"" ) ;
      t
        "ctrl+right at beg of end string moves to end"
        mlStrWSpace
        (ctrlRight 76)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ " 123456789_ abcdefghi, 123456789_ abcdef~\n"
        ^ "ghi,\"" ) ;
      t
        "DeletePrevWord at the end of line deletes word in front"
        mlStrWSpace
        (key K.DeletePrevWord 82)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ " 123456789_ abcdefghi, 123456789_ ~ghi,\"" ) ;
      t
        "DeletePrevWord at the beg of line goes to end of line above "
        mlStrWSpace
        (key K.DeletePrevWord 42)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,~\n"
        ^ " 123456789_ abcdefghi, 123456789_ abcdef\n"
        ^ "ghi,\"" ) ;
      t
        "DeleteNextWord at the end of line deletes up to the next whitespace"
        mlStrWSpace
        (key K.DeleteNextWord 82)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ " 123456789_ abcdefghi, 123456789_ abcdef~\"" ) ;
      t
        "DeleteNextWord at the beg of line deletes until the next whitespace"
        mlStrWSpace
        (key K.DeleteNextWord 42)
        ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
        ^ "~ abcdefghi, 123456789_ abcdefghi,\"" ) ;
      t
        "adding a quote at the front turns a partial into a string"
        (partial "abcdefgh\"" b)
        (ins '"' 0)
        "\"~abcdefgh\"" ;
      t
        "adding a quote at the back turns a partial into a string"
        (partial "\"abcdefgh" b)
        (ins '"' 9)
        "\"abcdefgh\"~" ;
      tp
        "just one quote doesn't turn a partial into a string"
        (partial "abcdefgh" b)
        (ins '"' 0)
        "\"~abcdefgh" ;
      () ) ;
  describe "Integers" (fun () ->
      t "insert 0 at front " anInt (ins '0' 0) "~12345" ;
      t "insert at end of short" aShortInt (ins '2' 1) "12~" ;
      t "insert not a number" anInt (ins 'c' 0) "~12345" ;
      t "insert start of number" anInt (ins '5' 0) "5~12345" ;
      t "del start of number" anInt (del 0) "~2345" ;
      t "bs start of number" anInt (bs 0) "~12345" ;
      t "insert end of number" anInt (ins '0' 5) "123450~" ;
      t "del end of number" anInt (del 5) "12345~" ;
      t "bs end of number" anInt (bs 5) "1234~" ;
      t "insert number at scale" aHugeInt (ins '9' 5) "200009~0000000000000" ;
      t "insert number at scale" aHugeInt (ins '9' 0) "9~20000000000000000" ;
      t "insert number at scale" aHugeInt (ins '9' 19) "2000000000000000000~" ;
      t
        "insert number at scale"
        oneShorterThanMax62BitInt
        (ins '3' 18)
        "4611686018427387903~" ;
      t
        "insert number at scale"
        oneShorterThanMax62BitInt
        (ins '4' 18)
        "461168601842738790~" ;
      t
        "ctrl+left go to beg of int moves to beg"
        oneShorterThanMax62BitInt
        (ctrlLeft 11)
        "~461168601842738790" ;
      t
        "ctrl+right go to end of int moves to end"
        oneShorterThanMax62BitInt
        (ctrlRight 11)
        "461168601842738790~" ;
      t
        "DeletePrevWord in the middle of an int deletes all the nums in front of cursor"
        oneShorterThanMax62BitInt
        (key K.DeletePrevWord 11)
        "~2738790" ;
      t
        "DeletePrevWord at the end of an int deletes it all"
        oneShorterThanMax62BitInt
        (key K.DeletePrevWord 18)
        "~___" ;
      () ) ;
  describe "Floats" (fun () ->
      t "insert . converts to float - end" anInt (ins '.' 5) "12345.~" ;
      t "insert . converts to float - middle" anInt (ins '.' 3) "123.~45" ;
      t "insert . converts to float - start" anInt (ins '.' 0) "~12345" ;
      t "insert . converts to float - short" aShortInt (ins '.' 1) "1.~" ;
      t "continue after adding dot" aPartialFloat (ins '2' 2) "1.2~" ;
      t "insert zero in whole - start" aFloat (ins '0' 0) "~123.456" ;
      t "insert int in whole - start" aFloat (ins '9' 0) "9~123.456" ;
      t "insert int in whole - middle" aFloat (ins '0' 1) "10~23.456" ;
      t "insert int in whole - end" aFloat (ins '0' 3) "1230~.456" ;
      t "insert int in fraction - start" aFloat (ins '0' 4) "123.0~456" ;
      t "insert int in fraction - middle" aFloat (ins '0' 6) "123.450~6" ;
      t "insert int in fraction - end" aFloat (ins '0' 7) "123.4560~" ;
      t "insert non-int in whole" aFloat (ins 'c' 2) "12~3.456" ;
      t "insert non-int in fraction" aFloat (ins 'c' 6) "123.45~6" ;
      t "del dot" aFloat (del 3) "123~456" ;
      t "del dot at scale" aHugeFloat (del 9) "123456789~123456789" ;
      t "del dot at limit1" maxPosIntWithDot (del 16) "4611686018427387~903" ;
      t
        "del dot at limit2"
        maxPosIntPlus1WithDot
        (del 16)
        "4611686018427387~90" ;
      t "del start of whole" aFloat (del 0) "~23.456" ;
      t "del middle of whole" aFloat (del 1) "1~3.456" ;
      t "del end of whole" aFloat (del 2) "12~.456" ;
      t "del start of fraction" aFloat (del 4) "123.~56" ;
      t "del middle of fraction" aFloat (del 5) "123.4~6" ;
      t "del end of fraction" aFloat (del 6) "123.45~" ;
      t "del dot converts to int" aFloat (del 3) "123~456" ;
      t "del dot converts to int, no fraction" aPartialFloat (del 1) "1~" ;
      t "bs dot" aFloat (bs 4) "123~456" ;
      t "bs dot at scale" aHugeFloat (bs 10) "123456789~123456789" ;
      t "bs dot at limit1" maxPosIntWithDot (bs 17) "4611686018427387~903" ;
      t "bs dot at limit2" maxPosIntPlus1WithDot (bs 17) "4611686018427387~90" ;
      t "bs start of whole" aFloat (bs 1) "~23.456" ;
      t "bs middle of whole" aFloat (bs 2) "1~3.456" ;
      t "bs end of whole" aFloat (bs 3) "12~.456" ;
      t "bs start of fraction" aFloat (bs 5) "123.~56" ;
      t "bs middle of fraction" aFloat (bs 6) "123.4~6" ;
      t "bs end of fraction" aFloat (bs 7) "123.45~" ;
      t "bs dot converts to int" aFloat (bs 4) "123~456" ;
      t "bs dot converts to int, no fraction" aPartialFloat (bs 2) "1~" ;
      t "continue after adding dot" aPartialFloat (ins '2' 2) "1.2~" ;
      t "ctrl+left start of whole moves to beg" aFloat (ctrlLeft 0) "~123.456" ;
      t "ctrl+left middle of whole moves to beg" aFloat (ctrlLeft 1) "~123.456" ;
      t "ctrl+left end of whole moves to beg" aFloat (ctrlLeft 2) "~123.456" ;
      t
        "ctrl+left start of fraction moves to beg"
        aFloat
        (ctrlLeft 4)
        "123~.456" ;
      t
        "ctrl+left middle of fraction moves to beg"
        aFloat
        (ctrlLeft 5)
        "123.~456" ;
      t "ctrl+left end of fraction moves to beg" aFloat (ctrlLeft 6) "123.~456" ;
      t
        "ctrl+right start of whole moves to end"
        aFloat
        (ctrlRight 0)
        "123~.456" ;
      t
        "ctrl+right middle of whole moves to end"
        aFloat
        (ctrlRight 1)
        "123~.456" ;
      t "ctrl+right end of whole moves to end" aFloat (ctrlRight 2) "123~.456" ;
      t "ctrl+right end of whole moves to end" aFloat (ctrlRight 3) "123.~456" ;
      t
        "ctrl+right start of fraction moves to end"
        aFloat
        (ctrlRight 4)
        "123.456~" ;
      t
        "ctrl+right middle of fraction moves to end"
        aFloat
        (ctrlRight 5)
        "123.456~" ;
      t
        "ctrl+right end of fraction moves to end"
        aFloat
        (ctrlRight 6)
        "123.456~" ;
      t
        "DeletePrevWord in the middle of an fraction deletes all the nums in front of cursor up to the ."
        aFloat
        (key K.DeletePrevWord 6)
        "123.~6" ;
      t
        "DeletePrevWord in the middle of a whole deletes all the nums in front of cursor"
        aFloat
        (key K.DeletePrevWord 2)
        "~3.456" ;
      t
        "DeletePrevWord in the end of a fraction deletes all the nums in up to the ."
        aFloat
        (key K.DeletePrevWord 7)
        "123.~" ;
      t
        "DeletePrevWord in the end of a whole deletes all the nums in front of cursor"
        aFloat
        (key K.DeletePrevWord 3)
        "~.456" ;
      t
        "DeleteNextWord in the middle of an fraction deletes all the nums after the cursor"
        aFloat
        (key K.DeleteNextWord 6)
        "123.45~" ;
      t
        "DeleteNextWord in the middle of a whole deletes all the nums in front of the ."
        aFloat
        (key K.DeleteNextWord 2)
        "12~.456" ;
      t
        "DeleteNextWord in the end of a fraction does nothing"
        aFloat
        (key K.DeleteNextWord 7)
        "123.456~" ;
      t
        "DeleteNextWord in the end of a whole deletes dot"
        aFloat
        (key K.DeleteNextWord 3)
        "123~456" ;
      () ) ;
  describe "Bools" (fun () ->
      tp "insert start of true" trueBool (ins 'c' 0) "c~true" ;
      tp "del start of true" trueBool (del 0) "~rue" ;
      t "bs start of true" trueBool (bs 0) "~true" ;
      tp "insert end of true" trueBool (ins '0' 4) "true0~" ;
      t "del end of true" trueBool (del 4) "true~" ;
      tp "bs end of true" trueBool (bs 4) "tru~" ;
      tp "insert middle of true" trueBool (ins '0' 2) "tr0~ue" ;
      tp "del middle of true" trueBool (del 2) "tr~e" ;
      tp "bs middle of true" trueBool (bs 2) "t~ue" ;
      tp "insert start of false" falseBool (ins 'c' 0) "c~false" ;
      tp "del start of false" falseBool (del 0) "~alse" ;
      t "bs start of false" falseBool (bs 0) "~false" ;
      tp "insert end of false" falseBool (ins '0' 5) "false0~" ;
      t "del end of false" falseBool (del 5) "false~" ;
      tp "bs end of false" falseBool (bs 5) "fals~" ;
      tp "insert middle of false" falseBool (ins '0' 2) "fa0~lse" ;
      tp "del middle of false" falseBool (del 2) "fa~se" ;
      tp "bs middle of false" falseBool (bs 2) "f~lse" ;
      t "ctrl+left start of true doesnt move" trueBool (ctrlLeft 0) "~true" ;
      t "ctrl+right start of true moves to beg" trueBool (ctrlRight 0) "true~" ;
      t "ctrl+left middle of true moves to beg" trueBool (ctrlLeft 2) "~true" ;
      t "ctrl+left end of true moves to bed" trueBool (ctrlLeft 4) "~true" ;
      t "ctrl+right end of true doesnt move" trueBool (ctrlRight 4) "true~" ;
      t "ctrl+right middle of true moves to end" trueBool (ctrlRight 2) "true~" ;
      t "ctrl+left start of false doesnt move" falseBool (ctrlLeft 0) "~false" ;
      t
        "ctrl+right start of false moves to end"
        falseBool
        (ctrlRight 0)
        "false~" ;
      t "ctrl+left end of false moves to beg" falseBool (ctrlLeft 5) "~false" ;
      t "ctrl+right end of false moves to end" falseBool (ctrlRight 5) "false~" ;
      t
        "ctrl+left middle of false moves to beg"
        falseBool
        (ctrlLeft 2)
        "~false" ;
      t
        "ctrl+right middle of false moves to end"
        falseBool
        (ctrlRight 2)
        "false~" ;
      t
        "DeletePrevWord at the end of a true deletes entire true"
        trueBool
        (key K.DeletePrevWord 4)
        "~___" ;
      t
        "DeletePrevWord at the end of a false deletes entire false"
        falseBool
        (key K.DeletePrevWord 5)
        "~___" ;
      tp
        "DeletePrevWord at the mid of a true deletes to beg"
        trueBool
        (key K.DeletePrevWord 2)
        "~ue" ;
      tp
        "DeletePrevWord at the mid of a false deletes to beg"
        falseBool
        (key K.DeletePrevWord 3)
        "~se" ;
      t
        "DeleteNextWord at the end of a true does nothing"
        trueBool
        (key K.DeleteNextWord 4)
        "true~" ;
      t
        "DeleteNextWord at the end of a false does nothing"
        falseBool
        (key K.DeleteNextWord 5)
        "false~" ;
      tp
        "DeleteNextWord at the mid of a true deletes to end"
        trueBool
        (key K.DeleteNextWord 2)
        "tr~" ;
      tp
        "DeleteNextWord at the mid of a false deletes to end"
        falseBool
        (key K.DeleteNextWord 3)
        "fal~" ;
      () ) ;
  describe "Nulls" (fun () ->
      tp "insert start of null" aNull (ins 'c' 0) "c~null" ;
      tp "del start of null" aNull (del 0) "~ull" ;
      t "bs start of null" aNull (bs 0) "~null" ;
      t "ctrl+left start of null doesnt move" aNull (ctrlLeft 0) "~null" ;
      t "ctrl+right start of null moves to end" aNull (ctrlRight 0) "null~" ;
      tp "insert end of null" aNull (ins '0' 4) "null0~" ;
      t "del end of null" aNull (del 4) "null~" ;
      tp "bs end of null" aNull (bs 4) "nul~" ;
      t "ctrl+left end of null doesnt move" aNull (ctrlLeft 4) "~null" ;
      t "ctrl+right end of null moves to beg" aNull (ctrlRight 4) "null~" ;
      tp "insert middle of null" aNull (ins '0' 2) "nu0~ll" ;
      tp "del middle of null" aNull (del 2) "nu~l" ;
      tp "bs middle of null" aNull (bs 2) "n~ll" ;
      t "ctrl+left middle of null moves to beg" aNull (ctrlLeft 2) "~null" ;
      t "ctrl+right middle of null moves to end" aNull (ctrlRight 2) "null~" ;
      t
        "DeletePrevWord at the end of a null deletes entire null"
        aNull
        (key K.DeletePrevWord 4)
        "~___" ;
      tp
        "DeletePrevWord at the mid of a null deletes to beg"
        aNull
        (key K.DeletePrevWord 2)
        "~ll" ;
      t
        "DeleteNextWord at the end of a null does nothing"
        aNull
        (key K.DeleteNextWord 4)
        "null~" ;
      tp
        "DeleteNextWord at the mid of a null deletes to end"
        aNull
        (key K.DeleteNextWord 2)
        "nu~" ;
      () ) ;
  describe "Blanks" (fun () ->
      t "insert middle of blank->string" b (ins '"' 3) "\"~\"" ;
      t "del middle of blank->blank" b (del 3) "___~" ;
      t "bs middle of blank->blank" b (bs 3) "~___" ;
      t "ctrl+left middle of null moves to beg" b (ctrlLeft 2) "~___" ;
      t "ctrl+right middle of null moves to end" b (ctrlRight 2) "___~" ;
      t "insert blank->string" b (ins '"' 0) "\"~\"" ;
      t "del blank->string" emptyStr (del 0) "~___" ;
      t "bs blank->string" emptyStr (bs 1) "~___" ;
      t "insert blank->int" b (ins '5' 0) "5~" ;
      t "insert blank->int" b (ins '0' 0) "0~" ;
      t "del int->blank " five (del 0) "~___" ;
      t "bs int->blank " five (bs 1) "~___" ;
      t "insert end of blank->int" b (ins '5' 1) "5~" ;
      tp "insert partial" b (ins 't' 0) "t~" ;
      t
        "backspacing your way through a partial finishes"
        trueBool
        (keys [K.Backspace; K.Backspace; K.Backspace; K.Backspace; K.Left] 4)
        "~___" ;
      t "insert blank->space" b (space 0) "~___" ;
      t
        "DeletePrevWord at the end of a blank does nothing"
        b
        (key K.DeletePrevWord 3)
        "~___" ;
      t
        "DeleteNextWord at the end of a blank does nothing"
        b
        (key K.DeleteNextWord 3)
        "___~" ;
      () ) ;
  describe "Fields" (fun () ->
      tp "insert middle of fieldname" aField (ins 'c' 5) "obj.fc~ield" ;
      t "cant insert invalid chars fieldname" aField (ins '$' 5) "obj.f~ield" ;
      tp "del middle of fieldname" aField (del 5) "obj.f~eld@" ;
      tp "del fieldname" aShortField (del 4) "obj.~***" ;
      tp "bs fieldname" aShortField (bs 5) "obj.~***" ;
      tp "insert end of fieldname" aField (ins 'c' 9) "obj.fieldc~" ;
      tp "insert end of varname" aField (ins 'c' 3) "objc~.field" ;
      tp "insert start of fieldname" aField (ins 'c' 4) "obj.c~field" ;
      tp "insert blank fieldname" aBlankField (ins 'c' 4) "obj.c~" ;
      t "del fieldop with name" aShortField (del 3) "obj~" ;
      t "bs fieldop with name" aShortField (bs 4) "obj~" ;
      t "del fieldop with blank" aBlankField (del 3) "obj~" ;
      t "bs fieldop with blank" aBlankField (bs 4) "obj~" ;
      t "del fieldop in nested" aNestedField (del 3) "obj~.field2" ;
      t "bs fieldop in nested" aNestedField (bs 4) "obj~.field2" ;
      tp "add dot after variable" aVar (ins '.' 8) "variable.~***" ;
      tp "add dot after partial " aPartialVar (ins '.' 3) "request.~***" ;
      tp "add dot after field" aField (ins '.' 9) "obj.field.~***" ;
      t "insert space in blank " aBlankField (space 4) "obj.~***" ;
      t
        "ctrl+left in name moves to beg of name"
        aShortField
        (ctrlLeft 2)
        "~obj.f" ;
      t
        "ctrl+right in name moves to end of name"
        aShortField
        (ctrlRight 2)
        "obj~.f" ;
      t
        "ctrl+left in beg of fieldname moves to beg of fieldname"
        aNestedField
        (ctrlLeft 4)
        "~obj.field.field2" ;
      t
        "ctrl+right in beg of fieldname moves to the end of fieldname"
        aNestedField
        (ctrlRight 4)
        "obj.field~.field2" ;
      t
        "ctrl+left in middle of fieldname moves to end of fieldname"
        aNestedField
        (ctrlLeft 5)
        "obj.~field.field2" ;
      t
        "ctrl+right in middle of fieldname moves to beg of fieldname"
        aNestedField
        (ctrlRight 5)
        "obj.field~.field2" ;
      tp
        "DeletePrevWord in middle of fieldname deletes to beg of fieldname"
        aNestedField
        (key K.DeletePrevWord 6)
        "obj.~eld@@.field2" ;
      tp
        "DeletePrevWord at end of fieldname deletes entire fieldname"
        aNestedField
        (key K.DeletePrevWord 9)
        "obj.~***@@.field2" ;
      t
        "DeletePrevWord at end of dot deletes fieldname"
        aNestedField
        (key K.DeletePrevWord 4)
        "obj~.field2" ;
      tp
        "DeleteNextWord in middle of fieldname deletes to end of fieldname"
        aNestedField
        (key K.DeleteNextWord 6)
        "obj.fi~@l@.field2" ;
      t
        "DeleteNextWord at end of fieldname deletes next fieldname"
        aNestedField
        (key K.DeleteNextWord 9)
        "obj.field~" ;
      tp
        "DeleteNextWord at end of dot deletes fieldname"
        aNestedField
        (key K.DeleteNextWord 4)
        "obj.~***@@.field2" ;
      tp
        "insert dot to complete partial field"
        (EPartial
           ( gid ()
           , "body"
           , EFieldAccess (gid (), EVariable (ID "12", "request"), gid (), "")
           ))
        (ins ~clone:false '.' 11)
        "request.body.~***" ;
      tp
        "insert dot even when no content in the field"
        (EVariable (ID "12", "request"))
        (keys ~clone:false [K.Period; K.Period] 7)
        "request.body.~***" ;
      tp
        "bs fieldpartial character"
        (partial "a" (fieldAccess b ""))
        (bs 5)
        "___.~***" ;
      tp
        "del fieldpartial character"
        (partial "a" (fieldAccess b ""))
        (del 4)
        "___.~***" ;
      () ) ;
  describe "Functions" (fun () ->
      t
        "space on a sep goes to next arg"
        aFnCall
        (space 10)
        "Int::add 5 ~_________" ;
      tp "bs function renames" aFnCall (bs 8) "Int::ad~@ 5 _________" ;
      tp "deleting a function renames" aFnCall (del 7) "Int::ad~@ 5 _________" ;
      t
        "renaming a function maintains unaligned params in let scope"
        (partial "Int::" (fn "Int::add" [five; six]))
        (keys [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        "let b = 6\n~Int::sqrt 5" ;
      t
        "renaming a function doesn't maintain unaligned params if they're already set to variables"
        (partial "Int::" (fn "Int::add" [var "a"; var "b"]))
        (keys [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        "Int::sqrt ~a" ;
      t
        "renaming a function doesn't maintain unaligned params if they're not set (blanks)"
        (partial "Int::" (fn "Int::add" [b; b]))
        (keys [K.Letter 's'; K.Letter 'q'; K.Enter] 5)
        "Int::sqrt ~_________" ;
      (* TODO: functions are not implemented fully. I deld bs and
       * del because we were switching to partials, but this isn't
       * implemented. Some tests we need:
         * myFunc arg1 arg2, 6 => Backspace => myFun arg1 arg2, with a ghost and a partial.
         * same with del *)
      tp
        "del on function with version"
        aFnCallWithVersion
        (del 11)
        "DB::getAllv~@ ___________________" ;
      tp
        "bs on function with version"
        aFnCallWithVersion
        (bs 12)
        "DB::getAllv~@ ___________________" ;
      tp
        "del on function with version in between the version and function name"
        aFnCallWithVersion
        (del 10)
        "DB::getAll~1@ ___________________" ;
      tp
        "bs on function with version in between the version and function name"
        aFnCallWithVersion
        (bs 10)
        "DB::getAl~v1@ ___________________" ;
      tp
        "del on function with version in function name"
        aFnCallWithVersion
        (del 7)
        "DB::get~llv1@ ___________________" ;
      tp
        "bs on function with version in function name"
        aFnCallWithVersion
        (bs 8)
        "DB::get~llv1@ ___________________" ;
      t
        "adding function with version goes to the right place"
        b
        (keys [K.Letter 'd'; K.Letter 'b'; K.Enter] 0)
        "DB::getAllv1 ~___________________" ;
      t
        "backspacing a fn arg's separator goes to the right place"
        (fn "Int::add" [five; six])
        (bs 11)
        "Int::add 5~ 6" ;
      tp
        "DeletePrevWord in middle of function deletes to beg of function"
        aFnCallWithVersion
        (key K.DeletePrevWord 6)
        "~tAllv1@Allv@ ___________________" ;
      tp
        "DeletePrevWord in end of function version deletes to function"
        aFnCallWithVersion
        (key K.DeletePrevWord 12)
        "DB::getAll~@@ ___________________" ;
      tp
        "DeleteNextWord in middle of function deletes to beg of function"
        aFnCallWithVersion
        (key K.DeleteNextWord 6)
        "DB::ge~v1@lv@ ___________________" ;
      t
        "DeleteNextWord in end of function version moves cursor to end of blank "
        aFnCallWithVersion
        (key K.DeleteNextWord 12)
        "DB::getAllv1 ___________________~" ;
      let string40 = "0123456789abcdefghij0123456789abcdefghij" in
      let string80 = string40 ^ string40 in
      let string160 = string80 ^ string80 in
      t
        "reflows work for functions"
        (fn
           "HttpClient::post_v4"
           [str string40; record [(string80, b)]; emptyRecord; emptyRecord])
        render
        "~HttpClient::postv4\n  \"0123456789abcdefghij0123456789abcdefghij\"\n  {\n    0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij : ___\n  }\n  {}\n  {}" ;
      t
        "reflows work for functions with long strings"
        (fn "HttpClient::post_v4" [str string160; b; b; b])
        render
        "~HttpClient::postv4\n  \"0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\"\n  ____________\n  ______________\n  ________________" ;
      tp
        "reflows work for partials too "
        (partial "TEST" (fn "HttpClient::post_v4" [str string160; b; b; b]))
        render
        "~TEST@lient::postv@\n  \"0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\n  0123456789abcdefghij0123456789abcdefghij\"\n  ____________\n  ______________\n  ________________" ;
      t
        "reflows happen for functions whose arguments have newlines"
        (fn "HttpClient::post_v4" [emptyStr; emptyRowRecord; b; b])
        render
        "~HttpClient::postv4\n  \"\"\n  {\n    *** : ___\n  }\n  ______________\n  ________________" ;
      t
        "reflows don't happen for functions whose only newline is in the last argument"
        (fn "HttpClient::post_v4" [emptyStr; b; b; emptyRowRecord])
        render
        "~HttpClient::postv4 \"\" ____________ ______________ {\n                                                    *** : ___\n                                                  }" ;
      tp
        "reflows put the caret in the right place on insert"
        (let justShortEnoughNotToReflow =
           "abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij01"
         in
         fn
           "HttpClient::post_v4"
           [emptyStr; emptyRecord; emptyRecord; var justShortEnoughNotToReflow])
        (ins ~wrap:false 'x' 120)
        "HttpClient::postv4\n  \"\"\n  {}\n  {}\n  abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcde~fghij01x"
      (* TODO: This should be 129, but reflow puts the caret in the wrong
           * place for new partials *) ;
      tp
        "reflows put the caret in the right place on bs"
        (let justLongEnoughToReflow =
           "abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij012"
         in
         fn
           "HttpClient::post_v4"
           [emptyStr; emptyRecord; emptyRecord; var justLongEnoughToReflow])
        (bs ~wrap:false 129)
        "HttpClient::postv4 \"\" {} {} abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij01~"
      (* TODO: This should be 120, but reflow puts the caret in the wrong
           * place for new partials *) ;
      t
        "ctrl+left on function in middle of version moves to beg of version"
        aFnCallWithVersion
        (ctrlLeft 11)
        "DB::getAll~v1 ___________________" ;
      t
        "ctrl+right on function in middle of version moves to end of version"
        aFnCallWithVersion
        (ctrlRight 11)
        "DB::getAllv1~ ___________________" ;
      t
        "ctrl+left on function in middle of function name moves to beg of fn name"
        aFnCallWithVersion
        (ctrlLeft 7)
        "~DB::getAllv1 ___________________" ;
      t
        "ctrl+right on function in middle of function name moves to end of fn name"
        aFnCallWithVersion
        (ctrlRight 7)
        "DB::getAll~v1 ___________________" ;
      () ) ;
  describe "Binops" (fun () ->
      tp "pipe key starts partial" trueBool (key K.Pipe 4) "true |~" ;
      t
        "pressing enter completes partial"
        trueBool
        (keys [K.Pipe; K.Down; K.Enter] 4)
        "true ||~ __________" ;
      t
        "pressing space completes partial"
        trueBool
        (keys [K.Pipe; K.Down; K.Space] 4)
        "true || ~__________" ;
      tp "pressing plus key starts partial" trueBool (key K.Plus 4) "true +~" ;
      tp "pressing caret key starts partial" anInt (key K.Caret 5) "12345 ^~" ;
      t
        "pressing pipe twice then space completes partial"
        trueBool
        (keys [K.Pipe; K.Pipe; K.Space] 4)
        "true || ~__________" ;
      t
        "piping into newline creates pipe"
        trueBool
        (keys [K.Pipe; K.GreaterThan; K.Space] 4)
        "true\n|>~___\n" ;
      t
        "pressing bs to clear partial reverts for blank rhs"
        (partial "|" (binop "||" anInt b))
        (bs 7)
        "12345~" ;
      t
        "pressing bs to clear partial reverts for blank rhs, check lhs pos goes to start"
        (partial "|" (binop "||" b b))
        (bs 12)
        "~___" ;
      t
        "pressing del to clear partial reverts for blank rhs"
        (partial "|" (binop "||" anInt b))
        (del 6)
        "12345~" ;
      t
        "pressing del to clear partial reverts for blank rhs, check lhs pos goes to start"
        (partial "|" (binop "||" b b))
        (del 11)
        "~___" ;
      t
        "using bs to remove an infix with a placeholder goes to right place"
        (partial "|" (binop "||" b b))
        (bs 12)
        "~___" ;
      t
        "using bs to remove an infix with a placeholder goes to right place 2"
        (partial "|" (binop "||" five b))
        (bs 3)
        "5~" ;
      t
        "pressing bs to clear rightpartial reverts for blank rhs"
        (rightPartial "|" b)
        (bs 5)
        "~___" ;
      t
        "pressing bs on single digit binop leaves lhs"
        (binop "+" anInt anInt)
        (bs 7)
        "12345~" ;
      t
        "using del to remove an infix with a placeholder goes to right place"
        (partial "|" (binop "||" b b))
        (del 11)
        "~___" ;
      t
        "pressing del to clear rightpartial reverts for blank rhs"
        (rightPartial "|" b)
        (del 4)
        "~___" ;
      t
        "pressing del on single digit binop leaves lhs"
        (binop "+" anInt anInt)
        (del 6)
        "12345~" ;
      t
        "pressing del to remove a string binop combines lhs and rhs"
        (binop "++" (str "five") (str "six"))
        (keys [K.Delete; K.Delete] 7)
        "\"five~six\"" ;
      t
        "pressing backspace to remove a string binop combines lhs and rhs"
        (binop "++" (str "five") (str "six"))
        (keys [K.Backspace; K.Backspace] 9)
        "\"five~six\"" ;
      t
        "pressing letters and numbers on a partial completes it"
        b
        (keys [K.Number '5'; K.Plus; K.Number '5'] 0)
        "5 + 5~" ;
      tp
        "pressing pipe while editing a partial works properly"
        (partial "|" (binop "||" anInt anInt))
        (key K.Pipe 7)
        "12345 ||~ 12345" ;
      tp
        "pressing = after < should go to partial"
        (binop "<" anInt anInt)
        (key K.Equals 7)
        "12345 <=~ 12345" ;
      t
        "changing binop to fn should work"
        (partial "Int::add" (binop "+" anInt anInt))
        (keys [K.Enter] 14)
        "Int::add 12345 ~12345" ;
      t
        "changing fn to binops should work"
        (partial "+" (fn "Int::add" [anInt; anInt]))
        (keys [K.Enter] 1)
        "~12345 + 12345" ;
      t
        "changing binop should work"
        (binop "<" anInt anInt)
        (keys [K.Equals; K.Enter] 7)
        "12345 <=~ 12345" ;
      tp
        "adding binop in `if` works"
        (if' b b b)
        (key K.Percent 3)
        "if %~\nthen\n  ___\nelse\n  ___" ;
      tp "show ghost partial" aFullBinOp (bs 8) "myvar |~@ 5" ;
      t
        "ctrl+left from end of < moves to front of <"
        (binop "<" anInt anInt)
        (ctrlLeft 7)
        "12345 ~< 12345" ;
      t
        "ctrl+right from end of < moves to end of second int"
        (binop "<" anInt anInt)
        (ctrlRight 7)
        "12345 < 12345~" ;
      t
        "ctrl+left from beg of < moves to front of first int"
        (binop "<" anInt anInt)
        (ctrlLeft 6)
        "~12345 < 12345" ;
      t
        "ctrl+right from beg of < moves to end of <"
        (binop "<" anInt anInt)
        (ctrlRight 6)
        "12345 <~ 12345" ;
      t
        "DeletePrevWord in end of binop deletes binop and first int"
        (binop "<" anInt anInt)
        (key K.DeletePrevWord 7)
        "12345~" ;
      t
        "DeletePrevWord in front of binop deletes first int"
        (binop "<" anInt anInt)
        (key K.DeletePrevWord 5)
        "~_________ < 12345" ;
      t
        "DeleteNextWord in end of binop deletes second int"
        (binop "<" anInt anInt)
        (key K.DeleteNextWord 8)
        "12345 < ~_________" ;
      t
        "DeleteNextWord in front of binop deletes binop and second int"
        (binop "<" anInt anInt)
        (key K.DeleteNextWord 6)
        "12345~" ;
      (* TODO bs on empty partial does something *)
      (* TODO support del on all the bs commands *)
      (* TODO pressing enter at the end of the partialGhost *)
      () ) ;
  describe "Constructors" (fun () ->
      tp "arguments work in constructors" aConstructor (ins 't' 5) "Just t~" ;
      t "int arguments work in constructors" aConstructor (ins '5' 5) "Just 5~" ;
      tp
        "bs on a constructor converts it to a partial with ghost"
        aConstructor
        (bs 4)
        "Jus~@ ___" ;
      tp
        "del on a constructor converts it to a partial with ghost"
        aConstructor
        (del 0)
        "~ust@ ___" ;
      t
        "space on a constructor blank does nothing"
        aConstructor
        (space 5)
        "Just ~___" ;
      t
        "ctrl+left mid constructor moves to beg"
        aConstructor
        (ctrlLeft 2)
        "~Just ___" ;
      t
        "ctrl+left mid constructor moves to end"
        aConstructor
        (ctrlRight 2)
        "Just~ ___" ;
      t
        "DeletePrevWord at end of constructor deletes to beg "
        aConstructor
        (key K.DeletePrevWord 4)
        "~___" ;
      tp
        "DeletePrevWord mid constructor deletes to beg "
        aConstructor
        (key K.DeletePrevWord 2)
        "~st@@ ___" ;
      t
        "DeleteNextWord at end of constructor moves to blank "
        aConstructor
        (key K.DeleteNextWord 4)
        "Just ___~" ;
      tp
        "DeleteNextWord mid constructor deletes to end "
        aConstructor
        (key K.DeleteNextWord 2)
        "Ju~@@ ___" ;
      (* TODO: test renaming constructors.
       * It's not too useful yet because there's only 4 constructors and,
       * hence, unlikely that anyone will rename them this way.
       * Also, the names of the temporary variables used to store the old arguments of a changed
       * constructor are randomly generated and would be hard to test *)
      () ) ;
  describe "Lambdas" (fun () ->
      (* type -> to move through a lambda *)
      t
        "type - after a lambda var to move into a lambda arrow"
        aLambda
        (key Minus 4)
        "\\*** -~> ___" ;
      t
        "type - before a lambda arrow to move into a lambda arrow"
        aLambda
        (key Minus 5)
        "\\*** -~> ___" ;
      t
        "type > inside a lambda arrow to move past it"
        aLambda
        (key GreaterThan 6)
        "\\*** -> ~___" ;
      (* end type -> to move through a lambda *)
      t "bs over lambda symbol" aLambda (bs 1) "~___" ;
      t "insert space in lambda" aLambda (key K.Space 1) "\\~*** -> ___" ;
      t "bs non-empty lambda symbol" nonEmptyLambda (bs 1) "\\~*** -> 5" ;
      t "del lambda symbol" aLambda (del 0) "~___" ;
      t "del non-empty lambda symbol" nonEmptyLambda (del 0) "~\\*** -> 5" ;
      t
        "insert changes occurence of binding var"
        (lambdaWithUsedBinding "binding")
        (ins 'c' 8)
        "\\bindingc~ -> bindingc" ;
      t
        "insert changes occurence of binding 2nd var"
        (lambdaWithUsed2ndBinding "binding")
        (ins 'c' 17)
        "\\somevar, bindingc~ -> bindingc" ;
      t
        "dont jump in lambdavars with infix chars"
        aLambda
        (key K.Plus 1)
        "\\~*** -> ___" ;
      t
        "dont allow name to start with a number"
        aLambda
        (ins '5' 1)
        "\\~*** -> ___" ;
      t
        "dont allow name to start with a number, pt 2"
        (lambdaWithBinding "test" five)
        (ins '2' 1)
        "\\~test -> 5" ;
      t
        "dont allow name to start with a number, pt 3"
        aLambda
        (ins '5' 3)
        (* TODO: this looks wrong *)
        "\\**~* -> ___" ;
      t
        "creating lambda in block placeholder should set arguments"
        aFnCallWithBlockArg
        (key (K.Letter '\\') 24)
        "Dict::map _____________ \\~key, value -> ___" ;
      t
        "creating lambda in block placeholder should set arguments when wrapping expression is inside pipe"
        (pipe b [b])
        (keys
           (* we have to insert the function with completion here
            * so the arguments are adjusted based on the pipe *)
           [K.Letter 'm'; K.Letter 'a'; K.Letter 'p'; K.Enter; K.Letter '\\']
           6)
        "___\n|>Dict::map \\~key, value -> ___\n" ;
      t
        "deleting a lambda argument should work"
        lambdaWithTwoBindings
        (del 2)
        "\\x~ -> ___" ;
      t
        "backspacing a lambda argument should work"
        lambdaWithTwoBindings
        (bs 3)
        "\\x~ -> ___" ;
      t
        "deleting a lambda argument should update used variable"
        (lambdaWithUsed2ndBinding "x")
        (del 8)
        "\\somevar~ -> ___" ;
      t
        "can add lambda arguments when blank"
        aLambda
        (ins ',' 4)
        "\\***, ~*** -> ___" ;
      t
        "can add lambda arguments to used binding"
        lambdaWithTwoBindings
        (ins ',' 5)
        "\\x, y, ~*** -> ___" ;
      t
        "can add lambda arguments in middle used binding"
        lambdaWithTwoBindings
        (ins ',' 2)
        "\\x, ~***, y -> ___" ;
      t
        "can add lambda arguments in the front"
        lambdaWithTwoBindings
        (ins ',' 1)
        "\\~***, x, y -> ___" ;
      t
        "can add lambda arguments in front of middle"
        lambdaWithTwoBindings
        (ins ',' 4)
        "\\x, ~***, y -> ___" ;
      t
        "cant insert a blank from outside the lambda"
        lambdaWithTwoBindings
        (ins ',' 0)
        "~\\x, y -> ___" ;
      t
        "cant bs a blank from the space in a lambda"
        lambdaWithTwoBindings
        (bs 4)
        "\\x,~ y -> ___" ;
      t
        "ctrl+left twice over lamda from beg moves to beg of first param"
        lambdaWithTwoBindings
        (keys [K.GoToStartOfWord; K.GoToStartOfWord] 1)
        "~\\x, y -> ___" ;
      t
        "ctrl+right twice over lamda from beg moves to last blank"
        lambdaWithTwoBindings
        (keys [K.GoToEndOfWord; K.GoToEndOfWord] 1)
        "\\x, y~ -> ___" ;
      t
        "ctrl+left twice over lamda from end moves to end of second param"
        lambdaWithTwoBindings
        (keys [K.GoToStartOfWord; K.GoToStartOfWord] 12)
        "\\x, ~y -> ___" ;
      t
        "ctrl+right twice over lamda from end doesnt move"
        lambdaWithTwoBindings
        (keys [K.GoToEndOfWord; K.GoToEndOfWord] 12)
        "\\x, y -> ___~" ;
      () ) ;
  describe "Variables" (fun () ->
      tp "insert middle of variable" aVar (ins 'c' 5) "variac~ble" ;
      tp "del middle of variable" aVar (del 5) "varia~le" ;
      tp "insert capital works" aVar (key (K.Letter 'A') 5) "variaA~ble" ;
      t "can't insert invalid" aVar (key K.Dollar 5) "varia~ble" ;
      t "del variable" aShortVar (del 0) "~___" ;
      tp "del long variable" aVar (del 0) "~ariable" ;
      tp "del mid variable" aVar (del 6) "variab~e" ;
      t "bs variable" aShortVar (bs 1) "~___" ;
      tp "bs mid variable" aVar (bs 8) "variabl~" ;
      tp "bs mid variable" aVar (bs 6) "varia~le" ;
      t
        "variable doesn't override if"
        (let' "i" b (partial "i" b))
        (keys [K.Letter 'f'; K.Enter] 13)
        "let i = ___\nif ~___\nthen\n  ___\nelse\n  ___" ;
      t
        "ctrl+left from beg of variable doesnt move"
        aVar
        (ctrlLeft 0)
        "~variable" ;
      t
        "ctrl+right from beg of variable moves to end"
        aVar
        (ctrlRight 0)
        "variable~" ;
      t
        "ctrl+left from end of variable moves to beg"
        aVar
        (ctrlLeft 8)
        "~variable" ;
      t
        "ctrl+right from end of variable doesnt move"
        aVar
        (ctrlRight 8)
        "variable~" ;
      () ) ;
  describe "Match" (fun () ->
      t
        "move to the front of match"
        emptyMatch
        (key K.GoToStartOfLine 6)
        "~match ___\n  *** -> ___\n" ;
      t
        "move to the end of match"
        emptyMatch
        (key K.GoToEndOfLine 0)
        "match ___~\n  *** -> ___\n" ;
      t
        "move to the front of match on line 2"
        emptyMatch
        (key K.GoToStartOfLine 15)
        "match ___\n  ~*** -> ___\n" ;
      t
        "move to the end of match on line 2"
        emptyMatch
        (key K.GoToEndOfLine 12)
        "match ___\n  *** -> ___~\n" ;
      t
        "move back over match"
        emptyMatch
        (key K.Left 6)
        "~match ___\n  *** -> ___\n" ;
      t
        "move forward over match"
        emptyMatch
        (key K.Right 0)
        "match ~___\n  *** -> ___\n" ;
      t "bs over empty match" emptyMatch (bs 6) "~___" ;
      t
        "bs over empty match with 2 patterns"
        emptyMatchWithTwoPatterns
        (bs 6)
        "~___" ;
      t
        "bs over match with 2 patterns"
        matchWithPatterns
        (bs 6)
        "match ~___\n  3 -> ___\n" ;
      t "del over empty match" emptyMatch (del 0) "~___" ;
      t
        "del over empty match with 2 patterns"
        emptyMatchWithTwoPatterns
        (del 0)
        "~___" ;
      t
        "del over match with 2 patterns"
        matchWithPatterns
        (del 0)
        "~match ___\n  3 -> ___\n" ;
      t
        "del constructor in match pattern"
        matchWithConstructorPattern
        (del 12)
        "match ___\n  ~ust -> ___\n" ;
      t
        "bs constructor in match pattern"
        matchWithConstructorPattern
        (bs 16)
        "match ___\n  Jus~ -> ___\n" ;
      t
        "insert changes occurence of non-shadowed var in case"
        (matchWithBinding "binding" (var "binding"))
        (ins 'c' 19)
        "match ___\n  bindingc~ -> bindingc\n" ;
      t
        "insert changes occurence of non-shadowed var in case constructor"
        (matchWithConstructorBinding "binding" (var "binding"))
        (ins 'c' 22)
        "match ___\n  Ok bindingc~ -> bindingc\n" ;
      t
        "insert space in blank match"
        emptyMatch
        (key K.Space 6)
        "match ~___\n  *** -> ___\n" ;
      t
        "insert space in blank match on line 2"
        emptyMatch
        (key K.Space 12)
        "match ___\n  ~*** -> ___\n" ;
      t
        "enter at the end of the cond creates a new row"
        matchWithPatterns
        (enter 9)
        "match ___\n  ~*** -> ___\n  3 -> ___\n" ;
      t
        "enter at the end of a row creates a new row"
        emptyMatchWithTwoPatterns
        (enter 22)
        "match ___\n  *** -> ___\n  ~*** -> ___\n  *** -> ___\n" ;
      t
        "enter at the end of the last row creates a new row"
        emptyMatchWithTwoPatterns
        (enter 35)
        "match ___\n  *** -> ___\n  *** -> ___\n  ~*** -> ___\n" ;
      t
        "enter at the end of the last row in nested match creates a new row"
        nestedMatch
        (enter 50)
        "match ___\n  *** -> match ___\n           *** -> ___\n           ~*** -> ___\n" ;
      t
        "enter at beginning of line after match adds let, not match row"
        (let' "a" emptyMatch five)
        (enter 39)
        "let a = match ___\n          *** -> ___\nlet *** = ___\n~5" ;
      t
        "enter at the start of a row creates a new row"
        matchWithPatterns
        (enter 12)
        "match ___\n  *** -> ___\n  ~3 -> ___\n" ;
      t
        "backspace first row deletes it"
        emptyMatchWithTwoPatterns
        (bs 12)
        "match ___~\n  *** -> ___\n" ;
      t
        "backspace second row deletes it"
        emptyMatchWithTwoPatterns
        (bs 25)
        "match ___\n  *** -> ___~\n" ;
      t
        "backspacing only row doesn't delete"
        emptyMatch
        (bs 12)
        "match ___~\n  *** -> ___\n" ;
      t
        "ctrl+left 2 times from end moves to first blank"
        emptyMatch
        (keys [K.GoToStartOfWord; K.GoToStartOfWord] 22)
        "match ___\n  ~*** -> ___\n" ;
      t
        "ctrl+right 2 times from end doesnt move"
        emptyMatch
        (keys [K.GoToEndOfWord; K.GoToEndOfWord] 22)
        "match ___\n  *** -> ___\n~" ;
      t
        "ctrl+left 2 times from beg doesnt move"
        emptyMatch
        (keys [K.GoToStartOfWord; K.GoToStartOfWord] 0)
        "~match ___\n  *** -> ___\n" ;
      t
        "ctrl+right 2 times from beg moves to last blank"
        emptyMatch
        (keys [K.GoToEndOfWord; K.GoToEndOfWord] 0)
        "match ___\n  ***~ -> ___\n" ;
      t
        "ctrl+left from mid moves to previous blank "
        emptyMatch
        (ctrlLeft 15)
        "match ___\n  ~*** -> ___\n" ;
      t
        "ctrl+right from mid moves to next blank"
        emptyMatch
        (ctrlRight 15)
        "match ___\n  *** -> ___~\n" ;
      (* delete row with delete *)
      () ) ;
  describe "Lets" (fun () ->
      t
        "move to the front of let"
        emptyLet
        (key K.GoToStartOfLine 4)
        "~let *** = ___\n5" ;
      t
        "move to the end of let"
        emptyLet
        (key K.GoToEndOfLine 4)
        "let *** = ___~\n5" ;
      t "move back over let" emptyLet (key K.Left 4) "~let *** = ___\n5" ;
      t "move forward over let" emptyLet (key K.Right 0) "let ~*** = ___\n5" ;
      t "bs over empty let" emptyLet (bs 3) "~5" ;
      t "del empty let" emptyLet (del 0) "~5" ;
      t "bs over non-empty let" nonEmptyLet (bs 3) "let~ *** = 6\n5" ;
      t "del non-empty let" nonEmptyLet (del 0) "~let *** = 6\n5" ;
      t
        "insert space on blank let"
        emptyLet
        (key K.Space 4)
        "let ~*** = ___\n5" ;
      t "lhs on empty" emptyLet (ins 'c' 4) "let c~ = ___\n5" ;
      t "middle of blank" emptyLet (ins 'c' 5) "let c~ = ___\n5" ;
      t "bs letlhs" letWithLhs (bs 5) "let ~*** = 6\n5" ;
      t "del letlhs" letWithLhs (del 4) "let ~*** = 6\n5" ;
      t
        "equals skips over assignment"
        emptyLet
        (keys [K.Letter 'c'; K.Equals] 4)
        "let c = ~___\n5" ;
      t
        "equals skips over assignment 1"
        emptyLet
        (key K.Equals 7)
        "let *** = ~___\n5" ;
      t
        "equals skips over assignment 2"
        emptyLet
        (key K.Equals 8)
        "let *** = ~___\n5" ;
      t
        "equals skips over assignment 3"
        emptyLet
        (key K.Equals 9)
        "let *** = ~___\n5" ;
      t
        "bs changes occurence of binding var"
        (letWithUsedBinding "binding")
        (bs 11)
        "let bindin~ = 6\nbindin" ;
      t
        "insert changes occurence of binding var"
        (letWithUsedBinding "binding")
        (ins 'c' 11)
        "let bindingc~ = 6\nbindingc" ;
      t
        "insert changes occurence of binding in match nested expr"
        (letWithBinding
           "binding"
           (match'
              b
              [(pVar "binding", var "binding"); (pInt "5", var "binding")]))
        (ins 'c' 11)
        "let bindingc~ = 6\nmatch ___\n  binding -> binding\n  5 -> bindingc\n" ;
      t
        "insert doesn't change occurence of binding in shadowed lambda expr"
        (letWithBinding "binding" (lambda ["binding"] (var "binding")))
        (ins 'c' 11)
        "let bindingc~ = 6\n\\binding -> binding" ;
      t
        "insert changes occurence of binding in lambda expr"
        (letWithBinding "binding" (lambda ["somevar"] (var "binding")))
        (ins 'c' 11)
        "let bindingc~ = 6\n\\somevar -> bindingc" ;
      t
        "dont jump in letlhs with infix chars"
        emptyLet
        (key K.Plus 4)
        "let ~*** = ___\n5" ;
      t
        "dont allow letlhs to start with a number"
        emptyLet
        (ins '5' 4)
        "let ~*** = ___\n5" ;
      t
        "dont allow letlhs to start with a number, pt 2"
        letWithLhs
        (ins '2' 4)
        "let ~n = 6\n5" ;
      t
        "dont allow letlhs to start with a number, pt 3"
        emptyLet
        (ins '5' 6)
        "let **~* = ___\n5" ;
      t
        "enter on the end of let goes to blank"
        nonEmptyLetWithBlankEnd
        (enter 11)
        "let *** = 6\n~___" ;
      t
        "enter at the end of a line inserts let if no blank is next"
        nonEmptyLet
        (enter 11)
        "let *** = 6\nlet ~*** = ___\n5" ;
      t
        "enter at the start of a let creates let above"
        twoLets
        (enter 10)
        "let x = 5\nlet *** = ___\n~let y = 6\n7" ;
      t
        "enter at the start of first let creates let above"
        nonEmptyLet
        (enter 0)
        "let *** = ___\n~let *** = 6\n5" ;
      t
        "enter at the end of a let with a let below inserts new let"
        twoLets
        (enter 9)
        "let x = 5\nlet ~*** = ___\nlet y = 6\n7" ;
      t
        "enter on the end of first let inserts new let"
        matchWithTwoLets
        (enter 28)
        "match ___\n  *** -> let x = 5\n         let ~*** = ___\n         let y = 6\n         ___\n" ;
      t
        "enter on the end of second let goes to blank"
        matchWithTwoLets
        (enter 47)
        "match ___\n  *** -> let x = 5\n         let y = 6\n         ~___\n" ;
      t
        "enter at the start of a non-let also creates let above"
        anInt
        (enter 0)
        "let *** = ___\n~12345" ;
      t
        "Ctrl+left in front of a varname moves to previous editable text"
        matchWithTwoLets
        (ctrlLeft 23)
        "match ___\n  ~*** -> let x = 5\n         let y = 6\n         ___\n" ;
      t
        "Ctrl+right in front of a varname moves to next editable text"
        matchWithTwoLets
        (ctrlRight 15)
        "match ___\n  *** -> let x~ = 5\n         let y = 6\n         ___\n" ;
      t
        "enter at the end of a non-let wraps literal expr in let"
        aShortInt
        (enter 1)
        "let _ = 1\n~___" ;
      t
        "enter at the end of a non-let wraps fncall in let"
        aFullFnCall
        (enter 12)
        "let _ = Int::add 5 5\n~___" ;
      t
        "enter at the end of non-final arg, should just go to next line: #1"
        (let' "x" (fn "Int::add" [record [("", int "5")]; int "6"]) b)
        (enter 60)
        "let x = Int::add\n          {\n            *** : 5\n          }\n          ~6\n___" ;
      t
        "enter at the end of a non-final arg should just go to next line: #2"
        (fn "Int::add" [record [("", int "5")]; int "6"])
        (enter 28)
        "Int::add\n  {\n    *** : 5\n  }\n  ~6" ;
      t
        "enter at the start of ast also creates let"
        anInt
        (enter 0)
        "let *** = ___\n~12345" ;
      () ) ;
  describe "Pipes" (fun () ->
      (* TODO: add tests for clicking in the middle of a pipe (or blank) *)
      t
        "move to the front of pipe on line 1"
        aPipe
        (key K.GoToStartOfLine 2)
        "~[]\n|>List::append [5]\n|>List::append [5]\n" ;
      t
        "move to the end of pipe on line 1"
        aPipe
        (key K.GoToEndOfLine 0)
        "[]~\n|>List::append [5]\n|>List::append [5]\n" ;
      t
        "move to the front of pipe on line 2"
        aPipe
        (key K.GoToStartOfLine 8)
        "[]\n|>~List::append [5]\n|>List::append [5]\n" ;
      t
        "move to the end of pipe on line 2"
        aPipe
        (key K.GoToEndOfLine 5)
        "[]\n|>List::append [5]~\n|>List::append [5]\n" ;
      t
        "move to the front of pipe on line 3"
        aPipe
        (key K.GoToStartOfLine 40)
        "[]\n|>List::append [5]\n|>~List::append [5]\n" ;
      t
        "move to the end of pipe on line 3"
        aPipe
        (key K.GoToEndOfLine 24)
        "[]\n|>List::append [5]\n|>List::append [5]~\n" ;
      t
        "pipes appear on new lines"
        aPipe
        render
        "~[]\n|>List::append [5]\n|>List::append [5]\n" ;
      t
        "nested pipes will indent"
        aNestedPipe
        render
        "~[]\n|>List::append [5]\n               |>List::append [6]\n" ;
      t
        "backspacing a pipe's first pipe works"
        aLongPipe
        (bs 5)
        "[]~\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]\n" ;
      t
        "deleting a pipe's first pipe works"
        aLongPipe
        (del 3)
        "[]\n~|>List::append [3]\n|>List::append [4]\n|>List::append [5]\n" ;
      t
        "backspacing a pipe's second pipe works"
        aLongPipe
        (bs 24)
        "[]\n|>List::append [2]~\n|>List::append [4]\n|>List::append [5]\n" ;
      t
        "deleting a pipe's second pipe works"
        aLongPipe
        (del 22)
        "[]\n|>List::append [2]\n~|>List::append [4]\n|>List::append [5]\n" ;
      t
        "backspacing a pipe's third pipe works"
        aLongPipe
        (bs 43)
        "[]\n|>List::append [2]\n|>List::append [3]~\n|>List::append [5]\n" ;
      t
        "deleting a pipe's third pipe works"
        aLongPipe
        (del 41)
        "[]\n|>List::append [2]\n|>List::append [3]\n~|>List::append [5]\n" ;
      t
        "backspacing a pipe's last pipe works"
        aLongPipe
        (bs 62)
        "[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]~\n" ;
      t
        "deleting a pipe's last pipe works"
        aLongPipe
        (del 60)
        "[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]~\n" ;
      t
        "backspacing a pipe's first pipe that isn't in the first column works"
        aPipeInsideIf
        (bs 21)
        "if ___\nthen\n  []~\n  |>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___" ;
      t
        "deleting a pipe's first pipe that isn't in the first column works"
        aPipeInsideIf
        (del 19)
        "if ___\nthen\n  []\n  ~|>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___" ;
      t
        "backspacing a pipe's second pipe that isn't in the first column works"
        aPipeInsideIf
        (bs 42)
        "if ___\nthen\n  []\n  |>List::append [2]~\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___" ;
      t
        "deleting a pipe's second pipe that isn't in the first column works"
        aPipeInsideIf
        (del 40)
        "if ___\nthen\n  []\n  |>List::append [2]\n  ~|>List::append [4]\n  |>List::append [5]\nelse\n  ___" ;
      t
        "backspacing a pipe's third pipe that isn't in the first column works"
        aPipeInsideIf
        (bs 63)
        "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]~\n  |>List::append [5]\nelse\n  ___" ;
      t
        "deleting a pipe's third pipe that isn't in the first column works"
        aPipeInsideIf
        (del 61)
        "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  ~|>List::append [5]\nelse\n  ___" ;
      t
        "backspacing a pipe's fourth pipe that isn't in the first column works"
        aPipeInsideIf
        (bs 84)
        "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]~\nelse\n  ___" ;
      t
        "deleting a pipe's fourth pipe that isn't in the first column works"
        aPipeInsideIf
        (del 82)
        "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]~\nelse\n  ___" ;
      tp
        "backspacing a pipe's first fn works"
        aLongPipe
        (bs 17)
        "[]\n|>List::appen~@ [2]\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]\n" ;
      tp
        "backspacing a pipe's first binop works"
        aBinopPipe
        (bs 8)
        "___\n|>+~@ \"asd\"\n" ;
      t
        "adding infix functions adds the right number of blanks"
        emptyPipe
        (keys [K.Plus; K.Enter] 6)
        "___\n|>+ ~_________\n" ;
      t
        "creating a pipe from an fn via a partial works"
        (partial "|>" aFnCall)
        (enter 2)
        (* TODO: This really should end in 18, but too much work for now *)
        "Int::add 5 ~_________\n|>___\n" ;
      t
        "enter at the end of a pipe expr creates a new entry"
        aPipe
        (enter 21)
        "[]\n|>List::append [5]\n|>~___\n|>List::append [5]\n" ;
      t
        "enter at the end of the opening expr creates a new entry"
        aPipe
        (enter 2)
        "[]\n|>~___\n|>List::append [5]\n|>List::append [5]\n" ;
      t
        "enter at the start of a line creates a new entry"
        aPipe
        (enter 3)
        "[]\n|>___\n~|>List::append [5]\n|>List::append [5]\n" ;
      t
        "enter at start of blank (within pipe) creates a new entry"
        aPipe
        (enter 5)
        "[]\n|>___\n|>~List::append [5]\n|>List::append [5]\n" ;
      t
        "enter at the end of the last expr creates a new entry"
        aPipe
        (enter 40)
        "[]\n|>List::append [5]\n|>List::append [5]\n|>~___\n" ;
      t
        "enter at the end of the last nested expr creates a new entry"
        aNestedPipe
        (enter 55)
        "[]\n|>List::append [5]\n               |>List::append [6]\n               |>~___\n" ;
      t
        "enter at the end of pipe expression with line below creates a new entry"
        (let' "a" (pipe (list []) [listFn [aList5]]) five)
        (enter ~wrap:false 37)
        (* indent counting is all weird with wrapper *)
        "let a = []\n        |>List::append [5]\n        |>~___\n5" ;
      t
        "enter at the beginning of expression after pipe creates let, not pipe"
        (let' "a" (pipe (list []) [listFn [aList5]]) five)
        (enter ~wrap:false 38)
        (* indent counting is all weird with wrapper *)
        "let a = []\n        |>List::append [5]\nlet *** = ___\n~5" ;
      t
        "inserting a pipe into another pipe gives a single pipe1"
        (pipe five [listFn [rightPartial "|>" aList5]])
        (enter 23)
        "5\n|>List::append [5]\n|>~___\n" ;
      t
        "inserting a pipe into another pipe gives a single pipe2"
        (pipe five [listFn [aList5]])
        (key K.ShiftEnter 19)
        "5\n|>List::append [5]\n|>~___\n" ;
      t
        "inserting a pipe into another pipe gives a single pipe3"
        five
        (key K.ShiftEnter 1)
        "5\n|>~___\n" ;
      t
        "shift enter at a let's newline creates the pipe on the rhs"
        nonEmptyLet
        (key K.ShiftEnter 11)
        "let *** = 6\n          |>~___\n5" ;
      t
        "shift enter in a record's newline creates the pipe in the expr, not the entire record"
        (record [("f1", fiftySix); ("f2", seventyEight)])
        (key K.ShiftEnter 11)
        "{\n  f1 : 56\n       |>~___\n  f2 : 78\n}" ;
      t
        "ctrl+left moves to front of thread "
        aPipe
        (ctrlLeft 19)
        "[]\n|>~List::append [5]\n|>List::append [5]\n" ;
      t
        "ctrl+right moves to end of next thread "
        aPipe
        (ctrlRight 20)
        "[]\n|>List::append [5]\n|>List::append~ [5]\n" ;
      (* TODO: test for prefix fns *)
      (* TODO: test for deleting pipeed infix fns *)
      (* TODO: test for deleting pipeed prefix fns *)
      () ) ;
  describe "Ifs" (fun () ->
      t
        "move over indent 1"
        plainIf
        (key K.Left 12)
        "if 5\nthen~\n  6\nelse\n  7" ;
      t
        "move over indent 2"
        plainIf
        (key K.Left 21)
        "if 5\nthen\n  6\nelse~\n  7" ;
      t "bs over indent 1" plainIf (bs 12) "if 5\nthen~\n  6\nelse\n  7" ;
      t "bs over indent 2" plainIf (bs 21) "if 5\nthen\n  6\nelse~\n  7" ;
      t "bs over empty if" emptyIf (bs 2) "~___" ;
      t
        "move to front of line 1"
        plainIf
        (key K.GoToStartOfLine 4)
        "~if 5\nthen\n  6\nelse\n  7" ;
      t
        "move to end of line 1"
        plainIf
        (key K.GoToEndOfLine 0)
        "if 5~\nthen\n  6\nelse\n  7" ;
      t
        "move to front of line 3"
        plainIf
        (key K.GoToStartOfLine 13)
        "if 5\nthen\n  ~6\nelse\n  7" ;
      t
        "move to end of line 3"
        plainIf
        (key K.GoToEndOfLine 12)
        "if 5\nthen\n  6~\nelse\n  7" ;
      t
        "move to front of line 5 in nested if"
        nestedIf
        (key K.GoToStartOfLine 16)
        "if 5\nthen\n  ~if 5\n  then\n    6\n  else\n    7\nelse\n  7" ;
      t
        "move to end of line 5 in nested if"
        nestedIf
        (key K.GoToEndOfLine 12)
        "if 5\nthen\n  if 5~\n  then\n    6\n  else\n    7\nelse\n  7" ;
      t
        "try to insert space on blank"
        emptyIf
        (key K.Space 3)
        "if ~___\nthen\n  ___\nelse\n  ___" ;
      t
        "try to insert space on blank indent 2"
        emptyIf
        (key K.Space 14)
        "if ___\nthen\n  ~___\nelse\n  ___" ;
      t
        "enter in front of an if wraps in a let"
        plainIf
        (enter 0)
        "let *** = ___\n~if 5\nthen\n  6\nelse\n  7" ;
      t
        "enter at end of if line does nothing"
        plainIf
        (enter 4)
        "if 5\n~then\n  6\nelse\n  7" ;
      t
        "enter at the end of then line wraps expr in let"
        plainIf
        (enter 13)
        "if 5\nthen\n  let _ = 6\n  ~___\nelse\n  7" ;
      t
        "enter at the end of else line wraps expr in let"
        plainIf
        (enter 22)
        "if 5\nthen\n  6\nelse\n  let _ = 7\n  ~___" ;
      t
        "enter at end of then line inserts let if no blank next "
        plainIf
        (enter 9)
        "if 5\nthen\n  let ~*** = ___\n  6\nelse\n  7" ;
      t
        "enter at end of else line does inserts let if no blank next"
        (* TODO: This should probably do nothing, but right now it acts like
         * it's at the front of the line below. *)
        plainIf
        (enter 18)
        "if 5\nthen\n  6\nelse\n  let ~*** = ___\n  7" ;
      t
        "ctrl+left from value moves to condition "
        plainIf
        (ctrlLeft 12)
        "if ~5\nthen\n  6\nelse\n  7" ;
      t
        "ctrl+right from condition moves to value "
        plainIf
        (ctrlRight 4)
        "if 5\nthen\n  6~\nelse\n  7" ;
      () ) ;
  describe "Lists" (fun () ->
      t "create list" b (key K.LeftSquareBracket 0) "[~]" ;
      t "insert into empty list inserts" emptyList (ins '5' 1) "[5~]" ;
      t "inserting before the list does nothing" emptyList (ins '5' 0) "~[]" ;
      t "insert space into multi list" multi (key K.Space 6) "[56,78~]" ;
      t "insert space into single list" single (key K.Space 3) "[56~]" ;
      t "insert into existing list item" single (ins '4' 1) "[4~56]" ;
      t
        "insert separator before item creates blank"
        single
        (ins ',' 1)
        "[~___,56]" ;
      t
        "insert separator after item creates blank"
        single
        (ins ',' 3)
        "[56,~___]" ;
      t
        "insert separator between items creates blank"
        multi
        (ins ',' 3)
        "[56,~___,78]" ;
      (* t "insert separator mid integer makes two items" single (ins ',' 2) *)
      (*   ("[5,6]", 3) ; *)
      (* TODO: when on a separator in a nested list, pressing comma makes an entry outside the list. *)
      t
        "insert separator mid string does nothing special "
        withStr
        (ins ',' 3)
        "[\"a,~b\"]" ;
      t
        "backspacing open bracket of empty list dels list"
        emptyList
        (bs 1)
        "~___" ;
      t
        "backspacing close bracket of empty list moves inside list"
        emptyList
        (bs 2)
        "[~]" ;
      t
        "deleting open bracket of empty list dels list"
        emptyList
        (del 0)
        "~___" ;
      t
        "close bracket at end of list is swallowed"
        emptyList
        (key K.RightSquareBracket 1)
        "[]~" ;
      t
        "bs on first separator between items dels item after separator"
        multi
        (bs 4)
        "[56~]" ;
      t
        "del before first separator between items dels item after separator"
        multi
        (del 3)
        "[56~]" ;
      t
        "bs on middle separator between items dels item after separator"
        longList
        (bs 10)
        "[56,78,56~,56,78]" ;
      t
        "del before middle separator between items dels item after separator"
        longList
        (del 9)
        "[56,78,56~,56,78]" ;
      t
        "bs on middle separator between items dels blank after separator"
        listWithBlank
        (bs 7)
        "[56,78~,56]" ;
      t
        "del before middle separator between items dels blank after separator"
        listWithBlank
        (del 6)
        "[56,78~,56]" ;
      t
        "bs on last separator between a blank and item dels item after separator"
        listWithBlank
        (bs 11)
        "[56,78,___~]" ;
      t
        "del before last separator between a blank and item dels item after separator"
        listWithBlank
        (del 10)
        "[56,78,___~]" ;
      t
        "bs on separator between string items dels item after separator"
        multiWithStrs
        (bs 6)
        "[\"ab\"~,\"ef\"]" ;
      t
        "del before separator between string items dels item after separator"
        multiWithStrs
        (del 5)
        "[\"ab\"~,\"ef\"]" ;
      t
        "ctrl+left at the beg of list item moves to beg of next list item"
        longList
        (ctrlLeft 10)
        "[56,78,~56,78,56,78]" ;
      t
        "ctrl+right at the end of list item moves to end of next list item"
        longList
        (ctrlRight 12)
        "[56,78,56,78,56~,78]" ;
      () ) ;
  describe "Record" (fun () ->
      t "create record" b (key K.LeftCurlyBrace 0) "{~}" ;
      t
        "inserting before the record does nothing"
        emptyRecord
        (ins '5' 0)
        "~{}" ;
      t
        "inserting space between empty record does nothing"
        emptyRecord
        (space 1)
        "{~}" ;
      t
        "inserting valid text in an empty record works"
        emptyRecord
        (ins 'f' 1 ~wrap:false)
        "{\n  f~ : ___\n}" ;
      t
        "inserting valid text in an empty record works"
        emptyRecord
        (ins 'f' 1 ~wrap:false)
        "{\n  f~ : ___\n}" ;
      t
        "inserting text in nested record gets correct position"
        listWithRecord
        (ins 'f' 2 ~wrap:false)
        "[{\n   f~ : ___\n }]" ;
      t
        "inserting space in empty record field does nothing"
        emptyRowRecord
        (space 4)
        "{\n  ~*** : ___\n}" ;
      t
        "inserting space in empty record value does nothing"
        emptyRowRecord
        (space 10)
        "{\n  *** : ~___\n}" ;
      t
        "pressing enter in an the start of empty record adds a new line"
        emptyRecord
        (enter 1)
        "{\n  ~*** : ___\n}" ;
      t "enter fieldname" emptyRowRecord (ins 'c' 4) "{\n  c~ : ___\n}" ;
      t
        "move to the front of an empty record"
        emptyRowRecord
        (key K.GoToStartOfLine 13)
        "{\n  ~*** : ___\n}" ;
      t
        "move to the end of an empty record"
        emptyRowRecord
        (key K.GoToEndOfLine 4)
        "{\n  *** : ___~\n}" ;
      t
        "cant enter invalid fieldname"
        emptyRowRecord
        (ins '^' 4)
        "{\n  ~*** : ___\n}" ;
      t
        "backspacing open brace of empty record dels record"
        emptyRecord
        (bs 1)
        "~___" ;
      t
        "backspacing close brace of empty record moves inside record"
        emptyRecord
        (bs 2)
        "{~}" ;
      t
        "deleting open brace of empty record dels record"
        emptyRecord
        (del 0)
        "~___" ;
      t
        "close brace at end of record is swallowed"
        emptyRecord
        (key K.RightCurlyBrace 1)
        "{}~" ;
      t
        "backspacing empty record field clears entry"
        emptyRowRecord
        (bs 4)
        (* TODO: buggy. Should be 1 *)
        "{}~" ;
      t
        "pressing enter at start to insert field places the caret correctly"
        functionWrappedEmptyRecord
        (enter 22)
        "HttpClient::getv4\n  \"\"\n  {\n    ~*** : ___\n  }\n  {}" ;
      t
        "appending to int in expr works"
        singleRowRecord
        (ins '1' 11)
        "{\n  f1 : 561~\n}" ;
      t
        "appending to int in expr works"
        multiRowRecord
        (ins '1' 21)
        "{\n  f1 : 56\n  f2 : 781~\n}" ;
      t
        "move to the front of a record with multiRowRecordple values"
        multiRowRecord
        (key K.GoToStartOfLine 21)
        "{\n  f1 : 56\n  ~f2 : 78\n}" ;
      t
        "move to the end of a record with multiRowRecordple values"
        multiRowRecord
        (key K.GoToEndOfLine 14)
        "{\n  f1 : 56\n  f2 : 78~\n}" ;
      t
        "inserting at the end of the key works"
        emptyRowRecord
        (ins 'f' 6)
        "{\n  f~ : ___\n}" ;
      t
        "pressing enter at start adds a row"
        multiRowRecord
        (enter 1)
        "{\n  ~*** : ___\n  f1 : 56\n  f2 : 78\n}" ;
      t
        "pressing enter at the back adds a row"
        multiRowRecord
        (enter 22)
        "{\n  f1 : 56\n  f2 : 78\n  *** : ___\n~}" ;
      t
        "pressing enter at the start of a field adds a row"
        multiRowRecord
        (enter 14)
        "{\n  f1 : 56\n  *** : ___\n  ~f2 : 78\n}" ;
      t
        "pressing enter at the end of row adds a row"
        multiRowRecord
        (enter 11)
        "{\n  f1 : 56\n  ~*** : ___\n  f2 : 78\n}" ;
      t
        "dont allow weird chars in recordFieldnames"
        emptyRowRecord
        (key K.RightParens 4)
        "{\n  ~*** : ___\n}" ;
      t
        "dont jump in recordFieldnames with infix chars"
        emptyRowRecord
        (key K.Plus 4)
        "{\n  ~*** : ___\n}" ;
      t
        "dont jump in recordFieldnames with infix chars, pt 2"
        singleRowRecord
        (key K.Plus 6)
        "{\n  f1~ : 56\n}" ;
      t
        "colon should skip over the record colon"
        emptyRowRecord
        (key K.Colon 7)
        "{\n  *** : ~___\n}" ;
      t
        "dont allow key to start with a number"
        emptyRowRecord
        (ins '5' 4)
        "{\n  ~*** : ___\n}" ;
      t
        "dont allow key to start with a number, pt 2"
        singleRowRecord
        (ins '5' 4)
        "{\n  ~f1 : 56\n}" ;
      t
        "dont allow key to start with a number, pt 3"
        emptyRowRecord
        (ins '5' 6)
        (* TODO: looks wrong *)
        "{\n  **~* : ___\n}" ;
      t
        "ctrl+left at beg of value movese to beg of key"
        multiRowRecord
        (ctrlLeft 9)
        "{\n  ~f1 : 56\n  f2 : 78\n}" ;
      t
        "ctrl+right at end of key moves to end of value"
        multiRowRecord
        (ctrlRight 6)
        "{\n  f1 : 56~\n  f2 : 78\n}" ;
      () ) ;
  describe "Autocomplete" (fun () ->
      t
        "space autocompletes correctly"
        (partial "if" b)
        (space 2)
        "if ~___\nthen\n  ___\nelse\n  ___" ;
      t
        "let moves to right place"
        (partial "let" b)
        (enter 3)
        "let ~*** = ___\n___" ;
      t
        "autocomplete space moves forward by 1"
        aBinOp
        (keys [K.Letter 'r'; K.Space] 0)
        "request ~== _________" ;
      t
        "autocomplete enter moves to end of value"
        aBinOp
        (keys [K.Letter 'r'; K.Enter] 0)
        "request~ == _________" ;
      t "can tab to lambda blank" aLambda (tab 0) "\\~*** -> ___" ;
      t
        "autocomplete tab moves to next blank"
        aBinOp
        (keys [K.Letter 'r'; K.Tab] 0)
        "request == ~_________" ;
      t
        "autocomplete enter on bin-op moves to start of first blank"
        b
        (keys [K.Equals; K.Enter] 0)
        "~_________ == _________" ;
      t
        "autocomplete enter on function with parameters moves to start of first blank"
        (partial "sqrt" b)
        (enter 4)
        "Int::sqrt ~_________" ;
      t
        "autocomplete enter on niladic function moves to end of function"
        (partial "empty" b)
        (enter 5)
        "List::empty~" ;
      t
        "autocomplete tab on bin-op moves to start of second blank"
        b
        (keys [K.Equals; K.Tab] 0)
        "_________ == ~_________" ;
      t
        "autocomplete space on bin-op moves to start of first blank"
        b
        (keys [K.Equals; K.Space] 0)
        "~_________ == _________" ;
      t "variable moves to right place" (partial "req" b) (enter 3) "request~" ;
      t
        "pipe moves to right place on blank"
        b
        (keys [K.Letter '|'; K.Letter '>'; K.Enter] 2)
        "___\n|>~___\n" ;
      t
        "pipe moves to right place on placeholder"
        aFnCall
        (keys [K.Letter '|'; K.Letter '>'; K.Enter] 11)
        "Int::add 5 _________\n|>~___\n" ;
      t
        "pipe moves to right place in if then"
        emptyIf
        (keys [K.Letter '|'; K.Letter '>'; K.Enter] 14)
        "if ___\nthen\n  ___\n  |>~___\nelse\n  ___" ;
      t
        "pipe moves to right place in lambda body"
        aLambda
        (keys [K.Letter '|'; K.Letter '>'; K.Enter] 8)
        "\\*** -> ___\n        |>~___\n" ;
      t
        "pipe moves to right place in match body"
        emptyMatch
        (keys [K.Letter '|'; K.Letter '>'; K.Enter] 19)
        "match ___\n  *** -> ___\n         |>~___\n" ;
      t
        "shift enter autocompletes and creates pipe"
        (partial "empty" b)
        (key K.ShiftEnter 5)
        "List::empty\n|>~___\n" ;
      t
        "shift enter in pipe autocompletes and creates pipe"
        (pipe (list []) [partial "appe" b])
        (key ~wrap:false K.ShiftEnter 9)
        "[]\n|>List::append ___________\n|>~___\n" ;
      t "autocomplete for Just" (partial "Just" b) (enter 4) "Just ~___" ;
      t "autocomplete for Ok" (partial "Ok" b) (enter 2) "Ok ~___" ;
      t "autocomplete for Nothing" (partial "Nothing" b) (enter 7) "Nothing~" ;
      t
        "autocomplete for Nothing at end of a line"
        (if' b (partial "Nothing" b) b)
        (space 21)
        "if ___\nthen\n  Nothing~\nelse\n  ___" ;
      t "autocomplete for Error" (partial "Error" b) (enter 5) "Error ~___" ;
      t
        "autocomplete for field"
        (EPartial
           ( gid ()
           , "bo"
           , EFieldAccess (gid (), EVariable (ID "12", "request"), gid (), "")
           ))
        (enter ~clone:false 10)
        "request.body~" ;
      t
        "autocomplete shows first alphabetical item for fields"
        (let' "request" (int "5") (EVariable (ID "13", "request")))
        (keys [K.Period; K.Enter] ~clone:false 23)
        "let request = 5\nrequest.author~" ;
      t
        "autocomplete doesn't stick on the first alphabetical item for fields, when it refines further"
        (let' "request" (int "5") (EVariable (ID "13", "request")))
        (keys [K.Period; K.Letter 't'; K.Enter] ~clone:false 23)
        "let request = 5\nrequest.title~" ;
      t
        "autocomplete for field autocommits"
        (ELet
           ( gid ()
           , gid ()
           , "x"
           , EPartial
               ( gid ()
               , "body"
               , EFieldAccess
                   (gid (), EVariable (ID "12", "request"), gid (), "longfield")
               )
           , EBlank (gid ()) ))
        (* Right should make it commit *)
        (key ~clone:false K.Right 20)
        "let x = request.body\n~___" ;
      tp
        "autocomplete for field is committed by dot"
        (EPartial
           ( gid ()
           , "bod"
           , EFieldAccess
               (gid (), EVariable (ID "12", "request"), gid (), "longfield") ))
        (* Dot should select the autocomplete *)
        (key ~clone:false K.Period 11)
        "request.body.~***" ;
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
        (key K.Right 12)
        "if ___\nthen\n  ___~\nelse\n  ___" ;
      t
        "left skips over indent when in indent"
        emptyIf
        (key K.Left 13)
        "if ___\nthen~\n  ___\nelse\n  ___" ;
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
        ^ "          ~then\n"
        ^ "            6\n"
        ^ "          else\n"
        ^ "            7\n"
        ^ "var" ) ;
      t
        "end of if-then blank goes up properly"
        emptyIf
        (keys [K.Escape; K.Up] 17)
        "if ___\nthen~\n  ___\nelse\n  ___" ;
      t
        "end of if-then blank goes up properly, twice"
        emptyIf
        (keys [K.Escape; K.Up; K.Up] 17)
        "if __~_\nthen\n  ___\nelse\n  ___" ;
      t
        "end of if-then blank goes down properly"
        emptyIf
        (keys [K.Escape; K.Down] 5)
        "if ___\nthen~\n  ___\nelse\n  ___" ;
      t
        "end of if-then blank goes down properly, twice"
        emptyIf
        (keys [K.Escape; K.Down; K.Down] 5)
        "if ___\nthen\n  ___~\nelse\n  ___" ;
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
            (let ast = let' "var" (partial "false" b) b in
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
        (let' "x" (partial "Int::add" b) b)
        (key K.Right 16)
        "let x = Int::add ~_________ _________\n___" ;
      tp
        "pressing an infix which could be valid doesn't commit"
        b
        (keys [K.Pipe; K.Pipe] 0)
        "||~" ;
      tp
        "pressing an infix after true commits it "
        (partial "true" b)
        (key K.Plus 4)
        "true +~" ;
      t
        "moving left off a function autocompletes it anyway"
        (let' "x" (partial "Int::add" b) b)
        (key K.Left 8)
        "let x =~ Int::add _________ _________\n___" ;
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
  describe "Line-based Deletion" (fun () ->
      t
        "K.DeleteToStartOfLine with selection deletes just the selection"
        (let veryLongString =
           "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"
         in
         fn
           "HttpClient::post_v4"
           [ emptyStr
           ; record [("data", str veryLongString)]
           ; emptyRecord
           ; emptyRecord ])
        (selectionPress K.DeleteToStartOfLine 114 66)
        "HttpClient::postv4\n  \"\"\n  {\n    data : \"abcdefghijklmnopqrstuvwxyz~1234567890abcd\n           efghijklmnopqrstuvwxyz\"\n  }\n  {}\n  {}" ;
      t
        "K.DeleteToEndOfLine with selection deletes just the selection"
        (let veryLongString =
           "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"
         in
         fn
           "HttpClient::post_v4"
           [ emptyStr
           ; record [("data", str veryLongString)]
           ; emptyRecord
           ; emptyRecord ])
        (selectionPress K.DeleteToEndOfLine 114 66)
        "HttpClient::postv4\n  \"\"\n  {\n    data : \"abcdefghijklmnopqrstuvwxyz~1234567890abcd\n           efghijklmnopqrstuvwxyz\"\n  }\n  {}\n  {}" ;
      t
        "K.DeleteToStartOfLine with no selection deletes to visual start of line"
        (let veryLongString =
           "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"
         in
         fn
           "HttpClient::post_v4"
           [ emptyStr
           ; record [("data", str veryLongString)]
           ; emptyRecord
           ; emptyRecord ])
        (key K.DeleteToStartOfLine 66)
        "HttpClient::postv4\n  \"\"\n  {\n    ~*** : \"1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234\n          567890abcdefghijklmnopqrstuvwxyz\"\n  }\n  {}\n  {}" ;
      t
        "K.DeleteToEndOfLine with no selection deletes to visual end of line"
        (let veryLongString =
           "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"
         in
         fn
           "HttpClient::post_v4"
           [ emptyStr
           ; record [("data", str veryLongString)]
           ; emptyRecord
           ; emptyRecord ])
        (key K.DeleteToEndOfLine 66)
        "HttpClient::postv4\n  \"\"\n  {\n    data : \"abcdefghijklmnopqrstuvwxyz~EFGHIJKLMNOPQR\n           STUVWXYZ1234567890abcdefghijklmnopqrstuv\n           wxyz\"\n  }\n  {}\n  {}" ;
      t
        "K.DeleteToStartOfLine deletes up to line start at the end of a wrapping string"
        (let veryLongString =
           "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"
         in
         fn
           "HttpClient::post_v4"
           [ emptyStr
           ; record [("data", str veryLongString)]
           ; emptyRecord
           ; emptyRecord ])
        (key K.DeleteToStartOfLine 163)
        "HttpClient::postv4\n  \"\"\n  {\n    data : \"abcdefghijklmnopqrstuvwxyz1234567890ABCD\n           EFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefgh~\"\n  }\n  {}\n  {}" ;
      () ) ;
  describe "Selection Movement" (fun () ->
      ts
        "shift right selects"
        longLets
        (modkeys
           [ ( K.Right
             , { shiftKey = true
               ; altKey = false
               ; metaKey = false
               ; ctrlKey = false } ) ]
           0)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , (Some 0, 4) ) ;
      ts
        "shift down selects"
        longLets
        (modkeys
           [ ( K.Down
             , { shiftKey = true
               ; altKey = false
               ; metaKey = false
               ; ctrlKey = false } ) ]
           4)
        ( "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\""
        , (Some 4, 52) ) ;
      ts
        "shift left selects"
        longLets
        (modkeys
           [ ( K.Left
             , { shiftKey = true
               ; altKey = false
               ; metaKey = false
               ; ctrlKey = false } ) ]
           52)
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
        "let ~firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"" ;
      t
        "shiftless left aborts right-to-left selection on left"
        longLets
        (selectionPress K.Left 52 4)
        "let ~firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"" ;
      t
        "shiftless right aborts left-to-right selection on right"
        longLets
        (selectionPress K.Right 4 52)
        "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet ~secondLetName = \"0123456789\"\n\"RESULT\"" ;
      t
        "shiftless right aborts right-to-left selection on right"
        longLets
        (selectionPress K.Right 52 4)
        "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet ~secondLetName = \"0123456789\"\n\"RESULT\"" ;
      t
        "selecting an expression pipes from it 1"
        (binop "+" (int "4") (int "5"))
        (selectionPress ~shiftHeld:true K.ShiftEnter 4 5)
        "4 + 5\n    |>~___\n" ;
      t
        "selecting an expression pipes from it 2"
        (binop "+" (int "4") (int "5"))
        (selectionPress ~shiftHeld:true K.ShiftEnter 5 4)
        "4 + 5\n    |>~___\n" ;
      ts
        "K.ShiftEnter doesn't persist selection"
        anInt
        (selectionPress ~shiftHeld:true K.ShiftEnter 0 5)
        ("12345\n|>___\n", (None, 8)) ;
      ts
        "K.SelectAll selects all"
        longLets
        (key K.SelectAll 4)
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
      t "tab goes to first block in a let" emptyLet (tab 0) "let ~*** = ___\n5" ;
      t
        "tab goes when on blank"
        completelyEmptyLet
        (tab 10)
        "let *** = ___\n~___" ;
      t
        "tab goes to second block in a let"
        emptyLet
        (tab 4)
        "let *** = ~___\n5" ;
      t "tab wraps second block in a let" emptyLet (tab 15) "let ~*** = ___\n5" ;
      t
        "shift tab goes to last block in a let"
        emptyLet
        (shiftTab 14)
        "let *** = ~___\n5" ;
      t
        "shift tab goes to previous block in a let"
        emptyLet
        (shiftTab 10)
        "let ~*** = ___\n5" ;
      t
        "shift tab completes autocomplete"
        completelyEmptyLet
        (keys [K.Letter 'i'; K.Letter 'f'; K.ShiftTab] 14)
        "let *** = ~___\nif ___\nthen\n  ___\nelse\n  ___" ;
      t
        "shift-tab goes when on blank"
        completelyEmptyLet
        (shiftTab 14)
        "let *** = ~___\n___" ;
      t
        "shift tab wraps from start of let"
        emptyLet
        (shiftTab 4)
        "let *** = ~___\n5" ;
      t "cant tab to filled letLHS" letWithLhs (tab 0) "~let n = 6\n5" ;
      t "can tab to lambda blank" aLambda (tab 0) "\\~*** -> ___" ;
      t "can shift tab to field blank" aBlankField (shiftTab 0) "obj.~***" ;
      () ) ;
  ()
