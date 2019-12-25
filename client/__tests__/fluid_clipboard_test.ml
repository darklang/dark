open Tester
open Tc
open Types
open Prelude
module DClipboard = Clipboard
open Fluid
open Fluid_test_data
module B = Blank
module K = FluidKeyboard

type testResult = (* ast, clipboard, newPos *) string * string * int

let clipboardEvent () =
  [%bs.obj
    { clipboardData =
        (object (_self)
           method getData (contentType : string) =
             let _ = contentType in
             (* avoid warning *)
             [%bs.raw
               "this.hiddenContent ? (this.hiddenContent[contentType] ? this.hiddenContent[contentType] : \"\") : \"\""]

           method setData (contentType : string) (data : string) =
             let _ = data in
             (* avoid warning *)
             let _ = contentType in
             (* avoid warning *)
             let _ =
               [%bs.raw
                 "(this.hiddenContent = (this.hiddenContent || {})) && (this.hiddenContent[contentType] = data)"]
             in
             ()
        end [@bs])
    ; preventDefault = [%bs.raw {| function () { return null; } |}] }]


let execute_roundtrip (ast : fluidExpr) =
  let ast = Fluid.clone ast in
  let expectedString = Printer.eToString ast in
  let pos = String.length expectedString in
  let e = clipboardEvent () in
  let h = Fluid_utils.h ast in
  let m =
    { defaultTestModel with
      handlers = Handlers.fromList [h]
    ; cursorState = FluidEntering h.hTLID
    ; fluidState =
        { defaultTestState with
          selectionStart = Some 0
        ; oldPos = pos
        ; newPos = pos } }
  in
  let mod_ = App.update_ (ClipboardCutEvent e) m in
  let newM, _cmd = App.updateMod mod_ (m, Cmd.none) in
  let mod_ = App.update_ (ClipboardPasteEvent e) newM in
  let newM, _cmd = App.updateMod mod_ (newM, Cmd.none) in
  let newAST =
    TL.selectedAST newM
    |> Option.withDefault ~default:(Blank.new_ ())
    |> E.fromNExpr
  in
  newAST


let run () =
  E.functions := Fluid_test_data.defaultTestFunctions ;
  let process (e : clipboardEvent) ~debug (start, pos) ast msg : testResult =
    let clipboardData e =
      DClipboard.getData e
      |> FluidClipboard.clipboardContentsToExpr
      |> Option.map ~f:Printer.eToString
      |> Option.withDefault ~default:"Nothing in clipboard"
    in
    let h = Fluid_utils.h ast in
    let m =
      { defaultTestModel with
        handlers = Handlers.fromList [h]
      ; cursorState = FluidEntering h.hTLID
      ; fluidState =
          { defaultTestState with
            selectionStart = Some start
          ; oldPos = pos
          ; newPos = pos } }
    in
    if debug
    then (
      Js.log2 "state before " (Fluid_utils.debugState m.fluidState) ;
      Js.log2 "ast before" (Printer.eToStructure ast) ;
      Js.log2 "clipboard before" (clipboardData e) ) ;
    let mod_ = App.update_ msg m in
    let newM, _cmd = App.updateMod mod_ (m, Cmd.none) in
    let newState = newM.fluidState in
    let newAST =
      TL.selectedAST newM
      |> Option.withDefault ~default:(Blank.new_ ())
      |> E.fromNExpr
    in
    let finalPos = newState.newPos in
    if debug
    then (
      Js.log2 "state after" (Fluid_utils.debugState newState) ;
      Js.log2 "expr after" (Printer.eToStructure newAST) ;
      Js.log2 "clipboard after" (clipboardData e) ) ;
    (Printer.eToString newAST, clipboardData e, finalPos)
  in
  let copy ?(debug = false) (range : int * int) (expr : fluidExpr) : testResult
      =
    let e = clipboardEvent () in
    process ~debug e range expr (ClipboardCopyEvent e)
  in
  let cut ?(debug = false) (range : int * int) (expr : fluidExpr) : testResult =
    let e = clipboardEvent () in
    process ~debug e range expr (ClipboardCutEvent e)
  in
  let paste
      ?(debug = false)
      ~(clipboard : fluidExpr)
      (range : int * int)
      (expr : fluidExpr) : testResult =
    let e = clipboardEvent () in
    let data = FluidClipboard.exprToClipboardContents clipboard in
    DClipboard.setData data e ;
    process ~debug e range expr (ClipboardPasteEvent e)
  in
  let pasteText
      ?(debug = false)
      ~(clipboard : string)
      (range : int * int)
      (expr : fluidExpr) : testResult =
    let e = clipboardEvent () in
    DClipboard.setData (`Text clipboard) e ;
    process ~debug e range expr (ClipboardPasteEvent e)
  in
  let t
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      (expected : string * string * int) =
    let insertCursor (str, clipboardStr, pos) : string * string * int =
      let cursorString = "~" in
      ( match str |> String.splitAt ~index:pos with
      | a, b ->
          [a; b] |> String.join ~sep:cursorString )
      |> fun s -> (s, clipboardStr, pos)
    in
    test
      ( name
      ^ " - `"
      ^ ( Printer.eToString initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () ->
        expect (fn initial |> insertCursor) |> toEqual (expected |> insertCursor))
  in
  let roundtrip ?(debug = false) (ast : fluidExpr) =
    let name = "roundtripping: " in
    let expectedString = Printer.eToString ast in
    test
      ( name
      ^ " - `"
      ^ (expectedString |> Regex.replace ~re:(Regex.regex "\n") ~repl:" ")
      ^ "`" )
      (fun () ->
        if debug then Js.log2 "ast before" (Printer.eToStructure ast) ;
        let newAST = execute_roundtrip ast in
        expect expectedString |> toEqual (Printer.eToString newAST))
  in
  let pipeOn expr fns = EPipe (gid (), expr :: fns) in
  let emptyList = EList (gid (), []) in
  let aListNum n = EList (gid (), [EInteger (gid (), n)]) in
  let listFn args =
    EFnCall (gid (), "List::append", EPipeTarget (gid ()) :: args, NoRail)
  in
  let aPipe = pipeOn emptyList [listFn [aListNum "5"]; listFn [aListNum "5"]] in
  describe "Booleans" (fun () ->
      t
        "copying a bool adds an EBool to clipboard"
        (EBool (gid (), true))
        (copy (0, 4))
        ("true", "true", 4) ;
      t
        "copying a bool adds an EBool to clipboard 2"
        (EFnCall (gid (), "Bool::not", [EBool (gid (), true)], NoRail))
        (copy (10, 14))
        ("Bool::not true", "true", 14) ;
      t
        "cutting a bool adds an EBool to clipboard and leave a blank"
        (EBool (gid (), false))
        (cut (0, 5))
        ("___", "false", 0) ;
      t
        "cutting a bool adds an EBool to clipboard 2"
        (EFnCall (gid (), "Bool::not", [EBool (gid (), true)], NoRail))
        (cut (10, 14))
        ("Bool::not ___", "true", 10) ;
      t
        "pasting an EBool from clipboard on a blank should paste it"
        (EBlank (gid ()))
        (paste ~clipboard:(EBool (gid (), true)) (0, 0))
        ("true", "true", 4) ;
      ()) ;
  describe "Nulls" (fun () ->
      t
        "copying a null adds an ENull to clipboard"
        (ENull (gid ()))
        (copy (0, 4))
        ("null", "null", 4) ;
      t
        "copying a null adds an ENull to clipboard 2"
        (EFnCall (gid (), "Bool::isNull", [ENull (gid ())], NoRail))
        (copy (13, 17))
        ("Bool::isNull null", "null", 17) ;
      t
        "cutting a null adds an ENull to clipboard and leave a blank"
        (ENull (gid ()))
        (cut (0, 4))
        ("___", "null", 0) ;
      t
        "cutting a null adds an ENull to clipboard 2"
        (EFnCall (gid (), "Bool::isNull", [ENull (gid ())], NoRail))
        (cut (13, 17))
        ("Bool::isNull ___", "null", 13) ;
      t
        "pasting an ENull from clipboard on a blank should paste it"
        (EBlank (gid ()))
        (paste ~clipboard:(ENull (gid ())) (0, 0))
        ("null", "null", 4) ;
      ()) ;
  describe "Integers" (fun () ->
      t
        "copying an int adds an EInteger to clipboard"
        (EInteger (gid (), "1000"))
        (copy (0, 4))
        ("1000", "1000", 4) ;
      t
        "copying an int adds an EInteger to clipboard 2"
        (EFnCall (gid (), "Int::sqrt", [EInteger (gid (), "1000")], NoRail))
        (copy (10, 14))
        ("Int::sqrt 1000", "1000", 14) ;
      t
        "copying part of an int adds part of the EInteger to clipboard"
        (EInteger (gid (), "1234"))
        (copy (0, 2))
        ("1234", "12", 2) ;
      t
        "cutting an int adds an EInteger to clipboard and leaves a blank"
        (EInteger (gid (), "1000"))
        (cut (0, 4))
        ("___", "1000", 0) ;
      t
        "cutting an int adds an EInteger to clipboard and leaves a blank 2"
        (EFnCall (gid (), "Int::sqrt", [EInteger (gid (), "1000")], NoRail))
        (cut (10, 14))
        ("Int::sqrt _________", "1000", 10) ;
      t
        "cutting part of an int adds part of the EInteger to clipboard and leaves the remaining EInteger"
        (EInteger (gid (), "1234"))
        (cut (0, 2))
        ("34", "12", 0) ;
      t
        "pasting an EInteger from clipboard on a blank should paste it"
        (EBlank (gid ()))
        (paste ~clipboard:(EInteger (gid (), "1234")) (0, 0))
        ("1234", "1234", 4) ;
      t
        "pasting an EInteger into another integer should join the integers"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EInteger (gid (), "1234")) (1, 3))
        ("512348", "1234", 5) ;
      t
        "pasting an EFloat into an integer should convert to float"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EFloat (gid (), "12", "34")) (1, 3))
        ("512.348", "12.34", 6) ;
      t
        "pasting an EFloat into an integer should convert to float 2"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EFloat (gid (), "12", "34")) (0, 0))
        ("12.345678", "12.34", 5) ;
      t
        "pasting an EFloat into an integer should convert to float 3"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EFloat (gid (), "12", "34")) (4, 4))
        ("567812.34", "12.34", 9) ;
      t
        "pasting an EVariable into an integer should convert to parital"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EVariable (gid (), "myVar")) (0, 0))
        ("myVar5678", "myVar", 5) ;
      t
        "pasting an EVariable into an integer should convert to parital 2"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EVariable (gid (), "myVar")) (1, 1))
        ("5myVar678", "myVar", 6) ;
      t
        "pasting an int-only EString into an integer should extend integer"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EString (gid (), "1234")) (0, 0))
        ("12345678", "1234", 4) ;
      t
        "pasting an int-only EString into an integer should extend integer 2"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EString (gid (), "1234")) (4, 4))
        ("56781234", "1234", 8) ;
      t
        "pasting an int-only EString into an integer should extend integer 2"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EString (gid (), "1234")) (2, 2))
        ("56123478", "1234", 6) ;
      t
        "pasting an int-only EString over part of an integer should extend integer"
        (EInteger (gid (), "5678"))
        (paste ~clipboard:(EString (gid (), "1234")) (1, 3))
        ("512348", "1234", 5) ;
      ()) ;
  describe "Strings" (fun () ->
      t
        "copying a string adds an EString to clipboard"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (copy (0, 21))
        ("\"abcd EFGH ijkl 1234\"", "\"abcd EFGH ijkl 1234\"", 21) ;
      t
        "copying a string adds an EString to clipboard 2"
        (EFnCall
           ( gid ()
           , "String::reverse"
           , [EString (gid (), "abcd EFGH ijkl 1234")]
           , NoRail ))
        (copy (16, 37))
        ( "String::reverse \"abcd EFGH ijkl 1234\""
        , "\"abcd EFGH ijkl 1234\""
        , 37 ) ;
      t
        "copying part of a string adds an EString to clipboard"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (copy (4, 14))
        ("\"abcd EFGH ijkl 1234\"", "\"d EFGH ijk\"", 14) ;
      t
        "cutting a string adds an EString to clipboard"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (cut (0, 21))
        ("___", "\"abcd EFGH ijkl 1234\"", 0) ;
      t
        "cutting a string adds an EString to clipboard 2"
        (EFnCall
           ( gid ()
           , "String::reverse"
           , [EString (gid (), "abcd EFGH ijkl 1234")]
           , NoRail ))
        (cut (16, 37))
        ("String::reverse ___", "\"abcd EFGH ijkl 1234\"", 16) ;
      t
        "cutting part of a string adds an EString to clipboard"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (cut (4, 14))
        ("\"abcl 1234\"", "\"d EFGH ijk\"", 4) ;
      t
        "pasting an EString on a blank should paste it"
        (EBlank (gid ()))
        (paste ~clipboard:(EString (gid (), "abcd EFGH ijkl 1234")) (0, 0))
        ("\"abcd EFGH ijkl 1234\"", "\"abcd EFGH ijkl 1234\"", 21) ;
      t
        "pasting an EString in another string should paste it"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (paste ~clipboard:(EString (gid (), "newString")) (11, 15))
        ("\"abcd EFGH newString 1234\"", "\"newString\"", 20) ;
      t
        "pasting an EString in a TLStringMLStart should paste it"
        (EString
           ( gid ()
           , "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij" ))
        (paste ~clipboard:(EString (gid (), "XXX")) (11, 15))
        ( "\"0123456789XXXefghij0123456789abcdefghij0\n123456789abcdefghij\""
        , "\"XXX\""
        , 14 ) ;
      t
        "pasting an EString in the first TLStringMLMiddle should paste it"
        (EString
           ( gid ()
           , "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"
           ))
        (paste ~clipboard:(EString (gid (), "XXX")) (43, 50))
        ( "\"0123456789abcdefghij0123456789abcdefghij\n0XXX89abcdefghij0123456789abcdefghij0123\n456789abcdefghij0123456789abcdefghij0123\n456789abcdefghij\""
        , "\"XXX\""
        , 46 ) ;
      t
        "pasting an EString in the second TLStringMLMiddle should paste it"
        (EString
           ( gid ()
           , "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"
           ))
        (paste ~clipboard:(EString (gid (), "XXX")) (96, 84))
        ( "\"0123456789abcdefghij0123456789abcdefghij\n0123456789abcdefghij0123456789abcdefghij\n0XXXdefghij0123456789abcdefghij012345678\n9abcdefghij\""
        , "\"XXX\""
        , 87 ) ;
      t
        "pasting an EString in a TLStringMLEnd should paste it"
        (EString
           ( gid ()
           , "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"
           ))
        (paste ~clipboard:(EString (gid (), "XXX")) (125, 126))
        ( "\"0123456789abcdefghij0123456789abcdefghij\n0123456789abcdefghij0123456789abcdefghij\n0123456789abcdefghij0123456789abcdefghij\n0XXX23456789abcdefghij\""
        , "\"XXX\""
        , 128 ) ;
      t
        "pasting an EInteger in a string should paste it"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (paste ~clipboard:(EInteger (gid (), "5678")) (11, 15))
        ("\"abcd EFGH 5678 1234\"", "5678", 15) ;
      t
        "pasting an ERecord with a single key & no value in a string should paste key"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (paste
           ~clipboard:(ERecord (gid (), [(gid (), "key1", EBlank (gid ()))]))
           (11, 15))
        ("\"abcd EFGH key1 1234\"", "{\n  key1 : ___\n}", 15) ;
      t
        "pasting a regular ERecord with a single key in a string should paste stringified expr"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (paste
           ~clipboard:
             (ERecord (gid (), [(gid (), "key1", EInteger (gid (), "9876"))]))
           (11, 15))
        ("\"abcd EFGH {\n  key1 : 9876\n} 1234\"", "{\n  key1 : 9876\n}", 28) ;
      ()) ;
  describe "Floats" (fun () ->
      t
        "copying a float adds an EFloat to clipboard"
        (EFloat (gid (), "1234", "5678"))
        (copy (0, 9))
        ("1234.5678", "1234.5678", 9) ;
      t
        "copying a float adds an EFloat to clipboard 2"
        (EFnCall
           (gid (), "Float::round", [EFloat (gid (), "1234", "5678")], NoRail))
        (copy (13, 22))
        ("Float::round 1234.5678", "1234.5678", 22) ;
      t
        "copying the whole part w/o the point adds an EInt to clipboard"
        (EFloat (gid (), "1234", "5678"))
        (copy (0, 4))
        ("1234.5678", "1234", 4) ;
      t
        "copying the whole part w/ the point adds an EFloat with fraction value of 0 to clipboard"
        (EFloat (gid (), "1234", "5678"))
        (copy (0, 5))
        ("1234.5678", "1234.0", 5) ;
      t
        "copying the fraction part w/o the point adds an EInt to clipboard"
        (EFloat (gid (), "1234", "5678"))
        (copy (5, 9))
        ("1234.5678", "5678", 9) ;
      t
        "copying the fraction part w/ the point adds an EFloat with whole value of 0 to clipboard"
        (EFloat (gid (), "1234", "5678"))
        (copy (4, 9))
        ("1234.5678", "0.5678", 9) ;
      t
        "copying just the point adds an EFloat with 0.0 to clipboard"
        (EFloat (gid (), "1234", "5678"))
        (copy (4, 5))
        ("1234.5678", "0.0", 5) ;
      t
        "cutting a float adds an EFloat to clipboard"
        (EFloat (gid (), "1234", "5678"))
        (cut (0, 9))
        ("___", "1234.5678", 0) ;
      t
        "cutting a float adds an EFloat to clipboard 2"
        (EFnCall
           (gid (), "Float::round", [EFloat (gid (), "1234", "5678")], NoRail))
        (cut (13, 22))
        ("Float::round ___", "1234.5678", 13) ;
      t
        "cutting the whole part w/o the point adds an EInt to clipboard, leaves EFloat"
        (EFloat (gid (), "1234", "5678"))
        (cut (0, 4))
        (".5678", "1234", 0) ;
      t
        "cutting the whole part w/ the point adds an EFloat with fraction value of 0 to clipboard, leaves EInt"
        (EFloat (gid (), "1234", "5678"))
        (cut (0, 5))
        ("5678", "1234.0", 0) ;
      t
        "cutting the fraction part w/o the point adds an EInt to clipboard, leaves EFloat"
        (EFloat (gid (), "1234", "5678"))
        (cut (5, 9))
        ("1234.", "5678", 5) ;
      t
        "cutting the fraction part w/ the point adds an EFloat with whole value of 0 to clipboard, leaves EInt"
        (EFloat (gid (), "1234", "5678"))
        (cut (4, 9))
        ("1234", "0.5678", 4) ;
      t
        "cutting just the point adds an EFloat with 0.0 to clipboard, leaves EInt of joint expr"
        (EFloat (gid (), "1234", "5678"))
        (cut (4, 5))
        ("12345678", "0.0", 4) ;
      t
        "pasting an EFloat from clipboard on a blank should paste it"
        (EBlank (gid ()))
        (paste ~clipboard:(EFloat (gid (), "1234", "5678")) (0, 0))
        ("1234.5678", "1234.5678", 9) ;
      t
        "pasting an EInteger in a float whole part should paste it"
        (EFloat (gid (), "1234", "5678"))
        (paste ~clipboard:(EInteger (gid (), "9000")) (0, 0))
        ("90001234.5678", "9000", 4) ;
      t
        "pasting an EInteger in a float whole part should paste it 2"
        (EFloat (gid (), "1234", "5678"))
        (paste ~clipboard:(EInteger (gid (), "9000")) (1, 3))
        ("190004.5678", "9000", 5) ;
      t
        "pasting an EInteger in a float fraction part should paste it"
        (EFloat (gid (), "1234", "5678"))
        (paste ~clipboard:(EInteger (gid (), "9000")) (8, 8))
        ("1234.56790008", "9000", 12) ;
      t
        "pasting an EInteger over a float fraction part should paste it and remove selection"
        (EFloat (gid (), "1234", "5678"))
        (paste ~clipboard:(EInteger (gid (), "9000")) (6, 8))
        ("1234.590008", "9000", 10) ;
      t
        "pasting an EInteger before a float point should paste it"
        (EFloat (gid (), "1234", "5678"))
        (paste ~clipboard:(EInteger (gid (), "9000")) (4, 4))
        ("12349000.5678", "9000", 8) ;
      t
        "pasting an EInteger after a float point should paste it"
        (EFloat (gid (), "1234", "5678"))
        (paste ~clipboard:(EInteger (gid (), "9000")) (5, 5))
        ("1234.90005678", "9000", 9) ;
      t
        "pasting an EInteger over a float point should paste it"
        (EFloat (gid (), "1234", "5678"))
        (paste ~clipboard:(EInteger (gid (), "9000")) (3, 6))
        ("1239000678", "9000", 7) ;
      ()) ;
  describe "Variables" (fun () ->
      t
        "copying adds an EVariable to clipboard"
        (EVariable (gid (), "varName"))
        (copy (0, 7))
        ("varName", "varName", 7) ;
      t
        "copying part of it adds an EVariable to clipboard"
        (EVariable (gid (), "varName"))
        (copy (0, 3))
        ("varName", "var", 3) ;
      t
        "cutting adds an EVariable to clipboard and leaves a blank"
        (EVariable (gid (), "varName"))
        (cut (0, 7))
        ("___", "varName", 0) ;
      t
        "cutting part of it adds an EVariable to clipboard and leaves a partial"
        (EVariable (gid (), "varName"))
        (cut (0, 3))
        ("Name", "var", 0) ;
      t
        "pasting variable into blank works"
        (EBlank (gid ()))
        (paste ~clipboard:(EVariable (gid (), "varName")) (0, 0))
        ("varName", "varName", 7) ;
      t
        "pasting variable into empty let lhs works"
        (ELet (gid (), gid (), "", EBlank (gid ()), EBlank (gid ())))
        (paste ~clipboard:(EVariable (gid (), "varName")) (7, 7))
        ("let varName = ___\n___", "varName", 14) ;
      t
        "pasting variable into filled let lhs works"
        (ELet (gid (), gid (), "oldLetLhs", EBlank (gid ()), EBlank (gid ())))
        (paste ~clipboard:(EVariable (gid (), "varName")) (7, 7))
        ("let oldvarNameLetLhs = ___\n___", "varName", 14) ;
      t
        "pasting variable over filled let lhs works"
        (ELet (gid (), gid (), "oldLetLhs", EBlank (gid ()), EBlank (gid ())))
        (paste ~clipboard:(EVariable (gid (), "varName")) (7, 13))
        ("let oldvarName = ___\n___", "varName", 14) ;
      ()) ;
  describe "Field Accesses" (fun () ->
      t
        "copying adds an EFieldAccess to clipboard"
        (EFieldAccess (gid (), EVariable (gid (), "request"), gid (), "body"))
        (copy (0, 12))
        ("request.body", "request.body", 12) ;
      t
        "copying the preceding expresssion adds it to clipboard"
        (EFieldAccess (gid (), EVariable (gid (), "request"), gid (), "body"))
        (copy (0, 7))
        ("request.body", "request", 7) ;
      (* NOT WORKING YET
      t
        "copying field part adds an EVariable to clipboard"
        (EFieldAccess (gid (), EVariable (gid (), "request"), gid (), "body"))
        (copy (8, 12))
        ("request.body", "body", 8) ;
      t
        "cutting adds an EFieldAccess to clipboard and leaves a blank"
        (EFieldAccess (gid (), EVariable (gid (), "request"), gid (), "body"))
        (cut (0, 12))
        ("___", "request.body", 0) ; 
      t 
        "cutting the preceding expression adds an EFieldAccess w empty field to clipboard and leaves the field"
        (EFieldAccess (gid (), EVariable (gid (), "request"), gid (), "body"))
        (cut (0, 12))
        ("___.body", "request.***", 8) ;
      t
        "cutting the field part adds an EFieldAccess on a blank to clipboard and leaves the expression"
        (EFieldAccess (gid (), EVariable (gid (), "request"), gid (), "body"))
        (cut (8, 12))
        ("request.***", "___.body", 8) ; *)
      ()) ;
  describe "If conditions" (fun () ->
      t
        "copying the whole expression adds an EIf to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (copy (0, 45))
        ( "if true\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if true\nthen\n  \"then body\"\nelse\n  \"else body\""
        , 45 ) ;
      t
        "cutting the whole expression adds an EIf to clipboard and leaves a blank"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (cut (0, 45))
        ("___", "if true\nthen\n  \"then body\"\nelse\n  \"else body\"", 0) ;
      t
        "copying just the condition adds then condition expression to clipboard "
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (copy (3, 7))
        ("if true\nthen\n  \"then body\"\nelse\n  \"else body\"", "true", 7) ;
      t
        "copying the if keyword and the condition adds an EIf with blank then & else body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (copy (0, 7))
        ( "if true\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if true\nthen\n  ___\nelse\n  ___"
        , 7 ) ;
      t
        "copying the condition and the then keyword adds an EIf with blank then & else body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (copy (3, 12))
        ( "if true\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if true\nthen\n  ___\nelse\n  ___"
        , 12 ) ;
      t
        "copying just the then body adds the then expression to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (copy (15, 26))
        ( "if true\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "\"then body\""
        , 26 ) ;
      t
        "copying the then keyword and the then body adds an EIf with blank condition & else body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (copy (8, 26))
        ( "if true\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if ___\nthen\n  \"then body\"\nelse\n  ___"
        , 26 ) ;
      t
        "copying the then body and the else keyword adds an EIf with blank condition & else body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (copy (12, 31))
        ( "if true\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if ___\nthen\n  \"then body\"\nelse\n  ___"
        , 31 ) ;
      t
        "copying just the else body adds the else expression to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (copy (34, 45))
        ( "if true\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "\"else body\""
        , 45 ) ;
      t
        "copying the else keyword and else body adds an EIf with blank condition &then body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (copy (27, 45))
        ( "if true\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if ___\nthen\n  ___\nelse\n  \"else body\""
        , 45 ) ;
      t
        "cutting just the condition adds then condition expression to clipboard "
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (cut (3, 7))
        ("if ___\nthen\n  \"then body\"\nelse\n  \"else body\"", "true", 3) ;
      t
        "cutting the if keyword and the condition adds an EIf with blank then & else body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (cut (0, 7))
        ( "if ___\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if true\nthen\n  ___\nelse\n  ___"
        , 3 ) ;
      t
        "cutting the condition and the then keyword adds an EIf with blank then & else body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (cut (3, 12))
        ( "if ___\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if true\nthen\n  ___\nelse\n  ___"
        , 3 ) ;
      t
        "cutting just the then body adds the then expression to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (cut (15, 26))
        ("if true\nthen\n  ___\nelse\n  \"else body\"", "\"then body\"", 15) ;
      t
        "cutting the then keyword and the then body adds an EIf with blank condition & else body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (cut (8, 26))
        ( "if true\nthen\n  ___\nelse\n  \"else body\""
        , "if ___\nthen\n  \"then body\"\nelse\n  ___"
        , 8 ) ;
      t
        "cutting the then body and the else keyword adds an EIf with blank condition & else body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (cut (12, 31))
        ( "if true\nthen\n  ___\nelse\n  \"else body\""
        , "if ___\nthen\n  \"then body\"\nelse\n  ___"
        , 12 ) ;
      t
        "cutting just the else body adds the else expression to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (cut (34, 45))
        ("if true\nthen\n  \"then body\"\nelse\n  ___", "\"else body\"", 34) ;
      t
        "cutting the else keyword and else body adds an EIf with blank condition &then body to clipboard"
        (EIf
           ( gid ()
           , EBool (gid (), true)
           , EString (gid (), "then body")
           , EString (gid (), "else body") ))
        (cut (27, 45))
        ( "if true\nthen\n  \"then body\"\nelse\n  ___"
        , "if ___\nthen\n  ___\nelse\n  \"else body\""
        , 27 ) ;
      ()) ;
  describe "Bin-ops" (fun () ->
      (* NOT WORKING YET
      t
        "copying a single-char operator works"
        (EBinOp
           (gid (), "<", EInteger (gid (), "123"), EInteger (gid (), "456"), NoRail))
        (copy (6, 7))
        ("123 < 456", "_________ < _________", 4) ;
      t
        "copying a multi-char operator works"
        (EBinOp
           ( gid ()
           , "=="
           , EInteger (gid (), "123")
           , EInteger (gid (), "456")
           , NoRail ))
        (copy (4, 6))
        ("123 == 456", "_________ == _________", 4) ;
      t
        "copying part of a multi-char operator works"
        (EBinOp
           ( gid ()
           , "=="
           , EInteger (gid (), "123")
           , EInteger (gid (), "456")
           , NoRail ))
        (copy (4, 5))
        ("123 == 456", "_________ =@ _________", 4) ;
 *)
      ()) ;
  describe "Functions" (fun () ->
      t
        "copying a function name adds an EFnCall w blank arguments to clipboard"
        (fn "Int::sqrt" [int "122"])
        (copy (0, 9))
        ("Int::sqrt 122", "Int::sqrt _________", 9) ;
      t
        "copying a function name with a version adds an EFnCall, not a partial"
        (fn "HttpClient::post_v4" [str ""])
        (copy (0, 18))
        ("HttpClient::postv4 \"\"", "HttpClient::postv4 ______________", 18) ;
      t
        "copying part of a function name adds a partial EFnCall w blank arguments to clipboard"
        (fn "Int::sqrt" [int "122"])
        (copy (0, 4))
        ("Int::sqrt 122", "Int:@sqr@ _________", 4) ;
      t
        "copying a function's argument adds the argument's expression to clipboard"
        (EFnCall (gid (), "Int::sqrt", [EInteger (gid (), "122")], NoRail))
        (copy (10, 13))
        ("Int::sqrt 122", "122", 13) ;
      t
        "cutting a function name adds an EFnCall w blank arguments to clipboard and leaves a blank"
        (EFnCall (gid (), "Int::sqrt", [EInteger (gid (), "122")], NoRail))
        (cut (0, 9))
        ("___", "Int::sqrt _________", 0) ;
      t
        "cutting part of a fn name adds a partial EFnCall w blank arguments to clipboard and leaves a partial"
        (EFnCall (gid (), "Int::sqrt", [EInteger (gid (), "122")], NoRail))
        (cut (0, 4))
        (":sqrt@qr@ 122", "Int:@sqr@ _________", 0) ;
      t
        "cutting a function's argument adds the argument's expression to clipboard and leaves a blank there"
        (fn "Int::sqrt" [int "122"])
        (cut (10, 13))
        ("Int::sqrt _________", "122", 10) ;
      ()) ;
  describe "Pipes" (fun () ->
      t
        "copying first expression of pipe adds it to clipboard"
        aPipe
        (copy (0, 2))
        ("[]\n|>List::append [5]\n|>List::append [5]\n", "[]", 2) ;
      t
        "copying pipe adds it to clipboard"
        aPipe
        (copy (0, 41))
        ( "[]\n|>List::append [5]\n|>List::append [5]\n"
        , "[]\n|>List::append [5]\n|>List::append [5]\n"
        , 41 ) ;
      ()) ;
  describe "Lists" (fun () ->
      (* NOT WORKING YET
      t
        "copying opening bracket adds empty list expr to clipboard"
        (EList (gid (), [EInteger (gid (), "123")]))
        (copy (0, 1))
        ("[123]", "[]", 0) ; *)
      t
        "copying subset of elements adds subset list expr to clipboard"
        (EList
           ( gid ()
           , [ EInteger (gid (), "123")
             ; EInteger (gid (), "456")
             ; EInteger (gid (), "789") ] ))
        (copy (5, 12))
        ("[123,456,789]", "[456,789]", 12) ;
      t
        "cutting subset of elements adds subset list expr to clipboard and leaves remainder"
        (EList
           ( gid ()
           , [ EInteger (gid (), "123")
             ; EInteger (gid (), "456")
             ; EInteger (gid (), "789") ] ))
        (cut (5, 12))
        ("[123,___]", "[456,789]", 5) ;
      (* NOT WORKING b/c placing the cursor on either side of a separator
       * acts as though it's on the sub-expression
      t
        "pasting an expression into list expr at separator works"
        (EList
           ( gid ()
           , [ EInteger (gid (), "123")
             ; EInteger (gid (), "456")
             ; EInteger (gid (), "789") ] ))
        (paste ~clipboard:(EInteger (gid (), "9000")) (4, 5))
        ("[123,9000,456,789]", "9000", 9) ;
       *)
      t
        "pasting an expression over subset of list expr works"
        (EList
           ( gid ()
           , [ EInteger (gid (), "123")
             ; EInteger (gid (), "456")
             ; EInteger (gid (), "789") ] ))
        (paste ~clipboard:(EInteger (gid (), "9000")) (5, 12))
        ("[123,9000]", "9000", 9) ;
      ()) ;
  describe "Records" (fun () ->
      t
        "copying opening bracket adds empty record expr to clipboard"
        (ERecord (gid (), [(gid (), "key1", EInteger (gid (), "1234"))]))
        (copy (0, 1))
        ("{\n  key1 : 1234\n}", "{}", 1) ;
      t
        "copying a single key adds record w single key to clipboard"
        (ERecord (gid (), [(gid (), "key1", EInteger (gid (), "1234"))]))
        (copy (4, 8))
        ("{\n  key1 : 1234\n}", "{\n  key1 : ___\n}", 8) ;
      t
        "cutting a single key adds record w single key to clipboard and leaves blank in it's place"
        (ERecord (gid (), [(gid (), "key1", EInteger (gid (), "1234"))]))
        (cut (4, 8))
        ("{\n  *** : 1234\n}", "{\n  key1 : ___\n}", 4) ;
      t
        "copying a single k-v pair adds record w single k-v pair to clipboard"
        (ERecord (gid (), [(gid (), "key1", EInteger (gid (), "1234"))]))
        (copy (2, 15))
        ("{\n  key1 : 1234\n}", "{\n  key1 : 1234\n}", 15) ;
      ()) ;
  describe "Constructors" (fun () ->
      t
        "copying adds EConstructor to clipboard"
        (EConstructor (gid (), gid (), "Just", [EInteger (gid (), "100")]))
        (copy (0, 8))
        ("Just 100", "Just 100", 8) ;
      t
        "copying part adds partial EConstructor to clipboard"
        (EConstructor (gid (), gid (), "Just", [EInteger (gid (), "100")]))
        (copy (0, 3))
        ("Just 100", "Jus@ ___", 3) ;
      t
        "cutting adds EConstructor to clipboard and leaves blank"
        (EConstructor (gid (), gid (), "Just", [EInteger (gid (), "100")]))
        (cut (0, 8))
        ("___", "Just 100", 0) ;
      t
        "cutting part adds partial EConstructor to clipboard and leaves partial"
        (EConstructor (gid (), gid (), "Just", [EInteger (gid (), "100")]))
        (cut (0, 3))
        ("t@s@ 100", "Jus@ ___", 0) ;
      ()) ;
  describe "Match" (fun () ->
      (* TODO: test match statements, implementation is slightly inconsistent*)
      ()) ;
  describe "json" (fun () ->
      t
        "pasting a json int makes an int"
        b
        (pasteText ~clipboard:"6" (0, 0))
        ("6", "6", 1) ;
      t
        "pasting a json float makes a float"
        b
        (pasteText ~clipboard:"6.6" (0, 0))
        ("6.6", "6.6", 3) ;
      t
        "pasting a json array makes a list"
        b
        (pasteText ~clipboard:"[ 1 , 2 , 3 , 4 ]" (0, 0))
        ("[1,2,3,4]", "[1,2,3,4]", 9) ;
      t
        "pasting a object list makes a record"
        b
        (pasteText
           ~clipboard:"{ \"a\": \n\"b\", \"c\":[\n1\n,\n2], \"d\" : \n4.5 }"
           (0, 0))
        ( "{\n  a : \"b\"\n  c : [1,2]\n  d : 4.5\n}"
        , "{\n  a : \"b\"\n  c : [1,2]\n  d : 4.5\n}"
        , 35 ) ;
      t
        "pasting text into a string doesn't use the json conversion"
        (str "")
        (pasteText ~clipboard:"[ 1 , 5 ]" (1, 1))
        ( "\"[ 1 , 5 ]\""
        , (* this is wrong due to how the test works *) "[1,5]"
        , 10 ) ;
      ()) ;
  describe "Feature Flags" (fun () ->
      (* TODO: test feature flags, not yet in fluid *) ()) ;
  describe "Copy/paste roundtrip" (fun () ->
      let longString =
        str
          "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
      in
      roundtrip b ;
      roundtrip (int "6") ;
      (* TODO: broken. These are broken because they are copied as strings
       * without quotes, and then parsed as JSON. *)
      (* roundtrip (EString (gid (), "[1 , 5]")) ; *)
      (* roundtrip (EString (gid (), "12345678987654321.12345678987654321")) ; *)
      roundtrip aPipe ;
      roundtrip (fn "HttpClient::post_v4" [str ""]) ;
      roundtrip longString ;
      roundtrip (let' "myVariable" longString b) ;
      roundtrip (record [("a", record [("b", str "c")])]) ;
      ())
