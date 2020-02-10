open Prelude
open Tester
module DClipboard = Clipboard
open Fluid
open Fluid_test_data
module B = BlankOr
module K = FluidKeyboard
open FluidExpression
open FluidShortcuts

type testResult =
  (* ast, clipboard, newPos *)
  string * (string * string option) * int

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
  let ast = FluidExpression.clone ast in
  let expectedString = Printer.eToTestString ast in
  let pos = String.length expectedString in
  let e = clipboardEvent () in
  let mFor ast =
    let h = Fluid_utils.h ast in
    { defaultTestModel with
      handlers = Handlers.fromList [h]
    ; cursorState = FluidEntering h.hTLID
    ; fluidState =
        { defaultTestState with
          selectionStart = Some 0
        ; oldPos = pos
        ; newPos = pos } }
  in
  let m1 = mFor (FluidExpression.clone ast) in
  let mod_ = Main.update_ (ClipboardCutEvent e) m1 in
  let _m, _cmd = Main.updateMod mod_ (m1, Tea.Cmd.none) in
  let m2 = mFor (E.newB ()) in
  let mod_ = Main.update_ (ClipboardPasteEvent e) m2 in
  let newM, _cmd = Main.updateMod mod_ (m2, Tea.Cmd.none) in
  let newAST =
    TL.selectedAST newM
    |> Option.withDefault
         ~default:(EString (gid (), "fake result value for testing"))
  in
  newAST


let run () =
  OldExpr.functions := Fluid_test_data.defaultTestFunctions ;
  let insertCursor (str, pos) : string =
    str
    |> String.splitAt ~index:pos
    |> fun (a, b) -> [a; b] |> String.join ~sep:"~"
  in
  let process (e : clipboardEvent) ~debug (start, pos) ast msg : testResult =
    let clipboardData e =
      let text, expr = DClipboard.getData e in
      FluidClipboard.clipboardContentsToExpr (text, expr)
      |> Option.map ~f:Printer.eToTestString
      |> fun cp -> (text, cp)
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
      Js.log2 "ast before" (Printer.eToStructure ast) ) ;
    let mod_ = Main.update_ msg m in
    let newM, _cmd = Main.updateMod mod_ (m, Tea.Cmd.none) in
    let newState = newM.fluidState in
    let newAST =
      TL.selectedAST newM |> Option.withDefault ~default:(E.newB ())
    in
    let finalPos = newState.newPos in
    if debug
    then (
      Js.log2 "state after" (Fluid_utils.debugState newState) ;
      Js.log2 "expr after" (Printer.eToStructure newAST) ) ;
    (Printer.eToTestString newAST, clipboardData e, finalPos)
  in
  let nameToName (name : string) (initial : fluidExpr) =
    name
    ^ " - `"
    ^ ( Printer.eToTestString initial
      |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
    ^ "`"
  in
  let testCopy
      ?(debug = false)
      (name : string)
      (initial : fluidExpr)
      (range : int * int)
      (* This is the expr copied, and stringified. The copy logic for the
       * string copy buffer is super simple and doesn't need extensive testing.
       * *)
        (expectedClipboard : string) : unit =
    test (nameToName name initial) (fun () ->
        let e = clipboardEvent () in
        let _, (_, resultClipboard), _ =
          process ~debug e range initial (ClipboardCopyEvent e)
        in
        expect resultClipboard |> toEqual (Some expectedClipboard))
  in
  let testCut
      ?(debug = false)
      (name : string)
      (initial : fluidExpr)
      (range : int * int)
      ((expectedExpr, expectedClipboard) : string * string) : unit =
    test (nameToName name initial) (fun () ->
        let e = clipboardEvent () in
        let resultStr, (_, resultClipboard), resultPos =
          process ~debug e range initial (ClipboardCutEvent e)
        in
        expect (insertCursor (resultStr, resultPos), resultClipboard)
        |> toEqual (expectedExpr, Some expectedClipboard))
  in
  let testPasteExpr
      ?(debug = false)
      (name : string)
      (initial : fluidExpr)
      (range : int * int)
      (clipboard : fluidExpr)
      (expectedText : string) : unit =
    test (nameToName name initial) (fun () ->
        let e = clipboardEvent () in
        let text = FluidPrinter.eToTestString clipboard in
        let data =
          (text, Some (FluidClipboard.exprToClipboardContents clipboard))
        in
        DClipboard.setData data e ;
        let resultText, _, resultPos =
          process ~debug e range initial (ClipboardPasteEvent e)
        in
        expect (insertCursor (resultText, resultPos)) |> toEqual expectedText)
  in
  let testPasteText
      ?(debug = false)
      (name : string)
      (initial : fluidExpr)
      (range : int * int)
      (clipboard : string)
      (expectedText : string) : unit =
    test (nameToName name initial) (fun () ->
        let e = clipboardEvent () in
        DClipboard.setData (clipboard, None) e ;
        let resultText, _, resultPos =
          process ~debug e range initial (ClipboardPasteEvent e)
        in
        expect (insertCursor (resultText, resultPos)) |> toEqual expectedText)
  in
  let testCopyPaste
      (name : string)
      (initial : fluidExpr)
      (copyRange : int * int)
      (pasteRange : int * int)
      (expectedText : string) : unit =
    test (nameToName name initial) (fun () ->
        let h = Fluid_utils.h initial in
        let start, pos = copyRange in
        let preCopyModel =
          { defaultTestModel with
            handlers = Handlers.fromList [h]
          ; cursorState = FluidEntering h.hTLID
          ; fluidState =
              { defaultTestState with
                selectionStart = Some start
              ; oldPos = pos
              ; newPos = pos } }
        in
        let e = clipboardEvent () in
        let copyMod = Main.update_ (ClipboardCopyEvent e) preCopyModel in
        let copiedModel, _cmd =
          Main.updateMod copyMod (preCopyModel, Tea.Cmd.none)
        in
        let prePasteModel =
          let newPos, newStart = pasteRange in
          let newFluidState =
            {copiedModel.fluidState with newPos; selectionStart = Some newStart}
          in
          {copiedModel with fluidState = newFluidState}
        in
        let pasteMod = Main.update_ (ClipboardPasteEvent e) prePasteModel in
        let pastedModel, _cmd =
          Main.updateMod pasteMod (prePasteModel, Tea.Cmd.none)
        in
        let finalPos = pastedModel.fluidState.newPos in
        let newAST =
          TL.selectedAST pastedModel |> Option.withDefault ~default:(E.newB ())
        in
        let resultText =
          newAST
          |> Printer.eToTestString
          |> fun str -> insertCursor (str, finalPos)
        in
        expect resultText |> toEqual expectedText)
  in
  let testPaste
      ?(debug = false)
      (name : string)
      (initial : fluidExpr)
      (range : int * int)
      (clipboard : fluidExpr)
      (expectedText : string) : unit =
    let text = FluidPrinter.eToTestString clipboard in
    testPasteText ~debug name initial range text expectedText ;
    testPasteExpr ~debug name initial range clipboard expectedText
  in
  let testCopyText
      ?(debug = false)
      (name : string)
      (initial : fluidExpr)
      (range : int * int)
      (* This is the string copied.  *)
        (expectedClipboard : string) : unit =
    test (nameToName name initial) (fun () ->
        let e = clipboardEvent () in
        let _, (resultClipboard, _), _ =
          process ~debug e range initial (ClipboardCopyEvent e)
        in
        expect resultClipboard |> toEqual expectedClipboard)
  in
  let roundtrip ?(debug = false) (ast : fluidExpr) =
    let name = "roundtripping: " in
    let expectedString = Printer.eToTestString ast in
    test
      ( name
      ^ " - `"
      ^ (expectedString |> Regex.replace ~re:(Regex.regex "\n") ~repl:" ")
      ^ "`" )
      (fun () ->
        if debug then Js.log2 "ast before" (Printer.eToStructure ast) ;
        let newAST = execute_roundtrip ast in
        expect (Printer.eToTestString newAST) |> toEqual expectedString)
  in
  describe "Copy text" (fun () ->
      testCopyText
        "copying text just gets text"
        complexExpr
        (132, 257)
        "Http::Forbidden 403\nelse\n  Http::Forbidden" ;
      testCopyText "copying empty gets empty text" complexExpr (0, 0) "" ;
      ()) ;
  describe "Booleans" (fun () ->
      testCopy
        "copying a bool adds a bool to clipboard"
        (bool true)
        (0, 4)
        "true" ;
      testCopy
        "copying a bool adds a bool to clipboard 2"
        (fn "Bool::not" [bool true])
        (10, 14)
        "true" ;
      testCut
        "cutting a bool adds a bool to clipboard and leave a blank"
        (bool false)
        (0, 5)
        ("~___", "false") ;
      testCut
        "cutting a bool adds a bool to clipboard 2"
        (fn "Bool::not" [bool true])
        (10, 14)
        ("Bool::not ~___", "true") ;
      testPasteExpr
        "pasting a bool from clipboard on a blank should paste it"
        b
        (0, 0)
        (bool true)
        "true~" ;
      ()) ;
  describe "Nulls" (fun () ->
      testCopy "copying a null adds a null to clipboard" null (0, 4) "null" ;
      testCopy
        "copying a null adds a null to clipboard 2"
        (fn "Bool::isNull" [null])
        (13, 17)
        "null" ;
      testCut
        "cutting a null adds a null to clipboard and leave a blank"
        null
        (0, 4)
        ("~___", "null") ;
      testCut
        "cutting a null adds a null to clipboard 2"
        (fn "Bool::isNull" [null])
        (13, 17)
        ("Bool::isNull ~___", "null") ;
      testPaste
        "pasting a null from clipboard on a blank should paste it"
        b
        (0, 0)
        null
        "null~" ;
      ()) ;
  describe "Integers" (fun () ->
      testCopy
        "copying an int adds an int to clipboard"
        (int 1000)
        (0, 4)
        "1000" ;
      testCopy
        "copying an int adds an int to clipboard 2"
        (fn "Int::sqrt" [int 1000])
        (10, 14)
        "1000" ;
      testCopy
        "copying part of an int adds part of the int to clipboard"
        (int 1234)
        (0, 2)
        "12" ;
      testCut
        "cutting an int adds an int to clipboard and leaves a blank"
        (int 1000)
        (0, 4)
        ("~___", "1000") ;
      testCut
        "cutting an int adds an int to clipboard and leaves a blank 2"
        (fn "Int::sqrt" [int 1000])
        (10, 14)
        ("Int::sqrt ~_________", "1000") ;
      testCut
        "cutting part of an int adds part of the int to clipboard and leaves the remaining int"
        (int 1234)
        (0, 2)
        ("~34", "12") ;
      testPaste
        "pasting an int from clipboard on a blank should paste it"
        b
        (0, 0)
        (int 1234)
        "1234~" ;
      testPaste
        "pasting an int into another integer should join the integers"
        (int 5678)
        (1, 3)
        (int 1234)
        "51234~8" ;
      testPaste
        "pasting a float into an integer should convert to float"
        (int 5678)
        (1, 3)
        (float' "12" "34")
        "512.34~8" ;
      testPaste
        "pasting a float into an integer should convert to float 2"
        (int 5678)
        (0, 0)
        (float' "12" "34")
        "12.34~5678" ;
      testPaste
        "pasting a float into an integer should convert to float 3"
        (int 5678)
        (4, 4)
        (float' "12" "34")
        "567812.34~" ;
      testPaste
        "pasting an int-only string into an integer should extend integer"
        (int 5678)
        (0, 0)
        (str "1234")
        "1234~5678" ;
      testPaste
        "pasting an int-only string into an integer should extend integer 2"
        (int 5678)
        (4, 4)
        (str "1234")
        "56781234~" ;
      testPaste
        "pasting an int-only string into an integer should extend integer 2"
        (int 5678)
        (2, 2)
        (str "1234")
        "561234~78" ;
      testPaste
        "pasting an int-only string over part of an integer should extend integer"
        (int 5678)
        (1, 3)
        (str "1234")
        "51234~8" ;
      ()) ;
  describe "Strings" (fun () ->
      testCopy
        "copying a string adds a string to clipboard"
        (str "abcd EFGH ijkl 1234")
        (0, 21)
        "\"abcd EFGH ijkl 1234\"" ;
      testCopy
        "copying a string adds a string to clipboard 2"
        (fn "String::reverse" [str "abcd EFGH ijkl 1234"])
        (16, 37)
        "\"abcd EFGH ijkl 1234\"" ;
      testCopy
        "copying part of a string adds a string to clipboard"
        (str "abcd EFGH ijkl 1234")
        (4, 14)
        "\"d EFGH ijk\"" ;
      testCut
        "cutting a string adds a string to clipboard"
        (str "abcd EFGH ijkl 1234")
        (0, 21)
        ("~___", "\"abcd EFGH ijkl 1234\"") ;
      testCut
        "cutting a string adds a string to clipboard 2"
        (fn "String::reverse" [str "abcd EFGH ijkl 1234"])
        (16, 37)
        ("String::reverse ~___", "\"abcd EFGH ijkl 1234\"") ;
      testCut
        "cutting part of a string adds a string to clipboard"
        (str "abcd EFGH ijkl 1234")
        (4, 14)
        ("\"abc~l 1234\"", "\"d EFGH ijk\"") ;
      testPasteExpr
        "pasting a string on a blank should paste it"
        b
        (0, 0)
        (str "abcd EFGH ijkl 1234")
        "\"abcd EFGH ijkl 1234\"~" ;
      testCopyPaste
        "pasting a string in another string should paste it"
        (str "abcd EFGH ijkl 1234")
        (1, 5)
        (11, 15)
        "\"abcd EFGH abcd~ 1234\"" ;
      testCopyPaste
        "pasting a string in another string should paste it - opposite selection direction"
        (str "abcd EFGH ijkl 1234")
        (5, 1)
        (11, 15)
        "\"abcd EFGH abcd~ 1234\"" ;
      testCopyPaste
        "pasting a string in a TLStringMLStart should paste it"
        (str "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij")
        (2, 5)
        (11, 15)
        "\"0123456789123~efghij0123456789abcdefghij0\n123456789abcdefghij\"" ;
      testCopyPaste
        "pasting a string in the first TLStringMLMiddle should paste it"
        (str
           "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij")
        (2, 5)
        (43, 50)
        "\"0123456789abcdefghij0123456789abcdefghij\n0123~89abcdefghij0123456789abcdefghij0123\n456789abcdefghij0123456789abcdefghij0123\n456789abcdefghij\"" ;
      testCopyPaste
        "pasting a string in the second TLStringMLMiddle should paste it"
        (str
           "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij")
        (2, 5)
        (96, 84)
        "\"0123456789abcdefghij0123456789abcdefghij\n0123456789abcdefghij0123456789abcdefghij\n0123~defghij0123456789abcdefghij012345678\n9abcdefghij\"" ;
      testCopyPaste
        "pasting a string in a TLStringMLEnd should paste it"
        (str
           "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij")
        (2, 5)
        (125, 126)
        "\"0123456789abcdefghij0123456789abcdefghij\n0123456789abcdefghij0123456789abcdefghij\n0123456789abcdefghij0123456789abcdefghij\n0123~23456789abcdefghij\"" ;
      testPaste
        "pasting an int in a string should paste it"
        (str "abcd EFGH ijkl 1234")
        (11, 15)
        (int 5678)
        "\"abcd EFGH 5678~ 1234\"" ;
      testPasteExpr
        "pasting a regular record with a single key in a string should paste stringified expr"
        (str "abcd EFGH ijkl 1234")
        (11, 15)
        (record [("key1", int 9876)])
        "\"abcd EFGH {\n  key1 : 9876\n}~ 1234\"" ;
      ()) ;
  describe "Floats" (fun () ->
      testCopy
        "copying a float adds a float to clipboard"
        (float' "1234" "5678")
        (0, 9)
        "1234.5678" ;
      testCopy
        "copying a float adds a float to clipboard 2"
        (fn "Float::round" [float' "1234" "5678"])
        (13, 22)
        "1234.5678" ;
      testCopy
        "copying the whole part w/o the point adds an int to clipboard"
        (float' "1234" "5678")
        (0, 4)
        "1234" ;
      testCopy
        "copying the whole part w/ the point adds a float with fraction value of 0 to clipboard"
        (float' "1234" "5678")
        (0, 5)
        "1234.0" ;
      testCopy
        "copying the fraction part w/o the point adds an int to clipboard"
        (float' "1234" "5678")
        (5, 9)
        "5678" ;
      testCopy
        "copying the fraction part w/ the point adds a float with whole value of 0 to clipboard"
        (float' "1234" "5678")
        (4, 9)
        "0.5678" ;
      testCopy
        "copying just the point adds a float with 0.0 to clipboard"
        (float' "1234" "5678")
        (4, 5)
        "0.0" ;
      testCut
        "cutting a float adds a float to clipboard"
        (float' "1234" "5678")
        (0, 9)
        ("~___", "1234.5678") ;
      testCut
        "cutting a float adds a float to clipboard 2"
        (fn "Float::round" [float' "1234" "5678"])
        (13, 22)
        ("Float::round ~___", "1234.5678") ;
      testCut
        "cutting the whole part w/o the point adds an int to clipboard, leaves float'"
        (float' "1234" "5678")
        (0, 4)
        ("~.5678", "1234") ;
      testCut
        "cutting the whole part w/ the point adds a float with fraction value of 0 to clipboard, leaves int"
        (float' "1234" "5678")
        (0, 5)
        ("~5678", "1234.0") ;
      testCut
        "cutting the fraction part w/o the point adds an int to clipboard, leaves float'"
        (float' "1234" "5678")
        (5, 9)
        ("1234.~", "5678") ;
      testCut
        "cutting the fraction part w/ the point adds a float with whole value of 0 to clipboard, leaves int"
        (float' "1234" "5678")
        (4, 9)
        ("1234~", "0.5678") ;
      testCut
        "cutting just the point adds a float with 0.0 to clipboard, leaves int of joint expr"
        (float' "1234" "5678")
        (4, 5)
        ("1234~5678", "0.0") ;
      testPaste
        "pasting a float from clipboard on a blank should paste it"
        b
        (0, 0)
        (float' "1234" "5678")
        "1234.5678~" ;
      testPaste
        "pasting an int in a float whole part should paste it"
        (float' "1234" "5678")
        (0, 0)
        (int 9000)
        "9000~1234.5678" ;
      testPaste
        "pasting an int in a float whole part should paste it 2"
        (float' "1234" "5678")
        (1, 3)
        (int 9000)
        "19000~4.5678" ;
      testPaste
        "pasting an int in a float fraction part should paste it"
        (float' "1234" "5678")
        (8, 8)
        (int 9000)
        "1234.5679000~8" ;
      testPaste
        "pasting an int over a float fraction part should paste it and remove selection"
        (float' "1234" "5678")
        (6, 8)
        (int 9000)
        "1234.59000~8" ;
      testPaste
        "pasting an int before a float point should paste it"
        (float' "1234" "5678")
        (4, 4)
        (int 9000)
        "12349000~.5678" ;
      testPaste
        "pasting an int after a float point should paste it"
        (float' "1234" "5678")
        (5, 5)
        (int 9000)
        "1234.9000~5678" ;
      testPaste
        "pasting an int over a float point should paste it"
        (float' "1234" "5678")
        (3, 6)
        (int 9000)
        "1239000~678" ;
      ()) ;
  describe "Variables" (fun () ->
      testCopy
        "copying adds a var to clipboard"
        (var "varName")
        (0, 7)
        "varName" ;
      testCopy
        "copying part of it adds a var to clipboard"
        (var "varName")
        (0, 3)
        "var" ;
      testCut
        "cutting adds a var to clipboard and leaves a blank"
        (var "varName")
        (0, 7)
        ("~___", "varName") ;
      testCut
        "cutting part of it adds a var to clipboard and leaves a partial"
        (var "varName")
        (0, 3)
        ("~Name", "var") ;
      testPasteExpr
        "pasting variable into blank works"
        b
        (0, 0)
        (var "varName")
        "varName~" ;
      testPaste
        "pasting variable into empty let lhs works"
        (let' "" b b)
        (7, 7)
        (var "varName")
        "let varName~ = ___\n___" ;
      testPaste
        "pasting variable into filled let lhs works"
        (let' "oldLetLhs" b b)
        (7, 7)
        (var "varName")
        "let oldvarName~LetLhs = ___\n___" ;
      testPaste
        "pasting variable over filled let lhs works"
        (let' "oldLetLhs" b b)
        (7, 13)
        (var "varName")
        "let oldvarName~ = ___\n___" ;
      ()) ;
  describe "Field Accesses" (fun () ->
      testCopy
        "copying adds a fieldAccess to clipboard"
        (fieldAccess (var "request") "body")
        (0, 12)
        "request.body" ;
      testCopy
        "copying the preceding expresssion adds it to clipboard"
        (fieldAccess (var "request") "body")
        (0, 7)
        "request" ;
      testCopy
        "copying field part adds a var to clipboard"
        (fieldAccess (var "request") "body")
        (8, 12)
        "body" ;
      testCut
        "cutting adds a fieldAccess to clipboard and leaves a blank"
        (fieldAccess (var "request") "body")
        (0, 12)
        ("~___", "request.body") ;
      (* NOT WORKING YET
      testCut
        "cutting the preceding expression adds a fieldAccess w empty field to clipboard and leaves the field"
        (fieldAccess (var "request") "body")
        ((0, 8))
        ("___.body", "request.***", 0) ; *)
      testPasteText
        "pasting into a field pastes"
        (fieldAccess (var "request") "")
        (8, 8)
        "body"
        "request.body~" ;
      testPasteText
        "pasting into the middle of a field pastes"
        (fieldAccess (var "request") "existing")
        (10, 10)
        "body"
        "request.exbody~isting" ;
      ()) ;
  describe "If conditions" (fun () ->
      testCopy
        "copying the whole expression adds an if to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (0, 45)
        "if true\nthen\n  \"then body\"\nelse\n  \"else body\"" ;
      testCut
        "cutting the whole expression adds an if to clipboard and leaves a blank"
        (if' (bool true) (str "then body") (str "else body"))
        (0, 45)
        ("~___", "if true\nthen\n  \"then body\"\nelse\n  \"else body\"") ;
      testCopy
        "copying just the condition adds then condition expression to clipboard "
        (if' (bool true) (str "then body") (str "else body"))
        (3, 7)
        "true" ;
      testCopy
        "copying the if keyword and the condition adds an if with blank then & else body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (0, 7)
        "if true\nthen\n  ___\nelse\n  ___" ;
      testCopy
        "copying the condition and the then keyword adds an if with blank then & else body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (3, 12)
        "if true\nthen\n  ___\nelse\n  ___" ;
      testCopy
        "copying just the then body adds the then expression to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (15, 26)
        "\"then body\"" ;
      testCopy
        "copying the then keyword and the then body adds an if with blank condition & else body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (8, 26)
        "if ___\nthen\n  \"then body\"\nelse\n  ___" ;
      testCopy
        "copying the then body and the else keyword adds an if with blank condition & else body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (12, 31)
        "if ___\nthen\n  \"then body\"\nelse\n  ___" ;
      testCopy
        "copying just the else body adds the else expression to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (34, 45)
        "\"else body\"" ;
      testCopy
        "copying the else keyword and else body adds an if with blank condition &then body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (27, 45)
        "if ___\nthen\n  ___\nelse\n  \"else body\"" ;
      testCut
        "cutting just the condition adds then condition expression to clipboard "
        (if' (bool true) (str "then body") (str "else body"))
        (3, 7)
        ("if ~___\nthen\n  \"then body\"\nelse\n  \"else body\"", "true") ;
      testCut
        "cutting the if keyword and the condition adds an if with blank then & else body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (0, 7)
        ( "if ~___\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if true\nthen\n  ___\nelse\n  ___" ) ;
      testCut
        "cutting the condition and the then keyword adds an if with blank then & else body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (3, 12)
        ( "if ~___\nthen\n  \"then body\"\nelse\n  \"else body\""
        , "if true\nthen\n  ___\nelse\n  ___" ) ;
      testCut
        "cutting just the then body adds the then expression to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (15, 26)
        ("if true\nthen\n  ~___\nelse\n  \"else body\"", "\"then body\"") ;
      testCut
        "cutting the then keyword and the then body adds an if with blank condition & else body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (8, 26)
        ( "if true\n~then\n  ___\nelse\n  \"else body\""
        , "if ___\nthen\n  \"then body\"\nelse\n  ___" ) ;
      testCut
        "cutting the then body and the else keyword adds an if with blank condition & else body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (12, 31)
        ( "if true\nthen~\n  ___\nelse\n  \"else body\""
        , "if ___\nthen\n  \"then body\"\nelse\n  ___" ) ;
      testCut
        "cutting just the else body adds the else expression to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (34, 45)
        ("if true\nthen\n  \"then body\"\nelse\n  ~___", "\"else body\"") ;
      testCut
        "cutting the else keyword and else body adds an if with blank condition &then body to clipboard"
        (if' (bool true) (str "then body") (str "else body"))
        (27, 45)
        ( "if true\nthen\n  \"then body\"\n~else\n  ___"
        , "if ___\nthen\n  ___\nelse\n  \"else body\"" ) ;
      ()) ;
  describe "Bin-ops" (fun () ->
      testCopy
        "copying a single-char operator works"
        (binop "<" (int 123) (int 456))
        (4, 5)
        "_________ < _________" ;
      testCopy
        "copying a multi-char operator works"
        (binop "==" (int 123) (int 456))
        (4, 6)
        "_________ == _________" ;
      testCopy
        "copying part of a multi-char operator works"
        (binop "==" (int 123) (int 456))
        (4, 5)
        "_________ =@ _________" ;
      ()) ;
  describe "Functions" (fun () ->
      testCopy
        "copying a function name adds a fn w blank arguments to clipboard"
        (fn "Int::sqrt" [int 122])
        (0, 9)
        "Int::sqrt _________" ;
      testCopy
        "copying a function name with a version adds a fn, not a partial"
        (fn "HttpClient::post_v4" [str ""])
        (0, 18)
        "HttpClient::postv4 ______________" ;
      testCopy
        "copying part of a function name adds a partial fn w blank arguments to clipboard"
        (fn "Int::sqrt" [int 122])
        (0, 4)
        "Int:@sqr@ _________" ;
      testCopy
        "copying a function's argument adds the argument's expression to clipboard"
        (fn "Int::sqrt" [int 122])
        (10, 13)
        "122" ;
      testCut
        "cutting a function name adds a fn w blank arguments to clipboard and leaves a blank"
        (fn "Int::sqrt" [int 122])
        (0, 9)
        ("~___", "Int::sqrt _________") ;
      testCut
        "cutting part of a fn name adds a partial fn w blank arguments to clipboard and leaves a partial"
        (fn "Int::sqrt" [int 122])
        (0, 4)
        ("~:sqrt@qr@ 122", "Int:@sqr@ _________") ;
      testCut
        "cutting a function's argument adds the argument's expression to clipboard and leaves a blank there"
        (fn "Int::sqrt" [int 122])
        (10, 13)
        ("Int::sqrt ~_________", "122") ;
      ()) ;
  describe "Pipes" (fun () ->
      testCopy
        "copying first expression of pipe adds it to clipboard"
        aPipe
        (0, 2)
        "[]" ;
      testCopy
        "copying pipe adds it to clipboard"
        aPipe
        (0, 41)
        "[]\n|>List::append [5]\n|>List::append [5]\n" ;
      ()) ;
  describe "Lists" (fun () ->
      (* NOT WORKING YET
      testCopy
        "copying opening bracket adds empty list expr to clipboard"
        (list ([int ("123")]))
        ((0, 1))
        ("[123]",  "[]") ; *)
      testCopy
        "copying subset of elements adds subset list expr to clipboard"
        (list [int 123; int 456; int 789])
        (5, 12)
        "[456,789]" ;
      testCut
        "cutting subset of elements adds subset list expr to clipboard and leaves remainder"
        (list [int 123; int 456; int 789])
        (5, 12)
        ("[123,~___]", "[456,789]") ;
      (* NOT WORKING b/c placing the cursor on either side of a separator
       * acts as though it's on the sub-expression
      t
        "pasting an expression into list expr at separator works"
        (list
           ( 
           , [ int ("123")
             ; int ("456")
             ; int ("789") ] ))
        (paste ~clipboard:(int ("9000")) (4, 5))
        ("[123,9000,456,789]", "9000", 9) ;
       *)
      testPasteExpr
        "pasting an expression over subset of list expr works"
        (list [int 123; int 456; int 789])
        (5, 12)
        (int 9000)
        "[123,9000~]" ;
      ()) ;
  describe "Records" (fun () ->
      testCopy
        "copying opening bracket adds empty record expr to clipboard"
        (record [("key1", int 1234)])
        (0, 1)
        "{}" ;
      testCopy
        "copying a single key adds record w single key to clipboard"
        (record [("key1", int 1234)])
        (4, 8)
        "{\n  key1 : ___\n}" ;
      testCut
        "cutting a single key adds record w single key to clipboard and leaves blank in it's place"
        (record [("key1", int 1234)])
        (4, 8)
        ("{\n  ~*** : 1234\n}", "{\n  key1 : ___\n}") ;
      testCopy
        "copying a single k-v pair adds record w single k-v pair to clipboard"
        (record [("key1", int 1234)])
        (2, 15)
        "{\n  key1 : 1234\n}" ;
      testPasteText
        "pasting text into record keys works"
        (record [("", b)])
        (4, 4)
        "mykey"
        "{\n  mykey~ : ___\n}" ;
      testPasteText
        "pasting text into existing record keys works"
        (record [("existing", b)])
        (7, 7)
        "myKey"
        "{\n  eximyKey~sting : ___\n}" ;
      testPasteText
        "pasting text into empty row works"
        (record [("", b)])
        (4, 4)
        "SomeKey: [\"val1\", \"val2\" ]"
        "{\n  SomeKey : [\"val1\",\"val2\"]~\n}" ;
      testPasteText
        "pasting text into empty row works (alternate spacing)"
        (record [("", b)])
        (4, 4)
        "SomeKey: [ \"val1\", \"val2\" ]"
        "{\n  SomeKey : [\"val1\",\"val2\"]~\n}" ;
      testPasteText
        "pasting record text into empty blank"
        b
        (4, 4)
        "{\n  SomeKey: [ \"hi\", \"paul\" ]\n}"
        "{\n  SomeKey : [\"val1\",\"val2\"]~\n}" ;
      testPasteText
        "pasting 2 row record text into empty blank"
        b
        (4, 4)
        "{\n  Key: [\"a\", \"b\"]\n  Key2: [\"c\", \"d\"]\n}"
        "{\n  Key : [\"a\",\"b\"]\n  Key2 : [\"c\",\"d\"]\n}~" ;
      testPasteText
        "pasting 2 row record text into empty blank (js style)"
        b
        (4, 4)
        "{\n  Key: [\"a\", \"b\"],\n  Key2: [\"c\", \"d\"]\n}"
        "{\n  Key : [\"a\",\"b\"]\n  Key2 : [\"c\",\"d\"]\n}~" ;
      ()) ;
  describe "Constructors" (fun () ->
      testCopy
        "copying adds constructor to clipboard"
        (constructor "Just" [int 100])
        (0, 8)
        "Just 100" ;
      testCopy
        "copying part adds partial constructor to clipboard"
        (constructor "Just" [int 100])
        (0, 3)
        "Jus@ ___" ;
      testCut
        "cutting adds constructor to clipboard and leaves blank"
        (constructor "Just" [int 100])
        (0, 8)
        ("~___", "Just 100") ;
      testCut
        "cutting part adds partial constructor to clipboard and leaves partial"
        (constructor "Just" [int 100])
        (0, 3)
        ("~t@s@ 100", "Jus@ ___") ;
      ()) ;
  describe "Match" (fun () ->
      (* TODO: test match statements, implementation is slightly inconsistent*)
      ()) ;
  describe "json" (fun () ->
      testPasteText "pasting a json int makes an int" b (0, 0) "6" "6~" ;
      testPasteText "pasting a json float makes a float" b (0, 0) "6.6" "6.6~" ;
      testPasteText
        "pasting a json array makes a list"
        b
        (0, 0)
        "[ 1 , 2 , 3 , 4 ]"
        "[1,2,3,4]~" ;
      testPasteText
        "pasting a object list makes a record"
        b
        (0, 0)
        "{ \"a\": \n\"b\", \"c\":[\n1\n,\n2], \"d\" : \n4.5 }"
        "{\n  a : \"b\"\n  c : [1,2]\n  d : 4.5\n}~" ;
      testPasteText
        "pasting text into a string doesn't use the json conversion"
        (str "")
        (1, 1)
        "[ 1 , 5 ]"
        "\"[ 1 , 5 ]~\"" ;
      ()) ;
  describe "Feature Flags" (fun () ->
      (* TODO: test feature flags, not yet in fluid *) ()) ;
  describe "Copy/paste roundtrip" (fun () ->
      let longString =
        str
          "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
      in
      roundtrip b ;
      roundtrip (int 6) ;
      roundtrip (str "[1 , 5]") ;
      roundtrip (str "12345678987654321.12345678987654321") ;
      roundtrip (pipe (str "a") [binop "++" pipeTarget (str "b")]) ;
      roundtrip (pipe (str "a") [fn "String::append" [pipeTarget; str "b"]]) ;
      roundtrip aPipe ;
      roundtrip (binop "+" (if' (int 5) (int 5) (int 5)) (int 5)) ;
      roundtrip (partial "D" (constructor "d" [fn "k" []])) ;
      roundtrip (partial "D" (fn "X" [str "F"])) ;
      roundtrip (fn "HttpClient::post_v4" [str ""]) ;
      roundtrip longString ;
      roundtrip (let' "myVariable" longString b) ;
      roundtrip (record [("a", record [("b", str "c")])]) ;
      roundtrip (match' b [(pBlank, b)]) ;
      roundtrip (match' b [(pString "asd", b)]) ;
      ())
