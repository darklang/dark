open Jest
open Expect
open Tc
open Fluid
module K = FluidKeyboard

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
                    (gid (), EVariable (gid (), "request"), "headers")
                , "origin" )
            , EString (gid (), "https://usealtitude.com") )
        , EBinOp
            ( gid ()
            , "=="
            , EFieldAccess
                ( gid ()
                , EFieldAccess
                    (gid (), EVariable (gid (), "request"), "headers")
                , "origin" )
            , EString (gid (), "https://localhost:3000") ) )
    , ELet
        ( gid ()
        , ""
        , newB ()
        , EFnCall (gid (), "Http::Forbidden", [EInteger (gid (), 403)]) )
    , EFnCall (gid (), "Http::Forbidden", []) )


let () =
  let aStr = EString (gid (), "some string") in
  let emptyStr = EString (gid (), "") in
  let oneCharStr = EString (gid (), "c") in
  let anInt = EInteger (gid (), 12345) in
  let five = EInteger (gid (), 5) in
  let fiftySix = EInteger (gid (), 56) in
  let seventyEight = EInteger (gid (), 78) in
  let blank = EBlank (gid ()) in
  let aPartialVar = EPartial (gid (), "req") in
  let emptyLet = ELet (gid (), "", EBlank (gid ()), EInteger (gid (), 5)) in
  let nonEmptyLet =
    ELet (gid (), "", EInteger (gid (), 6), EInteger (gid (), 5))
  in
  let letWithLhs =
    ELet (gid (), "n", EInteger (gid (), 6), EInteger (gid (), 5))
  in
  let aVar = EVariable (gid (), "variable") in
  let aShortVar = EVariable (gid (), "v") in
  let emptyIf = EIf (gid (), newB (), newB (), newB ()) in
  let plainIf =
    EIf
      (gid (), EInteger (gid (), 5), EInteger (gid (), 6), EInteger (gid (), 7))
  in
  let aLambda = ELambda (gid (), [""], blank) in
  let nonEmptyLambda = ELambda (gid (), [""], five) in
  let aFnCall = EFnCall (gid (), "List::range", [five; blank]) in
  let aField = EFieldAccess (gid (), EVariable (gid (), "obj"), "field") in
  let aNestedField =
    EFieldAccess
      ( gid ()
      , EFieldAccess (gid (), EVariable (gid (), "obj"), "field")
      , "field2" )
  in
  let aShortField = EFieldAccess (gid (), EVariable (gid (), "obj"), "f") in
  let aBlankField = EFieldAccess (gid (), EVariable (gid (), "obj"), "") in
  let process ~(wrap : bool) (keys : K.key list) (pos : int) (ast : ast) :
      string * int =
    (* we wrap it so that there's something before and after the expr (esp
     * after it), which catches more bugs that ending the text area
     * immediately. Unfortunately, it doesn't work for well nested exprs, like
     * ifs. *)
    let ast =
      if wrap
      then ELet (gid (), "var", ast, EVariable (gid (), "var"))
      else ast
    in
    let extra = if wrap then 10 else 0 in
    let pos = pos + extra in
    let s = {Defaults.defaultFluidState with oldPos = pos; newPos = pos} in
    let newAST, newState =
      List.foldl keys ~init:(ast, s) ~f:(fun k (ast, s) -> updateKey k ast s)
    in
    let result =
      match newAST with ELet (_, _, expr, _) when wrap -> expr | expr -> expr
    in
    (eToString result, max 0 (newState.newPos - extra))
  in
  let delete ?(wrap = true) (pos : int) (expr : expr) : string * int =
    process ~wrap [K.Delete] pos expr
  in
  let backspace ?(wrap = true) (pos : int) (expr : expr) : string * int =
    process ~wrap [K.Backspace] pos expr
  in
  let tab ?(wrap = true) (pos : int) (expr : expr) : string * int =
    process ~wrap [K.Tab] pos expr
  in
  let shiftTab ?(wrap = true) (pos : int) (expr : expr) : string * int =
    process ~wrap [K.ShiftTab] pos expr
  in
  let press ?(wrap = true) (key : K.key) (pos : int) (expr : expr) :
      string * int =
    process ~wrap [key] pos expr
  in
  let presses ?(wrap = true) (keys : K.key list) (pos : int) (expr : expr) :
      string * int =
    process ~wrap keys pos expr
  in
  let insert ?(wrap = true) (char : char) (pos : int) (expr : expr) :
      string * int =
    let key = K.fromChar char in
    process ~wrap [key] pos expr
  in
  let b = "___" in
  let t
      (name : string)
      (initial : expr)
      (fn : expr -> string * int)
      (expected : string * int) =
    test
      ( name
      ^ " - `"
      ^ (eToString initial |> Regex.replace ~re:(Regex.regex "\n") ~repl:" ")
      ^ "`" )
      (fun () -> expect (fn initial) |> toEqual expected)
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
  describe "Numbers" (fun () ->
      t "insert not a number" anInt (insert 'c' 0) ("12345", 0) ;
      t "insert start of number" anInt (insert '5' 0) ("512345", 1) ;
      t "delete start of number" anInt (delete 0) ("2345", 0) ;
      t "backspace start of number" anInt (backspace 0) ("12345", 0) ;
      t "insert end of number" anInt (insert '0' 5) ("123450", 6) ;
      t "delete end of number" anInt (delete 5) ("12345", 5) ;
      t "backspace end of number" anInt (backspace 5) ("1234", 4) ;
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
      t "insert partial" blank (insert 't' 0) ("t", 1) ;
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
      t "insert end of varname" aField (insert 'c' 3) ("objc.field", 4) ;
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
      () ) ;
  describe "Functions" (fun () ->
      t
        "space on a sep goes to next arg"
        aFnCall
        (press K.Space 13)
        ("List::range 5 ___", 14) ;
      () ) ;
  (* describe "Binops" (fun () -> *)
  (*     t *)
  (*       "add a binop at the end of a var" *)
  (*       aShortVar *)
  (*       (press K.Percent 1) *)
  (*       ("v % ___", 4) ; *)
  (* () ) ; *)
  describe "Lambdas" (fun () ->
      t "backspace over lambda symbol" aLambda (backspace 1) ("___", 0) ;
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
      () ) ;
  describe "Variables" (fun () ->
      (* dont do insert until we have autocomplete *)
      (* t "insert middle of variable" (insert aVar 'c' 5) ("variabcle", 6) ; *)
      t "delete middle of variable" aVar (delete 5) ("variale", 5) ;
      t "insert capital works" aVar (press (K.Letter 'A') 5) ("variaAble", 6) ;
      t "can't insert invalid" aVar (press K.Dollar 5) ("variable", 5) ;
      t "delete variable" aShortVar (delete 0) (b, 0) ;
      t "delete long variable" aVar (delete 0) ("ariable", 0) ;
      t "delete mid variable" aVar (delete 6) ("variabe", 6) ;
      t "backspace variable" aShortVar (backspace 1) (b, 0) ;
      t "backspace mid variable" aVar (backspace 8) ("variabl", 7) ;
      t "backspace mid variable" aVar (backspace 6) ("variale", 5) ;
      () ) ;
  describe "Lets" (fun () ->
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
      () ) ;
  describe "Lists" (fun () ->
      let emptyList = EList (gid (), []) in
      let single = EList (gid (), [fiftySix]) in
      let multi = EList (gid (), [fiftySix; seventyEight]) in
      let withStr = EList (gid (), [EString (gid (), "ab")]) in
      t "create list" blank (press K.LeftSquareBracket 0) ("[]", 1) ;
      t
        "inserting before the list does nothing"
        emptyList
        (insert '5' 0)
        ("[]", 0) ;
      t "insert into empty list" emptyList (insert '5' 1) ("[5]", 2) ;
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
      () ) ;
  describe "Record" (fun () ->
      let emptyRecord = ERecord (gid (), []) in
      let emptyRow = ERecord (gid (), [("", blank)]) in
      (* let single = ERecord (gid (), [( "field", fiftySix)]) in *)
      (* let multi = EList (gid (), [fiftySix; seventyEight]) in *)
      (* let withStr = EList (gid (), [EString (gid (), "ab")]) in *)
      t "create record" blank (press K.LeftCurlyBrace 0) ("{}", 1) ;
      t
        "inserting before the record does nothing"
        emptyRecord
        (insert '5' 0)
        ("{}", 0) ;
      t
        "pressing enter in an empty record adds a new line"
        emptyRecord
        (press ~wrap:false K.Enter 1)
        ("{\n  *** : ___\n}", 4) ;
      t "enter fieldname" emptyRow (insert 'c' 4) ("{\n  c : ___\n}", 5) ;
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
      () ) ;
  describe "Autocomplete" (fun () ->
      t
        "space autocompletes correctly"
        (EPartial (gid (), "if"))
        (press K.Space 2)
        ("if ___\nthen\n  ___\nelse\n  ___", 3) ;
      t
        "let moves to right place"
        (EPartial (gid (), "let"))
        (press K.Enter 3)
        ("let *** = ___\n___", 4) ;
      t
        "variable moves to right place"
        (EPartial (gid (), "req"))
        (press K.Enter 3)
        ("request", 7) ;
      (* test "backspacing on variable reopens autocomplete" (fun () -> *)
      (*     expect (backspace (EVariable (5, "request"))). *)
      (*     gridFor ~pos:116 tokens) |> toEqual {row= 2; col= 2} ) ; *)
      () ) ;
  describe "Movement" (fun () ->
      let tokens = toTokens complexExpr in
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
        ("if ___\nthen\n  ___\nelse\n  ___", 14) ;
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
        (press K.Enter 11)
        ("let *** = 6\n5", 12) ;
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
      () ) ;
  describe "Tabs" (fun () ->
      t
        "tab goes to first block in a let"
        emptyLet
        (tab 0)
        ("let *** = ___\n5", 4) ;
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
        "shift tab wraps from the start of a let"
        emptyLet
        (shiftTab 4)
        ("let *** = ___\n5", 10) ;
      t "cant tab to filled letLHS" letWithLhs (tab 0) ("let n = 6\n5", 0) ;
      t "can tab to lambda blank" aLambda (tab 0) ("\\*** -> ___", 1) ;
      t "can shift tab to field blank" aBlankField (shiftTab 0) ("obj.***", 4) ;
      () ) ;
  ()
