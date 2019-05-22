open Jest
open Expect
open Tc
open Types
open Prelude
open Fluid
module K = FluidKeyboard

let tl ast =
  { id = TLID "7"
  ; pos = {x = 0; y = 0}
  ; data =
      TLHandler
        { ast = toExpr ast
        ; tlid = TLID "7"
        ; spec =
            { module_ = Blank.newF "HTTP"
            ; name = Blank.newF "/test"
            ; modifier = Blank.newF "GET" } } }


let () =
  let mID = gid () in
  let aStr = FPString (mID, gid (), "some string") in
  let emptyStr = FPString (mID, gid (), "") in
  let oneCharStr = FPString (mID, gid (), "c") in
  let aShortInt = FPInteger (mID, gid (), 1) in
  let anInt = FPInteger (mID, gid (), 12345) in
  let aHugeInt = FPInteger (mID, gid (), 2000000000) in
  (* let aFloat = FPFloat (gid (), "123", "456") in *)
  (* let aHugeFloat = FPFloat (gid (), "123456789", "123456789") in *)
  (* let aPartialFloat = FPFloat (gid (), "1", "") in *)
  (* let trueBool = FPBool (gid (), true) in *)
  (* let falseBool = EBool (gid (), false) in *)
  (* let aNull = FPNull (gid ()) in *)
  (* let five = FPInteger (gid (), 5) in *)
  (* let fiftySix = FPInteger (gid (), 56) in *)
  (* let seventyEight = FPInteger (gid (), 78) in *)
  let blank = FPBlank (mID, gid ()) in
  (* let aPartialVar = FPPartial (gid (), "req") in *)
  (* let aVar = FPVariable (gid (), "variable") in *)
  (* let aShortVar = FPVariable (gid (), "v") in *)
  let m = Defaults.defaultModel in
  let process (keys : K.key list) (pos : int) (pat : fluidPattern) :
      string * int =
    let ast = EMatch (mID, EBlank (gid ()), [(pat, EBlank (gid ()))]) in
    let extra = 12 in
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
      | EMatch (_, _, [(pat, _)]) ->
          pat
      | _ ->
          impossible "can't match"
    in
    (pToString result, max 0 (newState.newPos - extra))
  in
  let delete (pos : int) (pat : fluidPattern) : string * int =
    process [K.Delete] pos pat
  in
  let backspace (pos : int) (pat : fluidPattern) : string * int =
    process [K.Backspace] pos pat
  in
  (* let tab (pos : int) (pat : fluidPattern) : string * int = *)
  (*   process [K.Tab] pos pat *)
  (* in *)
  (* let shiftTab (pos : int) (pat : fluidPattern) : string * int = *)
  (*   process [K.ShiftTab] pos pat *)
  (* in *)
  (* let press (key : K.key) (pos : int) (pat : fluidPattern) : string * int = *)
  (*   process [key] pos pat *)
  (* in *)
  (* let presses (keys : K.key list) (pos : int) (pat : fluidPattern) : *)
  (*     string * int = *)
  (*   process keys pos pat *)
  (* in *)
  let insert (char : char) (pos : int) (pat : fluidPattern) : string * int =
    let key = K.fromChar char in
    process [key] pos pat
  in
  let b = "___" in
  let t
      (name : string)
      (initial : fluidPattern)
      (fn : fluidPattern -> string * int)
      (expected : string * int) =
    test
      ( name
      ^ " - `"
      ^ (pToString initial |> Regex.replace ~re:(Regex.regex "\n") ~repl:" ")
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
  (* describe "Floats" (fun () -> *)
  (*     t "insert . converts to float - end" anInt (insert '.' 5) ("12345.", 6) ; *)
  (*     t "insert . converts to float - middle" anInt (insert '.' 3) ("123.45", 4) ; *)
  (*     t "insert . converts to float - start" anInt (insert '.' 0) ("12345", 0) ; *)
  (*     t "insert . converts to float - short" aShortInt (insert '.' 1) ("1.", 2) ; *)
  (*     t "insert zero in whole - start" aFloat (insert '0' 0) ("123.456", 0) ; *)
  (*     t "insert int in whole - start" aFloat (insert '9' 0) ("9123.456", 1) ; *)
  (*     t "insert int in whole - middle" aFloat (insert '0' 1) ("1023.456", 2) ; *)
  (*     t "insert int in whole - end" aFloat (insert '0' 3) ("1230.456", 4) ; *)
  (*     t "insert int in fraction - start" aFloat (insert '0' 4) ("123.0456", 5) ; *)
  (*     t "insert int in fraction - middle" aFloat (insert '0' 6) ("123.4506", 7) ; *)
  (*     t "insert int in fraction - end" aFloat (insert '0' 7) ("123.4560", 8) ; *)
  (*     t "insert non-int in whole" aFloat (insert 'c' 2) ("123.456", 2) ; *)
  (*     t "insert non-int in fraction" aFloat (insert 'c' 6) ("123.456", 6) ; *)
  (*     t "delete dot" aFloat (delete 3) ("123456", 3) ; *)
  (*     t "delete dot at scale" aHugeFloat (delete 9) ("1234567891", 9) ; *)
  (*     t "backspace dot" aFloat (backspace 4) ("123456", 3) ; *)
  (*     t "backspace dot at scale" aHugeFloat (backspace 10) ("1234567891", 9) ; *)
  (*     t "delete start of whole" aFloat (delete 0) ("23.456", 0) ; *)
  (*     t "delete middle of whole" aFloat (delete 1) ("13.456", 1) ; *)
  (*     t "delete end of whole" aFloat (delete 2) ("12.456", 2) ; *)
  (*     t "delete start of fraction" aFloat (delete 4) ("123.56", 4) ; *)
  (*     t "delete middle of fraction" aFloat (delete 5) ("123.46", 5) ; *)
  (*     t "delete end of fraction" aFloat (delete 6) ("123.45", 6) ; *)
  (*     t "delete dot converts to int" aFloat (delete 3) ("123456", 3) ; *)
  (*     t *)
  (*       "delete dot converts to int, no fraction" *)
  (*       aPartialFloat *)
  (*       (delete 1) *)
  (*       ("1", 1) ; *)
  (*     t "backspace start of whole" aFloat (backspace 1) ("23.456", 0) ; *)
  (*     t "backspace middle of whole" aFloat (backspace 2) ("13.456", 1) ; *)
  (*     t "backspace end of whole" aFloat (backspace 3) ("12.456", 2) ; *)
  (*     t "backspace start of fraction" aFloat (backspace 5) ("123.56", 4) ; *)
  (*     t "backspace middle of fraction" aFloat (backspace 6) ("123.46", 5) ; *)
  (*     t "backspace end of fraction" aFloat (backspace 7) ("123.45", 6) ; *)
  (*     t "backspace dot converts to int" aFloat (backspace 4) ("123456", 3) ; *)
  (*     t *)
  (*       "backspace dot converts to int, no fraction" *)
  (*       aPartialFloat *)
  (*       (backspace 2) *)
  (*       ("1", 1) ; *)
  (*     t "continue after adding dot" aPartialFloat (insert '2' 2) ("1.2", 3) ; *)
  (* () ) ; *)
  (* describe "Bools" (fun () -> *)
  (*     t "insert start of bool" trueBool (insert 'c' 0) ("ctrue", 1) ; *)
  (*     t "delete start of bool" trueBool (delete 0) ("rue", 0) ; *)
  (*     t "backspace start of bool" trueBool (backspace 0) ("true", 0) ; *)
  (*     t "insert end of bool" trueBool (insert '0' 4) ("true0", 5) ; *)
  (*     t "delete end of bool" trueBool (delete 4) ("true", 4) ; *)
  (*     t "backspace end of bool" trueBool (backspace 4) ("tru", 3) ; *)
  (*     t "insert middle of bool" trueBool (insert '0' 2) ("tr0ue", 3) ; *)
  (*     t "delete middle of bool" trueBool (delete 2) ("tre", 2) ; *)
  (*     t "backspace middle of bool" trueBool (backspace 2) ("tue", 1) ; *)
  (*     () ) ; *)
  (* describe "Nulls" (fun () -> *)
  (*     t "insert start of null" aNull (insert 'c' 0) ("cnull", 1) ; *)
  (*     t "delete start of null" aNull (delete 0) ("ull", 0) ; *)
  (*     t "backspace start of null" aNull (backspace 0) ("null", 0) ; *)
  (*     t "insert end of null" aNull (insert '0' 4) ("null0", 5) ; *)
  (*     t "delete end of null" aNull (delete 4) ("null", 4) ; *)
  (*     t "backspace end of null" aNull (backspace 4) ("nul", 3) ; *)
  (*     t "insert middle of null" aNull (insert '0' 2) ("nu0ll", 3) ; *)
  (*     t "delete middle of null" aNull (delete 2) ("nul", 2) ; *)
  (*     t "backspace middle of null" aNull (backspace 2) ("nll", 1) ; *)
  (*     () ) ; *)
  describe "Blanks" (fun () ->
      t "insert middle of blank->string" blank (insert '"' 3) ("\"\"", 1) ;
      t "delete middle of blank->blank" blank (delete 3) (b, 3) ;
      t "backspace middle of blank->blank" blank (backspace 3) (b, 2) ;
      t "insert blank->string" blank (insert '"' 0) ("\"\"", 1) ;
      t "delete blank->string" emptyStr (delete 0) (b, 0) ;
      t "backspace blank->string" emptyStr (backspace 1) (b, 0) ;
      t "insert blank->int" blank (insert '5' 0) ("5", 1) ;
      t "insert blank->int" blank (insert '0' 0) ("0", 1) ;
      (* t "delete int->blank " five (delete 0) (b, 0) ; *)
      (* t "backspace int->blank " five (backspace 1) (b, 0) ; *)
      t "insert end of blank->int" blank (insert '5' 1) ("5", 1) ;
      t "insert partial" blank (insert 't' 0) ("t", 1) ;
      () ) ;
  (* describe "Variables" (fun () -> *)
  (*     (* dont do insert until we have autocomplete *) *)
  (*     (* t "insert middle of variable" (insert aVar 'c' 5) ("variabcle", 6) ; *) *)
  (*     t "delete middle of variable" aVar (delete 5) ("variale", 5) ; *)
  (*     t "insert capital works" aVar (press (K.Letter 'A') 5) ("variaAble", 6) ; *)
  (*     t "can't insert invalid" aVar (press K.Dollar 5) ("variable", 5) ; *)
  (*     t "delete variable" aShortVar (delete 0) (b, 0) ; *)
  (*     t "delete long variable" aVar (delete 0) ("ariable", 0) ; *)
  (*     t "delete mid variable" aVar (delete 6) ("variabe", 6) ; *)
  (*     t "backspace variable" aShortVar (backspace 1) (b, 0) ; *)
  (*     t "backspace mid variable" aVar (backspace 8) ("variabl", 7) ; *)
  (*     t "backspace mid variable" aVar (backspace 6) ("variale", 5) ; *)
  (*     () ) ; *)
  ()
