open Jest
open Expect
open Tc
open Types
open Prelude
open Fluid
module K = FluidKeyboard
module TL = Toplevel

(* These tests should be synced with the subset of tests in fluid_test.ml that
 * makes sense for patterns. See the extensive docs there for how this all
 * works. *)

let h ast =
  { ast = toExpr ast
  ; hTLID = TLID "7"
  ; spec =
      { space = Blank.newF "HTTP"
      ; name = Blank.newF "/test"
      ; modifier = Blank.newF "GET" }
  ; pos = {x = 0; y = 0} }


let () =
  let mID = gid () in
  let aStr = FPString (mID, gid (), "some string") in
  let emptyStr = FPString (mID, gid (), "") in
  let oneCharStr = FPString (mID, gid (), "c") in
  let aShortInt = FPInteger (mID, gid (), 1) in
  let anInt = FPInteger (mID, gid (), 12345) in
  let aHugeInt = FPInteger (mID, gid (), 2000000000) in
  let aFloat = FPFloat (mID, gid (), "123", "456") in
  let aHugeFloat = FPFloat (mID, gid (), "123456789", "123456789") in
  let aPartialFloat = FPFloat (mID, gid (), "1", "") in
  let trueBool = FPBool (mID, gid (), true) in
  let falseBool = FPBool (mID, gid (), false) in
  let aNull = FPNull (mID, gid ()) in
  let five = FPInteger (mID, gid (), 5) in
  (* let fiftySix = FPInteger (mID, gid (), 56) in *)
  (* let seventyEight = FPInteger (gid (), 78) in *)
  let b () = FPBlank (mID, gid ()) in
  (* let aPartialVar = FPPartial (gid (), "req") in *)
  let aVar = FPVariable (mID, gid (), "variable") in
  let aShortVar = FPVariable (mID, gid (), "v") in
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
      | EMatch (_, _, [(pat, _)]) ->
          pat
      | _ ->
          impossible ("can't match: " ^ eToString s newAST)
    in
    (pToString result, max 0 (newState.newPos - extra))
  in
  let del (pos : int) (pat : fluidPattern) : string * int =
    process [K.Delete] pos pat
  in
  let bs (pos : int) (pat : fluidPattern) : string * int =
    process [K.Backspace] pos pat
  in
  (* let tab (pos : int) (pat : fluidPattern) : string * int = *)
  (*   process [K.Tab] pos pat *)
  (* in *)
  (* let shiftTab (pos : int) (pat : fluidPattern) : string * int = *)
  (*   process [K.ShiftTab] pos pat *)
  (* in *)
  let press (key : K.key) (pos : int) (pat : fluidPattern) : string * int =
    process [key] pos pat
  in
  let presses (keys : K.key list) (pos : int) (pat : fluidPattern) :
      string * int =
    process keys pos pat
  in
  let insert (char : char) (pos : int) (pat : fluidPattern) : string * int =
    let key = K.fromChar char in
    process [key] pos pat
  in
  let blank = "***" in
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
      t "insert start of true" trueBool (insert 'c' 0) ("ctrue", 1) ;
      t "del start of true" trueBool (del 0) ("rue", 0) ;
      t "bs start of true" trueBool (bs 0) ("true", 0) ;
      t "insert end of true" trueBool (insert '0' 4) ("true0", 5) ;
      t "del end of true" trueBool (del 4) ("true", 4) ;
      t "bs end of true" trueBool (bs 4) ("tru", 3) ;
      t "insert middle of true" trueBool (insert '0' 2) ("tr0ue", 3) ;
      t "del middle of true" trueBool (del 2) ("tre", 2) ;
      t "bs middle of true" trueBool (bs 2) ("tue", 1) ;
      t "insert start of false" falseBool (insert 'c' 0) ("cfalse", 1) ;
      t "del start of false" falseBool (del 0) ("alse", 0) ;
      t "bs start of false" falseBool (bs 0) ("false", 0) ;
      t "insert end of false" falseBool (insert '0' 5) ("false0", 6) ;
      t "del end of false" falseBool (del 5) ("false", 5) ;
      t "bs end of false" falseBool (bs 5) ("fals", 4) ;
      t "insert middle of false" falseBool (insert '0' 2) ("fa0lse", 3) ;
      t "del middle of false" falseBool (del 2) ("fase", 2) ;
      t "bs middle of false" falseBool (bs 2) ("flse", 1) ;
      () ) ;
  describe "Nulls" (fun () ->
      t "insert start of null" aNull (insert 'c' 0) ("cnull", 1) ;
      t "del start of null" aNull (del 0) ("ull", 0) ;
      t "bs start of null" aNull (bs 0) ("null", 0) ;
      t "insert end of null" aNull (insert '0' 4) ("null0", 5) ;
      t "del end of null" aNull (del 4) ("null", 4) ;
      t "bs end of null" aNull (bs 4) ("nul", 3) ;
      t "insert middle of null" aNull (insert '0' 2) ("nu0ll", 3) ;
      t "del middle of null" aNull (del 2) ("nul", 2) ;
      t "bs middle of null" aNull (bs 2) ("nll", 1) ;
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
      t "insert partial" (b ()) (insert 't' 0) ("t", 1) ;
      t
        "backspacing your way through a partial finishes"
        trueBool
        (presses [K.Backspace; K.Backspace; K.Backspace; K.Backspace; K.Left] 4)
        ("***", 0) ;
      t "insert blank->space" (b ()) (press K.Space 0) (blank, 0) ;
      () ) ;
  describe "Variables" (fun () ->
      (* dont do insert until we have autocomplete *)
      (* t "insert middle of variable" (insert aVar 'c' 5) ("variabcle", 6) ; *)
      t "del middle of variable" aVar (del 5) ("variale", 5) ;
      t "insert capital works" aVar (press (K.Letter 'A') 5) ("variaAble", 6) ;
      t "can't insert invalid" aVar (press K.Dollar 5) ("variable", 5) ;
      t "del variable" aShortVar (del 0) (blank, 0) ;
      t "del long variable" aVar (del 0) ("ariable", 0) ;
      t "del mid variable" aVar (del 6) ("variabe", 6) ;
      t "bs variable" aShortVar (bs 1) (blank, 0) ;
      t "bs mid variable" aVar (bs 8) ("variabl", 7) ;
      t "bs mid variable" aVar (bs 6) ("variale", 5) ;
      () ) ;
  ()
