open Tester
open Prelude
open Fluid
module K = FluidKeyboard
module TL = Toplevel
open FluidPattern

(* These tests should be synced with the subset of tests in fluid_test.ml that
 * makes sense for patterns. See the extensive docs there for how this all
 * works. *)

let eToStructure = Printer.eToStructure

let eToTestString = Printer.eToTestString

let pToString = Printer.pToString

let h expr =
  { ast = FluidAST.ofExpr expr
  ; hTLID = TLID.fromString "7"
  ; spec =
      { space = BlankOr.newF "HTTP"
      ; name = BlankOr.newF "/test"
      ; modifier = BlankOr.newF "GET" }
  ; pos = {x = 0; y = 0} }


let run () =
  let mID = gid () in
  let aStr =
    FPString {matchID = mID; patternID = gid (); str = "some string"}
  in
  let emptyStr = FPString {matchID = mID; patternID = gid (); str = ""} in
  let oneCharStr = FPString {matchID = mID; patternID = gid (); str = "c"} in
  let aShortInt = FPInteger (mID, gid (), "1") in
  let anInt = FPInteger (mID, gid (), "12345") in
  let aHugeInt = FPInteger (mID, gid (), "2000000000000000000") in
  let aFloat = FPFloat (mID, gid (), "123", "456") in
  let aHugeFloat = FPFloat (mID, gid (), "123456789", "123456789") in
  let aShortFloat = FPFloat (mID, gid (), "1", "2") in
  let aPartialFloat = FPFloat (mID, gid (), "1", "") in
  let trueBool = FPBool (mID, gid (), true) in
  let falseBool = FPBool (mID, gid (), false) in
  let aNull = FPNull (mID, gid ()) in
  let five = FPInteger (mID, gid (), "5") in
  (* let fiftySix = FPInteger (mID, gid (), 56) in *)
  (* let seventyEight = FPInteger (gid (), 78) in *)
  let b () = FPBlank (mID, gid ()) in
  (* let aPartialVar = FPPartial (gid (), "req") in *)
  let aVar = FPVariable (mID, gid (), "variable") in
  let aShortVar = FPVariable (mID, gid (), "v") in
  let aConstructor = FPConstructor (mID, gid (), "Just", [b ()]) in
  let m = Fluid_test_data.defaultTestModel in
  let process
      ~(debug : bool)
      (inputs : fluidInputEvent list)
      (pos : int)
      (pat : FluidPattern.t) : string * int =
    let ast = E.EMatch (mID, EBlank (gid ()), [(pat, EBlank (gid ()))]) in
    let extra = 12 in
    let pos = pos + extra in
    let s =
      { Fluid_test_data.defaultTestState with
        ac = AC.reset
      ; oldPos = pos
      ; newPos = pos }
    in
    if debug
    then (
      Js.log2 "state before " (Fluid_utils.debugState s) ;
      Js.log2 "pattern before" (eToStructure ast) ) ;
    let newAST, newState =
      let h = h ast in
      let m = {m with handlers = Handlers.fromList [h]} in
      List.foldl inputs ~init:(h.ast, s) ~f:(fun input (ast, s) ->
          updateMsg m h.hTLID ast (FluidInputEvent input) s)
    in
    let result =
      match FluidAST.toExpr newAST with
      | EMatch (_, _, [(pat, _)]) ->
          pat
      | _ ->
          failwith ("can't match: " ^ eToTestString (FluidAST.toExpr newAST))
    in
    if debug
    then (
      Js.log2 "state after" (Fluid_utils.debugState newState) ;
      Js.log2 "pattern after" (eToStructure (FluidAST.toExpr newAST)) ) ;
    (pToString result, max 0 (newState.newPos - extra))
  in
  let keypress (key : K.key) : fluidInputEvent =
    Keypress
      {key; shiftKey = false; altKey = false; metaKey = false; ctrlKey = false}
  in
  let del ?(debug = false) (pos : int) (pat : fluidPattern) : string * int =
    process ~debug [DeleteContentForward] pos pat
  in
  let bs ?(debug = false) (pos : int) (pat : fluidPattern) : string * int =
    process ~debug [DeleteContentBackward] pos pat
  in
  let space ?(debug = false) (pos : int) (pat : fluidPattern) : string * int =
    process ~debug [keypress K.Space] pos pat
  in
  (* let tab (pos : int) (pat : fluidPattern) : string * int = *)
  (*   process [K.Tab] pos pat *)
  (* in *)
  (* let shiftTab (pos : int) (pat : fluidPattern) : string * int = *)
  (*   process [K.ShiftTab] pos pat *)
  (* in *)
  let press ?(debug = false) (key : K.key) (pos : int) (pat : fluidPattern) :
      string * int =
    process ~debug [keypress key] pos pat
  in
  let inputs
      ?(debug = false)
      (inputs : fluidInputEvent list)
      (pos : int)
      (pat : fluidPattern) : string * int =
    process ~debug inputs pos pat
  in
  let insert ?(debug = false) (s : string) (pos : int) (pat : fluidPattern) :
      string * int =
    process ~debug [InsertText s] pos pat
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
      t "insert mid string" aStr (insert "c" 3) ("\"socme string\"", 4) ;
      t "del mid string" aStr (del 3) ("\"soe string\"", 3) ;
      t "bs mid string" aStr (bs 4) ("\"soe string\"", 3) ;
      t "insert empty string" emptyStr (insert "c" 1) ("\"c\"", 2) ;
      t "del empty string" emptyStr (del 1) ("\"\"", 1) ;
      t "del empty string from outside" emptyStr (del 0) (blank, 0) ;
      t "bs empty string" emptyStr (bs 1) (blank, 0) ;
      t "bs outside empty string" emptyStr (bs 2) ("\"\"", 1) ;
      t "bs near-empty string" oneCharStr (bs 2) ("\"\"", 1) ;
      t "del near-empty string" oneCharStr (del 1) ("\"\"", 1) ;
      t "insert outside string" aStr (insert "c" 0) ("\"some string\"", 0) ;
      t "del outside string" aStr (del 0) ("\"some string\"", 0) ;
      t "bs outside string" aStr (bs 0) ("\"some string\"", 0) ;
      t "insert start of string" aStr (insert "c" 1) ("\"csome string\"", 2) ;
      t "del start of string" aStr (del 1) ("\"ome string\"", 1) ;
      t "bs start of string" aStr (bs 1) ("\"some string\"", 0) ;
      t "insert end of string" aStr (insert "c" 12) ("\"some stringc\"", 13) ;
      t "del end of string" aStr (del 12) ("\"some string\"", 12) ;
      t "bs end of string" aStr (bs 12) ("\"some strin\"", 11) ;
      t "insert after end" aStr (insert "c" 13) ("\"some string\"", 13) ;
      t "del after end of string" aStr (del 13) ("\"some string\"", 13) ;
      t "bs after end" aStr (bs 13) ("\"some string\"", 12) ;
      t "insert space in string" aStr (space 3) ("\"so me string\"", 4) ;
      t "del space in string" aStr (del 5) ("\"somestring\"", 5) ;
      t "bs space in string" aStr (bs 6) ("\"somestring\"", 5) ;
      t "final quote is swallowed" aStr (insert "\"" 12) ("\"some string\"", 13) ;
      ()) ;
  describe "Integers" (fun () ->
      t "insert 0 at front " anInt (insert "0" 0) ("12345", 0) ;
      t "insert at end of short" aShortInt (insert "2" 1) ("12", 2) ;
      t "insert not a number" anInt (insert "c" 0) ("12345", 0) ;
      t "insert start of number" anInt (insert "5" 0) ("512345", 1) ;
      t "del start of number" anInt (del 0) ("2345", 0) ;
      t "bs start of number" anInt (bs 0) ("12345", 0) ;
      t "insert end of number" anInt (insert "0" 5) ("123450", 6) ;
      t "del end of number" anInt (del 5) ("12345", 5) ;
      t "bs end of number" anInt (bs 5) ("1234", 4) ;
      t
        "insert number at scale"
        aHugeInt
        (insert "9" 5)
        ("2000090000000000000", 6) ;
      t
        "insert number at scale"
        aHugeInt
        (insert "9" 0)
        ("920000000000000000", 1) ;
      t
        "insert number at scale"
        aHugeInt
        (insert "9" 19)
        ("2000000000000000000", 19) ;
      (* let max62BitInt = FPInteger (mID, gid (), "4611686018427387903") in *)
      let oneShorterThanMax62BitInt =
        FPInteger (mID, gid (), "461168601842738790")
      in
      t
        "insert number at scale"
        oneShorterThanMax62BitInt
        (insert "3" 18)
        ("4611686018427387903", 19) ;
      t
        "insert number at scale"
        oneShorterThanMax62BitInt
        (insert "4" 18)
        ("461168601842738790", 18) ;
      ()) ;
  describe "Floats" (fun () ->
      t "insert . converts to float - end" anInt (insert "." 5) ("12345.", 6) ;
      t "insert . converts to float - middle" anInt (insert "." 3) ("123.45", 4) ;
      t "insert . converts to float - start" anInt (insert "." 0) (".12345", 1) ;
      t "insert . converts to float - short" aShortInt (insert "." 1) ("1.", 2) ;
      t "continue after adding dot" aPartialFloat (insert "2" 2) ("1.2", 3) ;
      t "insert zero in whole - start" aFloat (insert "0" 0) ("123.456", 0) ;
      t "insert int in whole - start" aFloat (insert "9" 0) ("9123.456", 1) ;
      t "insert int in whole - middle" aFloat (insert "0" 1) ("1023.456", 2) ;
      t "insert int in whole - end" aFloat (insert "0" 3) ("1230.456", 4) ;
      t "insert int in fraction - start" aFloat (insert "0" 4) ("123.0456", 5) ;
      t "insert int in fraction - middle" aFloat (insert "0" 6) ("123.4506", 7) ;
      t "insert int in fraction - end" aFloat (insert "0" 7) ("123.4560", 8) ;
      t "insert non-int in whole" aFloat (insert "c" 2) ("123.456", 2) ;
      t "insert non-int in fraction" aFloat (insert "c" 6) ("123.456", 6) ;
      t "del dot" aFloat (del 3) ("123456", 3) ;
      t "del dot at scale" aHugeFloat (del 9) ("123456789123456789", 9) ;
      let maxPosIntWithDot = FPFloat (mID, gid (), "4611686018427387", "903") in
      let maxPosIntPlus1WithDot =
        FPFloat (mID, gid (), "4611686018427387", "904")
      in
      t "del dot at limit" maxPosIntWithDot (del 16) ("4611686018427387903", 16) ;
      t
        "del dot at limit"
        maxPosIntPlus1WithDot
        (del 16)
        ("461168601842738790", 16) ;
      t "del start of whole" aFloat (del 0) ("23.456", 0) ;
      t "del middle of whole" aFloat (del 1) ("13.456", 1) ;
      t "del end of whole" aFloat (del 2) ("12.456", 2) ;
      t "del start of fraction" aFloat (del 4) ("123.56", 4) ;
      t "del middle of fraction" aFloat (del 5) ("123.46", 5) ;
      t "del end of fraction" aFloat (del 6) ("123.45", 6) ;
      t "del dot converts to int" aFloat (del 3) ("123456", 3) ;
      t "del dot converts to int, no fraction" aPartialFloat (del 1) ("1", 1) ;
      t "bs dot" aFloat (bs 4) ("123456", 3) ;
      t "bs frac of float" aShortFloat (bs 3) ("1.", 2) ;
      t "bs whole of float" aShortFloat (bs 1) (".2", 0) ;
      t "bs dot at scale" aHugeFloat (bs 10) ("123456789123456789", 9) ;
      t "bs dot at limit" maxPosIntWithDot (bs 17) ("4611686018427387903", 16) ;
      t
        "bs dot at limit"
        maxPosIntPlus1WithDot
        (bs 17)
        ("461168601842738790", 16) ;
      t "bs start of whole" aFloat (bs 1) ("23.456", 0) ;
      t "bs middle of whole" aFloat (bs 2) ("13.456", 1) ;
      t "bs end of whole" aFloat (bs 3) ("12.456", 2) ;
      t "bs start of fraction" aFloat (bs 5) ("123.56", 4) ;
      t "bs middle of fraction" aFloat (bs 6) ("123.46", 5) ;
      t "bs end of fraction" aFloat (bs 7) ("123.45", 6) ;
      t "bs dot converts to int" aFloat (bs 4) ("123456", 3) ;
      t "bs dot converts to int, no fraction" aPartialFloat (bs 2) ("1", 1) ;
      t "continue after adding dot" aPartialFloat (insert "2" 2) ("1.2", 3) ;
      ()) ;
  describe "Bools" (fun () ->
      t "insert start of true" trueBool (insert "c" 0) ("ctrue", 1) ;
      t "del start of true" trueBool (del 0) ("rue", 0) ;
      t "bs start of true" trueBool (bs 0) ("true", 0) ;
      t "insert end of true" trueBool (insert "0" 4) ("true0", 5) ;
      t "del end of true" trueBool (del 4) ("true", 4) ;
      t "bs end of true" trueBool (bs 4) ("tru", 3) ;
      t "insert middle of true" trueBool (insert "0" 2) ("tr0ue", 3) ;
      t "del middle of true" trueBool (del 2) ("tre", 2) ;
      t "bs middle of true" trueBool (bs 2) ("tue", 1) ;
      t "insert start of false" falseBool (insert "c" 0) ("cfalse", 1) ;
      t "del start of false" falseBool (del 0) ("alse", 0) ;
      t "bs start of false" falseBool (bs 0) ("false", 0) ;
      t "insert end of false" falseBool (insert "0" 5) ("false0", 6) ;
      t "del end of false" falseBool (del 5) ("false", 5) ;
      t "bs end of false" falseBool (bs 5) ("fals", 4) ;
      t "insert middle of false" falseBool (insert "0" 2) ("fa0lse", 3) ;
      t "del middle of false" falseBool (del 2) ("fase", 2) ;
      t "bs middle of false" falseBool (bs 2) ("flse", 1) ;
      ()) ;
  describe "Nulls" (fun () ->
      t "insert start of null" aNull (insert "c" 0) ("cnull", 1) ;
      t "del start of null" aNull (del 0) ("ull", 0) ;
      t "bs start of null" aNull (bs 0) ("null", 0) ;
      t "insert end of null" aNull (insert "0" 4) ("null0", 5) ;
      t "del end of null" aNull (del 4) ("null", 4) ;
      t "bs end of null" aNull (bs 4) ("nul", 3) ;
      t "insert middle of null" aNull (insert "0" 2) ("nu0ll", 3) ;
      t "del middle of null" aNull (del 2) ("nul", 2) ;
      t "bs middle of null" aNull (bs 2) ("nll", 1) ;
      ()) ;
  describe "Blanks" (fun () ->
      t "insert middle of blank->string" (b ()) (insert "\"" 3) ("\"\"", 1) ;
      t "del middle of blank->blank" (b ()) (del 3) (blank, 3) ;
      t "bs middle of blank->blank" (b ()) (bs 3) (blank, 0) ;
      t "insert blank->string" (b ()) (insert "\"" 0) ("\"\"", 1) ;
      t "del blank->string" emptyStr (del 0) (blank, 0) ;
      t "bs blank->string" emptyStr (bs 1) (blank, 0) ;
      t "insert blank->int" (b ()) (insert "5" 0) ("5", 1) ;
      t "insert blank->int" (b ()) (insert "0" 0) ("0", 1) ;
      t "del int->blank " five (del 0) (blank, 0) ;
      t "bs int->blank " five (bs 1) (blank, 0) ;
      t "insert end of blank->int" (b ()) (insert "5" 1) ("5", 1) ;
      t "insert partial" (b ()) (insert "t" 0) ("t", 1) ;
      t
        "backspacing your way through a partial finishes"
        trueBool
        (inputs
           [ DeleteContentBackward
           ; DeleteContentBackward
           ; DeleteContentBackward
           ; DeleteContentBackward
           ; keypress K.Left ]
           4)
        ("***", 0) ;
      t "insert blank->space" (b ()) (press K.Space 0) (blank, 0) ;
      ()) ;
  describe "Variables" (fun () ->
      t "insert middle of variable" aVar (insert "c" 5) ("variacble", 6) ;
      t "del middle of variable" aVar (del 5) ("variale", 5) ;
      t "insert capital works" aVar (insert "A" 5) ("variaAble", 6) ;
      t "can't insert invalid" aVar (insert "$" 5) ("variable", 5) ;
      t "del variable" aShortVar (del 0) (blank, 0) ;
      t "del long variable" aVar (del 0) ("ariable", 0) ;
      t "del mid variable" aVar (del 6) ("variabe", 6) ;
      t "bs variable" aShortVar (bs 1) (blank, 0) ;
      t "bs mid variable" aVar (bs 8) ("variabl", 7) ;
      t "bs mid variable" aVar (bs 6) ("variale", 5) ;
      ()) ;
  describe "Constructors" (fun () ->
      t
        "arguments work in constructors"
        aConstructor
        (insert "t" 5)
        ("Just t", 6) ;
      t
        "int arguments work in constructors"
        aConstructor
        (insert "5" 5)
        ("Just 5", 6) ;
      t "bs on a constructor deletes" aConstructor (bs 4) ("Jus", 3) ;
      t "del on a constructor deletes" aConstructor (del 0) ("ust", 0) ;
      t
        "space on a constructor blank does nothing"
        aConstructor
        (space 5)
        ("Just ***", 5) ;
      (* TODO: test renaming constructors.
       * It's not too useful yet because there's only 4 constructors and,
       * hence, unlikely that anyone will rename them this way.
       * Also, the names of the temporary variables used to store the old arguments of a changed
       * constructor are randomly generated and would be hard to test *)
      ()) ;
  ()
