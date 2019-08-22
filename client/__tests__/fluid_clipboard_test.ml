open Jest
open Expect
open Tc
open Types
open Prelude
open Fluid
module B = Blank
module K = FluidKeyboard

type testResult = (* ast, clipboard, newPos *) string * string * int

let () =
  let m = Defaults.defaultModel in
  let process ~debug ?(clipboard = None) range ast (keyEvent : K.keyEvent) :
      testResult =
    let s =
      { Defaults.defaultFluidState with
        ac = AC.reset m; selection = Some {range}; clipboard }
    in
    let pos = Tuple2.first range in
    let s = {s with oldPos = pos; newPos = pos} in
    let clipboardStr s =
      s.clipboard
      |> Option.map ~f:(eToString s)
      |> Option.withDefault ~default:""
    in
    if debug
    then (
      Js.log2 "state before " (Fluid_utils.debugState s) ;
      Js.log2 "ast before" (eToStructure s ast) ;
      Js.log2 "clipboard before" (clipboardStr s) ) ;
    let h = Fluid_utils.h ast in
    let m = {m with handlers = Handlers.fromList [h]} in
    let newAST, newState =
      updateMsg m h.hTLID ast (FluidKeyPress keyEvent) s
    in
    let last =
      toTokens newState newAST
      |> List.last
      |> deOption "last"
      |> fun x -> x.endPos
    in
    let finalPos = max 0 (min last newState.newPos) in
    if debug
    then (
      Js.log2 "state after" (Fluid_utils.debugState newState) ;
      Js.log2 "expr after" (eToStructure newState newAST) ;
      Js.log2 "clipboard after" (clipboardStr newState) ) ;
    (eToString newState newAST, clipboardStr newState, finalPos)
  in
  let copy ?(debug = false) (range : int * int) (expr : fluidExpr) : testResult
      =
    let keyEvent : K.keyEvent =
      { key = K.Letter 'c'
      ; shiftKey = false
      ; altKey = false
      ; metaKey = false
      ; ctrlKey = true (* since tests are run on linux *) }
    in
    process ~debug range expr keyEvent
  in
  let cut ?(debug = false) (range : int * int) (expr : fluidExpr) : testResult
      =
    let keyEvent : K.keyEvent =
      { key = K.Letter 'x'
      ; shiftKey = false
      ; altKey = false
      ; metaKey = false
      ; ctrlKey = true (* since tests are run on linux *) }
    in
    process ~debug range expr keyEvent
  in
  let paste ?(debug = false) ~clipboard (range : int * int) (expr : fluidExpr)
      : testResult =
    let keyEvent : K.keyEvent =
      { key = K.Letter 'v'
      ; shiftKey = false
      ; altKey = false
      ; metaKey = false
      ; ctrlKey = true (* since tests are run on linux *) }
    in
    process ~debug ~clipboard:(Some clipboard) range expr keyEvent
  in
  let t
      (name : string)
      (initial : fluidExpr)
      (fn : fluidExpr -> testResult)
      (expected : string * string * int) =
    test
      ( name
      ^ " - `"
      ^ ( eToString Defaults.defaultFluidState initial
        |> Regex.replace ~re:(Regex.regex "\n") ~repl:" " )
      ^ "`" )
      (fun () -> expect (fn initial) |> toEqual expected)
  in
  describe "Booleans" (fun () ->
      t
        "copying a bool should add an EBool to clipboard"
        (EBool (gid (), true))
        (copy (0, 4))
        ("true", "true", 0) ;
      t
        "copying a bool should add an EBool to clipboard 2"
        (EFnCall (gid (), "Bool::not", [EBool (gid (), true)], NoRail))
        (copy (10, 14))
        ("Bool::not true", "true", 10) ;
      t
        "cutting a bool should add an EBool to clipboard and leave a blank"
        (EBool (gid (), false))
        (cut (0, 5))
        ("___", "false", 0) ;
      t
        "cutting a bool should add an EBool to clipboard 2"
        (EFnCall (gid (), "Bool::not", [EBool (gid (), true)], NoRail))
        (cut (10, 14))
        ("Bool::not ___", "true", 10) ;
      t
        "pasting an EBool from clipboard on a blank should paste it"
        (EBlank (gid ()))
        (paste ~clipboard:(EBool (gid (), true)) (0, 0))
        ("true", "true", 0) ;
      () ) ;
  describe "Nulls" (fun () ->
      t
        "copying a null should add an ENull to clipboard"
        (ENull (gid ()))
        (copy (0, 4))
        ("null", "null", 0) ;
      t
        "copying a null should add an ENull to clipboard 2"
        (EFnCall (gid (), "Bool::isNull", [ENull (gid ())], NoRail))
        (copy (13, 17))
        ("Bool::isNull null", "null", 13) ;
      t
        "cutting a null should add an ENull to clipboard and leave a blank"
        (ENull (gid ()))
        (cut (0, 4))
        ("___", "null", 0) ;
      t
        "cutting a null should add an ENull to clipboard 2"
        (EFnCall (gid (), "Bool::isNull", [ENull (gid ())], NoRail))
        (cut (13, 17))
        ("Bool::isNull ___", "null", 13) ;
      t
        "pasting an ENull from clipboard on a blank should paste it"
        (EBlank (gid ()))
        (paste ~clipboard:(ENull (gid ())) (0, 0))
        ("null", "null", 0) ;
      () ) ;
  describe "Integers" (fun () ->
      t
        "copying an int should add an EInteger to clipboard"
        (EInteger (gid (), 1000))
        (copy (0, 4))
        ("1000", "1000", 0) ;
      t
        "copying an int should add an EInteger to clipboard and leave a blank 2"
        (EFnCall (gid (), "Int::sqrt", [EInteger (gid (), 1000)], NoRail))
        (copy (10, 14))
        ("Int::sqrt 1000", "1000", 10) ;
      t
        "cutting an int should add an EInteger to clipboard and leave a blank"
        (EInteger (gid (), 1000))
        (cut (0, 4))
        ("___", "1000", 0) ;
      t
        "cutting an int should add an EInteger to clipboard and leave a blank 2"
        (EFnCall (gid (), "Int::sqrt", [EInteger (gid (), 1000)], NoRail))
        (cut (10, 14))
        ("Int::sqrt ___", "1000", 10) ;
      t
        "pasting an EInteger from clipboard on a blank should paste it"
        (EBlank (gid ()))
        (paste ~clipboard:(EInteger (gid (), 1234)) (0, 0))
        ("1234", "1234", 0) ;
      () ) ;
  describe "Strings" (fun () ->
      t
        "copying a string should add an EString to clipboard"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (copy (0, 21))
        ("\"abcd EFGH ijkl 1234\"", "\"abcd EFGH ijkl 1234\"", 0) ;
      t
        "copying a string should add an EString to clipboard 2"
        (EFnCall
           ( gid ()
           , "String::reverse"
           , [EString (gid (), "abcd EFGH ijkl 1234")]
           , NoRail ))
        (copy (16, 37))
        ( "String::reverse \"abcd EFGH ijkl 1234\""
        , "\"abcd EFGH ijkl 1234\""
        , 16 ) ;
      t
        "cutting a string should add an EString to clipboard"
        (EString (gid (), "abcd EFGH ijkl 1234"))
        (cut (0, 21))
        ("___", "\"abcd EFGH ijkl 1234\"", 0) ;
      t
        "cutting a string should add an EString to clipboard 2"
        (EFnCall
           ( gid ()
           , "String::reverse"
           , [EString (gid (), "abcd EFGH ijkl 1234")]
           , NoRail ))
        (cut (16, 37))
        ("String::reverse ___", "\"abcd EFGH ijkl 1234\"", 16) ;
      t
        "pasting an EString from clipboard on a blank should paste it"
        (EBlank (gid ()))
        (paste ~clipboard:(EString (gid (), "abcd EFGH ijkl 1234")) (0, 0))
        ("\"abcd EFGH ijkl 1234\"", "\"abcd EFGH ijkl 1234\"", 0) ;
      () ) ;
  describe "Functions" (fun () ->
      (* NOT WORKING YET
      t
        "copying a function name should add an EFnCall w blank arguments to clipboard"
        (EFnCall
           ( gid ()
           , "Int::sqrt"
           , [EVariable (gid (), "a"); EVariable (gid (), "b")]
           , NoRail ))
        (copy (0, 9))
        ("Int::sqrt a b", "Int::sqrt ___ ___", 0) ;
      t
        "copying a function's argument should add the argument's expression to clipboard"
        (EFnCall
           ( gid ()
           , "Int::sqrt"
           , [EVariable (gid (), "a"); EVariable (gid (), "b")]
           , NoRail ))
        (copy (11, 12))
        ("Int::sqrt a b", "a", 0) ;
  *)
      () ) ;
  describe "Threads" (fun () ->
      (* NOT WORKING YET
      let threadOn expr fns = EThread (gid (), expr :: fns) in
      let emptyList = EList (gid (), []) in
      let aListNum n = EList (gid (), [EInteger (gid (), n)]) in
      let listFn args =
        EFnCall (gid (), "List::append", EThreadTarget (gid ()) :: args, NoRail)
      in
      let aThread =
        threadOn emptyList [listFn [aListNum 5]; listFn [aListNum 5]]
      in
      t
        "copying first expression of thread adds it to clipboard"
        aThread
        (copy (0, 2))
        ("[]\n|>List::append [5]\n|>List::append [5]", "[]", 0) ; *)
      () )
