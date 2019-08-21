open Jest
open Expect
open Tc
open Types
open Prelude
open Fluid
module B = Blank
module K = FluidKeyboard

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


type testResult = string * string * int

(* ast, clipboard, newPos *)

let () =
  let process ~debug range ast (keyEvent : K.keyEvent) : testResult =
    let s =
      { Defaults.defaultFluidState with
        ac = AC.reset m; selection = Some {range} }
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
  (* NOTE: 
    * most tests don't work yet, they just serve as examples of how
    * the harness works
    *)
  describe "Boolean" (fun () ->
      t
        "copying a bool should add an EBool to clipboard"
        (EBool (gid (), true))
        (copy (0, 4))
        ("true", "true", 0) ;
      t
        "cutting a bool should add an EBool to clipboard and leave a blank"
        (EBool (gid (), true))
        (cut (0, 4))
        ("___", "true", 0) ;
      (* NOT WORKING YET
      t
        "copying a bool should add an EBool to clipboard 2"
        (EFnCall (gid (), "Bool::not", [EBool (gid (), true)], NoRail))
        (copy (10, 14))
        ("Bool::not true", "true", 0) ;
        *)
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
