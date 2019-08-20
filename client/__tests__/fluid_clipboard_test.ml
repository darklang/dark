open Jest
open Expect
open Tc
open Types
open Prelude
open Fluid
module B = Blank
module K = FluidKeyboard

let b () = newB ()

let complexExpr =
  EBinOp
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
    let newlinesBeforeStartPos =
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
    let ast = EIf (gid (), EBool (gid (), true), ast, EInteger (gid (), 5)) in
    let wrapperOffset = 15 in
    let extra = wrapperOffset + (newlinesBeforeStartPos * 2) in
    let pos = pos + extra in
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
    let result =
      match newAST with
      | EIf (_, _, expr, _) ->
          expr
      | expr ->
          impossible ("the wrapper is broken: " ^ eToString s expr)
    in
    let endPos = ref (newState.newPos - wrapperOffset) in
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
    let finalPos = max 0 (min last !endPos) in
    if debug
    then (
      Js.log2 "state after" (Fluid_utils.debugState newState) ;
      Js.log2 "expr after" (eToStructure newState result) ) ;
    (eToString newState result, clipboardStr newState, finalPos)
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
      { key = K.Letter 'c'
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
  describe "Boolean" (fun () ->
      t
        "copying a bool should add an EBool to clipboard"
        (EBool (gid (), true))
        (copy (0, 4))
        ("true", "true", 0) ;
      t
        "copying a bool should add an EBool to clipboard 2"
        (EFnCall (gid (), "Bool::not", [EBool (gid (), true)], NoRail))
        (copy (10, 14))
        ("Bool::not true", "true", 0) ;
      t
        "cutting a bool should add an EBool to clipboard and leave a blank"
        (EBool (gid (), true))
        (cut (0, 4))
        ("___", "true", 0) ;
      () ) ;
  describe "Functions" (fun () ->
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
      () )
