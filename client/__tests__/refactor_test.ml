open Tester
open! Tc
open Types
open Prelude
module B = Blank
module D = Defaults
module R = Refactor
module TL = Toplevel

let sampleFunctions =
  let par
      ?(paramDescription = "")
      ?(args = [])
      ?(paramOptional = false)
      paramName
      paramTipe : parameter =
    { paramName
    ; paramTipe
    ; paramOptional
    ; paramBlock_args = args
    ; paramDescription }
  in
  [ { fnName = "Int::add"
    ; fnParameters = [par "a" TInt; par "b" TInt]
    ; fnDescription = ""
    ; fnReturnTipe = TInt
    ; fnPreviewExecutionSafe = true
    ; fnDeprecated = false
    ; fnInfix = false }
  ; { fnName = "List::getAt_v1"
    ; fnParameters = [par "list" TList; par "index" TInt]
    ; fnDescription = ""
    ; fnReturnTipe = TOption
    ; fnPreviewExecutionSafe = true
    ; fnDeprecated = false
    ; fnInfix = false }
  ; { fnName = "Dict::map"
    ; fnParameters = [par "dict" TObj; par "f" TBlock ~args:["key"; "value"]]
    ; fnDescription = ""
    ; fnReturnTipe = TObj
    ; fnPreviewExecutionSafe = true
    ; fnDeprecated = false
    ; fnInfix = false }
  ; { fnName = "DB::set_v1"
    ; fnParameters = [par "val" TObj; par "key" TStr; par "table" TDB]
    ; fnDescription = ""
    ; fnReturnTipe = TObj
    ; fnPreviewExecutionSafe = true
    ; fnDeprecated = false
    ; fnInfix = false } ]


let defaultHandler =
  { hTLID = TLID "handler1"
  ; pos = {x = 0; y = 0}
  ; ast = EBlank (gid ())
  ; spec =
      {space = B.newF "HTTP"; name = B.newF "/src"; modifier = B.newF "POST"} }


let run () =
  FluidExpression.functions := sampleFunctions ;
  describe "takeOffRail & putOnRail" (fun () ->
      let f1 =
        { fnName = "Result::resulty"
        ; fnParameters = []
        ; fnDescription = ""
        ; fnReturnTipe = TResult
        ; fnPreviewExecutionSafe = true
        ; fnDeprecated = false
        ; fnInfix = false }
      in
      let f2 =
        { fnName = "Int::notResulty"
        ; fnParameters = []
        ; fnDescription = ""
        ; fnReturnTipe = TInt
        ; fnPreviewExecutionSafe = true
        ; fnDeprecated = false
        ; fnInfix = false }
      in
      let model hs =
        { D.defaultModel with
          builtInFunctions = [f1; f2]
        ; handlers = Handlers.fromList hs }
      in
      let handlerWithPointer fnName fnRail =
        let ast = EFnCall (ID "ast1", fnName, [], fnRail) in
        ({defaultHandler with ast}, PExpr (FluidExpression.toNExpr ast))
      in
      let init fnName fnRail =
        let h, pd = handlerWithPointer fnName fnRail in
        let m = model [h] in
        (m, h, pd)
      in
      test "toggles any fncall off rail" (fun () ->
          let m, h, pd = init "Int::notResulty" Rail in
          let op = Refactor.takeOffRail m (TLHandler h) pd in
          let res =
            match op with
            | RPC ([SetHandler (_, _, h)], _) ->
              ( match h.ast with
              | EFnCall (_, "Int::notResulty", [], NoRail) ->
                  true
              | _ ->
                  false )
            | _ ->
                false
          in
          expect res |> toEqual true) ;
      test "toggles any fncall off rail in a thread" (fun () ->
          let open Fluid_test_data in
          let module E = FluidExpression in
          let fn = fn ~ster:Rail "List::getAt_v2" [pipeTarget; int "5"] in
          let ast = pipe emptyList [fn] in
          let h = {defaultHandler with ast} in
          let m = model [h] in
          let id = deID (E.id fn) in
          let pd =
            PExpr
              (F
                 ( ID id
                 , FnCall
                     ( F (ID (id ^ "_name"), "List::getAt_v2")
                     , [F (gid (), Value "5")]
                     , Rail ) ))
          in
          (* this used to crash or just lose all its arguments *)
          let op = Refactor.takeOffRail m (TLHandler h) pd in
          let res =
            match op with
            | RPC ([SetHandler (_, _, h)], _) ->
              ( match h.ast with
              | EPipe
                  ( _
                  , [ EList (_, [])
                    ; EFnCall
                        ( _
                        , "List::getAt_v2"
                        , [EPipeTarget _; EInteger (_, "5")]
                        , NoRail ) ] ) ->
                  true
              | _ ->
                  false )
            | _ ->
                false
          in
          expect res |> toEqual true) ;
      test "toggles error-rail-y function onto rail" (fun () ->
          let m, h, pd = init "Result::resulty" NoRail in
          let op = Refactor.putOnRail m (TLHandler h) pd in
          let res =
            match op with
            | RPC ([SetHandler (_, _, h)], _) ->
              ( match h.ast with
              | EFnCall (_, "Result::resulty", [], Rail) ->
                  true
              | _ ->
                  false )
            | _ ->
                false
          in
          expect res |> toEqual true) ;
      test "does not put non-error-rail-y function onto rail" (fun () ->
          let m, h, pd = init "Int::notResulty" NoRail in
          let op = Refactor.putOnRail m (TLHandler h) pd in
          let res = match op with NoChange -> true | _ -> false in
          expect res |> toEqual true)) ;
  describe "renameDBReferences" (fun () ->
      let db0 =
        { dbTLID = TLID "db0"
        ; dbName = B.newF "ElmCode"
        ; cols = []
        ; version = 0
        ; oldMigrations = []
        ; activeMigration = None
        ; pos = {x = 0; y = 0} }
      in
      test "datastore renamed, handler updates variable" (fun () ->
          let h =
            { ast = EVariable (ID "ast1", "ElmCode")
            ; spec =
                { space = B.newF "HTTP"
                ; name = B.newF "/src"
                ; modifier = B.newF "POST" }
            ; hTLID = TLID "handler1"
            ; pos = {x = 0; y = 0} }
          in
          let f =
            { ufTLID = TLID "tl-3"
            ; ufMetadata =
                { ufmName = B.newF "f-1"
                ; ufmParameters = []
                ; ufmDescription = ""
                ; ufmReturnTipe = B.new_ ()
                ; ufmInfix = false }
            ; ufAST = EVariable (ID "ast3", "ElmCode") }
          in
          let model =
            { D.defaultModel with
              dbs = DB.fromList [db0]
            ; handlers = Handlers.fromList [h]
            ; userFunctions = Functions.fromList [f] }
          in
          let ops = R.renameDBReferences model "ElmCode" "WeirdCode" in
          let res =
            match List.sortBy ~f:Encoders.tlidOf ops with
            | [SetHandler (_, _, h); SetFunction f] ->
              ( match (h.ast, f.ufAST) with
              | EVariable (_, "WeirdCode"), EVariable (_, "WeirdCode") ->
                  true
              | _ ->
                  false )
            | _ ->
                false
          in
          expect res |> toEqual true) ;
      test "datastore renamed, handler does not change" (fun () ->
          let h =
            { ast = EVariable (ID "ast1", "request")
            ; spec =
                { space = B.newF "HTTP"
                ; name = B.newF "/src"
                ; modifier = B.newF "POST" }
            ; hTLID = TLID "handler1"
            ; pos = {x = 0; y = 0} }
          in
          let model =
            { D.defaultModel with
              dbs = DB.fromList [db0]
            ; handlers = Handlers.fromList [h] }
          in
          let ops = R.renameDBReferences model "ElmCode" "WeirdCode" in
          expect ops |> toEqual []) ;
      ()) ;
  describe "generateUserType" (fun () ->
      test "with None input" (fun () ->
          expect
            ( match R.generateUserType None with
            | Ok _ ->
                false
            | Error _ ->
                true )
          |> toBe true) ;
      test "with Some non-DObj input" (fun () ->
          expect
            ( match R.generateUserType (Some (DStr "foo")) with
            | Ok _ ->
                false
            | Error _ ->
                true )
          |> toBe true) ;
      test "with Some DObj input" (fun () ->
          let dobj =
            DObj
              ( [ ("str", DStr "foo")
                ; ("int", DInt 1)
                ; ("float", DFloat 1.0)
                ; ("obj", DObj StrDict.empty)
                ; ("date", DDate "2019-07-10T20:42:11Z")
                ; ("datestr", DStr "2019-07-10T20:42:11Z")
                ; ("uuid", DUuid "0a18ca77-9bae-4dfb-816f-0d12cb81c17b")
                ; ("uuidstr", DStr "0a18ca77-9bae-4dfb-816f-0d12cb81c17b") ]
              |> StrDict.fromList )
          in
          let expectedFields =
            (* Note: datestr and uuidstr are TDate and TUuid respectively, _not_ TStr *)
            [ ("str", TStr)
            ; ("int", TInt)
            ; ("float", TFloat)
            ; ("obj", TObj)
            ; ("date", TDate)
            ; ("datestr", TStr)
              (* for now, TStr; in future, maybe we coerce to
                                   TDate *)
            ; ("uuid", TUuid)
            ; ("uuidstr", TStr)
              (* for now, TStr; in future, maybe we coerce to
                                   TUuid *)
            ]
            |> List.map ~f:(fun (k, v) -> (Some k, Some v))
            (* sortBy here because the dobj gets sorted - not sure exactly
               where, but order doesn't matter except in this test *)
            |> List.sortBy ~f:(fun (k, _) -> k)
          in
          let _ = (dobj, expectedFields) in
          let tipe = R.generateUserType (Some dobj) in
          let fields =
            match tipe with
            | Error _ ->
                []
            | Ok ut ->
              ( match ut.utDefinition with
              | UTRecord utr ->
                  utr
                  |> List.map ~f:(fun urf ->
                         ( urf.urfName |> Blank.toMaybe
                         , urf.urfTipe |> Blank.toMaybe )) )
          in
          expect fields |> toEqual expectedFields)) ;
  describe "extractVarInAst" (fun () ->
      let modelAndTl (ast : fluidExpr) =
        let hTLID = TLID "handler1" in
        let tl =
          { hTLID
          ; ast
          ; pos = {x = 0; y = 0}
          ; spec =
              { space = B.newF "HTTP"
              ; name = B.newF "/src"
              ; modifier = B.newF "POST" } }
        in
        let m =
          { D.defaultModel with
            builtInFunctions = sampleFunctions
          ; handlers = [(hTLID, tl)] |> TLIDDict.fromList }
        in
        let m =
          { m with
            fluidState =
              {Defaults.defaultFluidState with ac = FluidAutocomplete.reset m}
          }
        in
        (m, TLHandler tl)
      in
      let exprToString expr : string =
        expr
        |> Tuple2.first
        |> FluidExpression.fromNExpr
        |> FluidPrinter.eToString
      in
      test "with sole expression" (fun () ->
          let expr = B.newF (Value "4") in
          let ast = expr in
          let m, tl = modelAndTl (FluidExpression.fromNExpr ast) in
          expect (R.extractVarInAst m tl expr ast "var" |> exprToString)
          |> toEqual "let var = 4\nvar") ;
      test "with expression inside let" (fun () ->
          let expr =
            B.newF
              (FnCall
                 ( B.newF "Int::add"
                 , [B.newF (Variable "b"); B.newF (Value "4")]
                 , NoRail ))
          in
          let ast = Let (B.newF "b", B.newF (Value "5"), expr) |> B.newF in
          let m, tl = modelAndTl (FluidExpression.fromNExpr ast) in
          expect (R.extractVarInAst m tl expr ast "var" |> exprToString)
          |> toEqual "let b = 5\nlet var = Int::add b 4\nvar") ;
      test "with expression inside thread inside let" (fun () ->
          let expr =
            FnCall
              ( B.newF "DB::set_v1"
              , [ B.newF
                    (FieldAccess (B.newF (Variable "request"), B.newF "body"))
                ; B.newF
                    (FnCall (B.newF "toString", [B.newF (Variable "id")], NoRail))
                ; B.new_ () ]
              , NoRail )
            |> B.newF
          in
          let threadedExpr =
            B.newF
              (FnCall
                 ( B.newF "Dict::set"
                 , [B.newF (Value "\"id\""); B.newF (Variable "id")]
                 , NoRail ))
          in
          let exprInThread = Thread [expr; threadedExpr] |> B.newF in
          let ast =
            Let
              ( B.newF "id"
              , B.newF (FnCall (B.newF "Uuid::generate", [], NoRail))
              , exprInThread )
            |> B.newF
          in
          let m, tl = modelAndTl (FluidExpression.fromNExpr ast) in
          expect (R.extractVarInAst m tl expr ast "var" |> exprToString)
          |> toEqual
               "let id = Uuid::generate\nlet var = DB::setv1 request.body toString id ___________________\nvar\n|>Dict::set \"id\" id\n"))
