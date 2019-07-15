open! Tc
open Jest
open Expect
open Types
module B = Blank
module D = Defaults
module R = Refactor
module TL = Toplevel

let () =
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
          builtInFunctions = [f1; f2]; handlers = Handlers.fromList hs }
      in
      let handlerWithPointer fnName fnRail =
        let ast = F (ID "ast1", FnCall (B.newF fnName, [], fnRail)) in
        ( { hTLID = TLID "handler1"
          ; pos = {x = 0; y = 0}
          ; ast
          ; spec =
              { space = B.newF "HTTP"
              ; name = B.newF "/src"
              ; modifier = B.newF "POST" } }
        , PExpr ast )
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
              | F (_, FnCall (F (_, "Int::notResulty"), [], NoRail)) ->
                  true
              | _ ->
                  false )
            | _ ->
                false
          in
          expect res |> toEqual true ) ;
      test "toggles error-rail-y function onto rail" (fun () ->
          let m, h, pd = init "Result::resulty" NoRail in
          let op = Refactor.putOnRail m (TLHandler h) pd in
          let res =
            match op with
            | RPC ([SetHandler (_, _, h)], _) ->
              ( match h.ast with
              | F (_, FnCall (F (_, "Result::resulty"), [], Rail)) ->
                  true
              | _ ->
                  false )
            | _ ->
                false
          in
          expect res |> toEqual true ) ;
      test "does not put non-error-rail-y function onto rail" (fun () ->
          let m, h, pd = init "Int::notResulty" NoRail in
          let op = Refactor.putOnRail m (TLHandler h) pd in
          let res = match op with NoChange -> true | _ -> false in
          expect res |> toEqual true ) ) ;
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
      test "database renamed, handler updates variable" (fun () ->
          let h =
            { ast = F (ID "ast1", Variable "ElmCode")
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
            ; ufAST = F (ID "ast3", Variable "ElmCode") }
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
              | F (_, Variable "WeirdCode"), F (_, Variable "WeirdCode") ->
                  true
              | _ ->
                  false )
            | _ ->
                false
          in
          expect res |> toEqual true ) ;
      test "database renamed, handler does not change" (fun () ->
          let h =
            { ast = F (ID "ast1", Variable "request")
            ; spec =
                { space = B.newF "HTTP"
                ; name = B.newF "/src"
                ; modifier = B.newF "POST" }
            ; hTLID = TLID "handler1"
            ; pos = {x = 0; y = 0} }
          in
          let model =
            { D.defaultModel with
              dbs = DB.fromList [db0]; handlers = Handlers.fromList [h] }
          in
          let ops = R.renameDBReferences model "ElmCode" "WeirdCode" in
          expect ops |> toEqual [] ) ;
      () ) ;
  describe "generateUserType" (fun () ->
      test "with None input" (fun () ->
          expect
            ( match R.generateUserType None with
            | Ok _ ->
                false
            | Error _ ->
                true )
          |> toBe true ) ;
      test "with Some non-DObj input" (fun () ->
          expect
            ( match R.generateUserType (Some (DStr "foo")) with
            | Ok _ ->
                false
            | Error _ ->
                true )
          |> toBe true ) ;
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
              ( match ut.utDefinition with UTRecord utr ->
                  utr
                  |> List.map ~f:(fun urf ->
                         ( urf.urfName |> Blank.toMaybe
                         , urf.urfTipe |> Blank.toMaybe ) ) )
          in
          expect fields |> toEqual expectedFields ) ) ;
  describe "extractVarInAst" (fun () ->
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
      let builtInFunctions =
        [ { fnName = "Int::add"
          ; fnParameters = [par "a" TInt; par "b" TInt]
          ; fnDescription = ""
          ; fnReturnTipe = TInt
          ; fnPreviewExecutionSafe = true
          ; fnDeprecated = false
          ; fnInfix = false }
        ; { fnName = "Dict::map"
          ; fnParameters =
              [par "dict" TObj; par "f" TBlock ~args:["key"; "value"]]
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
      in
      let modelAndTl (ast : expr) =
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
        let model =
          { D.defaultModel with
            builtInFunctions; handlers = [(hTLID, tl)] |> TLIDDict.fromList }
        in
        (model, TLHandler tl)
      in
      let exprToString expr : string =
        expr
        |> Tuple2.first
        |> Fluid.fromExpr Defaults.defaultFluidState
        |> Fluid.eToString Defaults.defaultFluidState
      in
      test "with sole expression" (fun () ->
          let expr = B.newF (Value "4") in
          let ast = expr in
          let m, tl = modelAndTl ast in
          expect (R.extractVarInAst m tl expr ast "var" |> exprToString)
          |> toEqual "let var = 4\nvar" ) ;
      test "with expression inside let" (fun () ->
          let expr =
            B.newF
              (FnCall
                 ( B.newF "Int::add"
                 , [B.newF (Variable "b"); B.newF (Value "4")]
                 , NoRail ))
          in
          let ast = Let (B.newF "b", B.newF (Value "5"), expr) |> B.newF in
          let m, tl = modelAndTl ast in
          expect (R.extractVarInAst m tl expr ast "var" |> exprToString)
          |> toEqual "let b = 5\nlet var = Int::add b 4\nvar" ) ;
      test "with expression inside thread inside let" (fun () ->
          let expr =
            FnCall
              ( B.newF "DB::set_v1"
              , [ B.newF
                    (FieldAccess (B.newF (Variable "request"), B.newF "body"))
                ; B.newF
                    (FnCall
                       (B.newF "toString", [B.newF (Variable "id")], NoRail))
                ; B.new_ () ]
              , NoRail )
            |> B.newF
          in
          let threadedExpr =
            B.newF
              (FnCall
                 ( B.newF "assoc"
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
          let m, tl = modelAndTl ast in
          expect (R.extractVarInAst m tl expr ast "var" |> exprToString)
          |> toEqual
               "let id = Uuid::generate\nlet var = DB::setv1 request.body toString id ___\nvar\n|>assoc \"id\" id"
      ) )
