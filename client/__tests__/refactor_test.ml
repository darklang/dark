open! Tc
open Jest
open Expect
open Types
module B = Blank
module D = Defaults
module R = Refactor

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
      let model tls =
        {D.defaultModel with builtInFunctions = [f1; f2]; toplevels = tls}
      in
      let handlerWithPointer fnName fnRail =
        let ast = F (ID "ast1", FnCall (B.newF fnName, [], fnRail)) in
        ( { id = TLID "handler1"
          ; pos = {x = 0; y = 0}
          ; data =
              TLHandler
                { ast
                ; spec =
                    { space = B.newF "HTTP"
                    ; name = B.newF "/src"
                    ; modifier = B.newF "POST" }
                ; tlid = TLID "handler1" } }
        , PExpr ast )
      in
      let init fnName fnRail =
        let tl, pd = handlerWithPointer fnName fnRail in
        let m = model [tl] in
        (m, tl, pd)
      in
      test "toggles any fncall off rail" (fun () ->
          let m, tl, pd = init "Int::notResulty" Rail in
          let op = Refactor.takeOffRail m tl pd in
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
          let m, tl, pd = init "Result::resulty" NoRail in
          let op = Refactor.putOnRail m tl pd in
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
          let m, tl, pd = init "Int::notResulty" NoRail in
          let op = Refactor.putOnRail m tl pd in
          let res = match op with NoChange -> true | _ -> false in
          expect res |> toEqual true ) ) ;
  describe "renameDBReferences" (fun () ->
      let db0 =
        { dbTLID = TLID "db0"
        ; dbName = B.newF "ElmCode"
        ; cols = []
        ; version = 0
        ; oldMigrations = []
        ; activeMigration = None }
      in
      test "database renamed, handler updates variable" (fun () ->
          let h =
            { ast = F (ID "ast1", Variable "ElmCode")
            ; spec =
                { space = B.newF "HTTP"
                ; name = B.newF "/src"
                ; modifier = B.newF "POST" }
            ; tlid = TLID "handler1" }
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
              toplevels =
                [ {id = TLID "tl-1"; pos = D.origin; data = TLDB db0}
                ; {id = TLID "tl-2"; pos = D.centerPos; data = TLHandler h} ]
            ; userFunctions = [f] }
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
            ; tlid = TLID "handler1" }
          in
          let model =
            { D.defaultModel with
              toplevels =
                [ {id = TLID "tl-1"; pos = D.origin; data = TLDB db0}
                ; {id = TLID "tl-2"; pos = D.centerPos; data = TLHandler h} ]
            }
          in
          let ops = R.renameDBReferences model "ElmCode" "WeirdCode" in
          expect ops |> toEqual [] ) ;
      () ) ;
  ()
