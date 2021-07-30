open Tester
open Prelude
open AST
module B = BlankOr
open FluidExpression
open FluidPattern
open FluidShortcuts

type ('a, 'b) transformation_test_result =
  | Pass
  | Fail of 'a * 'b

let run () =
  describe "ast" (fun () ->
      let id1 = ID.fromString "5" in
      let id2 = ID.fromString "10" in
      let id3 = ID.fromString "11" in
      let id4 = ID.fromString "12" in
      let id5 = ID.fromString "13" in
      let id6 = ID.fromString "14" in
      let id7 = ID.fromString "15" in
      test "lambda var is not free" (fun () ->
          expect
            (freeVariables
               (ELambda (id1, [(id2, "var")], EVariable (id3, "var"))))
          |> toEqual []) ;
      test "match pattern is not free" (fun () ->
          let e = EConstructor (id2, "Just", [EVariable (id4, "request")]) in
          let pats =
            [ ( FPConstructor
                  (id5, id1, "Just", [FPVariable (id6, id1, "anything")])
              , EVariable (id7, "anything") ) ]
          in
          expect (freeVariables (EMatch (id1, e, pats)))
          |> toEqual [(id4, "request")]) ;
      test "getArguments of binop works" (fun () ->
          let arg1 = int 4 in
          let arg2 = str "asf" in
          let e = binop "+" arg1 arg2 in
          let ast = FluidAST.ofExpr e in
          expect (getArguments (E.toID e) ast) |> toEqual [arg1; arg2]) ;
      test "getArguments of fncall works" (fun () ->
          let arg1 = int 4 in
          let arg2 = str "asf" in
          let e = fn "Int::add" [arg1; arg2] in
          let ast = FluidAST.ofExpr e in
          expect (getArguments (E.toID e) ast) |> toEqual [arg1; arg2]) ;
      test "getArguments of pipe works" (fun () ->
          let id = gid () in
          let arg1 = int 4 in
          let arg2 = str "asf" in
          let e = pipe arg1 [fn ~id "Int::add" [pipeTarget; arg2]] in
          let ast = FluidAST.ofExpr e in
          expect (getArguments id ast) |> toEqual [arg1; arg2]) ;
      test "getParamIndex of pipe works" (fun () ->
          let id1 = gid () in
          let id2 = gid () in
          let e =
            pipe
              (str ~id:id1 "asd")
              [fn "String::append" [pipeTarget; EBlank id2]]
          in
          let ast = FluidAST.ofExpr e in
          expect (getParamIndex id1 ast, getParamIndex id2 ast)
          |> toEqual (Some ("String::append", 0), Some ("String::append", 1))) ;
      test
        "variablesIn correctly identifies available vars in let RHS with incomplete LHS"
        (fun () ->
          let testId = ID.fromString "testme" in
          let inner = ELet (gid (), "", EBlank testId, E.newB ()) in
          let outer = ELet (gid (), "variable", int 4, inner) in
          let vars = variablesIn outer |. Map.get ~key:"testme" in
          let varsFor = vars |> Option.map ~f:(fun d -> Map.keys d) in
          expect varsFor |> toEqual (Some ["variable"])) ;
      test
        "variablesIn correctly gets rhs id of latest let definition"
        (fun () ->
          let let1ID = ID.fromString "let1ID" in
          let let2ID = ID.fromString "let2ID" in
          let a1ID = ID.fromString "a1ID" in
          let a1 = int ~id:a1ID 4 in
          let a2ID = ID.fromString "a2ID" in
          let a2 = int ~id:a2ID 9 in
          let lastBlank = EBlank (ID "lastBlankid") in
          let ast = ELet (let1ID, "a", a1, ELet (let2ID, "a", a2, lastBlank)) in
          expect
            ( variablesIn ast
            |> Map.get ~key:"lastBlankid"
            |> Option.andThen ~f:(fun d -> Map.get ~key:"a" d) )
          |> toEqual (Some a2ID)) ;
      test "variablesIn correctly gets the id of a pattern variable" (fun () ->
          let id1 = gid () in
          let targetID = gid () in
          let b1 = blank () in
          let target = EBlank targetID in
          let ast =
            match' b1 [(pConstructor "Just" [pVar ~id:id1 "myvar"], target)]
          in
          expect
            ( variablesIn ast
            |> Map.get ~key:(ID.toString targetID)
            |> Option.andThen ~f:(fun d -> Map.get ~key:"myvar" d) )
          |> toEqual (Some id1)) ;
      ()) ;
  describe "removePartials" (fun () ->
      let b () = EBlank (gid ()) in
      test "No changes when blank" (fun () ->
          let expr = b () in
          expect (removePartials expr) |> toEqual expr) ;
      test "No changes when not-partial" (fun () ->
          let expr =
            EFnCall
              ( gid ()
              , "+"
              , [EInteger (gid (), "3"); EInteger (gid (), "9")]
              , NoRail )
          in
          expect (removePartials expr) |> toEqual expr) ;
      test "Updates AST when there's a partial in fn args" (fun () ->
          let fnid = gid () in
          let argid = gid () in
          let blank = b () in
          let expr =
            EFnCall
              ( fnid
              , "+"
              , [EInteger (argid, "3"); EPartial (gid (), "abc", blank)]
              , NoRail )
          in
          expect (removePartials expr)
          |> toEqual
               (EFnCall (fnid, "+", [EInteger (argid, "3"); blank], NoRail))) ;
      test "Updates AST when there's a fn rename partial" (fun () ->
          let fnid = gid () in
          let b1 = b () in
          let b2 = b () in
          let expr =
            ERightPartial
              (gid (), "Int::a", EFnCall (fnid, "Int::add", [b1; b2], NoRail))
          in
          expect (removePartials expr)
          |> toEqual (EFnCall (fnid, "Int::add", [b1; b2], NoRail))) ;
      test "Updates AST when there is a left partial" (fun () ->
          let str = EString (gid (), "a string") in
          let expr = ELeftPartial (gid (), "if", str) in
          expect (removePartials expr) |> toEqual str) ;
      ())
