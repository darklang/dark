open! Tc
open Types
open Jest
open Expect
open Prelude
module B = Blank

type ('a, 'b) transformation_test_result =
  | Pass
  | Fail of 'a * 'b

let () =
  describe "ast" (fun () ->
      let id1 = ID "5" in
      let id2 = ID "10" in
      test "isThreadBlank for thread" (fun () ->
          expect (AST.isThreadBlank (F (id1, Thread [Blank id2])) id2)
          |> toEqual true ) ;
      test "isThreadBlank for blank" (fun () ->
          expect (AST.isThreadBlank (Blank id1) id1) |> toEqual false ) ;
      test "isThreadBlank for thread non-blank" (fun () ->
          expect (AST.isThreadBlank (F (id1, Thread [F (id2, Value "")])) id2)
          |> toEqual false ) ;
      test "replacing a function in a thread works" (fun () ->
          expect
            (let replacement =
               B.newF (FnCall (B.newF "+", [B.new_ (); B.new_ ()], NoRail))
             in
             let orig = B.new_ () in
             let result =
               AST.replace
                 (PExpr orig)
                 (PExpr replacement)
                 (B.newF (Thread [orig; B.new_ ()]))
             in
             match result with
             | F (_, Thread [r; _]) ->
                 if r = replacement then Pass else Fail (orig, result)
             | _ ->
                 Fail (orig, result))
          |> toEqual Pass ) ;
      test
        "promoting a threaded FnCall by removing the Thread, re-adds the missing argument"
        (fun () ->
          expect
            (let threaded =
               B.newF
                 (Thread
                    [ B.new_ ()
                    ; B.new_ ()
                    ; F
                        ( ID "6"
                        , FnCall
                            (F (ID "6_name", "+"), [Blank (ID "5")], NoRail) )
                    ; B.new_ () ])
             in
             match AST.closeThreads threaded with
             | F
                 ( ID "6"
                 , FnCall (F (_, "+"), [Blank _; Blank (ID "5")], NoRail) ) ->
                 Pass
             | r ->
                 Fail (threaded, r))
          |> toEqual Pass ) ;
      test
        "don't re-add the argument if it was already in the right place"
        (fun () ->
          expect
            (let fn =
               B.newF
                 (FnCall
                    ( B.newF "+"
                    , [B.newF (Value "3"); B.newF (Value "5")]
                    , NoRail ))
             in
             let open_ = B.newF (Thread [fn; B.new_ ()]) in
             let closed = AST.closeThreads open_ in
             if closed = fn then Pass else Fail (fn, closed))
          |> toEqual Pass ) ;
      test "simple thread is closed properly" (fun () ->
          expect
            (let open_ = B.newF (Thread [B.newF (Value "3"); B.new_ ()]) in
             let closed = AST.closeThreads open_ in
             match closed with
             | F (_, Value "3") ->
                 Pass
             | _ ->
                 Fail (open_, closed))
          |> toEqual Pass ) ;
      test "parent of a field is the expr" (fun () ->
          expect
            (let obj = B.newF (Variable "obj") in
             let fieldname = B.newF "field" in
             let expr = B.newF (FieldAccess (obj, fieldname)) in
             let parent = AST.findParentOfWithin (B.toID fieldname) expr in
             if parent = expr then Pass else Fail (parent, expr))
          |> toEqual Pass ) ;
      test "usesRail returns true when at top" (fun () ->
          expect
            (let expr = B.newF (FnCall (B.newF "test", [], Rail)) in
             AST.usesRail expr)
          |> toEqual true ) ;
      test "usesRail returns true when deep" (fun () ->
          expect
            (let withRail = B.newF (FnCall (B.newF "test2", [], Rail)) in
             let l = B.newF (Let (B.newF "v", withRail, B.new_ ())) in
             let expr = B.newF (FnCall (B.newF "test", [l], NoRail)) in
             AST.usesRail expr)
          |> toEqual true ) ;
      test "usesRail returns false when norail" (fun () ->
          expect
            (let deep = B.newF (FnCall (B.newF "test2", [], NoRail)) in
             let l = B.newF (Let (B.newF "v", deep, B.new_ ())) in
             let expr = B.newF (FnCall (B.newF "test", [l], NoRail)) in
             AST.usesRail expr)
          |> toEqual false ) ) ;
  describe "AST.allFnCalls" (fun () ->
      test "should return get one call" (fun () ->
          let ast =
            B.newF
              (If
                 ( B.new_ ()
                 , B.newF (FnCall (B.newF "Int::add_v0", [], NoRail))
                 , B.new_ () ))
          in
          let getCalls = AST.allFnCalls ast in
          expect (List.length getCalls) |> toEqual 1 ) ;
      test "should return empty list" (fun () ->
          let ast = B.newF (Value "hello world") in
          let getCalls = AST.allFnCalls ast in
          expect (List.isEmpty getCalls) |> toEqual true ) ) ;
  describe "AST filter fncalls" (fun () ->
      test "emit only" (fun () ->
          let calls =
            [ (gid (), "Int::add_v0")
            ; (gid (), "emit")
            ; (gid (), "DB::getAll_v2") ]
          in
          let res = AST.filterFnCallsEmitOnly calls in
          expect (match res with [(_, "emit")] -> true | _ -> false)
          |> toEqual true ) ;
      test "db functions only" (fun () ->
          let calls =
            [ (gid (), "Int::add_v0")
            ; (gid (), "emit")
            ; (gid (), "DB::getAll_v2") ]
          in
          let res = AST.filterFnCallsDBOnly calls in
          expect (match res with [(_, "DB::getAll_v2")] -> true | _ -> false)
          |> toEqual true ) ) ;
  describe "AST.findFnCall" (fun () ->
      let targetId = ID "123"
      and fnName = "DB::getAll_v2" in
      let isFnCallInAST ast =
        Option.isSome (AST.findFnCall ast targetId fnName)
      in
      test "FnCall at root" (fun () ->
          let ast = B.newF (FnCall (F (targetId, fnName), [], NoRail)) in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "Found FnCall but it is another function" (fun () ->
          let ast = B.newF (FnCall (B.newF "Int::add_v0", [], NoRail)) in
          expect (isFnCallInAST ast) |> toEqual false ) ;
      test "FnCall inside the arguments of a Blank named FnCall" (fun () ->
          let ast =
            B.newF
              (FnCall
                 ( B.new_ ()
                 , [ B.newF (Value "1")
                   ; B.newF (FnCall (F (targetId, fnName), [], NoRail)) ]
                 , NoRail ))
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "FnCall inside let definition" (fun () ->
          let ast =
            B.newF
              (Let
                 ( B.new_ ()
                 , B.newF (FnCall (F (targetId, fnName), [], NoRail))
                 , B.new_ () ))
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "FnCall inside let body" (fun () ->
          let ast =
            B.newF
              (Let
                 ( B.new_ ()
                 , B.new_ ()
                 , B.newF (FnCall (F (targetId, fnName), [], NoRail)) ))
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "Body has only a variable, FnCall not found" (fun () ->
          let ast = B.newF (Variable "request") in
          expect (isFnCallInAST ast) |> toEqual false ) ;
      test "FnCall inside lambda" (fun () ->
          let ast =
            B.newF
              (Lambda ([], B.newF (FnCall (F (targetId, fnName), [], NoRail))))
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "FnCall inside obj value" (fun () ->
          let ast =
            B.newF
              (ObjectLiteral
                 [ ( B.newF "books"
                   , B.newF (FnCall (F (targetId, fnName), [], NoRail)) ) ])
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "FnCall inside list item" (fun () ->
          let ast =
            B.newF
              (ListLiteral
                 [B.new_ (); B.newF (FnCall (F (targetId, fnName), [], NoRail))])
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "FnCall inside Thread" (fun () ->
          let ast =
            B.newF
              (Thread [B.newF (FnCall (F (targetId, fnName), [], NoRail))])
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "FnCall inside feature flag condition" (fun () ->
          let ast =
            B.newF
              (FeatureFlag
                 ( B.new_ ()
                 , B.newF (FnCall (F (targetId, fnName), [], NoRail))
                 , B.new_ ()
                 , B.new_ () ))
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "FnCall inside feature flag case B" (fun () ->
          let ast =
            B.newF
              (FeatureFlag
                 ( B.new_ ()
                 , B.new_ ()
                 , B.new_ ()
                 , B.newF (FnCall (F (targetId, fnName), [], NoRail)) ))
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "FnCall inside match expression" (fun () ->
          let ast =
            B.newF
              (Match (B.newF (FnCall (F (targetId, fnName), [], NoRail)), []))
          in
          expect (isFnCallInAST ast) |> toEqual true ) ;
      test "FnCall inside a pattern case" (fun () ->
          let ast =
            B.newF
              (Match
                 ( B.new_ ()
                 , [ ( B.newF (PLiteral "users")
                     , B.newF (FnCall (F (targetId, fnName), [], NoRail)) ) ]
                 ))
          in
          expect (isFnCallInAST ast) |> toEqual true ) ) ;
  describe "Get referenced names" (fun () ->
      test "AST.asDBNames returns db name" (fun () ->
          let refs =
            AST.asDBNames
              [ FnCall
                  (B.newF "DB::getAll_v2", [B.newF (Variable "Books")], NoRail)
              ]
          in
          expect (match refs with [RDBName "Books"] -> true | _ -> false)
          |> toEqual true ) ;
      test "AST.asDBNames db fn call with no args return empty list" (fun () ->
          let refs =
            AST.asDBNames [FnCall (B.newF "DB::getAll_v2", [], NoRail)]
          in
          expect (List.isEmpty refs) |> toEqual true ) ;
      test "AST.asEmitNames returns event space and name" (fun () ->
          let refs =
            AST.asEmitNames
              [ FnCall
                  ( B.newF "emit"
                  , [ B.new_ ()
                    ; B.newF (Value "JOB")
                    ; B.newF (Value "processOrder") ]
                  , NoRail ) ]
          in
          expect
            ( match refs with
            | [REmit ("JOB", "processOrder")] ->
                true
            | _ ->
                false )
          |> toEqual true ) ;
      test
        "AST.asEmitNames emit call with no args return empty list"
        (fun () ->
          let refs = AST.asEmitNames [FnCall (B.newF "emit", [], NoRail)] in
          expect (List.isEmpty refs) |> toEqual true ) ) ;
  ()
