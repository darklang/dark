open! Tc
open Types
open Jest
open Expect
open AST
module B = Blank

type ('a, 'b) transformation_test_result =
  | Pass
  | Fail of 'a * 'b

let () =
  describe "ast" (fun () ->
      let id1 = ID "5" in
      let id2 = ID "10" in
      test "isThreadBlank for thread" (fun () ->
          expect (isThreadBlank (F (id1, Thread [Blank id2])) id2)
          |> toEqual true ) ;
      test "isThreadBlank for blank" (fun () ->
          expect (isThreadBlank (Blank id1) id1) |> toEqual false ) ;
      test "isThreadBlank for thread non-blank" (fun () ->
          expect (isThreadBlank (F (id1, Thread [F (id2, Value "")])) id2)
          |> toEqual false ) ;
      test "replacing a function in a thread works" (fun () ->
          expect
            (let replacement =
               B.newF (FnCall (B.newF "+", [B.new_ (); B.new_ ()], NoRail))
             in
             let orig = B.new_ () in
             let result =
               replace
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
             match closeThreads threaded with
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
             let closed = closeThreads open_ in
             if closed = fn then Pass else Fail (fn, closed))
          |> toEqual Pass ) ;
      test "simple thread is closed properly" (fun () ->
          expect
            (let open_ = B.newF (Thread [B.newF (Value "3"); B.new_ ()]) in
             let closed = closeThreads open_ in
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
             let parent = findParentOfWithin (B.toID fieldname) expr in
             if parent = expr then Pass else Fail (parent, expr))
          |> toEqual Pass ) ;
      test "usesRail returns true when at top" (fun () ->
          expect
            (let expr = B.newF (FnCall (B.newF "test", [], Rail)) in
             usesRail expr)
          |> toEqual true ) ;
      test "usesRail returns true when deep" (fun () ->
          expect
            (let withRail = B.newF (FnCall (B.newF "test2", [], Rail)) in
             let l = B.newF (Let (B.newF "v", withRail, B.new_ ())) in
             let expr = B.newF (FnCall (B.newF "test", [l], NoRail)) in
             usesRail expr)
          |> toEqual true ) ;
      test "usesRail returns false when norail" (fun () ->
          expect
            (let deep = B.newF (FnCall (B.newF "test2", [], NoRail)) in
             let l = B.newF (Let (B.newF "v", deep, B.new_ ())) in
             let expr = B.newF (FnCall (B.newF "test", [l], NoRail)) in
             usesRail expr)
          |> toEqual false ) ;
      () ) ;
  ()
