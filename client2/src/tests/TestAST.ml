open Tea
open! Porting
module B = Blank
open Types
open UtilsForTests

let id1 = ID 5

let id2 = ID 10

let all : Test.test =
  Test.describe "ast"
    [ test "isThreadBlank for thread"
      <| expectTrue (AST.isThreadBlank (F (id1, Thread [Blank id2])) id2)
    ; test "isThreadBlank for blank"
      <| expectFalse (AST.isThreadBlank (Blank id1) id1)
    ; test "isThreadBlank for thread non-blank"
      <| expectFalse
           (AST.isThreadBlank (F (id1, Thread [F (id2, Value "")])) id2)
    ; test "replacing a function in a thread works"
      <| ( expectOk
         <|
         let replacement =
           B.newF (FnCall ("+", [B.new_ (); B.new_ ()], NoRail))
         in
         let orig = B.new_ () in
         let result =
           AST.replace (PExpr orig) (PExpr replacement)
             (B.newF (Thread [orig; B.new_ ()]))
         in
         match result with
         | F (_, Thread [r; _]) ->
             if r = replacement then pass else fail (orig, result)
         | _ -> fail (orig, result) )
    ; test
        "promoting a threaded FnCall by removing the Thread, re-adds the \
         missing argument"
      <| ( expectOk
         <|
         let threaded =
           B.newF
             (Thread
                [ B.new_ ()
                ; B.new_ ()
                ; F (ID 6, FnCall ("+", [Blank (ID 5)], NoRail))
                ; B.new_ () ])
         in
         match AST.closeThreads threaded with
         | F (ID 6, FnCall ("+", [Blank _; Blank (ID 5)], NoRail)) -> pass
         | r -> fail r )
    ; test "don't readd the argument if it was already in the right place"
      <| ( expectOk
         <|
         let fn =
           B.newF
             (FnCall ("+", [B.newF (Value "3"); B.newF (Value "5")], NoRail))
         in
         let open_ = B.newF (Thread [fn; B.new_ ()]) in
         let closed = AST.closeThreads open_ in
         if closed = fn then pass else fail {open_; fn; closed} )
    ; test "simple thread is closed properly"
      <| ( expectOk
         <|
         let open_ = B.newF (Thread [B.newF (Value "3"); B.new_ ()]) in
         let closed = AST.closeThreads open_ in
         match closed with
         | F (_, Value "3") -> pass
         | _ -> fail (open_, closed) )
    ; test "parent of a field is the expr"
      <| ( expectOk
         <|
         let obj = B.newF (Variable "obj") in
         let fieldname = B.newF "field" in
         let expr = B.newF (FieldAccess (obj, fieldname)) in
         let parent = AST.parentOf (B.toID fieldname) expr in
         if parent = expr then pass else fail (parent, expr) )
    ; test "usesRail returns true when at top"
      <| ( expectTrue
         <|
         let expr = B.newF (FnCall ("test", [], Rail)) in
         AST.usesRail expr )
    ; test "usesRail returns true when deep"
      <| ( expectTrue
         <|
         let withRail = B.newF (FnCall ("test2", [], Rail)) in
         let l = B.newF (Let (B.newF "v", withRail, B.new_ ())) in
         let expr = B.newF (FnCall ("test", [l], NoRail)) in
         AST.usesRail expr )
    ; test "usesRail returns false when norail "
      <| ( expectFalse
         <|
         let deep = B.newF (FnCall ("test2", [], NoRail)) in
         let l = B.newF (Let (B.newF "v", deep, B.new_ ())) in
         let expr = B.newF (FnCall ("test", [l], NoRail)) in
         AST.usesRail expr ) ]
