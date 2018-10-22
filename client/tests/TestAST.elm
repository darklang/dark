module TestAST exposing (all)

-- tests
import Test exposing (describe)
import Expect exposing (Expectation)

-- builtins

-- libs

-- dark
import Types exposing (..)
import AST
import Blank as B
import UtilsForTests exposing (..)

id1 = ID 5
id2 = ID 10


all : Test.Test
all =
  Test.describe "ast"
    [ test "isThreadBlank for thread" <|
        expectTrue
          (AST.isThreadBlank
            (F id1 (Thread [Blank id2]))
            id2)

    , test "isThreadBlank for blank" <|
        expectFalse
          (AST.isThreadBlank
            (Blank id1)
            id1)

    , test "isThreadBlank for thread non-blank" <|
        expectFalse
          (AST.isThreadBlank
            (F id1 (Thread [F id2 (Value "")]))
            id2)

    , test "replacing a function in a thread works" <|
        expectOk <|
          let replacement = B.newF (FnCall "+" [B.new (), B.new ()] NoRail)
              orig = B.new ()
              result =
                AST.replace
                 (PExpr orig)
                 (PExpr replacement)
                 (B.newF (Thread [orig, B.new ()]))
          in
            case result of
              F _ (Thread [r, _]) ->
                if r == replacement
                then pass
                else fail (orig, result)
              _ -> fail (orig, result)

    , test "promoting a threaded FnCall by removing the Thread, re-adds the missing argument" <|
        expectOk <|
          let threaded = B.newF (
                           Thread [ B.new ()
                                  , B.new ()
                                  , F (ID 6) (FnCall "+" [Blank (ID 5)] NoRail)
                                  , B.new ()])
          in
            case AST.closeThreads threaded of
              F (ID 6) (FnCall "+" [Blank _, Blank (ID 5)] NoRail) -> pass
              r -> fail r

    , test "don't readd the argument if it was already in the right place" <|
      expectOk <|
        let fn = B.newF
                   (FnCall "+"
                     [ B.newF (Value "3")
                     , B.newF (Value "5")]
                     NoRail)
            open = B.newF
                     (Thread
                       [ fn
                       , B.new ()])
            closed = AST.closeThreads open
        in
        if closed == fn
        then pass
        else fail ({open=open, fn=fn, closed=closed})



    , test "simple thread is closed properly" <|
      expectOk <|
        let open = (B.newF (Thread [ B.newF (Value "3"), B.new ()]))
            closed = AST.closeThreads open
        in
        case closed of
          F _ (Value "3") -> pass
          _ -> fail (open, closed)

    , test "parent of a field is the expr" <|
      expectOk <|
        let obj = B.newF (Variable "obj")
            fieldname = B.newF "field"
            expr = B.newF (FieldAccess obj fieldname)
            parent = AST.parentOf (B.toID fieldname) expr
        in
            if parent == expr
            then pass
            else fail (parent, expr)
    , test "usesRail returns true when at top" <|
      expectTrue <|
        let expr = B.newF (FnCall "test" [] Rail) in
        AST.usesRail expr
    , test "usesRail returns true when deep" <|
      expectTrue <|
        let withRail = B.newF (FnCall "test2" [] Rail)
            l = B.newF (Let (B.newF "v") withRail (B.new ()))
            expr = B.newF (FnCall "test" [l] NoRail) in
        AST.usesRail expr
    , test "usesRail returns false when norail " <|
      expectFalse <|
        let deep = B.newF (FnCall "test2" [] NoRail)
            l = B.newF (Let (B.newF "v") deep (B.new ()))
            expr = B.newF (FnCall "test" [l] NoRail) in
        AST.usesRail expr

    ]
