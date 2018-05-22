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

id1 = ID 5
id2 = ID 10

pass : TestResult
pass = Ok ()

fail : a -> TestResult
fail v = Err (toString v)

expectOk : TestResult -> Expectation
expectOk r =
  case r of
    Ok () -> Expect.pass
    Err msg -> Expect.fail msg

expectTrue : Bool -> Expectation
expectTrue = Expect.true ""

expectFalse : Bool -> Expectation
expectFalse = Expect.false ""

test : String -> Expectation -> Test.Test
test msg e =
  Test.test msg (\_ -> e)


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
          let replacement = B.newF (FnCall "+" [B.new (), B.new ()])
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
                                  , F (ID 6) (FnCall "+" [Blank (ID 5)])
                                  , B.new ()])
          in
            case AST.closeThread threaded of
              F (ID 6) (FnCall "+" [Blank _, Blank (ID 5)]) -> pass
              r -> fail r

    , test "don't readd the argument if it was already in the right place" <|
      expectOk <|
        let fn = B.newF
                   (FnCall "+"
                     [ B.newF (Value "3")
                     , B.newF (Value "5")])
            open = B.newF
                     (Thread
                       [ fn
                       , B.new ()])
            closed = AST.closeThread open
        in
        if closed == fn
        then pass
        else fail ({open=open, fn=fn, closed=closed})



    , test "simple thread is closed properly" <|
      expectOk <|
        let open = (B.newF (Thread [ B.newF (Value "3"), B.new ()]))
            closed = AST.closeThread open
        in
        case closed of
          F _ (Value "3") -> pass
          _ -> fail (open, closed)

    ]


