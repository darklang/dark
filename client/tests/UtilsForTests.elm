module UtilsForTests exposing (..)

-- tests
import Test exposing (describe)
import Expect exposing (Expectation)

-- builtins

-- libs

-- dark
import Types exposing (..)
import AST
import Blank as B


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


