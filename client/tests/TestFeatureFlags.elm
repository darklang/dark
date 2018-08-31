module TestFeatureFlags exposing (..)

-- tests
import Test exposing (..)
import Expect exposing (Expectation, pass, fail)

-- dark
import Blank as B
import Types exposing (..)
import Defaults exposing (defaultModel)
import FeatureFlags

--testFromFlagged : Test
--testFromFlagged =
--    let a = B.newF (Value "case A")
--        b = B.newF (Value "case B") 
--        t pick expr = test "start feature flag" (\_ -> Expect.pass)
--    in describe "testFromFlagged"
--    [ t True True
--    ]