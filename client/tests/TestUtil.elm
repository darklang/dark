module TestUtil exposing (..)

-- tests
import ElmTest.Extra exposing (Test, describe, test, skip, todo)
import Expect exposing (Expectation, pass, fail)

-- builtins

-- libs

-- dark
import Util exposing (deMaybe)

all : Test
all =
  describe "util"
  [containsOrdered]

containsOrdered : Test
containsOrdered =
  let t l r e =
   test (toString l ++ " <=> " ++ toString r)
        (\_ -> if Util.containsOrdered l r == e
               then Expect.pass
               else Expect.equal l r
                    |> Expect.onFail (if e then "was False, expected True" else "was True, expected False"))
  in
    describe "containsOrdered"
    [ t [1,2,4] [4,5,6,7,1,4,2,5,4] True
    , t [] [4,5,6] True
    , t [] [] True
    , t [1,2,4] [4,5,6,7,1,4,2,5,6] False
    , t [1,2,4] [] False
    , t [1,2,4] [1,2] False
    , t [1,2,4] [1,4,2] False
    , t [1,2,5] [1,4,2] False
    ]

