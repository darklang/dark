module TestUtil exposing (..)

-- tests
import Test exposing (..)
import Expect exposing (Expectation, pass, fail)

-- builtins

-- libs

-- dark
import Prelude exposing (..)
import Util

listContainsOrdered : Test
listContainsOrdered =
  let t l r e =
   test (toString l ++ " <=> " ++ toString r)
        (\_ -> if Util.listContainsOrdered l r == e
               then Expect.pass
               else Expect.equal l r
                    |> Expect.onFail (if e
                                      then "was False, expected True"
                                      else "was True, expected False"))
  in
    describe "listContainsOrdered"
    [ t [1,2,4] [4,5,6,7,1,4,2,5,4] True
    , t [] [4,5,6] True
    , t [] [] True
    , t [1,2,4] [4,5,6,7,1,4,2,5,6] False
    , t [1,2,4] [] False
    , t [1,2,4] [1,2] False
    , t [1,2,4] [1,4,2] False
    , t [1,2,5] [1,4,2] False
    ]

stringContainsOrdered : Test
stringContainsOrdered =
  let t l r e =
   test (toString l ++ " <=> " ++ toString r)
        (\_ -> if Util.stringContainsOrdered l r == e
               then Expect.pass
               else Expect.equal l r
                    |> Expect.onFail (if e
                                      then "was False, expected True"
                                      else "was True, expected False"))
  in
  describe "stringContainsOrdered"
      [ t "abc" "aaaaabbbbbcccc" True
      , t "abc" "xxxaaxcxbbxaxc" True
      , t "Twitt" "Twitter::users/lookup" True
      , t "abc" "xxxaaxcxbbxxxx" False
      ]


uniqueCombinations : Test
uniqueCombinations =
    let t l r = test (toString l ++ " <=> " ++ toString r)
                      (\_ -> if ((Util.uniqueCombinations l) == r)
                             then Expect.pass
                             else Expect.fail ("Expected: " ++ (toString r) ++ ", got: " ++ (l |> Util.uniqueCombinations |> toString)))
    in  describe "uniqueCombinations"
        [ t [1,2,3]   [(1,2), (1,3), (2,3)]
        , t [1,2,3,4] [(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)]
        , t [] []
        , t [1] []
        , t [1,2] [(1,2)]
        , t ["foo", "bar"] [("foo", "bar")]
        ]


