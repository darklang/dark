module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Autocomplete exposing (containsOrdered)

d : String -> List (() -> Bool) -> Test
d s fs = describe s (List.indexedMap
                       (\i f ->
                          test
                          ("test " ++ (toString i))
                          (\_ -> Expect.true "" (f ())))
                       fs
                    )

suite : Test
suite =
  d "containsOrdered works"
    [ \_ -> containsOrdered "abc" "aaaaabbbbbcccc"
    , \_ -> containsOrdered "abc" "xxxaaxcxbbxaxc"
    , \_ -> containsOrdered "Twitt" "Twitter::users/lookup"
    , \_ -> not (containsOrdered "abc" "xxxaaxcxbbxxxx")
    ]
