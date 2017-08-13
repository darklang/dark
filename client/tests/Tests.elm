module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Autocomplete exposing (..)

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
  let completes = [ "Twit::somefunc"
                  , "Twit::someOtherFunc"
                  , "Twit::yetAnother"
                  , "+"
                  , "Int::add"
                  , "Dict::keys"
                  , "List::head"
                  ] in
  describe "autocomplete"
    [ d "containsOrdered"
        [ \_ -> containsOrdered "abc" "aaaaabbbbbcccc"
        , \_ -> containsOrdered "abc" "xxxaaxcxbbxaxc"
        , \_ -> containsOrdered "Twitt" "Twitter::users/lookup"
        , \_ -> not (containsOrdered "abc" "xxxaaxcxbbxxxx")
       ]
    , d "sharedPrefix"
      [ \_ -> sharedPrefix ["aaaab", "aab", "aaxb"] == "aa"
      , \_ -> sharedPrefix ["abcdd", "abcdde"] == "abcdd"
      , \_ -> sharedPrefix ["abcdd", "bcddee"] == ""
      ]
    , d "joinPrefix"
      [ \_ -> joinPrefix "tWiTt" "Twitter::" == "tWiTter::"
      ]
    , d "query"
      -- Lowercase search still finds uppercase results
      [ \_ -> (init completes)
      |> query "lis"
      |> .current
      |> (==) ["List::head"]
      -- Search finds multiple prefixes
      , \_ -> (init completes)
      |> query "twit::"
      |> .current
      |> (==) ["Twit::somefunc", "Twit::someOtherFunc", "Twit::yetAnother"]
      -- Search finds only prefixed
      , \_ ->(init completes)
      |> query "twit::y"
      |> .current
      |> (==) ["Twit::yetAnother"]
      -- Search only finds from the start
      , \_ -> (init completes)
      |> query "Another"
      |> .current
      |> (==) []
      -- No results when the only option is the query
      , \_ -> (init completes)
      |> query "List::head"
      |> .current
      |> (==) []
      -- Scrolling down a bit works
      , \_ -> (init completes)
      |> query "Twit"
      |> selectDown
      |> selectDown
      |> .index
      |> (==) 2
      -- Scrolling loops one way
      , \_ -> (init completes)
      |> query "Twit"
      |> selectDown
      |> selectDown
      |> selectDown
      |> .index
      |> (==) 0
      -- Scrolling loops the other way
      , \_ -> (init completes)
      |> query "Twit"
      |> selectDown
      |> selectUp
      |> selectUp
      |> .index
      |> (==) 2
       -- Reduced results goes back to start of list
      , \_ -> (init completes)
      |> query "Twit"
      |> selectDown
      |> selectDown
      |> query "Twit::y"
      |> .index
      |> (==) 0
      -- Don't show cursor when the list is empty
      , \_ -> (init completes)
      |> query "Twit"
      |> selectDown
      |> selectDown
      |> query "Twitxxx"
      |> .index
      |> (==) -1
      ]
    ]
