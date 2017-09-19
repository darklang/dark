module Tests exposing (..)

-- tests
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

-- builtins
import Set

-- dark
import Types exposing (..)
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
  let completes =
        List.map (\(name,tipe) ->
                    { name = name
                    , parameters = [{ name = "x"
                                    , tipe = tipe
                                    , arity = 0
                                    , optional = False
                                    , description = ""
                                    }]
                    , return_type = "Boolean"
                    , description = ""
                    })
          [ ("Twit::somefunc", "Object")
          , ("Twit::someOtherFunc", "Object")
          , ("Twit::yetAnother", "Object")
          , ("+", "Integer")
          , ("Int::add", "Integer")
          , ("Dict::keys", "Object")
          , ("List::head", "List")
          ] in
  describe "autocomplete"
    [ d "containsOrdered"
        [ \_ -> containsOrdered "abc" "aaaaabbbbbcccc"
        , \_ -> containsOrdered "abc" "xxxaaxcxbbxaxc"
        , \_ -> containsOrdered "Twitt" "Twitter::users/lookup"
        , \_ -> not (containsOrdered "abc" "xxxaaxcxbbxxxx")
       ]
    , d "sharedPrefix"
      [ \_ -> sharedPrefixList ["aaaab", "aab", "aaxb"] == "aa"
      , \_ -> sharedPrefixList ["abcdd", "abcdde"] == "abcdd"
      , \_ -> sharedPrefixList ["abcdd", "bcddee"] == ""
      ]
    , d "joinPrefix"
      [ \_ -> joinPrefix "tWiTt" "Twitter::" == "tWiTter::"
      ]
    , d "query"
      -- Lowercase search still finds uppercase results
      [ \_ -> (init completes)
      |> query "lis"
      |> .completions
      |> List.map asString
      |> (==) ["List::head"]
      -- Search finds multiple prefixes
      , \_ -> (init completes)
      |> query "twit::"
      |> .completions
      |> List.map asString
      |> (==) ["Twit::somefunc", "Twit::someOtherFunc", "Twit::yetAnother"]
      -- Search finds only prefixed
      , \_ ->(init completes)
      |> query "twit::y"
      |> .completions
      |> List.map asString
      |> (==) ["Twit::yetAnother"]
      -- Search only finds from the start
      , \_ -> (init completes)
      |> query "Another"
      |> .completions
      |> List.map asString
      |> (==) []
      -- No results when the only option is the query
      , \_ -> (init completes)
      |> query "List::head"
      |> .completions
      |> List.map asString
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
       -- Scrolling loops the other way without going forward first
      , \_ -> (init completes)
      |> query "Twit"
      |> selectUp
      |> selectUp
      |> .index
      |> (==) 1
       -- Scrolling backward words if we haven't searched yet
      , \_ -> (init completes)
      |> selectUp
      |> selectUp
      |> .index
      |> (==) 5
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
      -- Filter by method signature for typed values
      , \_ -> (init completes)
      |> forLiveValue ("[]", "List", "[]")
      |> query ""
      |> .completions
      |> List.map asString
      |> Set.fromList
      |> (==) (Set.fromList ["List::head"])
       -- Show allowed fields for objects
      , \_ -> (init completes)
      |> forLiveValue ("5", "Integer", "5")
      |> query ""
      |> .completions
      |> List.map asString
      |> Set.fromList
      |> (==) (Set.fromList ["Int::add", "+"])
      -- By default the list shows results
      , \_ -> (init completes)
      |> query ""
      |> .completions
      |> List.length
      |> (/=) 0
      -- But not when we tell it not to
      , \_ -> (init completes)
      |> open False
      |> query ""
      |> .completions
      |> List.length
      |> (==) 0
      ]
    ]
