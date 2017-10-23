module TestAutocomplete exposing (all)

-- tests
import ElmTest.Extra exposing (Test, describe, test, skip, todo)
import Expect exposing (Expectation)

-- builtins
import Set

-- libs

-- dark
import Autocomplete exposing (..)
import Types exposing (..)


d : String -> List (() -> Bool) -> Test
d s fs = describe s (List.indexedMap
                       (\i f ->
                          test
                          ("test " ++ (toString i))
                          (\_ -> Expect.true "" (f ())))
                       fs
                    )


all : Test
all =
  let completes =
        List.map (\(name,tipe) ->
                    { name = name
                    , parameters = [{ name = "x"
                                    , tipe = tipe
                                    , block_args = []
                                    , optional = False
                                    , description = ""
                                    }]
                    , returnTipe = TBool
                    , description = ""
                    })
          [ ("Twit::somefunc", TObj)
          , ("Twit::someOtherFunc", TObj)
          , ("Twit::yetAnother", TObj)
          , ("+", TInt)
          , ("Int::add", TInt)
          , ("Dict::keys", TObj)
          , ("List::head", TList)
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
    , d "query"
      -- Lowercase search still finds uppercase results
      [ \_ -> (init completes)
      |> query "lis"
      |> .completions
      |> List.map asName
      |> (==) ["List::head"]
      -- Search finds multiple prefixes
      , \_ -> (init completes)
      |> query "twit::"
      |> .completions
      |> List.map asName
      |> (==) ["Twit::somefunc", "Twit::someOtherFunc", "Twit::yetAnother"]
      -- Search finds only prefixed
      , \_ ->(init completes)
      |> query "twit::y"
      |> .completions
      |> List.map asName
      |> (==) ["Twit::yetAnother"]
      -- Search anywhere
      , \_ -> (init completes)
      |> query "Another"
      |> .completions
      |> List.map asName
      |> (==) ["Twit::yetAnother"]
      -- Show results when the only option is the query
      , \_ -> (init completes)
      |> query "List::head"
      |> .completions
      |> List.map asName
      |> List.length
      |> (==) 1
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
      |> forLiveValue {value="[]", tipe=TList,json="[]", exc=Nothing}
      |> query ""
      |> .completions
      |> List.map asName
      |> Set.fromList
      |> (==) (Set.fromList ["List::head"])
       -- Show allowed fields for objects
      , \_ -> (init completes)
      |> forLiveValue {value="5", tipe=TInt, json="5", exc=Nothing}
      |> query ""
      |> .completions
      |> List.map asName
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

