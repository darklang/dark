module TestAutocomplete exposing (all)

-- tests
import Test exposing (..)
import Expect exposing (Expectation)

-- dark
import Autocomplete exposing (..)
import Types exposing (..)
import Defaults


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
                    , previewExecutionSafe = False
                    , description = ""
                    , infix = True
                    })
          [ ("Twit::somefunc", TObj)
          , ("Twit::someOtherFunc", TObj)
          , ("Twit::yetAnother", TObj)
          , ("+", TInt)
          , ("Int::add", TInt)
          , ("Dict::keys", TObj)
          , ("List::head", TList)
          , ("withlower", TObj)
          , ("withLower", TObj)
          , ("SomeModule::withLower", TObj)
          , ("SomeOtherModule::withlower", TObj)
          , ("HTTP::post", TAny)
          , ("HTTP::head", TAny)
          , ("HTTP::get", TAny)
          , ("HTTP::options", TAny)
          ]
      m = Defaults.defaultModel
      create () = init completes |> regenerate m
  in
  describe "autocomplete"
    [ d "sharedPrefix"
      [ \_ -> sharedPrefixList ["aaaab", "aab", "aaxb"] == "aa"
      , \_ -> sharedPrefixList ["abcdd", "abcdde"] == "abcdd"
      , \_ -> sharedPrefixList ["abcdd", "bcddee"] == ""
      ]
    , d "query" -- numbered from 0
      -- Empty autocomplete doesn't highlight
      [ \_ -> (create ())
      |> .index
      |> (==) -1

      -- Press a letter from the selected entry keeps the entry selected
      , \_ -> create ()
      |> selectDown
      |> selectDown
      |> setQuery "T"
      |> highlighted
      |> Maybe.map asName
      |> (==) (Just "Twit::someOtherFunc")

      -- Returning to empty unselects
      , \_ -> create ()
      |> setQuery "lis"
      |> setQuery ""
      |> highlighted
      |> (==) Nothing

      , \_ -> create ()
      |> setQuery "Twit::somefunc"
      |> setQuery "Twit::some"
      |> selectDown
      |> highlighted
      |> Maybe.map asName
      |> (==) (Just "Twit::someOtherFunc")

      -- Lowercase search still finds uppercase results
      , \_ -> create ()
      |> update m (ACSetQuery "lis")
      |> .completions
      |> List.concat
      |> List.map asName
      |> (==) ["List::head"]

      -- Search finds multiple prefixes
      , \_ -> create ()
      |> setQuery "twit::"
      |> .completions
      |> List.concat
      |> List.filter isStaticItem
      |> List.map asName
      |> (==) ["Twit::somefunc", "Twit::someOtherFunc", "Twit::yetAnother"]

      -- Search finds only prefixed
      , \_ -> create ()
      |> setQuery "twit::y"
      |> .completions
      |> List.concat
      |> List.filter isStaticItem
      |> List.map asName
      |> (==) ["Twit::yetAnother"]

      -- Search anywhere
      , \_ -> create ()
      |> setQuery "Another"
      |> .completions
      |> List.concat
      |> List.filter isStaticItem
      |> List.map asName
      |> (==) ["Twit::yetAnother"]

      -- Show results when the only option is the setQuery
      , \_ -> create ()
      |> setQuery "List::head"
      |> .completions
      |> List.concat
      |> List.filter isStaticItem
      |> List.map asName
      |> List.length
      |> (==) 1

      -- Scrolling down a bit works
      , \_ -> create ()
      |> setQuery "Twit"
      |> selectDown
      |> selectDown
      |> .index
      |> (==) 2

      -- Scrolling loops one way
      , \_ -> create ()
      |> setQuery "Twit"
      |> selectDown
      |> selectDown
      |> selectDown
      |> selectDown
      |> .index
      |> (==) 0

      -- Scrolling loops the other way
      , \_ -> create ()
      |> setQuery "Twit"
      |> selectDown
      |> selectUp
      |> selectUp
      |> selectUp
      |> .index
      |> (==) 2

      -- Scrolling loops the other way without going forward first
      , \_ -> create ()
      |> setQuery "Twit"
      |> selectUp
      |> selectUp
      |> selectUp
      |> .index
      |> (==) 1

      -- Scrolling backward works if we haven't searched yet
      , \_ -> create ()
      |> selectUp
      |> selectUp
      |> .index
      |> (==) 13

      -- Don't highlight when the list is empty
      , \_ -> create ()
      |> setQuery "Twit"
      |> selectDown
      |> selectDown
      |> setQuery "Twit::1334xxx"
      |> .index
      |> (==) -1

      -- Filter by method signature for typed values
      -- , \_ -> create ()
      -- |> forLiveValue {value="[]", tipe=TList,json="[]", exc=Nothing}
      -- |> setQuery ""
      -- |> .completions
      -- |> List.map asName
      -- |> Set.fromList
      -- |> (==) (Set.fromList ["List::head"])

      -- Show allowed fields for objects
      -- , \_ -> create ()
      -- |> forLiveValue {value="5", tipe=TInt, json="5", exc=Nothing}
      -- |> setQuery ""
      -- |> .completions
      -- |> List.map asName
      -- |> Set.fromList
      -- |> (==) (Set.fromList ["Int::add", "+"])

      -- By default the list shows results
      , \_ -> create ()
      |> setQuery ""
      |> .completions
      |> List.concat
      |> List.length
      |> (/=) 0

      -- ordering: startsWith, then case match, then case insensitive match
      , \_ -> create ()
      |> setQuery "withL"
      |> .completions
      |> List.concat
      |> List.filter isStaticItem
      |> List.map asName
      |> (==) ["withLower"
              , "withlower"
              ,"SomeModule::withLower"
              ,"SomeOtherModule::withlower"]

      -- typing literals works
      , \_ -> create ()
      |> setQuery "21434234"
      |> selectDown
      |> highlighted
      |> Maybe.map asName
      |> (==) (Just "21434234")

      -- typing db names works
      , \_ -> create ()
      |> setQuery "mydbname"
      |> selectDown
      |> highlighted
      |> (==) (Just (ACOmniAction (NewDB "mydbname")))

      -- db names can be multicase
      , \_ -> create ()
      |> setQuery "MyDBnaMe"
      |> selectDown
      |> highlighted
      |> (==) (Just (ACOmniAction (NewDB "MyDBnaMe")))

      -- alphabetical only #1
      , \_ -> create ()
      |> setQuery "dbname1234::"
      |> selectDown
      |> highlighted
      |> (==) Nothing

      -- alphabetical only #2
      , \_ -> create ()
      |> setQuery "db_name::"
      |> selectDown
      |> highlighted
      |> (==) Nothing

      -- No HTTP handler in general
      , \_ -> create ()
      |> setQuery "asdkkasd"
      |> .completions
      |> List.concat
      |> List.member (ACOmniAction NewHTTPSpace)
      |> (==) False

      -- HTTP handler
      , \_ -> create ()
      |> setQuery "HTT"
      |> highlighted
      |> (==) (Just (ACOmniAction NewHTTPSpace))

      -- Adding a dynamic item doesnt mess with the previous selection
      , \_ ->
        let old = create ()
                  |> setQuery "HTTP:"
                  |> selectDown
                  |> selectDown
                  |> selectDown
                  |> selectDown
        in
        old
        |> setQuery "HTTP"
        |> highlighted
        |> (==) (highlighted old)

      , \_ -> create ()
      |> setQuery "/"
      |> highlighted
      |> (==) (Just (ACOmniAction (NewHTTPRoute "/")))

      , \_ -> create ()
      |> setQuery "/asasdasd"
      |> highlighted
      |> (==) (Just (ACOmniAction (NewHTTPRoute "/asasdasd")))

      , \_ -> create ()
      -- A specific bug where + is interpreted as an ACLiteral
      |> setQuery "+"
      |> highlighted
      |> Maybe.map asName
      |> (==) (Just "+")

      -- A few different kinds of literals
      , \_ -> create ()
      |> setQuery "nu"
      |> highlighted
      |> (==) (Just (ACLiteral "null"))

      , \_ -> create ()
      |> setQuery "tr"
      |> highlighted
      |> (==) (Just (ACLiteral "true"))

      , \_ -> create ()
      |> setQuery "tR" -- case insensitive
      |> highlighted
      |> (==) (Just (ACLiteral "true"))

      , \_ -> create ()
      |> setQuery "false"
      |> highlighted
      |> (==) (Just (ACLiteral "false"))

      , \_ -> create ()
      |> setQuery "3.452"
      |> highlighted
      |> (==) (Just (ACLiteral "3.452"))


      ]
    ]


