module Tests exposing (..)

-- tests
import Expect exposing (Expectation)
import Test exposing (..)

-- builtins
import Set

-- dark
import Autocomplete exposing (..)
import Types exposing (..)
import Entry as E
import Util



d : String -> List (() -> Bool) -> Test
d s fs = describe s (List.indexedMap
                       (\i f ->
                          test
                          ("test " ++ (toString i))
                          (\_ -> Expect.true "" (f ())))
                       fs
                    )


autocomplete : Test
autocomplete =
  let completes =
        List.map (\(name,tipe) ->
                    { name = name
                    , parameters = [{ name = "x"
                                    , tipe = tipe
                                    , anon_args = []
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

entryParser : Test
entryParser =
                  -- basics fns
  let parses = [ "+ 3 4"
               , "+"
               , "+ 3"
               , "aFunction"
               , "AfunctionWith::sInIt 5 2"
               -- strings
               , "\"a string\""
               , "     \"spaces front\""
               , "\"spaces back\"        "
               , "\"escaped \\\" quote\""
               -- numbers
               , "6.42"
               , "-6"
               , "- -6 -7.53"
               -- vars 
               , "$a"
               , "- $a $c"
               -- bools
               , "true"
               , "    false    "
               , " FaLsE "
               -- infix
               , "$c + $a"
               , "$c - $a"
               -- infix with functions that aren't obvious infix things
               , "$c String::foreach"
               , "$c Int::add 5"
               , "(String::first $c) String::is_substring (String.concat \"a\" \"b\")"
               -- parens
               , "($c % 3) == 0"
               , "($c % 3) == ($a * 1)"
               , "(5) == (0)"
               -- Lists
               , "[ 1, 2, 3, $c - 5, $c ]"
               -- Objs
               , "{ 1: 2, 3: 4, $c: $d}"
               -- Complex
               , "{ $c: func ($d + 2) 5}"
               -- Fieldname
               , ".someField"
               , "   .someField2"
               , "$c.someField"
               -- Blank
               , ""
               ]
      doesntParse = [
                    -- letters
                      "$A"
                    -- bad
                    , "AFucntionWith;;InIt"
                    -- numbers
                    , "5. "
                    , " 5.6.3"
                    -- lists and objects
                    , "[a:asd,,,, ,m,se]"
                    , "{a:asd,,,, ,m,se}"
                    -- field
                    , "   .someF ield"
                    ]
  in

  describe "entryParser"
    [ describe "should parse"
       (List.map 
       (\str ->
         test str
               (\_ ->
                 let result = E.parseFully str in
                 Expect.equal True (Util.resultIsOk result)
                   |> Expect.onFail (toString result)))
       parses)

    , describe "shouldn't parse"
       (List.map 
       (\str ->
         test str
               (\_ ->
                 let result = E.parseFully str in
                 Expect.equal False (Util.resultIsOk result)
                   |> Expect.onFail (toString result)))
       doesntParse)
    ]
