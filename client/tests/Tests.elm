module Tests exposing (..)

-- tests
import Expect exposing (Expectation)
import Test exposing (..)

-- builtins
import Set

-- dark
import Autocomplete exposing (..)
import Types exposing (..)
import EntryParser as E
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

entryParser : Test
entryParser =
  let exactly =
        -- basics fns
        [ ("+"
          , E.PExpr (E.PFnCall "+" []))
        , ("aFunction"
          , E.PExpr (E.PFnCall "aFunction" []))
        , ("+ 3 4"
          , E.PExpr (E.PFnCall "+" ([E.PValue "3",E.PValue "4"])))
        , ("+ 3"
          , E.PExpr (E.PFnCall "+" ([E.PValue "3"])))
        , ("AfunctionWith::sInIt 5 2"
          , E.PExpr (E.PFnCall "AfunctionWith::sInIt" ([E.PValue "5",E.PValue "2"])))

        -- strings
        , ("\"a string\""
          , E.PExpr (E.PValue "\"a string\""))
        , ("\"spaces back\"        "
          , E.PExpr (E.PValue "\"spaces back\""))
        , ("     \"spaces front\""
          , E.PExpr (E.PValue "\"spaces front\""))
        , ("\"escaped \\\" quote\""
          , E.PExpr (E.PValue "\"escaped \\\" quote\""))

        -- numbers
        , ("5"
          , E.PExpr <| E.PValue "5")
        , ("6.42"
          , E.PExpr (E.PValue "6.42"))
        , ("-6"
          , E.PExpr (E.PValue "-6"))
        , ("- -6 -7.53"
          , E.PExpr (E.PFnCall "-" ([E.PValue "-6",E.PValue "-7.53"])))
 
        -- vars 
        , ("$a"
          , E.PExpr (E.PVar "a"))
        , ("- $a $c"
          , E.PExpr (E.PFnCall "-" ([E.PVar "a",E.PVar "c"])))

        -- bools
        , ("    false    "
          , E.PExpr (E.PValue "false"))
        , ("true"
          , E.PExpr (E.PValue "true"))
        , (" FaLsE "
          , E.PExpr (E.PValue "FaLsE"))

        -- infix
        , ("5 + 6"
          , E.PExpr (E.PFnCall "+" ([E.PValue "5",E.PValue "6"])))
        , ("$c + $a"
          , E.PExpr (E.PFnCall "+" ([E.PVar "c",E.PVar "a"])))
        , ("$c - $a"
          , E.PExpr (E.PFnCall "-" ([E.PVar "c",E.PVar "a"])))
        , ("-5 == -6"
          , E.PExpr (E.PFnCall "==" ([E.PValue "-5", E.PValue "-6"])))
        , ("6-2"
          , E.PExpr (E.PFnCall "-" ([E.PValue "6", E.PValue "2"])))
 
        -- infix with functions that aren't obvious infix things
        , ("$c String::foreach"
          , E.PExpr (E.PFnCall "String::foreach" ([E.PVar "c"])))
        , ("(String::first $c  ) String::is_substring (  String::concat \"a\" \"b\")"
          , E.PExpr (E.PFnCall "String::is_substring" ([E.PFnCall "String::first" ([E.PVar "c"]),E.PFnCall "String::concat" ([E.PValue "\"a\"",E.PValue "\"b\""])])))
        , ("$c Int::add 5"
          , E.PExpr (E.PFnCall "Int::add" ([E.PVar "c",E.PValue "5"])))

        -- parens
        , ("($c % 3) == ($a * 1)"
          , E.PExpr (E.PFnCall "==" ([E.PFnCall "%" ([E.PVar "c",E.PValue "3"]),E.PFnCall "*" ([E.PVar "a",E.PValue "1"])])))
        , ("($c % 3) == 0"
          , E.PExpr (E.PFnCall "==" ([E.PFnCall "%" ([E.PVar "c",E.PValue "3"]),E.PValue "0"])))
        , ("(5)"
          , E.PExpr <| E.PValue "5")

        -- Fieldname
        , (".someField"
          , E.PFieldname "someField")
        , ("   .someField2"
          , E.PFieldname "someField2")

        -- Objs
        , ("{ 1: 2, 3: 4}"
          , E.PExpr (E.PValue "{ 1: 2, 3: 4}"))

        -- Lists (TODO)
        , ("[1, 2, 3, 4]"
          , E.PExpr (E.PValue "[1, 2, 3, 4]"))

        , ("if ((3 % 3) == 0)"
          ,E.PExpr (E.PFnCall "if" ([E.PFnCall "==" ([E.PFnCall "%" ([E.PValue "3", E.PValue "3"]),E.PValue "0"])])))
        ]
      shouldntParse = [
                      -- letters
                        "$A"
                      -- bad
                      , "AFucntionWith;;InIt"
                      -- numbers
                      , "5. "
                      , " 5.6.3"
                      -- field
                      , "   .someF ield"
                      ]

      todo_should_parse =
        [ "{ 1: 2, 3: 4, $c: $d}"
        , "{ $c: func ($d + 2) 5}"
        , "[ 1, 2, 3, $c - 5, $c ]"
        , "$c.someField"
        , "+ + +"
        , "if (($c % 3) == 0)"
        , "$i % 3 == 0"
        , "+4"
          -- , E.PExpr (E.PFnCall "+" ([E.PValue "4"])))
        ]
      todo_should_error =
        [ "String.concat" -- we should parse this so we can give a better warning later
        , "[a:asd,,,, ,m,se]" -- error
        , "{a:asd,,,, ,m,se}" -- error
        , "(5 + 7)"
          -- , E.PExpr (E.PFnCall "+" ([E.PValue "5", E.PValue "7"])))
        , "(5) == -6"
          -- , E.PExpr (E.PFnCall "==" ([E.PValue "5", E.PValue "-6"])))
        , "(5) == (0)"
          -- , E.PExpr (E.PFnCall "==" ([E.PValue "5", E.PValue "0"])))
        , "$c == 3 && $a + 1 < 4"
          -- , E.PExpr
          --    (E.PFnCall "&&"
          --       [ (E.PFnCall "==" [E.PVar "c",E.PValue "3"])
          --       , (E.PFnCall "<"
          --          [E.PFnCall "+" [E.PVar "a",E.PValue "1"]
          --          , E.PValue "4"
          --          ]
          --         )
          --       ]
          --    ))
        ] 
      test_parsing expectedFn str =
        test ("parsing \"" ++ str ++ "\"")
          (\_ ->
            let result = E.parseFully str in
            Expect.true "" (expectedFn result)
              |> Expect.onFail (toString result))
 
  in

  describe "entryParser"
    [ describe "shouldn't parse"
     <| List.map (test_parsing (Util.resultIsOk >> not)) shouldntParse
    , describe "works exactly"
     <| List.map (\(str, expected) -> test_parsing ((==) <| Result.Ok expected) str) exactly
    ]
