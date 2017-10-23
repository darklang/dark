module TestEntryParser exposing (all)

-- tests
import Test exposing (..)
import Expect exposing (Expectation)

-- builtins

-- libs

-- dark
import EntryParser as E
import Util

all : Test
all =
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
        -- implicit placeholder
        , ("$_"
          , E.PExpr (E.PVar "_"))
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

      todoShouldParse =
        [ "$c.someField"
        , "+ + +"
        , "if (($c % 3) == 0)"
        , "$i % 3 == 0"
        , "if (1 + 0) (true && true)"
        , "3 <= 5"
        ]
      todoShouldParseExactly =
        [ ("+4"
          , E.PExpr (E.PFnCall "+" ([E.PValue "4"])))
        , ("(5 + 7)"
          , E.PExpr (E.PFnCall "+" ([E.PValue "5", E.PValue "7"])))
        , ("(5) == -6"
          , E.PExpr (E.PFnCall "==" ([E.PValue "5", E.PValue "-6"])))
        , ("(5) == (0)"
          , E.PExpr (E.PFnCall "==" ([E.PValue "5", E.PValue "0"])))
        , ("$c == 3 && $a + 1 < 4"
          , E.PExpr
             (E.PFnCall "&&"
                [ (E.PFnCall "==" [E.PVar "c",E.PValue "3"])
                , (E.PFnCall "<"
                   [E.PFnCall "+" [E.PVar "a",E.PValue "1"]
                   , E.PValue "4"
                   ]
                  )
                ]
             ))
        , ("{ 1: 2, 3: 4, $c: $d}", E.PExpr <| E.PValue "I dont know but not what it does parse to")
        , ("{ $c: func ($d + 2) 5}", E.PExpr <| E.PValue "I dont know but not what it does parse to")
        , ("[ 1, 2, 3, $c - 5, $c ]", E.PExpr <| E.PValue "I dont know but not what it does parse to")

        ]
      todoShouldError =
        [ "String.concat" -- we should parse this so we can give a better warning later
        , "[a:asd,,,, ,m,se]" -- error
        , "{a:asd,,,, ,m,se}" -- error
        ]
      testParsing expectedFn str =
        test ("parsing \"" ++ str ++ "\"")
          (\_ ->
            let result = E.parseFully str in
            Expect.true "" (expectedFn result)
              |> Expect.onFail (toString result))
      todoParsing expectedFn str =
        test ("parsing \"" ++ str ++ "\"")
          (\_ ->
            let result = E.parseFully str in
            Expect.true "" (expectedFn result)
              |> Expect.onFail (toString result))

  in

  describe "entryParser"
    [ describe "shouldn't parse"
     <| List.map (testParsing (Util.resultIsOk >> not)) shouldntParse
    , describe "works exactly"
     <| List.map (\(str, expected) -> testParsing ((==) <| Result.Ok expected) str) exactly
    , describe "todo: should parse"
     <| List.map (testParsing Util.resultIsOk) todoShouldParse
    , describe "todo: should parse exactly"
     <| List.map (\(str, expected) -> testParsing ((==) <| Result.Ok expected) str) todoShouldParseExactly
    , describe "todo: shouldnt parse"
     <| List.map (testParsing (Util.resultIsOk >> not)) todoShouldError
    ]
