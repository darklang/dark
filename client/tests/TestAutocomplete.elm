module TestAutocomplete exposing (all)

-- tests
import Test exposing (..)
import Expect exposing (Expectation)

-- dark
import Autocomplete exposing (..)
import Types exposing (..)
import Defaults
import Nineteen.String as String
import Blank as B
import Prelude exposing (..)


d : String -> List (() -> Bool) -> Test
d s fs = describe s (List.indexedMap
                       (\i f ->
                          test
                          ("test " ++ (String.fromInt i))
                          (\_ -> Expect.true "" (f ())))
                       fs
                    )

sampleFunctions : List Function
sampleFunctions =
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
  |> List.map
    (\(name,tipe) ->
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
      , deprecated = False
      })

debug : String -> Autocomplete -> Autocomplete
debug msg ac =
  let _  = Debug.log msg (highlighted ac) in
  ac

type Role = Admin | User

isAdmin : Role -> Bool
isAdmin r =
  case r of
    Admin -> True
    _ -> False

createEntering : Role -> Autocomplete
createEntering role =
  let targetBlankID = gid ()
      tlid = gtlid ()
      spec = { module_ = B.new ()
             , name = B.new ()
             , modifier = B.new ()
             , types = { input = B.new (), output = B.new () }
             }
      toplevel =
        { id = tlid
        , pos = { x = 0, y = 0 }
        , data =
          TLHandler ({ ast = Blank targetBlankID, spec = spec, tlid = tlid })
        }
      cursor =
        Entering (Filling tlid targetBlankID)
      default =
        Defaults.defaultModel
      m =
        { default | toplevels = [toplevel], cursorState = cursor }
  in
      init sampleFunctions (isAdmin role)
      |> setTarget m (Just (tlid, PExpr (Blank targetBlankID)))

createCreating : Role -> Autocomplete
createCreating role =
  let cursor =
        Entering (Creating { x = 0, y = 0 })
      default =
        Defaults.defaultModel
      m =
        { default | cursorState = cursor }
  in
      init sampleFunctions (isAdmin role)
      |> setTarget m Nothing

itemPresent : AutocompleteItem -> Autocomplete -> Bool
itemPresent aci ac =
  List.member aci (List.concat ac.completions)

itemMissing : AutocompleteItem -> Autocomplete -> Bool
itemMissing aci ac =
  not (itemPresent aci ac)

all : Test
all =
  describe "autocomplete"
    [ d "sharedPrefix"
      [ \_ -> sharedPrefixList ["aaaab", "aab", "aaxb"] == "aa"
      , \_ -> sharedPrefixList ["abcdd", "abcdde"] == "abcdd"
      , \_ -> sharedPrefixList ["abcdd", "bcddee"] == ""
      ]
    , d "queryWhenEntering" -- numbered from 0
      -- Empty autocomplete doesn't highlight
      [ \_ -> (createEntering User)
      |> .index
      |> (==) -1

      -- Press a letter from the selected entry keeps the entry selected
      , \_ -> createEntering User
      |> selectDown
      |> selectDown
      |> selectDown
      |> selectDown
      |> selectDown
      |> setQuery "T"
      |> highlighted
      |> Maybe.map asName
      |> (==) (Just "Twit::someOtherFunc")

      -- Returning to empty unselects
      , \_ -> createEntering User
      |> setQuery "lis"
      |> setQuery ""
      |> highlighted
      |> (==) Nothing

      , \_ -> createEntering User
      |> setQuery "Twit::somefunc"
      |> setQuery "Twit::some"
      |> selectDown
      |> highlighted
      |> Maybe.map asName
      |> (==) (Just "Twit::someOtherFunc")

      -- Lowercase search still finds uppercase results
      , \_ -> createEntering User
      |> setQuery "lis"
      |> .completions
      |> List.concat
      |> List.map asName
      |> (==) ["List::head"]

      -- Search finds multiple prefixes
      , \_ -> createEntering User
      |> setQuery "twit::"
      |> .completions
      |> List.concat
      |> List.filter isStaticItem
      |> List.map asName
      |> (==) ["Twit::somefunc", "Twit::someOtherFunc", "Twit::yetAnother"]

      -- Search finds only prefixed
      , \_ -> createEntering User
      |> setQuery "twit::y"
      |> .completions
      |> List.concat
      |> List.filter isStaticItem
      |> List.map asName
      |> (==) ["Twit::yetAnother"]

      -- Search anywhere
      , \_ -> createEntering User
      |> setQuery "Another"
      |> .completions
      |> List.concat
      |> List.filter isStaticItem
      |> List.map asName
      |> (==) ["Twit::yetAnother"]

      -- Show results when the only option is the setQuery
      , \_ -> createEntering User
      |> setQuery "List::head"
      |> .completions
      |> List.concat
      |> List.filter isStaticItem
      |> List.map asName
      |> List.length
      |> (==) 1

      -- Scrolling down a bit works
      , \_ -> createEntering User
      |> setQuery "Twit"
      |> selectDown
      |> selectDown
      |> .index
      |> (==) 2

      -- Scrolling loops one way
      , \_ -> createEntering User
      |> setQuery "Twit:"
      |> selectDown
      |> selectDown
      |> selectDown
      |> .index
      |> (==) 0

      -- Scrolling loops the other way
      , \_ -> createEntering User
      |> setQuery "Twit:"
      |> selectDown
      |> selectUp
      |> selectUp
      |> .index
      |> (==) 2

      -- Scrolling loops the other way without going forward first
      , \_ -> createEntering User
      |> setQuery "Twit:"
      |> selectUp
      |> selectUp
      |> .index
      |> (==) 1

      -- Scrolling backward works if we haven't searched yet
      , \_ -> createEntering User
      |> selectUp
      |> selectUp
      |> selectUp
      |> selectUp
      |> selectUp
      |> .index
      |> (==) 13

      -- Don't highlight when the list is empty
      , \_ -> createEntering User
      |> setQuery "Twit"
      |> selectDown
      |> selectDown
      |> setQuery "Twit::1334xxx"
      |> .index
      |> (==) -1

      -- Filter by method signature for typed values
      -- , \_ -> createEntering User
      -- |> forLiveValue {value="[]", tipe=TList,json="[]", exc=Nothing}
      -- |> setQuery ""
      -- |> .completions
      -- |> List.map asName
      -- |> Set.fromList
      -- |> (==) (Set.fromList ["List::head"])

      -- Show allowed fields for objects
      -- , \_ -> createEntering User
      -- |> forLiveValue {value="5", tipe=TInt, json="5", exc=Nothing}
      -- |> setQuery ""
      -- |> .completions
      -- |> List.map asName
      -- |> Set.fromList
      -- |> (==) (Set.fromList ["Int::add", "+"])

      -- By default the list shows results
      , \_ -> createEntering User
      |> setQuery ""
      |> .completions
      |> List.concat
      |> List.length
      |> (/=) 0

      -- ordering: startsWith, then case match, then case insensitive match
      , \_ -> createEntering User
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
      , \_ -> createEntering User
      |> setQuery "21434234"
      |> selectDown
      |> highlighted
      |> Maybe.map asName
      |> (==) (Just "21434234")


      , \_ -> createEntering User
      -- A specific bug where + is interpreted as an ACLiteral
      |> setQuery "+"
      |> highlighted
      |> Maybe.map asName
      |> (==) (Just "+")

      -- A few different kinds of literals
      , \_ -> createEntering User
      |> setQuery "nu"
      |> highlighted
      |> (==) (Just (ACLiteral "null"))

      , \_ -> createEntering User
      |> setQuery "tr"
      |> highlighted
      |> (==) (Just (ACLiteral "true"))

      , \_ -> createEntering User
      |> setQuery "tR" -- case insensitive
      |> highlighted
      |> (==) (Just (ACLiteral "true"))

      , \_ -> createEntering User
      |> setQuery "false"
      |> highlighted
      |> (==) (Just (ACLiteral "false"))

      , \_ -> createEntering User
      |> setQuery "3.452"
      |> highlighted
      |> (==) (Just (ACLiteral "3.452"))

      -- keywords appear in autocomplete
      , \_ -> createEntering User
      |> setQuery "if"
      |> highlighted
      |> (==) (Just (ACKeyword KIf))

      , \_ -> createEntering User
      |> setQuery "let"
      |> highlighted
      |> (==) (Just (ACKeyword KLet))

      , \_ -> createEntering User
      |> setQuery "lambda"
      |> highlighted
      |> (==) (Just (ACKeyword KLambda))


      ]
    , d "queryWhenCreating"
      [ \_ -> createCreating User
      |> setQuery "Mydbname"
      |> itemPresent (ACOmniAction (NewDB "Mydbname"))

      -- db names can be multicase
      , \_ -> createCreating User
      |> setQuery "MyDBnaMe"
      |> itemPresent (ACOmniAction (NewDB "MyDBnaMe"))

      -- alphabetical only #1
      , \_ -> createCreating User
      |> setQuery "dbname1234::"
      |> itemMissing (ACOmniAction (NewDB "dbname1234::"))

      -- alphabetical only #2
      , \_ -> createCreating User
      |> setQuery "db_name::"
      |> itemMissing (ACOmniAction (NewDB "db_name::"))

      -- require capital
      , \_ -> createCreating User
      |> setQuery "mydbname"
      |> itemMissing (ACOmniAction (NewDB "mydbname"))

      -- No HTTP handler in general
      , \_ -> createCreating User
      |> setQuery "asdkkasd"
      |> itemMissing (ACOmniAction NewHTTPHandler)

      -- HTTP handler
      , \_ -> createCreating User
      |> setQuery "HTT"
      |> itemPresent (ACOmniAction (NewEventSpace "HTT"))

      , \_ -> createCreating User
      |> setQuery "/"
      |> itemPresent (ACOmniAction (NewHTTPRoute "/"))

      , \_ -> createCreating User
      |> setQuery "/asasdasd"
      |> itemPresent (ACOmniAction (NewHTTPRoute "/asasdasd"))

      , \_ -> createCreating User
      |> itemPresent (ACOmniAction NewHandler)

      , \_ -> createCreating User
      |> itemPresent (ACOmniAction (NewFunction Nothing))

      , \_ -> createCreating User
      |> setQuery "myFunction"
      |> itemPresent (ACOmniAction (NewFunction (Just "myFunction")))

      ]
    ]
