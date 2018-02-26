module Autocomplete exposing (..)

-- builtin
import Dict
import Json.Decode as JSD
import Dom.Scroll
import Task

-- lib
import List.Extra as LE
-- import Maybe.Extra as ME
-- import String.Extra as SE

-- dark
import Util exposing (deMaybe, int2letter, letter2int)
import Types exposing (..)
import Runtime as RT
import Pointer as P
import Analysis

----------------------------
-- Focus
----------------------------

-- show the prev 5
-- obvi this should use getClientBoundingBox, but that's tough in Elm
height : Int -> Int
height i = if i < 4
           then 0
           else 17 * (i - 4)

focusItem : Int -> Cmd Msg
focusItem i = Dom.Scroll.toY "autocomplete-holder" (i |> height |> toFloat)
            |> Task.attempt FocusAutocompleteItem


----------------------------
-- External: utils
----------------------------

findFunction : Autocomplete -> String -> Maybe Function
findFunction a name =
  LE.find (\f -> f.name == name) a.functions

isStringEntry : Autocomplete -> Bool
isStringEntry a = String.startsWith "\"" a.value

isSmallStringEntry : Autocomplete -> Bool
isSmallStringEntry a =
  isStringEntry a && not (isLargeStringEntry a)

isLargeStringEntry : Autocomplete -> Bool
isLargeStringEntry a =
  isStringEntry a && String.contains "\n" a.value

getValue : Autocomplete -> String
getValue a =
  case highlighted a of
    Just item -> asName item
    Nothing -> a.value

sharedPrefix2 : String -> String -> String
sharedPrefix2 l r =
  case (String.uncons l, String.uncons r) of
    (Just (l1, lrest), Just (r1, rrest)) ->
      if l1 == r1 then
        String.fromChar l1 ++ sharedPrefix2 lrest rrest
      else
        ""
    _ -> ""

sharedPrefixList : List String -> String
sharedPrefixList strs =
  case List.head strs of
    Nothing -> ""
    Just s -> List.foldl sharedPrefix2 s strs

-- Find the shared prefix of all the possible suggestions (eg "List::")
sharedPrefix : Autocomplete -> String
sharedPrefix a =
  a.completions
  |> List.concat
  |> List.map asName
  |> sharedPrefixList

-- returns (indent, suggestion, search), where:
--  - indent is the string that occurs before the match
--  - suggestion is the match rewritten with the search
--  - search is the search rewritten to match the suggestion
compareSuggestionWithActual : Autocomplete -> String -> (String, String, String)
compareSuggestionWithActual a actual =
  let suggestion = sharedPrefix a
  in
    case String.indexes (String.toLower actual) (String.toLower suggestion) of
      [] -> ("", suggestion, actual)
      index :: _ ->
        let prefix = String.slice 0 index suggestion
            suffix = String.slice (index + String.length actual) (String.length suggestion) suggestion
        in
           (prefix, prefix ++ actual ++ suffix, actual)


----------------------------
-- Autocomplete state
----------------------------

empty : Autocomplete
empty = init []

init : List Function -> Autocomplete
init functions = { functions = functions
                 , completions = [List.map ACFunction functions]
                 , index = -1
                 , value = ""
                 , liveValue = Nothing
                 , tipe = Nothing
                 , target = Nothing
                 }

forLiveValue : Maybe LiveValue -> Autocomplete -> Autocomplete
forLiveValue lv a = { a | liveValue = lv }

-- forParamType : Tipe -> NodeList -> Autocomplete -> Autocomplete
-- forParamType tipe ns a = { a | tipe = Just tipe, nodes = Just ns }
--
reset : Autocomplete -> Autocomplete
reset a = init a.functions

clear : Autocomplete -> Autocomplete
clear a = let cleared = setQuery "" a in
          { cleared | index = -1 }

numCompletions : Autocomplete -> Int
numCompletions a =
  a.completions |> List.concat |> List.length

selectDown : Autocomplete -> Autocomplete
selectDown a = let max_ = numCompletions a
                   max = Basics.max max_ 1
                   new = (a.index + 1) % max
               in
                 { a | index = new }

selectUp : Autocomplete -> Autocomplete
selectUp a = let max = numCompletions a - 1 in
             { a | index = if a.index <= 0
                           then max
                           else a.index - 1
             }

-- variablesForType : NodeList -> Tipe -> List AutocompleteItem
-- variablesForType ns t =
--     ns
--       |> List.indexedMap (,)
--       |> List.filter (\(_, n) -> RT.isCompatible t n.liveValue.tipe)
--       |> List.map (\(i, n) -> (int2letter i, n))
--       |> List.map ACVariable
--
-- Implementation:
-- n The autocomplete list should include:
--    y all imported functions
--    y restricted by types that are allowed
--    y allowed field names
--    n library names
--    y case-insensitive
-- n order by most likely, offer other alternatives below
--   n slight typos
--   n slight typeos
-- y Press enter to select
-- y Press right to fill as much as is definitive

setQuery : String -> Autocomplete -> Autocomplete
setQuery q a = refilter q (LE.getAt 0 a.completions
                           |> Maybe.withDefault []) a

appendQuery : String -> Autocomplete -> Autocomplete
appendQuery str a =
  let q = if isStringEntry a
          then String.dropRight 1 a.value ++ str ++ "\""
          else a.value ++ str
  in setQuery q a

highlighted : Autocomplete -> Maybe AutocompleteItem
highlighted a = LE.getAt a.index (List.concat a.completions)

setTarget : Maybe (TLID, Pointer) -> Autocomplete -> Autocomplete
setTarget t a =
  { a | target = t }

update : Model -> AutocompleteMod -> Autocomplete -> Autocomplete
update m mod a =
  (case mod of
     ACSetQuery str -> setQuery str a
     ACAppendQuery str -> appendQuery str a
     ACReset -> reset a
     ACClear -> clear a
     ACSelectDown -> selectDown a
     ACSelectUp -> selectUp a
     ACFilterByLiveValue lv -> forLiveValue lv a
     ACSetTarget target -> setTarget target a
     ACRegenerate -> a
     -- ACFilterByParamType tipe nodes -> forParamType tipe nodes a
  )
  -- For now, we need to regenerate on any change. In the future, we
  -- should keep all non-ac state out of here (livevalue and target),
  -- and have a regenerate function that regenerates then and only then.
  |> regenerate m




------------------------------------
-- Create the list
------------------------------------

regenerate : Model -> Autocomplete -> Autocomplete
regenerate m a =
  let completions = generateFromModel m a
  in refilter a.value completions a

refilter : String -> List AutocompleteItem -> Autocomplete -> Autocomplete
refilter query initial a =
  let completions = filter initial query
      totalLength = completions |> List.concat |> List.length
  in { a | completions = completions
         , value = query
         , index = if totalLength == 0
                   then -1
                   else if totalLength < numCompletions a
                   then 0
                   else a.index
     }

filter : List AutocompleteItem -> String -> List (List AutocompleteItem)
filter list query =
  let lcq = query |> String.toLower

      stringify i = (if 1 >= String.length lcq
                     then asName i
                     else asString i)
                    |> Util.replace "⟶" "->"


      -- split into different lists
      candidates1 = List.filter (stringify
                                 >> String.toLower
                                 >> String.contains lcq
                                 ) list

      (startsWith, candidates2) =
        List.partition (stringify
                        >> String.startsWith query
                       ) candidates1

      (startsWithCI, candidates3) =
        List.partition (stringify
                        >> String.toLower
                        >> String.startsWith lcq
                        ) candidates2

      (substring, substringCI ) =
        List.partition (stringify
                        >> String.contains query
                        ) candidates3

  in
  [ startsWith, startsWithCI, substring, substringCI ]



generateFromModel : Model -> Autocomplete -> List AutocompleteItem
generateFromModel m a =
  let fields = case a.liveValue of
                 Just lv -> if lv.tipe == TObj
                            then jsonFields lv.json
                            else []
                 Nothing -> []
      --
      -- variables
      -- variables = case (a.tipe, a.nodes) of
      --                 (Just t, Just ns)  -> variablesForType ns t
      --                 _ -> []
      showFunctions =
        case a.target of
          Just (_, p) -> P.typeOf p == Expr
          Nothing -> True

      -- functions
      funcList = if showFunctions then a.functions else []
      functions =
        funcList
        |> List.filter
           (\{returnTipe} ->
              case a.tipe of
                Just t -> RT.isCompatible returnTipe t
                Nothing -> True)
        |> List.filter
          (\fn ->
             case a.liveValue of
               Just {tipe} -> Nothing /= findParamByType fn tipe
               Nothing -> True)
        |> List.map ACFunction

      extras =
        case a.target of
          Just (_, p) ->
            case P.typeOf p of
              HTTPVerb ->
                [ "GET"
                , "POST"
                , "PUT"
                , "DELETE"
                , "PATCH"
                ]
              DBColType ->
                [ "String"
                , "Int"
                , "Boolean"
                , "Float"
                , "Title"
                , "Url"
                , "Date"
                ]
              DarkType ->
                [ "Any"
                , "Empty"
                , "String"
                , "Int"
                , "{"
                ]
              _ -> []
          _ -> []

      varnames = Analysis.varnamesFor m a.target
    in
    List.map ACExtra extras
    ++ functions
    ++ fields
    ++ List.map ACVariable varnames



asName : AutocompleteItem -> String
asName aci =
  case aci of
    ACFunction {name} -> name
    ACField name -> name
    ACVariable name -> name
    ACExtra name -> name

asTypeString : AutocompleteItem -> String
asTypeString item =
  case item of
    ACFunction f -> f.parameters
                    |> List.map .tipe
                    |> List.map RT.tipe2str
                    |> String.join ", "
                    |> (\s -> "(" ++ s ++ ") ->  " ++ (RT.tipe2str f.returnTipe))
    ACField _ -> "field"
    ACVariable _ -> "variable"
    ACExtra _ -> ""

asString : AutocompleteItem -> String
asString aci =
  asName aci ++ asTypeString aci

-- parse the json, take the list of keys, add a . to the front of it
-- TODO: this needs refactoring
jsonFields : String -> List AutocompleteItem
jsonFields json =
  json
    |> JSD.decodeString (JSD.dict JSD.value)
    |> Result.toMaybe
    |> deMaybe "json decode result"
    |> Dict.keys
    |> List.map ACField


findParamByType : Function -> Tipe -> Maybe Parameter
findParamByType {parameters} tipe =
  parameters
  |> LE.find (\p -> RT.isCompatible p.tipe tipe)


