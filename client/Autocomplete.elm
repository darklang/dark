module Autocomplete exposing (..)

-- builtin
import Dict
import Json.Decode as JSD
import Dom.Scroll
import Task

-- lib
import List.Extra as LE

-- dark
import Util exposing (deMaybe)
import Types exposing (..)

-- show the prev 5
-- obvi this should use getClientBoundingBox, but that's tough in Elm
height : Int -> Int
height i = if i < 5
           then 0
           else 16 * (i - 5)

focusItem : Int -> Cmd Msg
focusItem i = Dom.Scroll.toY "autocomplete-holder" (i |> height |> toFloat)
            |> Task.attempt FocusAutocompleteItem

empty : Autocomplete
empty = init []

init : List Function -> Autocomplete
init functions = { functions = functions
                 , completions = List.map ACFunction functions
                 , index = -1
                 , open = True
                 , value = ""
                 , liveValue = Nothing
                 , tipe = Nothing
                 }

forLiveValue : LiveValue -> Autocomplete -> Autocomplete
forLiveValue lv a = { a | liveValue = Just lv }

forParamType : String -> Autocomplete -> Autocomplete
forParamType tipe a = { a | tipe = Just tipe }

reset : Autocomplete -> Autocomplete
reset a = init a.functions

clear : Autocomplete -> Autocomplete
clear a = let cleared = query "" a in
          { cleared | index = -1 }

open : Bool -> Autocomplete -> Autocomplete
open o a = { a | open = o }

complete : String -> Autocomplete -> Autocomplete
complete str a = { a | value = str
                     , completions = []
                     , index = -1
                     , open = True
                 }

selectDown : Autocomplete -> Autocomplete
selectDown a = let max_ = List.length a.completions
                   max = Basics.max max_ 1
                   new = (a.index + 1) % max
               in
                 { a | index = new }

selectUp : Autocomplete -> Autocomplete
selectUp a = let max = (List.length a.completions) - 1 in
             { a | index = if a.index <= 0
                           then max
                           else a.index - 1
             }

highlighted : Autocomplete -> Maybe AutocompleteItem
highlighted a = LE.getAt a.index a.completions

sharedPrefix2 : String -> String -> String
sharedPrefix2 l r =
  case (String.uncons l, String.uncons r) of
    (Just (l1, lrest), Just (r1, rrest)) ->
      if l1 == r1 then
        (String.fromChar l1) ++ (sharedPrefix2 lrest rrest)
      else
        ""
    _ -> ""

sharedPrefixList : List String -> String
sharedPrefixList strs =
  case List.head strs of
    Nothing -> ""
    Just s -> List.foldl sharedPrefix2 s strs

sharedPrefix : Autocomplete -> String
sharedPrefix a = sharedPrefixList (List.map asString a.completions)

joinPrefix : String -> String -> String
joinPrefix actual extension =
  let len = String.length actual
      suffix = String.dropLeft len extension
  in
    actual ++ suffix

asString : AutocompleteItem -> String
asString aci =
  case aci of
    ACFunction {name} -> name
    ACField name -> "." ++ name

containsOrdered : String -> String -> Bool
containsOrdered needle haystack =
  case String.uncons needle of
    Just (c, newneedle) ->
      let char = String.fromChar c in
      String.contains char haystack
        && containsOrdered newneedle (haystack
                                        |> String.split char
                                        |> List.drop 1
                                        |> String.join char)
    Nothing -> True

-- parse the json, take the list of keys, add a . to the front of it
-- TODO: this needs refactoring
jsonFields : String -> List AutocompleteItem
jsonFields json =
  json
    |> JSD.decodeString (JSD.dict JSD.value)
    |> Result.toMaybe
    |> deMaybe
    |> Dict.keys
    |> List.map ACField


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

query : String -> Autocomplete -> Autocomplete
query q a = { a | value = q } |> regenerate

regenerate : Autocomplete -> Autocomplete
regenerate a =
  let lcq = String.toLower a.value
      -- fields of objects
      fields = case a.liveValue of
                 Just (_, "Object", json) -> jsonFields json
                 _ -> []

      -- functions
      options =
        a.functions
        |> List.filter
           (\{return_type} ->
              a.tipe == Nothing || a.tipe == Just return_type)
        |> List.filter
          (\fn ->
             case a.liveValue of
               Just (_, tipe, _) -> Nothing /= findParamByType fn tipe
               Nothing -> True)
        |> List.map (\s -> ACFunction s)
        |> List.append fields
        |> List.filter
           (\i -> i |> asString |> String.toLower |> String.contains lcq)

      completions =
        if not a.open
        then []
        else case options of
               [ i ] -> if asString i == a.value then [] else [ i ]
               cs -> cs

  in { a | completions = completions
         , index = if List.length completions == 0
                   then -1
                   else if List.length completions < List.length a.completions
                   then 0
                   else a.index
     }

update : AutocompleteMod -> Autocomplete -> Autocomplete
update mod a =
  (case mod of
     Query str -> query str a
     Reset -> reset a
     Open o -> open o a
     Clear -> clear a
     Complete str -> complete str a
     SelectDown -> selectDown a
     SelectUp -> selectUp a
     FilterByLiveValue lv -> forLiveValue lv a
     FilterByParamType tipe -> forParamType tipe a)
    |> regenerate



findParamByType : Function -> TypeName -> Maybe Parameter
findParamByType {parameters} tipe =
  LE.find (\p -> p.tipe == tipe || p.tipe == "Any") parameters

findFunction : Autocomplete -> String -> Maybe Function
findFunction a name =
  LE.find (\f -> f.name == name) a.functions


