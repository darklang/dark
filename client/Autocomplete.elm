module Autocomplete exposing (..)

import List.Extra exposing (getAt)

import Types exposing (..)


empty : Autocomplete
empty = init []

init : List AutocompleteItem -> Autocomplete
init defaults = { defaults = defaults, current = defaults, index = -1, value = "", liveValue = Nothing }

forLiveValue : LiveValue -> Autocomplete -> Autocomplete
forLiveValue lv a = { a | liveValue = Just lv }

reset : Autocomplete -> Autocomplete
reset a = init a.defaults

selectDown : Autocomplete -> Autocomplete
selectDown a = let max_ = (List.length a.current)
                   max = Basics.max max_ 1
               in
                 { a | index = (a.index + 1) % max }

selectUp : Autocomplete -> Autocomplete
selectUp a = let max = (List.length a.current) - 1 in
             { a | index = if a.index == 0 then max else a.index - 1
             }

highlighted : Autocomplete -> Maybe AutocompleteItem
highlighted a = getAt a.index a.current

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
sharedPrefix a = sharedPrefixList (List.map .name a.current)

joinPrefix : String -> String -> String
joinPrefix actual extension =
  let len = String.length actual
      suffix = String.dropLeft len extension
  in
    actual ++ suffix



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


-- Implementation:
-- n The autocomplete list should include:
--    y all imported functions
--    n restricted by types that are allowed
--    n allowed field names
--    n library names
--    y case-insensitive
-- n order by most likely, offer other alternatives below
--   n slight typos
--   n slight typeos
-- y Press enter to select
-- y Press right to fill as much as is definitive

query : String -> Autocomplete -> Autocomplete
query q a =
  let lcq = String.toLower q
      current =
        a.defaults
          |> List.filter
             (\i -> String.startsWith lcq (String.toLower i.name))
          |> List.filter
             (\i -> case a.liveValue of
                      Just (_, tipe) -> [tipe] == i.types
                      Nothing -> True
             )

      newcurrent = case current of
                     [ i ] -> if i.name == q then [] else [ i ]
                     cs -> cs
  in
    { defaults = a.defaults
    , liveValue = a.liveValue
    , current = newcurrent
    , index = if List.length newcurrent < List.length a.current
              then if List.length newcurrent == 0
                   then -1
                   else 0
              else a.index
    , value = q
    }

update : AutocompleteMod -> Autocomplete -> Autocomplete
update mod a =
  case mod of
    SetEntry str -> query str a
    Reset -> query "" a
    SelectDown -> selectDown a
    SelectUp -> selectUp a
    FilterByLiveValue lv -> forLiveValue lv a
