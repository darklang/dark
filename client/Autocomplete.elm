module Autocomplete exposing (..)

import List.Extra exposing (getAt)

import Types exposing (..)


empty : Autocomplete
empty = init []

init : List String -> Autocomplete
init defaults = { defaults = defaults, current = defaults, index = -1, value = "" }

reset : Autocomplete -> Autocomplete
reset a = init a.defaults

selectDown : Autocomplete -> Autocomplete
selectDown a = let max_ = (List.length a.current)
                   max = min max_ 1
               in
                 { a | index = (a.index + 1) % max }

selectUp : Autocomplete -> Autocomplete
selectUp a = let max = (List.length a.current) - 1 in
             { a | index = if a.index == 0 then max else a.index - 1
             }

highlighted : Autocomplete -> Maybe String
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

sharedPrefix : List String -> String
sharedPrefix strs =
  case List.head strs of
    Nothing -> ""
    Just h -> List.foldl sharedPrefix2 h strs

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
      current = List.filter
                (\s -> String.startsWith lcq (String.toLower s))
                a.defaults
      newcurrent = case current of
                     [ x ] -> if x == q then [] else [ x ]
                     cs -> cs
  in
    { defaults = a.defaults
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
