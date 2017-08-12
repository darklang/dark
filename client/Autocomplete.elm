module Autocomplete exposing (..)

import List.Extra exposing (getAt)

import Types exposing (..)


empty : Autocomplete
empty = { defaults = [], current = [], index = -1 }

init : List String -> Autocomplete
init defaults = { defaults = defaults, current = defaults, index = -1 }

reset : Autocomplete -> Autocomplete
reset a = { defaults = a.defaults, current = a.defaults, index = -1 }

selectDown : Autocomplete -> Autocomplete
selectDown a = let max = (List.length a.current) in
               { a | index = (a.index + 1) % max }

selectUp : Autocomplete -> Autocomplete
selectUp a = let max = (List.length a.current) - 1 in
             { a | index = if a.index == 0 then max else a.index - 1
             }

highlighted : Autocomplete -> Maybe String
highlighted a = getAt a.index a.current

containsOrdered : String -> String -> Bool
containsOrdered needle haystack =
  case String.uncons needle of
    Just (c, newneedle) ->
      let char = String.fromChar c in
      String.contains char haystack
        && containsOrdered newneedle (haystack
                                        |> String.split char
                                        |> List.drop 1
                                        |> String.join "")
    Nothing -> True

query : Autocomplete -> String -> Autocomplete
query a str =
  { defaults = a.defaults
  , current = List.filter (\s -> String.contains str s) a.defaults
  , index = 0
  }

update : Autocomplete -> AutocompleteMod -> Autocomplete
update a mod =
  case mod of
    Query str -> query a str
    Reset -> reset a
    SelectDown -> selectDown a
    SelectUp -> selectUp a
