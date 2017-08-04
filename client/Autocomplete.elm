module Autocomplete exposing (..)

import Types exposing (..)

empty : Autocomplete
empty = { defaults = [], current = [] }

init : List String -> Autocomplete
init defaults = { defaults = defaults, current = defaults }

reset : Autocomplete -> Autocomplete
reset a = { defaults = a.defaults, current = a.defaults }

update : Autocomplete -> String -> Autocomplete
update a str = { defaults = a.defaults
               , current = List.filter (\s -> String.contains str s) a.defaults 
               }
