module StrDict exposing (..)

import Dict

type alias Dict a = Dict.Dict String a

empty = Dict.empty
toList = Dict.toList
fromList = Dict.fromList
get = Dict.get
insert = Dict.insert
update = Dict.update
