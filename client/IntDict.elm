module IntDict exposing (..)

import Dict

type alias Dict a = Dict.Dict Int a

empty = Dict.empty
toList = Dict.toList
fromList = Dict.fromList
get = Dict.get
insert = Dict.insert
update = Dict.update

