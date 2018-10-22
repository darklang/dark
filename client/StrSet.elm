module StrSet exposing (..)

import Set

type alias Set = Set.Set String

empty = Set.empty
isEmpty = Set.isEmpty
toList = Set.toList
fromList = Set.fromList
insert = Set.insert
member = Set.member
diff = Set.diff

