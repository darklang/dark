module IntSet exposing (..)

import Set

type alias Set = Set.Set Int

empty = Set.empty
isEmpty = Set.isEmpty
toList = Set.toList
fromList = Set.fromList
insert = Set.insert
member = Set.member
diff = Set.diff

