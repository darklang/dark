module SpecHeaders exposing (..)

-- builtin

-- lib

-- dark
import Types exposing (..)
import Pointer as P
import Blank as B

find : Handler -> Pointer -> Maybe (BlankOr String)
find h p =
  [h.spec.name, h.spec.modifier]
  |> List.filter (\spec -> B.toID spec == P.idOf p)
        -- TODO: opportunity to check pointer types
  |> List.head

replaceEventModifierBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceEventModifierBlank id value hs =
  { hs | modifier = if B.toID hs.modifier == id
                    then F (B.toID hs.modifier) value
                    else hs.modifier
  }
replaceEventNameBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceEventNameBlank id value hs =
  { hs | name = if B.toID hs.name == id
                then F (B.toID hs.name) value
                else hs.name
  }

replaceEventSpaceBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceEventSpaceBlank id value hs =
  { hs | module_ = if B.toID hs.module_ == id
                    then F (B.toID hs.module_) value
                    else hs.module_
  }


deleteEventNameBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteEventNameBlank p hs newID =
  { hs | name = if B.toID hs.name == (P.idOf p)
                then Blank newID
                else hs.name
  }

deleteEventModifierBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteEventModifierBlank p hs newID =
  { hs | modifier = if B.toID hs.modifier == (P.idOf p)
                    then Blank newID
                    else hs.modifier
  }

deleteEventSpaceBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteEventSpaceBlank p hs newID =
  { hs | module_ = if B.toID hs.module_ == (P.idOf p)
                    then Blank newID
                    else hs.module_
  }

allPointers : HandlerSpec -> List Pointer
allPointers spec =
  [ B.toP EventName spec.name
  , B.toP EventSpace spec.module_
  , B.toP EventModifier spec.modifier]


