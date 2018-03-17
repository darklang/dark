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
  |> List.filter (\spec -> B.toID spec == P.toID p)
        -- TODO: opportunity to check pointer types
  |> List.head

replaceEventModifier : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replaceEventModifier id value hs =
  { hs | modifier = if B.toID hs.modifier == id
                    then value
                    else hs.modifier
  }
replaceEventName : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replaceEventName id value hs =
  { hs | name = if B.toID hs.name == id
                then value
                else hs.name
  }

replaceEventSpace : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replaceEventSpace id value hs =
  { hs | module_ = if B.toID hs.module_ == id
                    then value
                    else hs.module_
  }


deleteEventName : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteEventName p hs newID =
  { hs | name = if B.toID hs.name == (P.toID p)
                then Blank newID
                else hs.name
  }

deleteEventModifier : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteEventModifier p hs newID =
  { hs | modifier = if B.toID hs.modifier == (P.toID p)
                    then Blank newID
                    else hs.modifier
  }

deleteEventSpace : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteEventSpace p hs newID =
  { hs | module_ = if B.toID hs.module_ == (P.toID p)
                    then Blank newID
                    else hs.module_
  }

allPointers : HandlerSpec -> List Pointer
allPointers spec =
  [ B.toP EventName spec.name
  , B.toP EventSpace spec.module_
  , B.toP EventModifier spec.modifier]


