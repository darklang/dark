module SpecHeaders exposing (..)

-- builtin

-- lib

-- dark
import Types exposing (..)
import Pointer as P

find : Handler -> Pointer -> Maybe (BlankOr String)
find h p =
  [h.spec.name, h.spec.modifier]
  |> List.filter (\spec -> blankOrID spec == P.idOf p)
        -- TODO: opportunity to check pointer types
  |> List.head

replaceEventModifierBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceEventModifierBlank id value hs =
  { hs | modifier = if blankOrID hs.modifier == id
                    then Filled (blankOrID hs.modifier) value
                    else hs.modifier
  }
replaceEventNameBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceEventNameBlank id value hs =
  { hs | name = if blankOrID hs.name == id
                then Filled (blankOrID hs.name) value
                else hs.name
  }

replaceEventSpaceBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceEventSpaceBlank id value hs =
  { hs | module_ = if blankOrID hs.module_ == id
                    then Filled (blankOrID hs.module_) value
                    else hs.module_
  }


deleteEventNameBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteEventNameBlank p hs newID =
  { hs | name = if blankOrID hs.name == (P.idOf p)
                then Blank newID
                else hs.name
  }

deleteEventModifierBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteEventModifierBlank p hs newID =
  { hs | modifier = if blankOrID hs.modifier == (P.idOf p)
                    then Blank newID
                    else hs.modifier
  }

deleteEventSpaceBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteEventSpaceBlank p hs newID =
  { hs | module_ = if blankOrID hs.module_ == (P.idOf p)
                    then Blank newID
                    else hs.module_
  }

allPointers : HandlerSpec -> List Pointer
allPointers spec =
  [ P.blankTo EventName spec.name
  , P.blankTo EventSpace spec.module_
  , P.blankTo EventModifier spec.modifier]


