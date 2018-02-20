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

replaceHTTPVerbBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceHTTPVerbBlank id value hs =
  { hs | modifier = if blankOrID hs.modifier == id
                    then Filled (blankOrID hs.modifier) value
                    else hs.modifier
  }
replaceHTTPRouteBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceHTTPRouteBlank id value hs =
  { hs | name = if blankOrID hs.name == id
                then Filled (blankOrID hs.name) value
                else hs.name
  }


deleteHTTPRouteBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteHTTPRouteBlank p hs newID =
  { hs | name = if blankOrID hs.name == (P.idOf p)
                then Blank newID
                else hs.name
  }

deleteHTTPVerbBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteHTTPVerbBlank p hs newID =
  { hs | modifier = if blankOrID hs.modifier == (P.idOf p)
                    then Blank newID
                    else hs.modifier
  }

allPointers : HandlerSpec -> List Pointer
allPointers spec =
  [ P.blankTo HTTPRoute spec.name
  , P.blankTo HTTPVerb spec.modifier]


