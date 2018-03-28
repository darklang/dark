module SpecHeaders exposing (..)

-- builtin

-- lib

-- dark
import Types exposing (..)
import Pointer as P
import Blank as B

find : Handler -> PointerData -> Maybe (BlankOr String)
find h p =
  [h.spec.name, h.spec.modifier]
  |> List.filter (\spec -> B.toID spec == P.toID p)
        -- TODO: opportunity to check pointer types
  |> List.head

replaceEventModifier : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replaceEventModifier id value hs =
  { hs | modifier = B.replace id value hs.modifier }

replaceEventName : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replaceEventName id value hs =
  { hs | name = B.replace id value hs.name}

replaceEventSpace : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replaceEventSpace id value hs =
  { hs | module_ = B.replace id value hs.module_}

replace : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replace id value hs =
  hs
  |> replaceEventModifier id value
  |> replaceEventName id value
  |> replaceEventSpace id value

delete : PointerData -> HandlerSpec -> ID -> HandlerSpec
delete pd hs newID =
  replace (P.toID pd) (Blank newID) hs

allData : HandlerSpec -> List PointerData
allData spec =
  [ PEventName spec.name
  , PEventSpace spec.module_
  , PEventModifier spec.modifier]



