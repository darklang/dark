module SpecHeaders exposing (..)

-- builtin

-- lib

-- dark
import Types exposing (..)
import Pointer as P
import Blank as B

replaceEventModifier : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replaceEventModifier search replacement hs =
  { hs | modifier = B.replace search replacement hs.modifier }

replaceEventName : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replaceEventName search replacement hs =
  { hs | name = B.replace search replacement hs.name }

replaceEventSpace : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replaceEventSpace search replacement hs =
  { hs | module_ = B.replace search replacement hs.module_ }

replace : ID -> BlankOr String -> HandlerSpec -> HandlerSpec
replace search replacement hs =
  hs
  |> replaceEventModifier search replacement
  |> replaceEventName search replacement
  |> replaceEventSpace search replacement

delete : PointerData -> HandlerSpec -> ID -> HandlerSpec
delete pd hs newID =
  replace (P.toID pd |> Debug.log "id") (Blank newID) hs

allData : HandlerSpec -> List PointerData
allData spec =
  [ PEventName spec.name
  , PEventSpace spec.module_
  , PEventModifier spec.modifier]



