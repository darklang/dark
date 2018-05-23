module SpecHeaders exposing (..)

-- builtin

-- lib

-- dark
import Types exposing (..)
import Pointer as P
import Blank as B

spaceOf : HandlerSpec -> HandlerSpace
spaceOf hs =
  let spaceOfStr s =
        let lwr =
            String.toLower s
        in
            if lwr == "http"
            then HSHTTP
            else if lwr == "cron"
            then HSCron
            else HSOther
  in
    case hs.module_ of
      Blank _ -> HSOther
      Flagged _ _ _ _ _ as ff ->
        case B.flattenFF ff of
          F _ s -> spaceOfStr s
          _ -> HSOther
      F _ s ->
        spaceOfStr s

visibleModifier : HandlerSpec -> Bool
visibleModifier hs =
  case spaceOf hs of
    HSHTTP -> True
    HSCron -> True
    HSOther -> False

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
  replace (P.toID pd) (Blank newID) hs

allData : HandlerSpec -> List PointerData
allData spec =
  [ PEventName spec.name
  , PEventSpace spec.module_
  , PEventModifier spec.modifier]



