module Analysis exposing (..)

-- builtin
import Dict
import List.Extra as LE

-- dark
import Toplevel as TL
import Types exposing (..)
import Pointer as P

varnamesFor : Model -> Maybe (TLID, PointerData) -> List VarName
varnamesFor m target =
  case target of
    Nothing -> m.globals
    Just (tlid, pd) ->
      getAvailableVarnames m tlid (P.toID pd)


getAnalysisResults : Model -> TLID -> List AResult
getAnalysisResults m tlid =
  m.analysis
  |> List.filter (\tlar -> tlar.id == tlid)
  |> List.head
  -- only handlers have analysis results, but lots of stuff expect this
  -- data to exist. It may be better to not do that, but this is fine
  -- for now.
  |> Maybe.map .results
  |> Maybe.withDefault [{ astValue = { value = "null"
                                     , tipe = TNull
                                     , json = "null"
                                     , exc = Nothing
                                     }
                        , liveValues = Dict.empty
                        , availableVarnames = Dict.empty
                        , inputValues = Dict.empty
                        }]

cursor : Model -> TLID -> Int
cursor m tlid =
  -- We briefly do analysis on a toplevel which does not have an
  -- analysis available, so be careful here.
  TL.get m tlid
  |> Maybe.map .cursor
  |> Maybe.withDefault 0

getLiveValuesDict : Model -> TLID -> LVDict
getLiveValuesDict m tlid =
  getAnalysisResults m tlid
  |> List.reverse
  |> LE.getAt (cursor m tlid)
  |> Maybe.map .liveValues
  |> Maybe.withDefault (Dict.empty)

getLiveValue : Model -> TLID -> ID -> Maybe LiveValue
getLiveValue m tlid (ID id) =
  tlid
  |> getLiveValuesDict m
  |> Dict.get id

getTipeOf : Model -> TLID -> ID -> Maybe Tipe
getTipeOf m tlid id =
  case getLiveValue m tlid id of
    Nothing -> Nothing
    Just lv -> Just lv.tipe

getAvailableVarnamesDict : Model -> TLID -> AVDict
getAvailableVarnamesDict m tlid =
  getAnalysisResults m tlid
  |> List.reverse
  |> LE.getAt (cursor m tlid)
  |> Maybe.map .availableVarnames
  |> Maybe.withDefault (Dict.empty)

getAvailableVarnames : Model -> TLID -> ID -> List VarName
getAvailableVarnames m tlid (ID id) =
  tlid
  |> getAvailableVarnamesDict m
  |> Dict.get id
  |> Maybe.withDefault []

