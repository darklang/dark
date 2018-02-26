module Analysis exposing (..)

-- builtin
import Dict
import List.Extra as LE

-- dark
import Toplevel as TL
import Types exposing (..)
import Pointer as P

varnamesFor : Model -> Maybe (TLID, Pointer) -> List VarName
varnamesFor m target =
  case target of
    Nothing -> m.globals
    Just (tlid, p) ->
      getAvailableVarnames
        m
        tlid
        (P.idOf p)


getAnalysisResults : Model -> TLID -> List AResult
getAnalysisResults m id =
  m.analysis
  |> List.filter (\tlar -> tlar.id == id)
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
                        }]

getLiveValuesDict : Model -> TLID -> LVDict
getLiveValuesDict m id =
  let cursor = TL.getTL m id |> .cursor
  in
  getAnalysisResults m id
  |> List.reverse
  |> LE.getAt cursor
  |> Maybe.map .liveValues
  |> Maybe.withDefault (Dict.empty)

getLiveValue : Model -> TLID -> ID -> Maybe LiveValue
getLiveValue m tlid (ID id) =
  tlid
  |> getLiveValuesDict m
  |> Dict.get id

getAvailableVarnamesDict : Model -> TLID -> AVDict
getAvailableVarnamesDict m id =
  let cursor = TL.getTL m id |> .cursor
  in
  getAnalysisResults m id
  |> List.reverse
  |> LE.getAt cursor
  |> Maybe.map .availableVarnames
  |> Maybe.withDefault (Dict.empty)

getAvailableVarnames : Model -> TLID -> ID -> List VarName
getAvailableVarnames m tlid (ID id) =
  tlid
  |> getAvailableVarnamesDict m
  |> Dict.get id
  |> Maybe.withDefault []

