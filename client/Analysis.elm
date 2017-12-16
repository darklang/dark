module Analysis exposing (..)

-- builtin
import Dict

-- dark
import Types exposing (..)

getAnalysisResults : Model -> TLID -> TLAResult
getAnalysisResults m id =
  m.analysis
  |> List.filter (\tlar -> tlar.id == id)
  |> List.head
  -- only handlers have analysis results, but lots of stuff expect this
  -- data to exist. It may be better to not do that, but this is fine
  -- for now.
  |> Maybe.withDefault { id = id
                       , astValue = { value = "null"
                                    , tipe = TNull
                                    , json = "null"
                                    , exc = Nothing
                                    }
                       , liveValues = Dict.empty
                       , availableVarnames = Dict.empty
                       }

getLiveValuesDict : Model -> TLID -> LVDict
getLiveValuesDict m id = getAnalysisResults m id |> .liveValues

getLiveValue : Model -> TLID -> ID -> Maybe LiveValue
getLiveValue m tlid (ID id) =
  tlid
  |> getLiveValuesDict m
  |> Dict.get id

getAvailableVarnamesDict : Model -> TLID -> AVDict
getAvailableVarnamesDict m id = getAnalysisResults m id |> .availableVarnames

getAvailableVarnames : Model -> TLID -> ID -> List VarName
getAvailableVarnames m tlid (ID id) =
  tlid
  |> getAvailableVarnamesDict m
  |> Dict.get id
  |> Maybe.withDefault []

