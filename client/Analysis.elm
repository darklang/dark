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

getLiveValues : Model -> TLID -> LVDict
getLiveValues m id = getAnalysisResults m id |> .liveValues

getAvailableVarnames : Model -> TLID -> AVDict
getAvailableVarnames m id = getAnalysisResults m id |> .availableVarnames
