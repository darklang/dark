module Analysis exposing (..)

-- dark
import Types exposing (..)
import Util exposing (deMaybe)

getAnalysisResults : Model -> TLID -> TLAResult
getAnalysisResults m id =
  m.analysis
  |> List.filter (\tlar -> tlar.id == id)
  |> List.head
  |> deMaybe

getLiveValues : Model -> TLID -> LVDict
getLiveValues m id = getAnalysisResults m id |> .liveValues

getAvailableVarnames : Model -> TLID -> AVDict
getAvailableVarnames m id = getAnalysisResults m id |> .availableVarnames
