module Analysis exposing (..)

-- builtin
import Dict
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import Prelude exposing (..)
import Types exposing (..)
import Pointer as P
import Runtime as RT
import Blank as B
import Toplevel as TL
import AST
import StrDict
import IntDict

-- "current" in this indicates that it uses the cursor to pick the right inputValue

currentVarnamesFor : Model -> Maybe (TLID, PointerData) -> List VarName
currentVarnamesFor m target =
  case target of
    Nothing -> []
    Just (tlid, pd) ->
      getCurrentAvailableVarnames m tlid (P.toID pd)

defaultResults : AnalysisResults
defaultResults =
  { liveValues = IntDict.empty
  , availableVarnames = IntDict.empty
  }

getCurrentAnalysisResults : Model -> TLID -> AnalysisResults
getCurrentAnalysisResults m tlid =
  let traceIndex = cursor m tlid
      traceID = Dict.get (deTLID tlid) m.traces
                |> Maybe.andThen (LE.getAt traceIndex)
                |> Maybe.map .id
                |> Maybe.withDefault "invalid trace key"
  in
  -- only handlers have analysis results, but lots of stuff expect this
  -- data to exist. It may be better to not do that, but this is fine
  -- for now.
  Dict.get traceID m.analyses
  |> Maybe.withDefault defaultResults

record : Analyses -> TraceID -> AnalysisResults -> Analyses
record old id result =
  Dict.insert id result old

cursor : Model -> TLID -> Int
cursor m tlid =
  cursor_ m.tlCursors tlid

cursor_ : TLCursors -> TLID -> Int
cursor_ cursors tlid =
  -- We briefly do analysis on a toplevel which does not have an
  -- analysis available, so be careful here.
  Dict.get (deTLID tlid) cursors
  |> Maybe.withDefault 0

setCursor : Model -> TLID -> Int -> Model
setCursor m tlid cursorNum =
  let newCursors = Dict.insert (deTLID tlid) cursorNum m.tlCursors in
  { m | tlCursors =  newCursors}


getCurrentLiveValuesDict : Model -> TLID -> LVDict
getCurrentLiveValuesDict m tlid =
  getCurrentAnalysisResults m tlid
  |> .liveValues

getCurrentLiveValue : Model -> TLID -> ID -> Maybe Dval
getCurrentLiveValue m tlid (ID id) =
  tlid
  |> getCurrentLiveValuesDict m
  |> Dict.get id

getCurrentTipeOf : Model -> TLID -> ID -> Maybe Tipe
getCurrentTipeOf m tlid id =
  case getCurrentLiveValue m tlid id of
    Nothing -> Nothing
    Just dv -> Just (RT.typeOf dv)

getCurrentAvailableVarnamesDict : Model -> TLID -> AVDict
getCurrentAvailableVarnamesDict m tlid =
  getCurrentAnalysisResults m tlid
  |> .availableVarnames

getCurrentAvailableVarnames : Model -> TLID -> ID -> List VarName
getCurrentAvailableVarnames m tlid (ID id) =
  tlid
  |> getCurrentAvailableVarnamesDict m
  |> Dict.get id
  |> Maybe.withDefault []

getTraces : Model -> TLID -> List Trace
getTraces m tlid =
  Dict.get (deTLID tlid) m.traces
  |> Maybe.withDefault []

getCurrentTrace : Model -> TLID -> Maybe Trace
getCurrentTrace m tlid =
  Dict.get (deTLID tlid) m.traces
  |> Maybe.andThen (LE.getAt (cursor m tlid))

replaceFunctionResult : Model -> TLID -> TraceID -> ID -> String -> DvalArgsHash -> Dval -> Model
replaceFunctionResult m tlid traceID callerID fnName hash dval =
  let newResult =
        { fnName = fnName
        , callerID = callerID
        , argHash = hash
        , value = dval
        }
      traces =
        m.traces
        |> Dict.update (deTLID tlid)
             (\ml ->
               ml
               |> Maybe.withDefault [{ id = traceID
                                     , input = StrDict.empty
                                     , functionResults = [newResult]
                                     }]
               |> List.map (\t ->
                              if t.id == traceID
                              then
                                { t | functionResults =
                                         newResult :: t.functionResults
                                }
                              else t)
               |> Just)
  in
  { m | traces = traces }

getArguments : Model -> TLID -> TraceID -> ID -> Maybe (List Dval)
getArguments m tlid traceID callerID =
  let tl = TL.get m tlid in
  case tl of
    Nothing -> Nothing
    Just tl ->
      let caller = TL.find tl callerID
          threadPrevious =
            case TL.rootOf tl of
              Just (PExpr expr) ->
                ME.toList (AST.threadPrevious callerID expr)
              _ -> []
          args =
            case caller of
              Just (PExpr (F _ (FnCall _ args _))) -> threadPrevious ++ args
              _ -> []
          argIDs = List.map B.toID args
          analyses = Dict.get traceID m.analyses
          dvals =
            case analyses of
              Just analyses_ ->
                List.filterMap
                  (\id -> Dict.get (deID id) analyses_.liveValues)
                  argIDs
              Nothing -> []
  in
  if List.length dvals == List.length argIDs
  then Just dvals
  else Nothing
