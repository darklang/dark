module IntegrationTest exposing (..)

-- builtin
import Result exposing (Result (..))

-- dark
import Types exposing (..)
import Toplevel as TL
import Util exposing (deMaybe)

trigger : String -> IntegrationTestState
trigger name =
  IntegrationTestExpectation <|
  case name of
    "test_enter_changes_state" -> enterChangesState
    "test_field_access" -> fieldAccess
    "test_field_access_closes" -> fieldAccessCloses
    "test_pipeline_let_equals" -> pipelineLetEquals
    n -> Debug.crash ("I have no idea what this test is: " ++ n)

pass : TestResult
pass = Ok ()

fail : a -> TestResult
fail v = Err (toString v)




enterChangesState : Model -> TestResult
enterChangesState m =
  case m.state of
    Entering (Creating _) -> pass
    _ -> fail m.state


fieldAccess : Model -> TestResult
fieldAccess m =
  case m.toplevels
       |> List.head
       |> deMaybe
       |> TL.asHandler
       |> deMaybe
       |> .ast
       of
    FieldAccess _ (Variable _ "request") (Filled _ "body") -> pass
    expr -> fail expr


fieldAccessCloses : Model -> TestResult
fieldAccessCloses m =
  case m.state of
    Entering (Filling _ id) ->
      let tl = m.toplevels
               |> List.head
               |> deMaybe in
      if TL.allBlanks tl == TL.specBlanks (tl |> TL.asHandler |> deMaybe)
      then pass
      else fail (TL.allBlanks tl)
    _ ->
      fail m.state

pipelineLetEquals : Model -> TestResult
pipelineLetEquals m =
  -- should be a simple let, not in a pipeline, entering 1 hole
  let astR =
        case m.toplevels
             |> List.head
             |> deMaybe
             |> TL.asHandler
             |> deMaybe
             |> .ast
             of
          Let _ (Filled _ "value") (Value _ "3") (Hole _) ->
            pass
          expr ->
            fail expr
      stateR =
        case m.state of
          Entering _ -> pass
          _ -> fail m.state
  in
      Result.map2 (\() () -> ()) astR stateR



