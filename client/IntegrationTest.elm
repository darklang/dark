module IntegrationTest exposing (..)

import Types exposing (..)

trigger : String -> IntegrationTestState
trigger name =
  case name of
    "test_empty_integration_test" -> enterChangesState ()
    n -> Debug.crash ("I have no idea what this test is: " ++ n)


enterChangesState : () -> IntegrationTestState
enterChangesState () =
  IntegrationTestExpectation (\m ->
    case m.state of
      Entering (Creating _) -> True
      _ -> False
    )
