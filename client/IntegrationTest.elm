module IntegrationTest exposing (..)

import Types exposing (..)

trigger : String -> IntegrationTestState
trigger name =
  case name of
    "test_enter_changes_state" -> enterChangesState ()
    n -> Debug.crash ("I have no idea what this test is: " ++ n)


enterChangesState : () -> IntegrationTestState
enterChangesState () =
  IntegrationTestExpectation (\m ->
    let _ = Debug.log "state: " m.state in
    case m.state of
      Entering (Creating _) -> True
      _ -> False
    )
