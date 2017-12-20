module IntegrationTest exposing (..)

import Types exposing (..)

trigger : String -> IntegrationTestState
trigger name =
  IntegrationTestExpectation <|
  case name of
    "test_enter_changes_state" -> enterChangesState
    "test_field_access" -> fieldAccess
    n -> Debug.crash ("I have no idea what this test is: " ++ n)


enterChangesState : Model -> Bool
enterChangesState m =
  case m.state of
    Entering (Creating _) -> True
    _ -> False


fieldAccess : Model -> Bool
fieldAccess m =
  False
