module IntegrationTest exposing (..)

import Types exposing (..)

trigger : String -> IntegrationExpectation
trigger name =
  Debug.crash <| "success at failing an integration test: " ++ name
