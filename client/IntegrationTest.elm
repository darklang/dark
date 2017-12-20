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
    FieldAccess _ (Variable _ "request") (Full _ "body") -> pass
    expr -> fail expr


fieldAccessCloses : Model -> TestResult
fieldAccessCloses m =
  case m.state of
    Selecting _ _ -> pass
    _ -> fail m.state
