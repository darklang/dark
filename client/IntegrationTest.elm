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
    "test_field_access_pipes" -> fieldAccessPipes
    "test_pipeline_let_equals" -> pipelineLetEquals
    "test_tabbing_works" -> tabbingWorks
    "test_next_sibling_works" -> nextSiblingWorks
    "test_varbinds_are_editable" -> varbindsAreEditable
    n -> Debug.crash ("Test " ++ n ++ " not added to IntegrationTest.trigger")

pass : TestResult
pass = Ok ()

fail : a -> TestResult
fail v = Err (toString v)

onlyTL : Model -> Toplevel
onlyTL m =
  m.toplevels
  |> List.head
  |> deMaybe

onlyAST : Model -> AST
onlyAST m =
  m.toplevels
  |> List.head
  |> deMaybe
  |> TL.asHandler
  |> deMaybe
  |> .ast



enterChangesState : Model -> TestResult
enterChangesState m =
  case m.state of
    Entering (Creating _) -> pass
    _ -> fail m.state


fieldAccess : Model -> TestResult
fieldAccess m =
  case onlyAST m of
    FieldAccess _ (Variable _ "request") (Filled _ "body") -> pass
    expr -> fail expr


fieldAccessCloses : Model -> TestResult
fieldAccessCloses m =
  case m.state of
    Entering (Filling _ id) ->
      let tl = onlyTL m in
      if TL.allBlanks tl == TL.specBlanks (tl |> TL.asHandler |> deMaybe)
      then pass
      else fail (TL.allBlanks tl)
    _ ->
      fail m.state

fieldAccessPipes : Model -> TestResult
fieldAccessPipes m =
  case onlyAST m of
    Thread _ [FieldAccess _ (Variable _ "request") (Filled _ "body"), Hole _] -> pass
    expr -> fail expr




pipelineLetEquals : Model -> TestResult
pipelineLetEquals m =
  -- should be a simple let, not in a pipeline, entering 1 hole
  let astR =
        case onlyAST m of
          Let _ (Filled _ "value") (Value _ "3") (Hole _) ->
            pass
          e ->
            fail e
      stateR =
        case m.state of
          Entering _ -> pass
          _ -> fail m.state
  in
      Result.map2 (\() () -> ()) astR stateR


tabbingWorks : Model -> TestResult
tabbingWorks m =
  case onlyAST m of
    If _ (Hole _) (Value _ "5") (Hole _) -> pass
    e -> fail e

nextSiblingWorks : Model -> TestResult
nextSiblingWorks m =
  case onlyAST m of
    Let _ (Blank _) (Hole id1)  (Hole _) ->
      case m.state of
        Selecting _ (Just (PBlank _ id2)) ->
          if id1 == id2
          then pass
          else fail (id1, id2)
        s -> fail m.state
    e -> fail e


varbindsAreEditable : Model -> TestResult
varbindsAreEditable m =
  case onlyAST m of
    Let _ (Filled id1 "var") (Hole _)  (Hole _) ->
      case m.state of
        Entering (Filling _ (PFilled _ id2)) ->
          if id1 == id2
          then pass
          else fail (id1, id2)
        s -> fail m.state
    e -> fail e
