module IntegrationTest exposing (..)

-- builtin
import Result exposing (Result (..))

-- dark
import Types exposing (..)
import Toplevel as TL
import Util exposing (deMaybe)
import Pointer as P

trigger : String -> IntegrationTestState
trigger name =
  IntegrationTestExpectation <|
  case name of
    "test_enter_changes_state" -> enterChangesState
    "test_field_access" -> fieldAccess
    "test_field_access_closes" -> fieldAccessCloses
    "test_field_access_pipes" -> fieldAccessPipes
    "test_field_access_nested" -> fieldAccessNested
    "test_pipeline_let_equals" -> pipelineLetEquals
    "test_pipe_within_let" -> pipeWithinLet
    "test_tabbing_works" -> tabbingWorks
    "test_next_sibling_works" -> nextSiblingWorks
    "test_varbinds_are_editable" -> varbindsAreEditable
    "test_editing_request_edits_request" -> editingRequestEditsRequest
    "test_autocomplete_highlights_on_partial_match" ->
      autocompleteHighlightsOnPartialMatch
    "test_no_request_global_in_non_http_space" ->
      noRequestGlobalInNonHttpSpace
    "test_hover_values_for_varnames" ->
      hoverValuesForVarnames
    "test_pressing_up_doesnt_return_to_start" ->
      pressingUpDoesntReturnToStart
    n -> Debug.crash ("Test " ++ n ++ " not added to IntegrationTest.trigger")

pass : TestResult
pass = Ok ()

fail : a -> TestResult
fail v = Err (toString v)

onlyTL : Model -> Toplevel
onlyTL m =
  m.toplevels
  |> List.head
  |> deMaybe "test"

onlyAST : Model -> Expr
onlyAST m =
  let len = List.length m.toplevels
      _ = if len == 0
          then Debug.crash ("no toplevels")
          else if len > 1
          then Debug.crash ("too many toplevels: " ++ (toString m.toplevels))
          else "nothing to see here" in
  m.toplevels
  |> List.head
  |> deMaybe "test"
  |> TL.asHandler
  |> deMaybe "test"
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
      if TL.blanksWhere (\p -> P.ownerOf p == POAst) tl == []
      then pass
      else fail (TL.allBlanks tl)
    _ ->
      fail m.state

fieldAccessPipes : Model -> TestResult
fieldAccessPipes m =
  case onlyAST m of
    Thread _ [FieldAccess _ (Variable _ "request") (Filled _ "body"), Hole _] -> pass
    expr -> fail expr

fieldAccessNested : Model -> TestResult
fieldAccessNested m =
  case onlyAST m of
    FieldAccess _
      (FieldAccess _
         (FieldAccess _ (Variable _ "request") (Filled _ "body"))
         (Filled _ "field"))
      (Filled _ "field2")
      -> pass
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

pipeWithinLet : Model -> TestResult
pipeWithinLet m =
  case onlyAST m of
    Let _
      (Filled _ "value")
      (Value _ "3")
      (Thread _
        [ Variable _ "value"
        , FnCall _ "assoc" [Hole _, Hole _]]) ->
      pass
    e ->
      fail e



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


editingRequestEditsRequest : Model -> TestResult
editingRequestEditsRequest m =
  case onlyAST m of
    FieldAccess _ (Variable id1 "request") (Blank _) ->
      case m.complete.completions of
        [cs, _, _, _] ->
          case cs of
            [ACVariable "request"] -> pass
            _ -> fail cs
        allcs -> fail allcs
    e -> fail e

autocompleteHighlightsOnPartialMatch : Model -> TestResult
autocompleteHighlightsOnPartialMatch m =
  case onlyAST m of
    FnCall _ "Int::add" _ -> pass
    e -> fail e


noRequestGlobalInNonHttpSpace : Model -> TestResult
noRequestGlobalInNonHttpSpace m =
  case onlyAST m of
    -- this might change but this is the answer for now.
    FnCall _ "Http::bad_request" _ -> pass
    -- Hole _ -> pass
    e -> fail e

hoverValuesForVarnames : Model -> TestResult
hoverValuesForVarnames m =
  let tlid = m.toplevels
           |> List.head
           |> deMaybe "test"
           |> .id
  in pass


pressingUpDoesntReturnToStart : Model -> TestResult
pressingUpDoesntReturnToStart m =
  case onlyAST m of
    FnCall _ "Char::toASCIIChar" _ -> pass
    e -> fail e
