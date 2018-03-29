module IntegrationTest exposing (..)

-- builtin
import Result exposing (Result (..))

-- dark
import Types exposing (..)
import Toplevel as TL
import Util exposing (deMaybe)
import Blank as B
import Pointer as P
import AST

trigger : String -> IntegrationTestState
trigger test_name =
  let name = String.dropLeft 5 test_name in
  IntegrationTestExpectation <|
  case name of
    "enter_changes_state" -> enter_changes_state
    "field_access" -> field_access
    "field_access_closes" -> field_access_closes
    "field_access_pipes" -> field_access_pipes
    "field_access_nested" -> field_access_nested
    "pipeline_let_equals" -> pipeline_let_equals
    "pipe_within_let" -> pipe_within_let
    "tabbing_works" -> tabbing_works
    "next_sibling_works" -> next_sibling_works
    "varbinds_are_editable" -> varbinds_are_editable
    "editing_request_edits_request" -> editing_request_edits_request
    "autocomplete_highlights_on_partial_match" -> autocomplete_highlights_on_partial_match
    "no_request_global_in_non_http_space" -> no_request_global_in_non_http_space
    "hover_values_for_varnames" -> hover_values_for_varnames
    "pressing_up_doesnt_return_to_start" -> pressing_up_doesnt_return_to_start
    "deleting_selects_the_blank" -> deleting_selects_the_blank
    "right_number_of_blanks" -> right_number_of_blanks
    "ellen_hello_world_demo" -> ellen_hello_world_demo
    "editing_headers" -> editing_headers
    "tabbing_through_let" -> tabbing_through_let

    n -> Debug.crash ("Test " ++ n ++ " not added to IntegrationTest.trigger")

pass : TestResult
pass = Ok ()

fail : a -> TestResult
fail v = Err (toString v)

onlyTL : Model -> Toplevel
onlyTL m =
  m.toplevels
  |> List.head
  |> deMaybe "test1"

onlyAST : Model -> NExpr
onlyAST m =
  let len = List.length m.toplevels
      _ = if len == 0
          then Debug.crash ("no toplevels")
          else if len > 1
          then Debug.crash ("too many toplevels: " ++ (toString m.toplevels))
          else "nothing to see here" in
  m.toplevels
  |> List.head
  |> deMaybe "test2"
  |> TL.asHandler
  |> deMaybe "test3"
  |> .ast
  |> B.asF
  |> deMaybe "test4"



enter_changes_state : Model -> TestResult
enter_changes_state m =
  case m.cursorState of
    Entering (Creating _) -> pass
    _ -> fail m.cursorState


field_access : Model -> TestResult
field_access m =
  case onlyAST m of
    FieldAccess (F _ (Variable "request")) (F _ "body") -> pass
    expr -> fail expr


field_access_closes : Model -> TestResult
field_access_closes m =
  case m.cursorState of
    Entering (Filling _ id) ->
      let ast = onlyTL m |> TL.asHandler |> deMaybe "test" |> .ast in
      if (AST.allData ast |> List.filter P.isBlank) == []
      then pass
      else fail (TL.allBlanks (onlyTL m))
    _ ->
      fail m.cursorState

field_access_pipes : Model -> TestResult
field_access_pipes m =
  case onlyAST m of
    Thread [F _ (FieldAccess (F _ (Variable "request")) (F _ "body")), Blank _] -> pass
    expr -> fail expr

field_access_nested : Model -> TestResult
field_access_nested m =
  case onlyAST m of
    FieldAccess
      (F _ (FieldAccess
         (F _ (FieldAccess (F _ (Variable "request")) (F _ "body")))
         (F _ "field")))
      (F _ "field2")
      -> pass
    expr -> fail expr


pipeline_let_equals : Model -> TestResult
pipeline_let_equals m =
  -- should be a simple let, not in a pipeline, entering 1 blank
  let astR =
        case onlyAST m of
          Let (F _ "value") (F _ (Value "3")) (Blank _) ->
            pass
          e ->
            fail e
      stateR =
        case m.cursorState of
          Entering _ -> pass
          _ -> fail m.cursorState
  in
      Result.map2 (\() () -> ()) astR stateR

pipe_within_let : Model -> TestResult
pipe_within_let m =
  case onlyAST m of
    Let
      (F _ "value")
      (F _ (Value "3"))
      (F _ (Thread
        [ F _ (Variable "value")
        , F _ (FnCall "assoc" [Blank _, Blank _])])) ->
      pass
    e ->
      fail e



tabbing_works : Model -> TestResult
tabbing_works m =
  case onlyAST m of
    If (Blank _) (F _ (Value "5")) (Blank _) -> pass
    e -> fail e

next_sibling_works : Model -> TestResult
next_sibling_works m =
  case onlyAST m of
    Let (Blank _) (Blank id1) (Blank _) ->
      case m.cursorState of
        Selecting _ (Just id2) ->
          if id1 == id2
          then pass
          else fail (id1, id2)
        s -> fail m.cursorState
    e -> fail e


varbinds_are_editable : Model -> TestResult
varbinds_are_editable m =
  case onlyAST m of
    Let (F id1 "var") (Blank _) (Blank _) as l ->
      case m.cursorState of
        Entering (Filling _ id2)->
          if id1 == id2
          then pass
          else fail (l, m.cursorState)
        s -> fail (l, m.cursorState)
    e -> fail e


editing_request_edits_request : Model -> TestResult
editing_request_edits_request m =
  case onlyAST m of
    FieldAccess (F id1 (Variable "request")) (Blank _) ->
      case m.complete.completions of
        [cs, _, _, _] ->
          case cs of
            [ACVariable "request"] -> pass
            _ -> fail cs
        allcs -> fail allcs
    e -> fail e

autocomplete_highlights_on_partial_match : Model -> TestResult
autocomplete_highlights_on_partial_match m =
  case onlyAST m of
    FnCall "Int::add" _ -> pass
    e -> fail e


no_request_global_in_non_http_space : Model -> TestResult
no_request_global_in_non_http_space m =
  case onlyAST m of
    -- this might change but this is the answer for now.
    FnCall "Http::bad_request" _ -> pass
    -- Blank _ -> pass
    e -> fail e

hover_values_for_varnames : Model -> TestResult
hover_values_for_varnames m =
  let tlid = m.toplevels
           |> List.head
           |> deMaybe "test"
           |> .id
  in pass


pressing_up_doesnt_return_to_start : Model -> TestResult
pressing_up_doesnt_return_to_start m =
  case onlyAST m of
    FnCall "Char::toASCIIChar" _ -> pass
    e -> fail e

deleting_selects_the_blank : Model -> TestResult
deleting_selects_the_blank m =
  case onlyAST m of
    Value "6" -> pass
    e -> fail e


right_number_of_blanks : Model -> TestResult
right_number_of_blanks m =
  case onlyAST m of
    FnCall "assoc" [Blank _, Blank _, Blank _] -> pass
    e -> fail e

ellen_hello_world_demo : Model -> TestResult
ellen_hello_world_demo m =
  let spec = onlyTL m
             |> TL.asHandler
             |> deMaybe "hw2"
             |> .spec
  in
  case (spec.module_, spec.name, spec.modifier, onlyAST m) of
    ( F _ "HTTP"
    , F _ "/hello"
    , F _ "GET"
    , Value "\"Hello world!\"") ->
      pass
    other -> fail other

editing_headers : Model -> TestResult
editing_headers m =
  let spec = onlyTL m
             |> TL.asHandler
             |> deMaybe "hw2"
             |> .spec
  in
  case (spec.module_, spec.name, spec.modifier) of
    ( F _ "HTTP"
    , F _ "/myroute"
    , F _ "GET") ->
      pass
    other -> fail other

tabbing_through_let : Model -> TestResult
tabbing_through_let m =
  case onlyAST m of
    Let (F _ "myvar") (F _ (Value "5")) (F _ (Value "5")) ->
      pass
    e -> fail e
