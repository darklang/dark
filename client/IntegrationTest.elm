module IntegrationTest exposing (..)

-- builtin
import Result exposing (Result (..))
import Result.Extra as RE

-- dark
import Types exposing (..)
import Prelude exposing (..)
import Toplevel as TL
import Blank as B
import Pointer as P
import AST
import Analysis

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
    "left_right_works" -> left_right_works
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
    "case_sensitivity" -> case_sensitivity
    "focus_on_ast_in_new_empty_tl" -> focus_on_ast_in_new_empty_tl
    "focus_on_path_in_new_filled_tl" -> focus_on_path_in_new_filled_tl
    "focus_on_cond_in_new_tl_with_if" -> focus_on_cond_in_new_tl_with_if
    "dont_shift_focus_after_filling_last_blank" -> dont_shift_focus_after_filling_last_blank
    "rename_db_fields" -> rename_db_fields
    "rename_db_type" -> rename_db_type
    "paste_right_number_of_blanks" -> paste_right_number_of_blanks
    "paste_keeps_focus" -> paste_keeps_focus
    "nochange_for_failed_paste" -> nochange_for_failed_paste
    n -> Debug.crash ("Test " ++ n ++ " not added to IntegrationTest.trigger")

pass : TestResult
pass = Ok ()

fail : a -> TestResult
fail v = Err (toString v)

onlyTL : Model -> Toplevel
onlyTL m =
  let len = List.length m.toplevels
      _ = if len == 0
          then Debug.crash ("no toplevels")
          else if len > 1
          then Debug.crash ("too many toplevels: " ++ (toString m.toplevels))
          else "nothing to see here" in
  m.toplevels
  |> List.head
  |> deMaybe "onlytl1"

onlyHandler : Model -> Handler
onlyHandler m =
  m
  |> onlyTL
  |> TL.asHandler
  |> deMaybe "onlyhandler"

onlyDB : Model -> DB
onlyDB m =
  m
  |> onlyTL
  |> TL.asDB
  |> deMaybe "onlyDB"

onlyExpr : Model -> NExpr
onlyExpr m =
  m
  |> onlyHandler
  |> .ast
  |> B.asF
  |> deMaybe "onlyast4"



enter_changes_state : Model -> TestResult
enter_changes_state m =
  case m.cursorState of
    Entering (Creating _) -> pass
    _ -> fail m.cursorState


field_access : Model -> TestResult
field_access m =
  case onlyExpr m of
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
  case onlyExpr m of
    Thread [F _ (FieldAccess (F _ (Variable "request")) (F _ "body")), Blank _] -> pass
    expr -> fail expr

field_access_nested : Model -> TestResult
field_access_nested m =
  case onlyExpr m of
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
        case onlyExpr m of
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
  case onlyExpr m of
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
  case onlyExpr m of
    If (Blank _) (F _ (Value "5")) (Blank _) -> pass
    e -> fail e

left_right_works : Model -> TestResult
left_right_works m =
  let h = onlyHandler m in
  case m.cursorState of
    Selecting tlid (Just id) ->
      let pd = TL.getTL m tlid |> \tl -> TL.find tl id in
      case pd of
        Just (PEventSpace _) -> pass
        other -> fail (m.cursorState, other)
    s -> fail m.cursorState


varbinds_are_editable : Model -> TestResult
varbinds_are_editable m =
  case onlyExpr m of
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
  case onlyExpr m of
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
  case onlyExpr m of
    FnCall "Int::add" _ -> pass
    e -> fail e


no_request_global_in_non_http_space : Model -> TestResult
no_request_global_in_non_http_space m =
  case onlyExpr m of
    -- this might change but this is the answer for now.
    FnCall "Http::badRequest" _ -> pass
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
  case onlyExpr m of
    FnCall "Char::toASCIIChar" _ -> pass
    e -> fail e

deleting_selects_the_blank : Model -> TestResult
deleting_selects_the_blank m =
  case onlyExpr m of
    Value "6" -> pass
    e -> fail e


right_number_of_blanks : Model -> TestResult
right_number_of_blanks m =
  case onlyExpr m of
    FnCall "assoc" [Blank _, Blank _, Blank _] -> pass
    e -> fail e

ellen_hello_world_demo : Model -> TestResult
ellen_hello_world_demo m =
  let spec = onlyTL m
             |> TL.asHandler
             |> deMaybe "hw2"
             |> .spec
  in
  case (spec.module_, spec.name, spec.modifier, onlyExpr m) of
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
  case onlyExpr m of
    Let (F _ "myvar") (F _ (Value "5")) (F _ (Value "5")) ->
      pass
    e -> fail e

case_sensitivity : Model -> TestResult
case_sensitivity m =
  if List.length m.toplevels /= 3
  then fail m.toplevels
  else
    m.toplevels
    |> List.map
         (\tl ->
             case tl.data of
               TLDB {name, cols} ->
                 case (name, cols) of
                   ( "TestUnicode"
                   , [ (F _ "cOlUmNnAmE", F _ "Str")
                     , (Blank _, Blank _)
                     ]
                   ) -> pass
                   other -> fail other
               TLHandler h ->
                 case h.ast of
                   F _ (Thread
                     [ F _ (Value "{}")
                     , F _ (FnCall "assoc"
                             [ F _ (Value "\"cOlUmNnAmE\"")
                             , F _ (Value "\"some value\"")])
                     , F _ (FnCall "DB::insert"
                             [F _ (Variable "TestUnicode")])
                       ]) -> pass

                   F id (Thread
                     [ F _ (Variable "TestUnicode")
                     , F _ (FnCall "DB::fetchAll" [])
                     , F _ (FnCall "List::head" [])
                     , F _ (Lambda ["var"]
                             (F _ (FieldAccess
                                     (F _ (Variable "var"))
                                     (F _ "cOlUmNnAmE"))))]) ->

                     Analysis.getLiveValue m tl.id id
                     |> Maybe.map (\lv ->
                       if lv.value == "\"some value\""
                       then pass
                       else fail lv)
                     |> Maybe.withDefault (fail h.ast)
                   _ -> fail h.ast

               other -> fail other)
    |> RE.combine
    |> Result.map (\_ -> ())

focus_on_ast_in_new_empty_tl : Model -> TestResult
focus_on_ast_in_new_empty_tl m =
  case (onlyHandler m).ast of
    Blank id ->
      if idOf m.cursorState == Just id
      then pass
      else fail (id, m.cursorState)
    e -> fail e

focus_on_path_in_new_filled_tl : Model -> TestResult
focus_on_path_in_new_filled_tl m =
  case (onlyHandler m).spec.name of
    Blank id ->
      if idOf m.cursorState == Just id
      then pass
      else fail (id, m.cursorState)
    e -> fail e

focus_on_cond_in_new_tl_with_if : Model -> TestResult
focus_on_cond_in_new_tl_with_if m =
  case onlyExpr m of
    If cond _ _ ->
      if idOf m.cursorState == Just (B.toID cond)
      then pass
      else fail m.cursorState
    e -> fail e

dont_shift_focus_after_filling_last_blank : Model -> TestResult
dont_shift_focus_after_filling_last_blank m =
  case m.cursorState of
    Selecting _ mId ->
      if mId == (m
                 |> onlyHandler
                 |> .spec
                 |> .modifier
                 |> B.toID
                 |> Just)
      then pass
      else fail (m.toplevels, m.cursorState)
    s -> fail (m.toplevels, m.cursorState)

rename_db_fields : Model -> TestResult
rename_db_fields m =
  m.toplevels
  |> List.map (\tl ->
    case tl.data of
      TLDB {name, cols} ->
        case cols of
          [ (F id "field6", F _ "Str")
          , (F _ "field2", F _ "Str")
          , (Blank _, Blank _)] ->
            case m.cursorState of
              Selecting _ (Just sid) ->
                if sid == id
                then pass
                else fail (cols, m.cursorState)
              _ -> fail m.cursorState
          _ -> fail cols
      _ -> pass)
  |> RE.combine
  |> Result.map (\_ -> ())

rename_db_type : Model -> TestResult
rename_db_type m =
  m.toplevels
  |> List.map (\tl ->
    case tl.data of
      TLDB {name, cols} ->
        case cols of
          [ (F id "field1", F _ "Int")
          , (F _ "field2", F _ "Int")
          , (Blank _, Blank _)] ->
            case m.cursorState of
              Selecting _ (Just sid) ->
                if sid == id
                then pass
                else fail (cols, m.cursorState)
              _ -> fail m.cursorState
          _ -> fail cols
      _ -> pass)
  |> RE.combine
  |> Result.map (\_ -> ())

paste_right_number_of_blanks : Model -> TestResult
paste_right_number_of_blanks m =
   m.toplevels
  |> List.map (\tl ->
    case tl.data of
      TLHandler {ast} ->
        case ast of
          F _ (Thread [_, F _ (FnCall "-" [Blank _])]) ->
            pass
          F _ (FnCall "-" [Blank _, Blank _]) ->
            pass -- ignore this TL
          _ -> fail ast
      _ -> fail ("Shouldn't be other handlers here", tl.data))
  |> RE.combine
  |> Result.map (\_ -> ())

paste_keeps_focus : Model -> TestResult
paste_keeps_focus m =
  case onlyExpr m of
    FnCall "+" [F _ (Value "3"), F id (Value "3")] as fn ->
      case m.cursorState of
        Selecting _ sid ->
          if Just id == sid
          then pass
          else fail (fn, m.cursorState)
        _ -> fail (fn, m.cursorState)
    other -> fail other

nochange_for_failed_paste : Model -> TestResult
nochange_for_failed_paste m =
  case onlyExpr m of
    Let (F id "x") (F _ (Value "2")) _ ->
      case m.cursorState of
        Selecting _ sid ->
          if Just id == sid
          then pass
          else fail m.cursorState
        _ -> fail m.cursorState
    other -> fail other
