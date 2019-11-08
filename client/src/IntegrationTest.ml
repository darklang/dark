open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TL = Toplevel
module TD = TLIDDict

let pass : testResult = Ok ()

let fail ?(f : 'a -> string = Js.String.make) (v : 'a) : testResult =
  Error (f v)


let testIntOption ~(errMsg : string) ~(expected : int) ~(actual : int option) :
    testResult =
  match actual with
  | Some a when a = expected ->
      pass
  | Some a ->
      fail
        ( errMsg
        ^ " (Actual: "
        ^ string_of_int a
        ^ ", Expected: "
        ^ string_of_int expected
        ^ ")" )
  | None ->
      fail
        (errMsg ^ " (Actual: None, Expected: " ^ string_of_int expected ^ ")")


let testInt ~(errMsg : string) ~(expected : int) ~(actual : int) : testResult =
  if actual = expected
  then pass
  else
    fail
      ( errMsg
      ^ " (Actual: "
      ^ string_of_int actual
      ^ ", Expected: "
      ^ string_of_int expected
      ^ ")" )


let showToplevels tls = tls |> TD.values |> show_list ~f:show_toplevel

let deOption msg v = match v with Some v -> v | None -> Debug.crash msg

let onlyTL (m : model) : toplevel =
  let tls = TL.all m in
  let len = TD.count tls in
  ignore
    ( if len = 0
    then Debug.crash "no toplevels"
    else if len > 1
    then Debug.crash ("too many toplevels: " ^ showToplevels tls)
    else "nothing to see here" ) ;
  tls |> StrDict.values |> List.head |> deOption "onlytl1"


let onlyHandler (m : model) : handler =
  m |> onlyTL |> TL.asHandler |> deOption "onlyhandler"


let onlyDB (m : model) : db = m |> onlyTL |> TL.asDB |> deOption "onlyDB"

let onlyExpr (m : model) : nExpr =
  m |> onlyHandler |> (fun x -> x.ast) |> B.asF |> deOption "onlyast4"


let enter_changes_state (m : model) : testResult =
  match m.cursorState with
  | Entering (Creating _) ->
      pass
  | _ ->
      fail ~f:show_cursorState m.cursorState


let field_access (m : model) : testResult =
  match onlyExpr m with
  | FieldAccess (F (_, Variable "request"), F (_, "body")) ->
      pass
  | expr ->
      fail ~f:show_nExpr expr


let field_access_closes (m : model) : testResult =
  match m.cursorState with
  | Entering (Filling (_, _)) ->
      let ast =
        onlyTL m |> TL.asHandler |> deOption "test" |> fun x -> x.ast
      in
      if AST.allData ast |> List.filter ~f:P.isBlank = []
      then pass
      else fail ~f:(show_list ~f:show_pointerData) (TL.allBlanks (onlyTL m))
  | _ ->
      fail ~f:show_cursorState m.cursorState


let field_access_pipes (m : model) : testResult =
  match onlyExpr m with
  | Thread
      [F (_, FieldAccess (F (_, Variable "request"), F (_, "body"))); Blank _]
    ->
      pass
  | expr ->
      fail ~f:show_nExpr expr


let field_access_nested (m : model) : testResult =
  match onlyExpr m with
  | FieldAccess
      ( F
          ( _
          , FieldAccess
              ( F (_, FieldAccess (F (_, Variable "request"), F (_, "body")))
              , F (_, "field") ) )
      , F (_, "field2") ) ->
      pass
  | expr ->
      fail ~f:show_nExpr expr


let pipeline_let_equals (m : model) : testResult =
  (* should be a simple let, not in a pipeline, entering 1 blank *)
  let astR =
    match onlyExpr m with
    | Let (F (_, "value"), F (_, Value "3"), Blank _) ->
        pass
    | e ->
        fail ~f:show_nExpr e
  in
  let stateR =
    match m.cursorState with
    | Entering _ ->
        pass
    | _ ->
        fail ~f:show_cursorState m.cursorState
  in
  Result.map2 ~f:(fun () () -> ()) astR stateR


let pipe_within_let (m : model) : testResult =
  match onlyExpr m with
  | Let
      ( F (_, "value")
      , F (_, Value "3")
      , F
          ( _
          , Thread
              [ F (_, Variable "value")
              ; F (_, FnCall (F (_, "Int::add"), [Blank _], _)) ] ) ) ->
      pass
  | e ->
      fail ~f:show_nExpr e


let tabbing_works (m : model) : testResult =
  match onlyExpr m with
  | If (Blank _, F (_, Value "5"), Blank _) ->
      pass
  | e ->
      fail ~f:show_nExpr e


let varbinds_are_editable (m : model) : testResult =
  match onlyExpr m with
  | Let (F (id1, "var"), Blank _, Blank _) as l ->
    ( match m.cursorState with
    | Entering (Filling (_, id2)) ->
        if id1 = id2
        then pass
        else fail (show_nExpr l ^ ", " ^ show_cursorState m.cursorState)
    | _ ->
        fail (show_nExpr l ^ ", " ^ show_cursorState m.cursorState) )
  | e ->
      fail ~f:show_nExpr e


let editing_request_edits_request (m : model) : testResult =
  match onlyExpr m with
  | FieldAccess (F (_, Variable "request"), Blank _) ->
    ( match m.complete.completions with
    | [ACVariable ("request", Some _); ACFunction {fnName = "Http::badRequest"}]
      ->
        pass
    | cs ->
        fail ~f:(show_list ~f:show_autocompleteItem) cs )
  | e ->
      fail ~f:show_nExpr e


let autocomplete_highlights_on_partial_match (m : model) : testResult =
  match onlyExpr m with
  | FnCall (F (_, "Int::add"), _, _) ->
      pass
  | e ->
      fail ~f:show_nExpr e


let no_request_global_in_non_http_space (m : model) : testResult =
  (* this might change but this is the answer for now. *)
  match onlyExpr m with
  | FnCall (F (_, "Http::badRequest"), _, _) ->
      pass
  | e ->
      fail ~f:show_nExpr e


let hover_values_for_varnames (m : model) : testResult =
  ignore (TL.all m |> TD.values |> List.head |> deOption "test") ;
  pass


let pressing_up_doesnt_return_to_start (m : model) : testResult =
  match onlyExpr m with
  | FnCall (F (_, "Int::add"), _, _) ->
      pass
  | e ->
      fail ~f:show_nExpr e


let deleting_selects_the_blank (m : model) : testResult =
  match onlyExpr m with Value "6" -> pass | e -> fail ~f:show_nExpr e


let right_number_of_blanks (m : model) : testResult =
  match onlyExpr m with
  | FnCall (F (_, "Dict::set"), [Blank _; Blank _; Blank _], _) ->
      pass
  | e ->
      fail ~f:show_nExpr e


let ellen_hello_world_demo (m : model) : testResult =
  let spec = onlyTL m |> TL.asHandler |> deOption "hw2" |> fun x -> x.spec in
  match ((spec.space, spec.name), (spec.modifier, onlyExpr m)) with
  | (F (_, "HTTP"), F (_, "/hello")), (F (_, "GET"), Value "\"Hello world!\"")
    ->
      pass
  | other ->
      fail other


let editing_does_not_deselect (m : model) : testResult =
  match m.cursorState with
  | Entering (Filling (tlid, id)) ->
    ( match TL.getPD m tlid id with
    | Some (PExpr (F (_, Value "\"hello zane\""))) ->
        pass
    | other ->
        fail other )
  | other ->
      fail ~f:show_cursorState other


let editing_headers (m : model) : testResult =
  let spec = onlyTL m |> TL.asHandler |> deOption "hw2" |> fun x -> x.spec in
  match (spec.space, spec.name, spec.modifier) with
  | F (_, "HTTP"), F (_, "/myroute"), F (_, "GET") ->
      pass
  | other ->
      fail other


let switching_from_http_space_removes_leading_slash (m : model) : testResult =
  let spec = onlyTL m |> TL.asHandler |> deOption "hw2" |> fun x -> x.spec in
  match (spec.space, spec.name, spec.modifier) with
  | F (_, newSpace), F (_, "spec_name"), _ when newSpace != "HTTP" ->
      pass
  | other ->
      fail other


let switching_from_http_to_cron_space_removes_leading_slash =
  switching_from_http_space_removes_leading_slash


let switching_from_http_to_repl_space_removes_leading_slash =
  switching_from_http_space_removes_leading_slash


let switching_from_http_space_removes_variable_colons (m : model) : testResult
    =
  let spec = onlyTL m |> TL.asHandler |> deOption "hw2" |> fun x -> x.spec in
  match (spec.space, spec.name, spec.modifier) with
  | F (_, newSpace), F (_, "spec_name/variable"), _ when newSpace != "HTTP" ->
      pass
  | other ->
      fail other


let switching_to_http_space_adds_slash (m : model) : testResult =
  let spec = onlyTL m |> TL.asHandler |> deOption "hw2" |> fun x -> x.spec in
  match (spec.space, spec.name, spec.modifier) with
  | F (_, "HTTP"), F (_, "/spec_name"), _ ->
      pass
  | other ->
      fail other


let switching_from_default_repl_space_removes_name (m : model) : testResult =
  let spec = onlyTL m |> TL.asHandler |> deOption "hw2" |> fun x -> x.spec in
  match (spec.space, spec.name, spec.modifier) with
  | F (_, newSpace), _, _ when newSpace != "REPL" ->
      pass
  | other ->
      fail other


let tabbing_through_let (m : model) : testResult =
  match onlyExpr m with
  | Let (F (_, "myvar"), F (_, Value "5"), F (_, Value "5")) ->
      pass
  | e ->
      fail ~f:show_nExpr e


let focus_on_ast_in_new_empty_tl (m : model) : testResult =
  match (onlyHandler m).ast with
  | Blank id ->
      if idOf m.cursorState = Some id
      then pass
      else fail (show_id id ^ ", " ^ show_cursorState m.cursorState)
  | e ->
      fail ~f:show_expr e


let focus_on_cond_in_new_tl_with_if (m : model) : testResult =
  match onlyExpr m with
  | If (cond, _, _) ->
      if idOf m.cursorState = Some (B.toID cond)
      then pass
      else fail ~f:show_cursorState m.cursorState
  | e ->
      fail ~f:show_nExpr e


let dont_shift_focus_after_filling_last_blank (m : model) : testResult =
  let tls = TL.all m in
  match m.cursorState with
  | Selecting (_, mId) ->
      if mId
         = (m |> onlyHandler |> (fun x -> x.ast) |> B.toID |> fun x -> Some x)
      then pass
      else fail (showToplevels tls ^ ", " ^ show_cursorState m.cursorState)
  | _ ->
      fail (showToplevels tls ^ ", " ^ show_cursorState m.cursorState)


let rename_db_fields (m : model) : testResult =
  m.dbs
  |> TD.mapValues ~f:(fun {cols} ->
         match cols with
         | [ (F (_, "field6"), F (_, "String"))
           ; (F (_, "field2"), F (_, "String"))
           ; (Blank _, Blank _) ] ->
           ( match m.cursorState with
           | Selecting (_, None) ->
               pass
           | _ ->
               fail ~f:show_cursorState m.cursorState )
         | _ ->
             fail ~f:(show_list ~f:show_dbColumn) cols )
  |> Result.combine
  |> Result.map (fun _ -> ())


let rename_db_type (m : model) : testResult =
  m.dbs
  |> TD.mapValues ~f:(fun {cols; dbTLID} ->
         match cols with
         (* this was previously an Int *)
         | [ (F (_, "field1"), F (_, "String"))
           ; (F (_, "field2"), F (_, "Int"))
           ; (Blank _, Blank _) ] ->
           ( match m.cursorState with
           | Selecting (tlid, None) ->
               if tlid = dbTLID
               then pass
               else
                 fail
                   ( show_list ~f:show_dbColumn cols
                   ^ ", "
                   ^ show_cursorState m.cursorState )
           | _ ->
               fail ~f:show_cursorState m.cursorState )
         | _ ->
             fail ~f:(show_list ~f:show_dbColumn) cols )
  |> Result.combine
  |> Result.map (fun _ -> ())


let paste_right_number_of_blanks (m : model) : testResult =
  m.handlers
  |> TD.mapValues ~f:(fun {ast} ->
         match ast with
         | F (_, Thread [_; F (_, FnCall (F (_, "-"), [Blank _], _))]) ->
             pass
         | F (_, FnCall (F (_, "-"), [Blank _; Blank _], _)) ->
             pass (* ignore this TL *)
         | _ ->
             fail ~f:show_expr ast )
  |> Result.combine
  |> Result.map (fun _ -> ())


let paste_keeps_focus (m : model) : testResult =
  match onlyExpr m with
  | FnCall (F (_, "+"), [F (_, Value "3"); F (id, Value "3")], _) as fn ->
    ( match m.cursorState with
    | Selecting (_, sid) ->
        if Some id = sid
        then pass
        else fail (show_nExpr fn ^ ", " ^ show_cursorState m.cursorState)
    | _ ->
        fail (show_nExpr fn ^ ", " ^ show_cursorState m.cursorState) )
  | other ->
      fail ~f:show_nExpr other


let nochange_for_failed_paste (m : model) : testResult =
  match onlyExpr m with
  | Let (F (id, "x"), F (_, Value "2"), _) ->
    ( match m.cursorState with
    | Selecting (_, sid) ->
        if Some id = sid then pass else fail ~f:show_cursorState m.cursorState
    | _ ->
        fail ~f:show_cursorState m.cursorState )
  | other ->
      fail ~f:show_nExpr other


let feature_flag_works (m : model) : testResult =
  let h = onlyHandler m in
  let ast = h.ast in
  match ast with
  | F
      ( _
      , Let
          ( F (_, "a")
          , F (_, Value "13")
          , F
              ( id
              , FeatureFlag
                  ( F (_, "myflag")
                  , F
                      ( _
                      , FnCall
                          ( F (_, "Int::greaterThan")
                          , [F (_, Variable "a"); F (_, Value "10")]
                          , _ ) )
                  , F (_, Value "\"A\"")
                  , F (_, Value "\"B\"") ) ) ) ) ->
      let res =
        Analysis.getSelectedTraceID m h.hTLID
        |> Option.andThen ~f:(Analysis.getLiveValue m id)
      in
      ( match res with
      | Some val_ ->
          if val_ = DStr "B" then pass else fail (show_expr ast, val_)
      | _ ->
          fail (show_expr ast, res) )
  | _ ->
      fail (show_expr ast, show_cursorState m.cursorState)


let feature_flag_in_function (m : model) : testResult =
  let fun_ = m.userFunctions |> TD.values |> List.head in
  match fun_ with
  | Some f ->
    ( match f.ufAST with
    | F
        ( _
        , FnCall
            ( F (_, "+")
            , [ F
                  ( _
                  , FeatureFlag
                      ( F (_, "myflag")
                      , F (_, Value "true")
                      , F (_, Value "5")
                      , F (_, Value "3") ) )
              ; F (_, Value "5") ]
            , NoRail ) ) ->
        pass
    (* TODO: validate result should evaluate true turning  5 + 5 --> 3 + 5 == 8 *)
    (* let res = Analysis.getLiveValue m f.tlid id in *)
    (* case res of *)
    (*   Just val -> if val.value == "\"8\"" then pass else fail (f.ast, value) *)
    (*   _ -> fail (f.ast, res) *)
    | _ ->
        fail ~f:show_expr f.ufAST )
  | None ->
      fail "Cant find function"


let simple_tab_ordering (m : model) : testResult =
  let ast = onlyHandler m |> fun x -> x.ast in
  match ast with
  | F (_, Let (Blank _, F (_, Value "4"), Blank id)) ->
    ( match m.cursorState with
    | Entering (Filling (_, sid)) ->
        if id = sid
        then pass
        else fail (show_expr ast, show_cursorState m.cursorState, id)
    | _ ->
        fail (show_expr ast, show_cursorState m.cursorState, id) )
  | _ ->
      fail (show_expr ast, show_cursorState m.cursorState)


let variable_extraction (m : model) : testResult =
  let ast = onlyHandler m |> fun x -> x.ast in
  match ast with
  | F
      ( _
      , Let
          ( F (_, "foo")
          , F (_, Value "1")
          , F
              ( _
              , Let
                  ( F (_, "bar")
                  , F (_, Value "2")
                  , F
                      ( _
                      , Let
                          ( F (_, "new_variable")
                          , F
                              ( _
                              , FnCall
                                  ( F (_, "+")
                                  , [ F (_, Variable "foo")
                                    ; F (_, Variable "bar") ]
                                  , _ ) )
                          , F
                              ( _
                              , Let
                                  ( F (_, "baz")
                                  , F (_, Value "5")
                                  , F (_, Variable "new_variable") ) ) ) ) ) )
          ) ) ->
      pass
  | _ ->
      fail (show_expr ast ^ ", " ^ show_cursorState m.cursorState)


let invalid_syntax (m : model) : testResult =
  match onlyHandler m |> fun x -> x.ast with
  | Blank id ->
    ( match m.cursorState with
    | Entering (Filling (_, sid)) ->
        if id = sid then pass else fail ~f:show_cursorState m.cursorState
    | _ ->
        fail ~f:show_cursorState m.cursorState )
  | other ->
      fail ~f:show_expr other


let editing_stays_in_same_place_with_enter (m : model) : testResult =
  match (m.cursorState, onlyExpr m) with
  | Selecting (_, id1), Let (F (id2, "v2"), _, _) ->
      if id1 = Some id2
      then pass
      else fail (show_cursorState m.cursorState, show_nExpr (onlyExpr m))
  | _ ->
      fail (show_cursorState m.cursorState, show_nExpr (onlyExpr m))


let editing_goes_to_next_with_tab (m : model) : testResult =
  match (m.cursorState, onlyExpr m) with
  | Entering (Filling (_, id1)), Let (F (_, "v2"), Blank id2, _) ->
      if id1 = id2
      then pass
      else fail (show_cursorState m.cursorState, show_nExpr (onlyExpr m))
  | _ ->
      fail (show_cursorState m.cursorState, show_nExpr (onlyExpr m))


let editing_starts_a_thread_with_shift_enter (m : model) : testResult =
  match (m.cursorState, onlyExpr m) with
  | ( Entering (Filling (_, id1))
    , Let (_, F (_, Thread [F (_, Value "52"); Blank id2]), _) ) ->
      if id1 = id2
      then pass
      else fail (show_cursorState m.cursorState, show_nExpr (onlyExpr m))
  | _ ->
      fail (show_cursorState m.cursorState, show_nExpr (onlyExpr m))


let object_literals_work (m : model) : testResult =
  match (m.cursorState, onlyExpr m) with
  | ( Entering (Filling (_, id))
    , ObjectLiteral
        [ (F (_, "k1"), Blank _)
        ; (F (_, "k2"), F (_, Value "2"))
        ; (F (_, "k3"), F (_, Value "3"))
        ; (F (_, "k4"), Blank _)
        ; (Blank _, Blank id2) ] ) ->
      if id = id2
      then pass
      else fail (show_cursorState m.cursorState, show_nExpr (onlyExpr m))
  | _ ->
      fail (show_cursorState m.cursorState, show_nExpr (onlyExpr m))


let rename_function (m : model) : testResult =
  match m.handlers |> TD.values |> List.head with
  | Some {ast = F (_, FnCall (F (_, "hello"), _, _))} ->
      pass
  | other ->
      fail other


let rename_pattern_variable (m : model) : testResult =
  let expr = onlyExpr m in
  match expr with
  | Let (_, _, F (_, Match (_, cases))) ->
    ( match cases with
    | [ (F (_, PLiteral "1"), F (_, Variable "foo"))
      ; (F (_, PVariable "bar"), F (_, Variable "bar"))
      ; (Blank _, Blank _) ] ->
        pass
    | _ ->
        fail ~f:show_nExpr expr )
  | _ ->
      fail ~f:show_nExpr expr


let taking_off_rail_works (m : model) : testResult =
  let ast = onlyHandler m |> fun x -> x.ast in
  match ast with
  | F (_, FnCall (F (_, "List::head_v2"), _, NoRail)) ->
      pass
  | _ ->
      fail ~f:show_expr ast


let execute_function_works (_ : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let function_version_renders (_ : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let only_backspace_out_of_strings_on_last_char (m : model) : testResult =
  let ast = onlyHandler m |> fun x -> x.ast in
  if m.complete.value = "" then pass else fail ~f:show_expr ast


let delete_db_col (m : model) : testResult =
  let db = onlyDB m in
  match db.cols with
  | [(Blank _, Blank _)] ->
      pass
  | cols ->
      fail ~f:(show_list ~f:show_dbColumn) cols


let cant_delete_locked_col (m : model) : testResult =
  let db =
    m.dbs
    |> fun dbs ->
    if TD.count dbs > 1
    then Debug.crash "More than one db!"
    else
      TD.values dbs
      |> List.head
      |> deOption "Somehow got zero dbs after checking length?"
  in
  match db.cols with
  | [(F (_, "cantDelete"), F (_, "Int")); (Blank _, Blank _)] ->
      pass
  | cols ->
      fail ~f:(show_list ~f:show_dbColumn) cols


let result_ok_roundtrips (m : model) : testResult =
  let ast = onlyHandler m |> fun x -> x.ast in
  match ast with
  | F (_, Constructor (F (_, "Ok"), [Blank _])) ->
      pass
  | _ ->
      fail ~f:show_expr ast


let passwords_are_redacted (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let select_route (m : model) : testResult =
  match m.cursorState with
  | Selecting (_, None) ->
      pass
  | _ ->
      fail ~f:show_cursorState m.cursorState


let function_analysis_works (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let fourohfours_parse (m : model) : testResult =
  match m.f404s with
  | [x] ->
      if x.space = "HTTP"
         && x.path = "/nonexistant"
         && x.modifier = "GET"
         && x.timestamp = "2019-03-15T22:16:40Z"
         && x.traceID = "0623608c-a339-45b3-8233-0eec6120e0df"
      then pass
      else fail ~f:show_fourOhFour x
  | _ ->
      fail ~f:(show_list ~f:show_fourOhFour) m.f404s


let autocomplete_visible_height (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let return_to_architecture_on_deselect (m : model) : testResult =
  match m.currentPage with
  | Architecture ->
    ( match m.cursorState with
    | Deselected ->
        pass
    | _ ->
        fail ~f:show_cursorState m.cursorState )
  | _ ->
      fail ~f:show_page m.currentPage


let fn_page_returns_to_lastpos (m : model) : testResult =
  match TL.get m (TLID "123") with
  | Some tl ->
      let centerPos = Viewport.centerCanvasOn tl in
      if m.canvasProps.offset = centerPos
      then pass
      else fail ~f:show_pos m.canvasProps.offset
  | None ->
      fail "no tl found"


let fn_page_to_handler_pos (_m : model) : testResult = pass

let load_with_unnamed_function (_m : model) : testResult = pass

let extract_from_function (m : model) : testResult =
  match m.cursorState with
  | Selecting (TLID "123", Some _) ->
      if TD.count m.userFunctions = 2 then pass else fail m.userFunctions
  | _ ->
      fail (show_cursorState m.cursorState)


let fluidGetSelectionRange (s : fluidState) : (int * int) option =
  match s.selectionStart with
  | Some beginIdx ->
      Some (beginIdx, s.newPos)
  | None ->
      None


let fluid_double_click_selects_token (m : model) : testResult =
  match fluidGetSelectionRange m.fluidState with
  | Some (34, 40) ->
      pass
  | Some (a, b) ->
      fail
        ( "incorrect selection range for token: ("
        ^ string_of_int a
        ^ ", "
        ^ string_of_int b
        ^ ")" )
  | None ->
      fail "no selection range"


let fluid_single_click_on_token_in_deselected_handler_focuses (m : model) :
    testResult =
  match m.currentPage with
  | FocusedHandler (tlid, _) when tlid = TLID.fromString "598813411" ->
      pass
  | _ ->
      fail "handler is not focused"


let fluid_click_2x_on_token_places_cursor (m : model) : testResult =
  let focusedPass =
    match m.currentPage with
    | FocusedHandler (tlid, _) when tlid = TLID.fromString "1835485706" ->
        pass
    | _ ->
        fail "handler is not focused"
  in
  let expectedCursorPos = 6 in
  let browserCursorPass =
    testIntOption
      ~errMsg:"incorrect browser cursor position"
      ~expected:expectedCursorPos
      ~actual:(Entry.getFluidCaretPos ())
  in
  let cursorPass =
    match m.cursorState with
    | FluidEntering _ ->
        testInt
          ~errMsg:"incorrect cursor position"
          ~expected:expectedCursorPos
          ~actual:m.fluidState.newPos
    | _ ->
        fail "incorrect cursor state"
  in
  Result.combine [focusedPass; browserCursorPass; cursorPass]
  |> Result.map (fun _ -> ())


let fluid_click_2x_in_function_places_cursor (m : model) : testResult =
  let focusedPass =
    match m.currentPage with
    | FocusedFn tlid when tlid = TLID.fromString "1352039682" ->
        pass
    | _ ->
        fail "function is not focused"
  in
  let expectedCursorPos = 17 in
  let browserCursorPass =
    testIntOption
      ~errMsg:"incorrect browser cursor position"
      ~expected:expectedCursorPos
      ~actual:(Entry.getFluidCaretPos ())
  in
  let cursorPass =
    match m.cursorState with
    | FluidEntering _ ->
        testInt
          ~errMsg:"incorrect cursor position"
          ~expected:expectedCursorPos
          ~actual:m.fluidState.newPos
    | _ ->
        fail "incorrect cursor state"
  in
  Result.combine [focusedPass; browserCursorPass; cursorPass]
  |> Result.map (fun _ -> ())


let fluid_double_click_with_alt_selects_expression (m : model) : testResult =
  match fluidGetSelectionRange m.fluidState with
  | Some (34, 965) ->
      pass
  | Some (a, b) ->
      fail
        ( "incorrect selection range for expression: ("
        ^ string_of_int a
        ^ ", "
        ^ string_of_int b
        ^ ")" )
  | None ->
      fail "no selection range"


let fluid_shift_right_selects_chars_in_front (m : model) : testResult =
  match fluidGetSelectionRange m.fluidState with
  | Some (262, 341) ->
      pass
  | Some (a, b) ->
      fail
        ( "incorrect selection range for token: ("
        ^ string_of_int a
        ^ ", "
        ^ string_of_int b
        ^ ")" )
  | None ->
      fail "no selection range"


let fluid_shift_left_selects_chars_at_back (m : model) : testResult =
  match fluidGetSelectionRange m.fluidState with
  | Some (339, 261) ->
      pass
  | Some (a, b) ->
      fail
        ( "incorrect selection range for expression: ("
        ^ string_of_int a
        ^ ", "
        ^ string_of_int b
        ^ ")" )
  | None ->
      fail "no selection range"


let fluid_undo_redo_happen_exactly_once (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let varnames_are_incomplete (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let center_toplevel (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let max_callstack_bug (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let sidebar_opens_function (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let trigger (test_name : string) : integrationTestState =
  let name = String.dropLeft ~count:5 test_name in
  IntegrationTestExpectation
    ( match name with
    | "enter_changes_state" ->
        enter_changes_state
    | "field_access" ->
        field_access
    | "field_access_closes" ->
        field_access_closes
    | "field_access_pipes" ->
        field_access_pipes
    | "field_access_nested" ->
        field_access_nested
    | "pipeline_let_equals" ->
        pipeline_let_equals
    | "pipe_within_let" ->
        pipe_within_let
    | "tabbing_works" ->
        tabbing_works
    | "varbinds_are_editable" ->
        varbinds_are_editable
    | "editing_request_edits_request" ->
        editing_request_edits_request
    | "autocomplete_highlights_on_partial_match" ->
        autocomplete_highlights_on_partial_match
    | "no_request_global_in_non_http_space" ->
        no_request_global_in_non_http_space
    | "hover_values_for_varnames" ->
        hover_values_for_varnames
    | "pressing_up_doesnt_return_to_start" ->
        pressing_up_doesnt_return_to_start
    | "deleting_selects_the_blank" ->
        deleting_selects_the_blank
    | "right_number_of_blanks" ->
        right_number_of_blanks
    | "ellen_hello_world_demo" ->
        ellen_hello_world_demo
    | "editing_does_not_deselect" ->
        editing_does_not_deselect
    | "editing_headers" ->
        editing_headers
    | "switching_from_http_to_cron_space_removes_leading_slash" ->
        switching_from_http_to_cron_space_removes_leading_slash
    | "switching_from_http_to_repl_space_removes_leading_slash" ->
        switching_from_http_to_repl_space_removes_leading_slash
    | "switching_from_http_space_removes_variable_colons" ->
        switching_from_http_space_removes_variable_colons
    | "switching_to_http_space_adds_slash" ->
        switching_to_http_space_adds_slash
    | "switching_from_default_repl_space_removes_name" ->
        switching_from_default_repl_space_removes_name
    | "tabbing_through_let" ->
        tabbing_through_let
    | "focus_on_ast_in_new_empty_tl" ->
        focus_on_ast_in_new_empty_tl
    | "focus_on_cond_in_new_tl_with_if" ->
        focus_on_cond_in_new_tl_with_if
    | "dont_shift_focus_after_filling_last_blank" ->
        dont_shift_focus_after_filling_last_blank
    | "rename_db_fields" ->
        rename_db_fields
    | "rename_db_type" ->
        rename_db_type
    | "paste_right_number_of_blanks" ->
        paste_right_number_of_blanks
    | "paste_keeps_focus" ->
        paste_keeps_focus
    | "nochange_for_failed_paste" ->
        nochange_for_failed_paste
    | "feature_flag_works" ->
        feature_flag_works
    | "simple_tab_ordering" ->
        simple_tab_ordering
    | "variable_extraction" ->
        variable_extraction
    | "invalid_syntax" ->
        invalid_syntax
    | "editing_stays_in_same_place_with_enter" ->
        editing_stays_in_same_place_with_enter
    | "editing_goes_to_next_with_tab" ->
        editing_goes_to_next_with_tab
    | "editing_starts_a_thread_with_shift_enter" ->
        editing_starts_a_thread_with_shift_enter
    | "object_literals_work" ->
        object_literals_work
    | "rename_function" ->
        rename_function
    | "rename_pattern_variable" ->
        rename_pattern_variable
    | "taking_off_rail_works" ->
        taking_off_rail_works
    | "feature_flag_in_function" ->
        feature_flag_in_function
    | "execute_function_works" ->
        execute_function_works
    | "function_version_renders" ->
        function_version_renders
    | "only_backspace_out_of_strings_on_last_char" ->
        only_backspace_out_of_strings_on_last_char
    | "delete_db_col" ->
        delete_db_col
    | "cant_delete_locked_col" ->
        cant_delete_locked_col
    | "result_ok_roundtrips" ->
        result_ok_roundtrips
    | "passwords_are_redacted" ->
        passwords_are_redacted
    | "select_route" ->
        select_route
    | "function_analysis_works" ->
        function_analysis_works
    | "fourohfours_parse" ->
        fourohfours_parse
    | "return_to_architecture_on_deselect" ->
        return_to_architecture_on_deselect
    | "fn_page_returns_to_lastpos" ->
        fn_page_returns_to_lastpos
    | "fn_page_to_handler_pos" ->
        fn_page_to_handler_pos
    | "autocomplete_visible_height" ->
        autocomplete_visible_height
    | "load_with_unnamed_function" ->
        load_with_unnamed_function
    | "extract_from_function" ->
        extract_from_function
    | "fluid_single_click_on_token_in_deselected_handler_focuses" ->
        fluid_single_click_on_token_in_deselected_handler_focuses
    | "fluid_click_2x_on_token_places_cursor" ->
        fluid_click_2x_on_token_places_cursor
    | "fluid_click_2x_in_function_places_cursor" ->
        fluid_click_2x_in_function_places_cursor
    | "fluid_double_click_selects_token" ->
        fluid_double_click_selects_token
    | "fluid_double_click_with_alt_selects_expression" ->
        fluid_double_click_with_alt_selects_expression
    | "fluid_shift_right_selects_chars_in_front" ->
        fluid_shift_right_selects_chars_in_front
    | "fluid_shift_left_selects_chars_at_back" ->
        fluid_shift_left_selects_chars_at_back
    | "fluid_undo_redo_happen_exactly_once" ->
        fluid_undo_redo_happen_exactly_once
    | "varnames_are_incomplete" ->
        varnames_are_incomplete
    | "center_toplevel" ->
        center_toplevel
    | "max_callstack_bug" ->
        max_callstack_bug
    | "sidebar_opens_function" ->
        sidebar_opens_function
    | n ->
        Debug.crash ("Test " ^ n ^ " not added to IntegrationTest.trigger") )
