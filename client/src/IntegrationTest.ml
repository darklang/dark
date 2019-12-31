open Tc
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TL = Toplevel
module TD = TLIDDict
module E = FluidExpression

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
      fail (errMsg ^ " (Actual: None, Expected: " ^ string_of_int expected ^ ")")


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

let deOption msg v = match v with Some v -> v | None -> failwith msg

let onlyTL (m : model) : toplevel =
  let tls = TL.all m in
  let len = TD.count tls in
  ignore
    ( if len = 0
    then failwith "no toplevels"
    else if len > 1
    then failwith ("too many toplevels: " ^ showToplevels tls)
    else "nothing to see here" ) ;
  tls |> StrDict.values |> List.head |> deOption "onlytl1"


let onlyHandler (m : model) : handler =
  m |> onlyTL |> TL.asHandler |> deOption "onlyhandler"


let onlyDB (m : model) : db = m |> onlyTL |> TL.asDB |> deOption "onlyDB"

let onlyExpr (m : model) : E.t = m |> onlyTL |> TL.getAST |> deOption "onlyast4"

let enter_changes_state (m : model) : testResult =
  match m.cursorState with
  | Entering (Creating _) ->
      pass
  | _ ->
      fail ~f:show_cursorState m.cursorState


let field_access_closes (m : model) : testResult =
  match m.cursorState with
  | FluidEntering _ ->
    ( match onlyExpr m with
    | EFieldAccess (_, EVariable (_, "request"), "body") ->
        pass
    | expr ->
        fail ~f:E.show expr )
  | _ ->
      fail ~f:show_cursorState m.cursorState


let field_access_pipes (m : model) : testResult =
  match onlyExpr m with
  | EPipe (_, [EFieldAccess (_, EVariable (_, "request"), "body"); EBlank _]) ->
      pass
  | expr ->
      fail ~f:show_fluidExpr expr


let tabbing_works (m : model) : testResult =
  match onlyExpr m with
  | EIf (_, EBlank _, EInteger (_, "5"), EBlank _) ->
      pass
  | e ->
      fail ~f:show_fluidExpr e


let autocomplete_highlights_on_partial_match (m : model) : testResult =
  match onlyExpr m with
  | EFnCall (_, "Int::add", _, _) ->
      pass
  | e ->
      fail ~f:show_fluidExpr e


let no_request_global_in_non_http_space (m : model) : testResult =
  (* this might change but this is the answer for now. *)
  match onlyExpr m with
  | EFnCall (_, "Http::badRequest", _, _) ->
      pass
  | e ->
      fail ~f:show_fluidExpr e


let ellen_hello_world_demo (m : model) : testResult =
  let spec = onlyTL m |> TL.asHandler |> deOption "hw2" |> fun x -> x.spec in
  match ((spec.space, spec.name), (spec.modifier, onlyExpr m)) with
  | (F (_, "HTTP"), F (_, "/hello")), (F (_, "GET"), EString (_, "Hello world!"))
    ->
      pass
  | other ->
      fail other


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


let switching_from_http_space_removes_variable_colons (m : model) : testResult =
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
  | ELet (_, "myvar", EInteger (_, "5"), EInteger (_, "5")) ->
      pass
  | e ->
      fail ~f:show_fluidExpr e


let rename_db_fields (m : model) : testResult =
  m.dbs
  |> TD.mapValues ~f:(fun {cols; _} ->
         match cols with
         | [ (F (_, "field6"), F (_, "String"))
           ; (F (_, "field2"), F (_, "String"))
           ; (Blank _, Blank _) ] ->
           ( match m.cursorState with
           | FluidEntering _ ->
               pass
           | _ ->
               fail ~f:show_cursorState m.cursorState )
         | _ ->
             fail ~f:(show_list ~f:show_dbColumn) cols)
  |> Result.combine
  |> Result.map (fun _ -> ())


let rename_db_type (m : model) : testResult =
  m.dbs
  |> TD.mapValues ~f:(fun {cols; dbTLID; _} ->
         match cols with
         (* this was previously an Int *)
         | [ (F (_, "field1"), F (_, "String"))
           ; (F (_, "field2"), F (_, "Int"))
           ; (Blank _, Blank _) ] ->
           ( match m.cursorState with
           | FluidEntering tlid ->
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
             fail ~f:(show_list ~f:show_dbColumn) cols)
  |> Result.combine
  |> Result.map (fun _ -> ())


let paste_right_number_of_blanks (m : model) : testResult =
  m.handlers
  |> TD.mapValues ~f:(fun {ast; _} ->
         match ast with
         | EPipe (_, [_; EFnCall (_, "-", [EBlank _], _)]) ->
             pass
         | EFnCall (_, "-", [EBlank _; EBlank _], _) ->
             pass (* ignore this TL *)
         | _ ->
             fail ~f:show_fluidExpr ast)
  |> Result.combine
  |> Result.map (fun _ -> ())


let feature_flag_works (m : model) : testResult =
  let h = onlyHandler m in
  let ast = h.ast in
  match ast with
  | ELet
      ( _
      , "a"
      , EInteger (_, "13")
      , EFeatureFlag
          ( id
          , "myflag"
          , EFnCall
              ( _
              , "Int::greaterThan"
              , [EVariable (_, "a"); EInteger (_, "10")]
              , _ )
          , EString (_, "\"A\"")
          , EString (_, "\"B\"") ) ) ->
      let res =
        Analysis.getSelectedTraceID m h.hTLID
        |> Option.andThen ~f:(Analysis.getLiveValue m id)
      in
      ( match res with
      | Some val_ ->
          if val_ = DStr "B" then pass else fail (show_fluidExpr ast, val_)
      | _ ->
          fail (show_fluidExpr ast, res) )
  | _ ->
      fail (show_fluidExpr ast, show_cursorState m.cursorState)


let feature_flag_in_function (m : model) : testResult =
  let fun_ = m.userFunctions |> TD.values |> List.head in
  match fun_ with
  | Some f ->
    ( match f.ufAST with
    | EFnCall
        ( _
        , "+"
        , [ EFeatureFlag
              ( _
              , "myflag"
              , EBool (_, true)
              , EInteger (_, "5")
              , EInteger (_, "3") )
          ; EInteger (_, "5") ]
        , NoRail ) ->
        pass
    (* TODO: validate result should evaluate true turning  5 + 5 --> 3 + 5 == 8 *)
    (* let res = Analysis.getLiveValue m f.tlid id in *)
    (* case res of *)
    (*   Just val -> if val.value == "\"8\"" then pass else fail (f.ast, value) *)
    (*   _ -> fail (f.ast, res) *)
    | _ ->
        fail ~f:show_fluidExpr f.ufAST )
  | None ->
      fail "Cant find function"


let rename_function (m : model) : testResult =
  match m.handlers |> TD.values |> List.head with
  | Some {ast = EFnCall (_, "hello", _, _); _} ->
      pass
  | Some {ast; _} ->
      fail (show_fluidExpr ast)
  | None ->
      fail "no handlers"


let execute_function_works (_ : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let fluid_execute_function_shows_live_value (_ : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let function_version_renders (_ : model) : testResult =
  (* The test logic is in tests.js *)
  pass


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
    then failwith "More than one db!"
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
  | FluidEntering (TLID "123") ->
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


let fluid_ctrl_left_on_string (_m : model) : testResult =
  let expectedPos = 7 in
  testIntOption
    ~errMsg:
      ( "incorrect browser cursor position, expected: "
      ^ (expectedPos |> string_of_int)
      ^ ", current: "
      ^ ( Entry.getFluidCaretPos ()
        |> Option.withDefault ~default:0
        |> string_of_int ) )
    ~expected:expectedPos
    ~actual:(Entry.getFluidCaretPos ())


let fluid_ctrl_right_on_string (_m : model) : testResult =
  let expectedPos = 14 in
  testIntOption
    ~errMsg:
      ( "incorrect browser cursor position, expected: "
      ^ (expectedPos |> string_of_int)
      ^ ", current: "
      ^ ( Entry.getFluidCaretPos ()
        |> Option.withDefault ~default:0
        |> string_of_int ) )
    ~expected:expectedPos
    ~actual:(Entry.getFluidCaretPos ())


let fluid_ctrl_left_on_empty_match (_m : model) : testResult =
  let expectedPos = 6 in
  testIntOption
    ~errMsg:
      ( "incorrect browser cursor position, expected: "
      ^ (expectedPos |> string_of_int)
      ^ ", current: "
      ^ ( Entry.getFluidCaretPos ()
        |> Option.withDefault ~default:0
        |> string_of_int ) )
    ~expected:expectedPos
    ~actual:(Entry.getFluidCaretPos ())


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


let sha256hmac_for_aws (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let fluid_fn_pg_change (_m : model) : testResult =
  (* The test logic is in tests.js *)
  pass


let fluid_creating_an_http_handler_focuses_the_verb (_m : model) : testResult =
  pass


let fluid_tabbing_from_an_http_handler_spec_to_ast (_m : model) : testResult =
  pass


let fluid_tabbing_from_handler_spec_past_ast_back_to_verb (_m : model) :
    testResult =
  pass


let fluid_shift_tabbing_from_handler_ast_back_to_route (_m : model) : testResult
    =
  pass


let trigger (test_name : string) : integrationTestState =
  let name = String.dropLeft ~count:5 test_name in
  IntegrationTestExpectation
    ( match name with
    | "enter_changes_state" ->
        enter_changes_state
    | "field_access_closes" ->
        field_access_closes
    | "field_access_pipes" ->
        field_access_pipes
    | "tabbing_works" ->
        tabbing_works
    | "autocomplete_highlights_on_partial_match" ->
        autocomplete_highlights_on_partial_match
    | "no_request_global_in_non_http_space" ->
        no_request_global_in_non_http_space
    | "ellen_hello_world_demo" ->
        ellen_hello_world_demo
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
    | "rename_db_fields" ->
        rename_db_fields
    | "rename_db_type" ->
        rename_db_type
    | "paste_right_number_of_blanks" ->
        paste_right_number_of_blanks
    | "feature_flag_works" ->
        feature_flag_works
    | "rename_function" ->
        rename_function
    | "feature_flag_in_function" ->
        feature_flag_in_function
    | "execute_function_works" ->
        execute_function_works
    | "fluid_execute_function_shows_live_value" ->
        fluid_execute_function_shows_live_value
    | "function_version_renders" ->
        function_version_renders
    | "delete_db_col" ->
        delete_db_col
    | "cant_delete_locked_col" ->
        cant_delete_locked_col
    | "passwords_are_redacted" ->
        passwords_are_redacted
    | "select_route" ->
        select_route
    | "function_analysis_works" ->
        function_analysis_works
    | "fourohfours_parse" ->
        fourohfours_parse
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
    | "fluid_ctrl_left_on_string" ->
        fluid_ctrl_left_on_string
    | "fluid_ctrl_right_on_string" ->
        fluid_ctrl_right_on_string
    | "fluid_ctrl_left_on_empty_match" ->
        fluid_ctrl_left_on_empty_match
    | "varnames_are_incomplete" ->
        varnames_are_incomplete
    | "center_toplevel" ->
        center_toplevel
    | "max_callstack_bug" ->
        max_callstack_bug
    | "sidebar_opens_function" ->
        sidebar_opens_function
    | "sha256hmac_for_aws" ->
        sha256hmac_for_aws
    | "fluid_fn_pg_change" ->
        fluid_fn_pg_change
    | "fluid_creating_an_http_handler_focuses_the_verb" ->
        fluid_creating_an_http_handler_focuses_the_verb
    | "fluid_tabbing_from_an_http_handler_spec_to_ast" ->
        fluid_tabbing_from_an_http_handler_spec_to_ast
    | "fluid_tabbing_from_handler_spec_past_ast_back_to_verb" ->
        fluid_tabbing_from_handler_spec_past_ast_back_to_verb
    | "fluid_shift_tabbing_from_handler_ast_back_to_route" ->
        fluid_shift_tabbing_from_handler_ast_back_to_route
    | n ->
        failwith ("Test " ^ n ^ " not added to IntegrationTest.trigger") )
