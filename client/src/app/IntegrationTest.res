open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TL = Toplevel
module TD = TLID.Dict
module E = FluidExpression
type testResult = AppTypes.IntegrationTests.testResult

type model = AppTypes.model

let pass: testResult = Ok()

let stringify = (msg: 'a): string =>
  switch Js.Json.stringifyAny(msg) {
  | Some(str) => str
  | None => "Could not stringify"
  }

let fail = (~f: 'a => string=stringify, v: 'a): testResult => Error(f(v))

let testIntOption = (~errMsg: string, ~expected: int, ~actual: option<int>): testResult =>
  switch actual {
  | Some(a) if a == expected => pass
  | Some(a) =>
    fail(
      errMsg ++
      (" (Actual: " ++
      (string_of_int(a) ++ (", Expected: " ++ (string_of_int(expected) ++ ")")))),
    )
  | None => fail(errMsg ++ (" (Actual: None, Expected: " ++ (string_of_int(expected) ++ ")")))
  }

let testInt = (~errMsg: string, ~expected: int, ~actual: int): testResult =>
  if actual == expected {
    pass
  } else {
    fail(
      errMsg ++
      (" (Actual: " ++
      (string_of_int(actual) ++ (", Expected: " ++ (string_of_int(expected) ++ ")")))),
    )
  }

let onlyTL = (m: model): option<toplevel> => {
  let tls = TL.structural(m)
  switch Map.values(tls) {
  | list{a} => Some(a)
  | _ => None
  }
}

let onlyHandler = (m: model): option<PT.Handler.t> => m |> onlyTL |> Option.andThen(~f=TL.asHandler)

let onlyDB = (m: model): option<PT.DB.t> => m |> onlyTL |> Option.andThen(~f=TL.asDB)

let onlyExpr = (m: model): option<E.t> =>
  m |> onlyTL |> Option.andThen(~f=TL.getAST) |> Option.map(~f=FluidAST.toExpr)

let showOption = (f: 'e => string, o: option<'e>): string =>
  switch o {
  | Some(x) => "Some " ++ f(x)
  | None => "None"
  }

let enter_changes_state = (m: model): testResult =>
  switch m.cursorState {
  | Omnibox(_) => pass
  | _ => fail(~f=AppTypes.CursorState.show, m.cursorState)
  }

let field_access_closes = (m: model): testResult =>
  switch m.cursorState {
  | FluidEntering(_) =>
    switch onlyExpr(m) {
    | Some(EFieldAccess(_, EVariable(_, "request"), "body")) => pass
    | expr => fail(~f=showOption(E.show), expr)
    }
  | _ => fail(~f=AppTypes.CursorState.show, m.cursorState)
  }

let field_access_pipes = (m: model): testResult =>
  switch onlyExpr(m) {
  | Some(EPipe(_, EFieldAccess(_, EVariable(_, "request"), "body"), EBlank(_), list{})) => pass
  | expr => fail(~f=showOption(show_fluidExpr), expr)
  }

let tabbing_works = (m: model): testResult =>
  switch onlyExpr(m) {
  | Some(EIf(_, EBlank(_), EInteger(_, 5L), EBlank(_))) => pass
  | e => fail(~f=showOption(show_fluidExpr), e)
  }

let autocomplete_highlights_on_partial_match = (m: model): testResult =>
  switch onlyExpr(m) {
  | Some(EFnCall(_, Stdlib({module_: "Int", function: "add", version: 0}), _, _)) => pass
  | e => fail(~f=showOption(show_fluidExpr), e)
  }

let no_request_global_in_non_http_space = (m: model): testResult =>
  // this might change but this is the answer for now.
  switch onlyExpr(m) {
  | Some(EFnCall(
      _,
      Stdlib({module_: "HttpClient", function: "badRequest", version: 0}),
      _,
      _,
    )) => pass
  | e => fail(~f=showOption(show_fluidExpr), e)
  }

let ellen_hello_world_demo = (m: model): testResult => {
  let spec =
    onlyTL(m) |> Option.andThen(~f=TL.asHandler) |> Option.map(~f=(h: PT.Handler.t) => h.spec)

  switch spec {
  | Some(spec) =>
    switch ((spec.space, spec.name), (spec.modifier, onlyExpr(m))) {
    | ((F(_, "HTTP"), F(_, "/hello")), (F(_, "GET"), Some(EString(_, "Hello world!")))) => pass
    | other => fail(other)
    }
  | other => fail(other)
  }
}

let editing_headers = (m: model): testResult => {
  let spec =
    onlyTL(m) |> Option.andThen(~f=TL.asHandler) |> Option.map(~f=(h: PT.Handler.t) => h.spec)

  switch spec {
  | Some(s) =>
    switch (s.space, s.name, s.modifier) {
    | (F(_, "HTTP"), F(_, "/myroute"), F(_, "GET")) => pass
    | other => fail(other)
    }
  | other => fail(other)
  }
}

@ppx.deriving(show)
type rec handler_triple = (BlankOr.t<string>, BlankOr.t<string>, BlankOr.t<string>)

let switching_from_http_space_removes_leading_slash = (m: model): testResult => {
  let spec =
    onlyTL(m) |> Option.andThen(~f=TL.asHandler) |> Option.map(~f=(h: PT.Handler.t) => h.spec)

  switch spec {
  | Some(s) =>
    switch (s.space, s.name, s.modifier) {
    | (F(_, newSpace), F(_, "spec_name"), _) if newSpace !== "HTTP" => pass
    | other => fail(~f=show_handler_triple, other)
    }
  | other => fail(other)
  }
}

let switching_from_http_to_cron_space_removes_leading_slash = switching_from_http_space_removes_leading_slash

let switching_from_http_to_repl_space_removes_leading_slash = switching_from_http_space_removes_leading_slash

let switching_from_http_space_removes_variable_colons = (m: model): testResult => {
  let spec =
    onlyTL(m) |> Option.andThen(~f=TL.asHandler) |> Option.map(~f=(h: PT.Handler.t) => h.spec)

  switch spec {
  | Some(s) =>
    switch (s.space, s.name, s.modifier) {
    | (F(_, newSpace), F(_, "spec_name/variable"), _) if newSpace !== "HTTP" => pass
    | other => fail(other)
    }
  | other => fail(other)
  }
}

let switching_to_http_space_adds_slash = (m: model): testResult => {
  let spec =
    onlyTL(m) |> Option.andThen(~f=TL.asHandler) |> Option.map(~f=(h: PT.Handler.t) => h.spec)

  switch spec {
  | Some(s) =>
    switch (s.space, s.name, s.modifier) {
    | (F(_, "HTTP"), F(_, "/spec_name"), _) => pass
    | other => fail(other)
    }
  | other => fail(other)
  }
}

let switching_from_default_repl_space_removes_name = (m: model): testResult => {
  let spec =
    onlyTL(m) |> Option.andThen(~f=TL.asHandler) |> Option.map(~f=(h: PT.Handler.t) => h.spec)

  switch spec {
  | Some(s) =>
    switch (s.space, s.name, s.modifier) {
    | (F(_, newSpace), _, _) if newSpace !== "REPL" => pass
    | other => fail(other)
    }
  | other => fail(other)
  }
}

let tabbing_through_let = (m: model): testResult =>
  switch onlyExpr(m) {
  | Some(ELet(_, "myvar", EInteger(_, 5L), EInteger(_, 5L))) => pass
  | e => fail(~f=showOption(show_fluidExpr), e)
  }

let rename_db_fields = (m: model): testResult =>
  m.dbs
  |> Map.mapValues(~f=({cols, _}: PT.DB.t) =>
    switch cols {
    | list{
        (F(_, "field6"), F(_, "String")),
        (F(_, "field2"), F(_, "String")),
        (Blank(_), Blank(_)),
      } =>
      switch m.cursorState {
      | Selecting(_) => pass
      | _ => fail(~f=AppTypes.CursorState.show, m.cursorState)
      }
    | _ => fail(~f=show_list(~f=PT.DB.Col.show), cols)
    }
  )
  |> Result.combine
  |> Result.map(~f=_ => ())

let rename_db_type = (m: model): testResult =>
  m.dbs
  |> Map.mapValues(~f=({cols, tlid: dbTLID, _}: PT.DB.t) =>
    switch cols {
    // this was previously an Int
    | list{(F(_, "field1"), F(_, "String")), (F(_, "field2"), F(_, "Int")), (Blank(_), Blank(_))} =>
      switch m.cursorState {
      | Selecting(tlid, None) =>
        if tlid == dbTLID {
          pass
        } else {
          fail(
            show_list(~f=PT.DB.Col.show, cols) ++
            (", " ++
            AppTypes.CursorState.show(m.cursorState)),
          )
        }
      | _ => fail(~f=AppTypes.CursorState.show, m.cursorState)
      }
    | _ => fail(~f=show_list(~f=PT.DB.Col.show), cols)
    }
  )
  |> Result.combine
  |> Result.map(~f=_ => ())

let feature_flag_works = (m: model): testResult => {
  let h = onlyHandler(m)
  let ast = h |> Option.map(~f=(h: PT.Handler.t) => h.ast |> FluidAST.toExpr)
  switch ast {
  | Some(ELet(
      _,
      "a",
      EInteger(_, 13L),
      EFeatureFlag(
        id,
        "myflag",
        EFnCall(
          _,
          Stdlib({module_: "Int", function: "greaterThan", version: 0}),
          list{EVariable(_, "a"), EInteger(_, 10L)},
          _,
        ),
        EString(_, "\"A\""),
        EString(_, "\"B\""),
      ),
    )) =>
    let res =
      h
      |> Option.map(~f=(h: PT.Handler.t) => h.tlid)
      |> Option.andThen(~f=Analysis.getSelectedTraceID(m))
      |> Option.andThen(~f=Analysis.getLiveValue(m, id))

    switch res {
    | Some(val_) =>
      if val_ == DStr("B") {
        pass
      } else {
        fail((showOption(show_fluidExpr, ast), val_))
      }
    | _ => fail((showOption(show_fluidExpr, ast), res))
    }
  | _ => fail((showOption(show_fluidExpr, ast), AppTypes.CursorState.show(m.cursorState)))
  }
}

let feature_flag_in_function = (m: model): testResult => {
  let fun_ = m.userFunctions |> Map.values |> List.head
  switch fun_ {
  | Some(f) =>
    switch f.ast |> FluidAST.toExpr {
    | EFnCall(
        _,
        Stdlib({module_: "", function: "+", version: 0}),
        list{
          EFeatureFlag(_, "myflag", EBool(_, true), EInteger(_, 5L), EInteger(_, 3L)),
          EInteger(_, 5L),
        },
        NoRail,
      ) => pass
    // TODO: validate result should evaluate true turning  5 + 5 --> 3 + 5 == 8
    // let res = Analysis.getLiveValue m f.tlid id in
    // case res of
    // Just val -> if val.value == "\"8\"" then pass else fail (f.ast, value)
    // _ -> fail (f.ast, res)
    | expr => fail(~f=show_fluidExpr, expr)
    }
  | None => fail("Cant find function")
  }
}

let rename_function = (m: model): testResult =>
  switch m.handlers
  |> Map.values
  |> List.head
  |> Option.map(~f=(h: PT.Handler.t) => h.ast |> FluidAST.toExpr) {
  | Some(EFnCall(_, User("hello"), _, _)) => pass
  | Some(expr) => fail(show_fluidExpr(expr))
  | None => fail("no handlers")
  }

let execute_function_works = (_: model): testResult =>
  // The test logic is in tests.js
  pass

let correct_field_livevalue = (_: model): testResult =>
  // The test logic is in tests.js
  pass

let int_add_with_float_error_includes_fnname = (_: model): testResult =>
  // The test logic is in tests.js
  pass

let fluid_execute_function_shows_live_value = (_: model): testResult =>
  // The test logic is in tests.js
  pass

let function_version_renders = (_: model): testResult =>
  // The test logic is in tests.js
  pass

let delete_db_col = (m: model): testResult => {
  let db = onlyDB(m) |> Option.map(~f=(d: PT.DB.t) => d.cols)
  switch db {
  | Some(list{(Blank(_), Blank(_))}) => pass
  | cols => fail(~f=showOption(show_list(~f=PT.DB.Col.show)), cols)
  }
}

let cant_delete_locked_col = (m: model): testResult => {
  let db = m.dbs |> (
    dbs =>
      if Map.length(dbs) > 1 {
        None
      } else {
        Map.values(dbs) |> List.head |> Option.map(~f=(db: PT.DB.t) => db.cols)
      }
  )

  switch db {
  | Some(list{(F(_, "cantDelete"), F(_, "Int")), (Blank(_), Blank(_))}) => pass
  | cols => fail(~f=showOption(show_list(~f=PT.DB.Col.show)), cols)
  }
}

let passwords_are_redacted = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let select_route = (m: model): testResult =>
  switch m.cursorState {
  | Selecting(_, None) => pass
  | _ => fail(~f=AppTypes.CursorState.show, m.cursorState)
  }

let function_analysis_works = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let jump_to_error = (m: model): testResult => {
  let focusedPass = switch m.currentPage {
  | FocusedHandler(tlid, _, _) if tlid == TLID.fromInt(123) => pass
  | _ => fail("function is not focused")
  }

  let expectedCursorPos = 16
  let browserCursorPass = testIntOption(
    ~errMsg="incorrect browser cursor position",
    ~expected=expectedCursorPos,
    ~actual=Entry.getFluidCaretPos(),
  )

  let cursorPass = switch m.cursorState {
  | FluidEntering(_) =>
    testInt(
      ~errMsg="incorrect cursor position",
      ~expected=expectedCursorPos,
      ~actual=m.fluidState.newPos,
    )
  | _ => fail("incorrect cursor state")
  }

  Result.combine(list{focusedPass, browserCursorPass, cursorPass}) |> Result.map(~f=_ => ())
}

let fourohfours_parse = (m: model): testResult =>
  switch m.f404s {
  | list{x} =>
    if (
      x.space == "HTTP" &&
        (x.path == "/nonexistant" &&
        (x.modifier == "GET" &&
          (x.timestamp == "2019-03-15T22:16:40Z" &&
          x.traceID == "0623608c-a339-45b3-8233-0eec6120e0df")))
    ) {
      pass
    } else {
      fail(~f=AnalysisTypes.FourOhFour.show, x)
    }
  | _ => fail(~f=show_list(~f=AnalysisTypes.FourOhFour.show), m.f404s)
  }

let autocomplete_visible_height = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let fn_page_returns_to_lastpos = (m: model): testResult =>
  switch TL.get(m, TLID.fromInt(123)) {
  | Some(tl) =>
    let centerPos = Viewport.centerCanvasOn(tl)
    if m.canvasProps.offset == centerPos {
      pass
    } else {
      fail(~f=Pos.show, m.canvasProps.offset)
    }
  | None => fail("no tl found")
  }

let fn_page_to_handler_pos = (_m: model): testResult => pass

let load_with_unnamed_function = (_m: model): testResult => pass

let create_new_function_from_autocomplete = (m: model): testResult => {
  module TD = TLID.Dict
  switch (Map.toList(m.userFunctions), Map.toList(m.handlers)) {
  | (
      list{(
        _,
        {
          ast,
          metadata: {
            name: F(_, "myFunctionName"),
            parameters: list{},
            description: "",
            returnType: F(_, TAny),
            infix: false,
          },
          _,
        },
      )},
      list{(_, {ast: ufAST, _})},
    ) =>
    switch (FluidAST.toExpr(ast), FluidAST.toExpr(ufAST)) {
    | (EBlank(_), EFnCall(_, User("myFunctionName"), list{}, _)) => pass
    | _ => fail("bad asts")
    }
  | (fns, hs) => fail((fns, hs))
  }
}

let extract_from_function = (m: model): testResult =>
  switch m.cursorState {
  | FluidEntering(tlid) if tlid == TLID.fromInt(123) =>
    if Map.length(m.userFunctions) == 2 {
      pass
    } else {
      fail(m.userFunctions)
    }
  | _ => fail(AppTypes.CursorState.show(m.cursorState))
  }

let fluidGetSelectionRange = (s: AppTypes.fluidState): option<(int, int)> =>
  switch s.selectionStart {
  | Some(beginIdx) => Some(beginIdx, s.newPos)
  | None => None
  }

let fluid_doubleclick_selects_token = (m: model): testResult =>
  switch fluidGetSelectionRange(m.fluidState) {
  | Some(34, 39) => pass
  | Some(a, b) =>
    fail(
      "incorrect selection range for token: (" ++
      (string_of_int(a) ++
      (", " ++ (string_of_int(b) ++ ")"))),
    )
  | None => fail("no selection range")
  }

let fluid_doubleclick_with_alt_selects_expression = (m: model): testResult =>
  switch fluidGetSelectionRange(m.fluidState) {
  | Some(34, 965) => pass
  | Some(a, b) =>
    fail(
      "incorrect selection range for expression: (" ++
      (string_of_int(a) ++
      (", " ++ (string_of_int(b) ++ ")"))),
    )
  | None => fail("no selection range")
  }

let fluid_doubleclick_selects_word_in_string = (m: model): testResult =>
  switch fluidGetSelectionRange(m.fluidState) {
  | Some(13, 22) => pass
  | Some(a, b) =>
    fail(
      "incorrect selection range for token: (" ++
      (string_of_int(a) ++
      (", " ++ (string_of_int(b) ++ ")"))),
    )
  | None => fail("no selection range")
  }

let fluid_doubleclick_selects_entire_fnname = (m: model): testResult =>
  switch fluidGetSelectionRange(m.fluidState) {
  | Some(0, 14) => pass
  | Some(a, b) =>
    fail(
      "incorrect selection range for token: (" ++
      (string_of_int(a) ++
      (", " ++ (string_of_int(b) ++ ")"))),
    )
  | None => fail("no selection range")
  }

let fluid_single_click_on_token_in_deselected_handler_focuses = (m: model): testResult =>
  switch m.currentPage {
  | FocusedHandler(tlid, _, _) if tlid == TLID.fromInt(598813411) => pass
  | _ => fail("handler is not focused")
  }

let fluid_click_2x_on_token_places_cursor = (m: model): testResult => {
  let focusedPass = switch m.currentPage {
  | FocusedHandler(tlid, _, _) if tlid == TLID.fromInt(1835485706) => pass
  | _ => fail("handler is not focused")
  }

  let expectedCursorPos = 6
  let browserCursorPass = testIntOption(
    ~errMsg="incorrect browser cursor position",
    ~expected=expectedCursorPos,
    ~actual=Entry.getFluidCaretPos(),
  )

  let cursorPass = switch m.cursorState {
  | FluidEntering(_) =>
    testInt(
      ~errMsg="incorrect cursor position",
      ~expected=expectedCursorPos,
      ~actual=m.fluidState.newPos,
    )
  | _ => fail("incorrect cursor state")
  }

  Result.combine(list{focusedPass, browserCursorPass, cursorPass}) |> Result.map(~f=_ => ())
}

let fluid_click_2x_in_function_places_cursor = (m: model): testResult => {
  let focusedPass = switch m.currentPage {
  | FocusedFn(tlid, _) if tlid == TLID.fromInt(1352039682) => pass
  | _ => fail("function is not focused")
  }

  let expectedCursorPos = 17
  let browserCursorPass = testIntOption(
    ~errMsg="incorrect browser cursor position",
    ~expected=expectedCursorPos,
    ~actual=Entry.getFluidCaretPos(),
  )

  let cursorPass = switch m.cursorState {
  | FluidEntering(_) =>
    testInt(
      ~errMsg="incorrect cursor position",
      ~expected=expectedCursorPos,
      ~actual=m.fluidState.newPos,
    )
  | _ => fail("incorrect cursor state")
  }

  Result.combine(list{focusedPass, browserCursorPass, cursorPass}) |> Result.map(~f=_ => ())
}

let fluid_shift_right_selects_chars_in_front = (m: model): testResult =>
  switch fluidGetSelectionRange(m.fluidState) {
  | Some(262, 341) => pass
  | Some(a, b) =>
    fail(
      "incorrect selection range for token: (" ++
      (string_of_int(a) ++
      (", " ++ (string_of_int(b) ++ ")"))),
    )
  | None => fail("no selection range")
  }

let fluid_shift_left_selects_chars_at_back = (m: model): testResult =>
  switch fluidGetSelectionRange(m.fluidState) {
  | Some(339, 261) => pass
  | Some(a, b) =>
    fail(
      "incorrect selection range for expression: (" ++
      (string_of_int(a) ++
      (", " ++ (string_of_int(b) ++ ")"))),
    )
  | None => fail("no selection range")
  }

let fluid_undo_redo_happen_exactly_once = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let fluid_ctrl_left_on_string = (_m: model): testResult => {
  let expectedPos = 7
  testIntOption(
    ~errMsg="incorrect browser cursor position, expected: " ++
    ((expectedPos |> string_of_int) ++
    (", current: " ++ (Entry.getFluidCaretPos() |> Option.unwrap(~default=0) |> string_of_int))),
    ~expected=expectedPos,
    ~actual=Entry.getFluidCaretPos(),
  )
}

let fluid_ctrl_right_on_string = (_m: model): testResult => {
  let expectedPos = 14
  testIntOption(
    ~errMsg="incorrect browser cursor position, expected: " ++
    ((expectedPos |> string_of_int) ++
    (", current: " ++ (Entry.getFluidCaretPos() |> Option.unwrap(~default=0) |> string_of_int))),
    ~expected=expectedPos,
    ~actual=Entry.getFluidCaretPos(),
  )
}

let fluid_ctrl_left_on_empty_match = (_m: model): testResult => {
  let expectedPos = 6
  testIntOption(
    ~errMsg="incorrect browser cursor position, expected: " ++
    ((expectedPos |> string_of_int) ++
    (", current: " ++ (Entry.getFluidCaretPos() |> Option.unwrap(~default=0) |> string_of_int))),
    ~expected=expectedPos,
    ~actual=Entry.getFluidCaretPos(),
  )
}

let varnames_are_incomplete = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let center_toplevel = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let max_callstack_bug = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let sidebar_opens_function = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let empty_fn_never_called_result = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let empty_fn_been_called_result = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let sha256hmac_for_aws = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let fluid_fn_pg_change = (_m: model): testResult =>
  // The test logic is in tests.js
  pass

let fluid_creating_an_http_handler_focuses_the_verb = (_m: model): testResult => pass

let fluid_tabbing_from_an_http_handler_spec_to_ast = (_m: model): testResult => pass

let fluid_tabbing_from_handler_spec_past_ast_back_to_verb = (_m: model): testResult => pass

let fluid_shift_tabbing_from_handler_ast_back_to_route = (_m: model): testResult => pass

let fluid_test_copy_request_as_curl = (m: model): testResult => {
  // test logic is here b/c testcafe couldn't get clipboard data
  // CLEANUP
  let curl = CurlCommand.curlFromHttpClientCall(
    m,
    TLID.fromInt(91390945),
    ID.fromInt(753586717),
    PT.FQFnName.stdlib("HttpClient", "post", 0),
  )

  let expected = "curl -H 'h:3' -d 'some body' -X post 'https://foo.com?q=1'"
  switch curl {
  | None => fail("Expected a curl command, got nothing")
  | Some(s) =>
    if s !== expected {
      fail("Expected: '" ++ (expected ++ ("', got '" ++ (s ++ "'."))))
    } else {
      pass
    }
  }
}

let fluid_ac_validate_on_lose_focus = (m: model): testResult =>
  switch onlyExpr(m) {
  | Some(EFieldAccess(_, EVariable(_, "request"), "body")) => pass
  | e =>
    fail("Expected: `request.body`, got `" ++ (showOption(FluidPrinter.eToHumanString, e) ++ "`"))
  }

let upload_pkg_fn_as_admin = (_m: model): testResult => pass

let use_pkg_fn = (_m: model): testResult => pass

let fluid_show_docs_for_command_on_selected_code = (_m: model): testResult => pass

let fluid_bytes_response = (_m: model): testResult => pass

let double_clicking_blankor_selects_it = (_m: model): testResult => pass

let abridged_sidebar_content_visible_on_hover = (_m: model): testResult => pass

let abridged_sidebar_category_icon_click_disabled = (_m: model): testResult => pass

let function_docstrings_are_valid = (m: model): testResult => {
  open PrettyDocs
  let failed = m.functions.builtinFunctions |> List.filterMap(~f=(fn: RT.BuiltInFn.t) =>
    switch convert_(fn.description) {
    | ParseSuccess(_) => None
    | ParseFail(messages) => Some(fn.name->RT.FQFnName.StdlibFnName.toString, messages)
    }
  )

  if List.isEmpty(failed) {
    pass
  } else {
    let nl = " \n "
    let combineErrors = errors =>
      errors
      |> List.map(~f=((fnname, messages)) => {
        let problems =
          messages
          |> List.map(~f=((txt, msg)) => msg ++ (" in \"" ++ (txt ++ "\"")))
          |> String.join(~sep=nl)

        fnname ++ (nl ++ problems)
      })
      |> String.join(~sep=nl)

    fail(~f=combineErrors, failed)
  }
}

let record_consent_saved_across_canvases = (_m: model): testResult => pass

// let exe_flow_fades (_m : model) : testResult = pass

let unexe_code_unfades_on_focus = (_m: model): testResult => pass

let create_from_404 = (_m: model) => pass

let unfade_command_palette = (_m: model): testResult => pass

let redo_analysis_on_toggle_erail = (_m: model): testResult => pass

let redo_analysis_on_commit_ff = (_m: model): testResult => pass

let package_function_references_work = (_m: model): testResult => pass

let focus_on_secret_field_on_insert_modal_open = (_m: model): testResult => pass

let analysis_performed_in_appropriate_timezone = (_m: model): testResult => pass

let trigger = (test_name: string): AppTypes.IntegrationTests.t<model> => {
  let name = String.dropLeft(~count=5, test_name)
  IntegrationTestExpectation(
    switch name {
    | "enter_changes_state" => enter_changes_state
    | "field_access_closes" => field_access_closes
    | "field_access_pipes" => field_access_pipes
    | "tabbing_works" => tabbing_works
    | "autocomplete_highlights_on_partial_match" => autocomplete_highlights_on_partial_match
    | "no_request_global_in_non_http_space" => no_request_global_in_non_http_space
    | "ellen_hello_world_demo" => ellen_hello_world_demo
    | "editing_headers" => editing_headers
    | "switching_from_http_to_cron_space_removes_leading_slash" => switching_from_http_to_cron_space_removes_leading_slash
    | "switching_from_http_to_repl_space_removes_leading_slash" => switching_from_http_to_repl_space_removes_leading_slash
    | "switching_from_http_space_removes_variable_colons" => switching_from_http_space_removes_variable_colons
    | "switching_to_http_space_adds_slash" => switching_to_http_space_adds_slash
    | "switching_from_default_repl_space_removes_name" => switching_from_default_repl_space_removes_name
    | "tabbing_through_let" => tabbing_through_let
    | "rename_db_fields" => rename_db_fields
    | "rename_db_type" => rename_db_type
    | "feature_flag_works" => feature_flag_works
    | "rename_function" => rename_function
    | "feature_flag_in_function" => feature_flag_in_function
    | "execute_function_works" => execute_function_works
    | "correct_field_livevalue" => correct_field_livevalue
    | "int_add_with_float_error_includes_fnname" => int_add_with_float_error_includes_fnname
    | "fluid_execute_function_shows_live_value" => fluid_execute_function_shows_live_value
    | "function_version_renders" => function_version_renders
    | "delete_db_col" => delete_db_col
    | "cant_delete_locked_col" => cant_delete_locked_col
    | "passwords_are_redacted" => passwords_are_redacted
    | "select_route" => select_route
    | "function_analysis_works" => function_analysis_works
    | "jump_to_error" => jump_to_error
    | "fourohfours_parse" => fourohfours_parse
    | "fn_page_returns_to_lastpos" => fn_page_returns_to_lastpos
    | "fn_page_to_handler_pos" => fn_page_to_handler_pos
    | "autocomplete_visible_height" => autocomplete_visible_height
    | "load_with_unnamed_function" => load_with_unnamed_function
    | "create_new_function_from_autocomplete" => create_new_function_from_autocomplete
    | "extract_from_function" => extract_from_function
    | "fluid_single_click_on_token_in_deselected_handler_focuses" => fluid_single_click_on_token_in_deselected_handler_focuses
    | "fluid_click_2x_on_token_places_cursor" => fluid_click_2x_on_token_places_cursor
    | "fluid_click_2x_in_function_places_cursor" => fluid_click_2x_in_function_places_cursor
    | "fluid_doubleclick_selects_token" => fluid_doubleclick_selects_token
    | "fluid_doubleclick_selects_word_in_string" => fluid_doubleclick_selects_word_in_string
    | "fluid_doubleclick_with_alt_selects_expression" => fluid_doubleclick_with_alt_selects_expression
    | "fluid_doubleclick_selects_entire_fnname" => fluid_doubleclick_selects_entire_fnname
    | "fluid_shift_right_selects_chars_in_front" => fluid_shift_right_selects_chars_in_front
    | "fluid_shift_left_selects_chars_at_back" => fluid_shift_left_selects_chars_at_back
    | "fluid_undo_redo_happen_exactly_once" => fluid_undo_redo_happen_exactly_once
    | "fluid_ctrl_left_on_string" => fluid_ctrl_left_on_string
    | "fluid_ctrl_right_on_string" => fluid_ctrl_right_on_string
    | "fluid_ctrl_left_on_empty_match" => fluid_ctrl_left_on_empty_match
    | "varnames_are_incomplete" => varnames_are_incomplete
    | "center_toplevel" => center_toplevel
    | "max_callstack_bug" => max_callstack_bug
    | "sidebar_opens_function" => sidebar_opens_function
    | "empty_fn_never_called_result" => empty_fn_never_called_result
    | "empty_fn_been_called_result" => empty_fn_been_called_result
    | "sha256hmac_for_aws" => sha256hmac_for_aws
    | "fluid_fn_pg_change" => fluid_fn_pg_change
    | "fluid_creating_an_http_handler_focuses_the_verb" => fluid_creating_an_http_handler_focuses_the_verb
    | "fluid_tabbing_from_an_http_handler_spec_to_ast" => fluid_tabbing_from_an_http_handler_spec_to_ast
    | "fluid_tabbing_from_handler_spec_past_ast_back_to_verb" => fluid_tabbing_from_handler_spec_past_ast_back_to_verb
    | "fluid_shift_tabbing_from_handler_ast_back_to_route" => fluid_shift_tabbing_from_handler_ast_back_to_route
    | "fluid_test_copy_request_as_curl" => fluid_test_copy_request_as_curl
    | "fluid_ac_validate_on_lose_focus" => fluid_ac_validate_on_lose_focus
    | "upload_pkg_fn_as_admin" => upload_pkg_fn_as_admin
    | "use_pkg_fn" => use_pkg_fn
    | "fluid_show_docs_for_command_on_selected_code" => fluid_show_docs_for_command_on_selected_code
    | "fluid-bytes-response" => fluid_bytes_response
    | "double_clicking_blankor_selects_it" => double_clicking_blankor_selects_it
    | "abridged_sidebar_content_visible_on_hover" => abridged_sidebar_content_visible_on_hover
    | "abridged_sidebar_category_icon_click_disabled" => abridged_sidebar_category_icon_click_disabled
    | "function_docstrings_are_valid" => function_docstrings_are_valid
    | "record_consent_saved_across_canvases" => record_consent_saved_across_canvases
    /* | "exe_flow_fades" ->
     exe_flow_fades */
    | "unexe_code_unfades_on_focus" => unexe_code_unfades_on_focus
    | "create_from_404" => create_from_404
    | "unfade_command_palette" => unfade_command_palette
    | "redo_analysis_on_toggle_erail" => redo_analysis_on_toggle_erail
    | "redo_analysis_on_commit_ff" => redo_analysis_on_commit_ff
    | "package_function_references_work" => package_function_references_work
    | "focus_on_secret_field_on_insert_modal_open" => focus_on_secret_field_on_insert_modal_open
    | "analysis_performed_in_appropriate_timezone" => analysis_performed_in_appropriate_timezone
    | n => _ => fail("Test " ++ (n ++ " not added to IntegrationTest.trigger"))
    },
  )
}
