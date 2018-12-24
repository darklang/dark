import { ClientFunction, Selector } from 'testcafe';

fixture `Integration Tests`
  // To add this user, run the backend tests
  .httpAuth({ username: 'test', password: 'fVm2CUePzGKCwoEQQdNJktUQ'})
  .beforeEach( async t => {
    const testname = t.testRun.test.name;
    const url = "http://darklang.localhost:8000/a/test-" + testname + "/integration_test";
    const pageLoaded = await Selector('#finishIntegrationTest').exists;
    await t
      .navigateTo(url)
      .takeScreenshot()
      ;
  })
  .afterEach( async t => {
    const finish = Selector('#finishIntegrationTest');
    const signal = Selector('#integrationTestSignal');
    let flushLogs = async () => {
      const { log, warn, error, info } = await t.getBrowserConsoleMessages();
      console.error("msg/mod logs for: " + t.testRun.test.name);
      const msgs = Array.concat(["Console Logs:"], log, ["\n\nConsole Warnings"], warn, ["\n\nConsole Errors:"], error, ["\n\nConsole Infos:"], info);
      for (var l of msgs) {
        console.error(l)
      }

      return true;
    }

    let flushedLogs = false
    try {
      // TODO: clicks on this button are not registered in function space
      // We should probably figure out why.
      // For now, putting a more helpful error message
      await t
        .click(finish)
        .expect(finish.exists).notOk("Finish button click failed: did you try to click it from the function space?")
        .expect(signal.exists).ok("Test evaluation has timed out. Has the application crashed?")
        ;

      const { error } = await t.getBrowserConsoleMessages();
      await t.expect(error).eql([])


      if (await signal.hasClass("failure")) {
        await t.takeScreenshot();
        flushedLogs = flushLogs();
        await t.expect("test state").eql(await signal.textContent);
      }

      await t.expect(signal.hasClass("success")).eql(true)
    } catch (e) {
      if (!flushedLogs) {
        flushLogs();
      }
      throw e;
    }
  })

//********************************
// Utilities
//********************************

function user_content_url (t, endpoint) {
  return "http://test-" + t.testRun.test.name + ".builtwithdark.localhost:8000" + endpoint;
}

//********************************
// Avoiding test race conditions
//********************************
// Testcafe automatically waits for the next thing you've specified. So
// if you .typeText("#entry-box", ...), it will wait for the entryBox.
// But we sometimes need to explicitly wait if TestCafe can't tell what
// we're waiting on.

function astAvailable() {
  return Selector('.ast').exists;
}
function entryBoxAvailable() {
  return Selector('#entry-box').exists;
}
function available(css) {
  return Selector(css).exists;
}

// Allow us wait for a certain autocomplete entry to be selected
function acHighlighted(content) {
  return Selector('.autocomplete-item.highlighted')
                 .withExactText(content);
}


// ------------------------
// Tests below here. Don't forget to update client2/src/IntegrationTest.ml
// ------------------------

test('enter_changes_state', async t => {
  await t
    .pressKey("enter")
    .expect(entryBoxAvailable()).ok()
   ;
});

test('field_access', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "req")
    .expect(acHighlighted("request")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "bo")
    .expect(acHighlighted("body")).ok()
    .pressKey("enter")
    ;
});


test('field_access_closes', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "req")
    .expect(acHighlighted("request")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "b")
    .typeText("#entry-box", "o")
    .expect(acHighlighted("body")).ok()
    .pressKey("enter")
    ;
});

// This has a race condition somewhere
test('field_access_pipes', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .pressKey("enter")

    .typeText("#entry-box", "req")
    .expect(acHighlighted("request")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "bo")
    .expect(acHighlighted("body")).ok()
    .pressKey("shift+enter")
    ;
});

test('field_access_nested', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")

    .typeText("#entry-box", "req")
    .expect(acHighlighted("request")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "bo")
    .expect(acHighlighted("body")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "field.")
    .typeText("#entry-box", "field2")
    .pressKey("enter")
    ;
});


test('pipeline_let_equals', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "3")
    .pressKey("shift+enter")
    .typeText("#entry-box", "=value")
    .pressKey("enter")
    ;
});

test('pipe_within_let', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "3")
    .pressKey("shift+enter")
    .typeText("#entry-box", "=value")
    .pressKey("enter")
    .typeText("#entry-box", "value")
    .pressKey("shift+enter")
    .typeText("#entry-box", "Int::add")
    .pressKey("enter")
    .pressKey("esc")
    ;
});

test('tabbing_works', async t => {
  // Fill in "then" box in if stmt
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "if")
    .pressKey("enter")
    .pressKey("esc")
    .pressKey("tab")
    .pressKey("enter")
    .typeText("#entry-box", "5")
    .pressKey("enter")
    ;
});

test('left_right_works', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .pressKey("esc")
    .pressKey("tab")
    .pressKey("right")
    .pressKey("right")
    .pressKey("right")
    .pressKey("right") // stop on final elem
    .pressKey("right")
    .pressKey("left")
    ;
});

test('varbinds_are_editable', async t => {
  await t
    .click(".letbind")
    .pressKey("enter")
    .pressKey("enter")
    ;
});

test('editing_does_not_deselect', async t => {
  await t
    .doubleClick(".ast .blankOr > .letrhs > .blankOr")
    .click("#entry-box")
});

test('editing_request_edits_request', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "req")
    .expect(acHighlighted("request")).ok()
    .typeText("#entry-box", ".")

    .pressKey("esc")
    .pressKey("left")
    .pressKey("enter")
    ;
});

test('autocomplete_highlights_on_partial_match', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "nt::add")
    .expect(acHighlighted("Int::add")).ok()
    .pressKey("enter")
    ;
});

test('no_request_global_in_non_http_space', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .click(".module")
    .pressKey("enter")
    .typeText("#entry-box", "NOT_HTTP_SPACE")
    .pressKey("enter")
    .typeText("#entry-box", "request")
    .expect(acHighlighted("Http::badRequest")).ok()
    .pressKey("enter")
});

test('hover_values_for_varnames', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "let")
    .pressKey("enter")
    .typeText("#entry-box", "myvar")
    .pressKey("enter")
    .typeText("#entry-box", "5")
    .pressKey("enter")
});


test('pressing_up_doesnt_return_to_start', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "Char::")
    .expect(acHighlighted("Char::toASCIIChar")).ok()
    .pressKey("down")
    .pressKey("up")
    .expect(acHighlighted("Char::toASCIIChar")).ok()
    .typeText("#entry-box", "toASCII")
    .pressKey("enter")
});

test('deleting_selects_the_blank', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "5")
    .pressKey("enter")
    .click(".ast .value")
    .pressKey("delete")
    .typeText("#entry-box", "6")
    .pressKey("enter")
});

test('right_number_of_blanks', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "assoc")
    .pressKey("enter")
});

// This is how Ellen demos, and should be kept in sync with that if she
// changes.
test('ellen_hello_world_demo', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")

    // route
    .pressKey("tab")
    .typeText("#entry-box", "/hello")
    .pressKey("enter")

    // space
    .typeText("#entry-box", "H")
    .pressKey("down")
    .pressKey("enter")

    // verb
    .typeText("#entry-box", "g")
    .pressKey("down")
    .pressKey("enter")

    // string
    .typeText("#entry-box", "\"Hello world!")
    .pressKey("enter")
});

test('editing_headers', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")

    // add headers
    .doubleClick(".spec-header > .name")
    .typeText("#entry-box", "/hello")
    .pressKey("enter")

    .doubleClick(".spec-header > .modifier")
    .typeText("#entry-box", "PO")
    .expect(acHighlighted("POST")).ok()
    .pressKey("enter")

    .doubleClick(".spec-header > .module")
    .typeText("#entry-box", "HTTP")
    .pressKey("enter")

    // edit them
    .doubleClick(".spec-header > .name")
    .pressKey("backspace")
    .pressKey("backspace")
    .pressKey("backspace")
    .pressKey("backspace")
    .pressKey("backspace")
    .pressKey("backspace")
    .typeText("#entry-box", "/myroute")
    .pressKey("enter")

    .click(".spec-header > .modifier")
    .pressKey("delete")
    .typeText("#entry-box", "GET")
    .pressKey("enter")
});

test('tabbing_through_let', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "let")
    .pressKey("enter")

    // fill in the headers first
    .pressKey("tab")
    .pressKey("tab")
    .pressKey("tab")
    .typeText("#entry-box", "/route")
    .pressKey("enter")
    .typeText("#entry-box", "HTTP")
    .pressKey("enter")
    .typeText("#entry-box", "GET")
    .pressKey("enter")

    // round trip through the let blanks once
    .pressKey("tab")
    .pressKey("tab")
    .pressKey("tab")

    // go to the body and fill it in
    .pressKey("tab")
    .pressKey("tab")
    .typeText("#entry-box", "5")
    .pressKey("enter")

    // go to the rhs and fill it in
    .pressKey("tab")
    .typeText("#entry-box", "5")
    .pressKey("enter")

    // fill in the var
    .typeText("#entry-box", "myvar")
    .pressKey("enter")
});

test('focus_on_ast_in_new_empty_tl', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
});

test('focus_on_path_in_new_filled_tl', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "5")
    .pressKey("enter")
});

test('focus_on_cond_in_new_tl_with_if', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "if")
    .pressKey("enter")
});

test('dont_shift_focus_after_filling_last_blank', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "5")
    .pressKey("enter")
    .typeText("#entry-box", "/")
    .pressKey("enter")
    .typeText("#entry-box", "HTTP")
    .pressKey("enter")
    .typeText("#entry-box", "GET")
    .pressKey("enter")
});

test('rename_db_fields', async t => {

  const callBackend = ClientFunction(
    function (url) {
      var xhttp = new XMLHttpRequest();
      xhttp.open("POST", url, true);
      xhttp.setRequestHeader("Content-type", "application/json");
      xhttp.send('{ "field6": "a", "field2": "b" }');
    });

  // rename
  await t
    .doubleClick(Selector('.name').withText('field1'))
    .pressKey("backspace")
    .typeText("#entry-box", "field6")
    .pressKey("tab")
    .pressKey("esc")
    ;


  // add data and check we can't rename again
  await callBackend(user_content_url(t, "/add"));

  // This is super-shaky if we remove this. There's some timing things
  // around when the .fa-lock appears, and the selectors we'd expect
  // (below) doesn't work. But if we split it into two it works. Who
  // knows.
  // await t.expect(Selector('.fa-lock', {timeout: 5000})().exists).ok() ;

  await Selector('.fa-lock', {timeout: 5000})();
  await t.expect(Selector('.fa-lock').exists).ok();

  await t
    .doubleClick(Selector('.name').withText('field6'))
    .pressKey("enter")
    ;
});

test('rename_db_type', async t => {

  const callBackend = ClientFunction(
    function (url) {
      var xhttp = new XMLHttpRequest();
      xhttp.open("POST", url, true);
      xhttp.setRequestHeader("Content-type", "application/json");
      xhttp.send('{ "field1": "a", "field2": 5 }');
    });

  // rename
  await t
    .doubleClick(Selector('.type').withText('Int'))
    .pressKey("backspace")
    .pressKey("backspace")
    .pressKey("backspace")
    .typeText("#entry-box", "String")
    .pressKey("enter")
    ;


  // add data and check we can't rename again
  await callBackend(user_content_url(t, "/add"));

  // This is super-shaky if we remove this. There's some timing things
  // around when the .fa-lock appears, and the selectors we'd expect
  // (below) doesn't work. But if we split it into two it works. Who
  // knows.
  // await t.expect(Selector('.fa-lock', {timeout: 5000})().exists).ok() ;

  await Selector('.fa-lock', {timeout: 5000})();
  await t.expect(Selector('.fa-lock').exists).ok();

  await t
    .doubleClick(Selector('.type').withText('String'))
    .pressKey("enter")
    ;
});

test('paste_right_number_of_blanks', async t => {
  await t
    .click(Selector('.fnname').withText('-'))
    .pressKey("ctrl+c")
    .click(Selector('.fnname').withText('(+)'))
    .pressKey("ctrl+v")
});


test('paste_keeps_focus', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .pressKey("+")
    .pressKey("enter")
    .pressKey("3")
    .pressKey("enter")
    .pressKey("2")

    .click(Selector('.fnname').withText('+'))
    .pressKey("enter")
    .pressKey("ctrl+c")
    .pressKey("right")
    .pressKey("ctrl+v")
});

test('nochange_for_failed_paste', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "let")
    .pressKey("enter")
    .pressKey("x")
    .pressKey("enter")
    .pressKey("2")
    .pressKey("enter")

    .click('.letrhs')
    .pressKey("ctrl+c")
    .pressKey("left")
    .pressKey("ctrl+v")
});

test('feature_flag_works', async t => {
  await t
    // Create an empty let
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "let")
    .pressKey("enter")
    .typeText("#entry-box", "a")
    .pressKey("enter")
    .typeText("#entry-box", "13")
    .pressKey("enter")
    .pressKey("down")
    .pressKey("esc")

    // Click feature name
    .click('.expr-actions .flag')

    // Name it
    .doubleClick(Selector('.flag-name'))
    .typeText("#entry-box", "myflag")
    .pressKey("enter")

    // Set condition
    .typeText("#entry-box", "Int::greaterThan")
    .pressKey("enter")
    .typeText("#entry-box", "a")
    .pressKey("enter")
    .typeText("#entry-box", "10")
    .pressKey("enter")

    // Case A
    .typeText("#entry-box", "\"")
    .typeText("#entry-box", "A")
    .pressKey("enter")

    // Case B
    .typeText("#entry-box", "\"")
    .typeText("#entry-box", "B")
    .pressKey("enter")

});

test('feature_flag_in_function', async t => {
  await t
    // Go to function
    .click(".fun1")
    .click(".fa-edit")

    .expect(available(".tl-2296485551")).ok()
    .click(".tl-2296485551")
    .pressKey("enter")

    // Make feature Flag
    .click('.expr-actions .flag')

    .expect(available(".feature-flag")).ok()
    .typeText("#entry-box", "myflag")
    .pressKey("enter")

    // Set condition
    .typeText("#entry-box", "true")
    .pressKey("enter")

    // Case B
    .typeText("#entry-box", "3")
    .pressKey("enter")

    // Return to main canvas to finish tests
    .click(".return-to-canvas")
});

test('simple_tab_ordering', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "let")
    .pressKey("enter")
    .pressKey("tab")
    .pressKey("4")
    .pressKey("enter")
});

test('variable_extraction', async t => {
  await t
    .click(Selector('.fnname').withText('+'))
    .pressKey("ctrl+shift+l")
    .typeText("#entry-box", "new_variable")
    .pressKey("enter")
})

// Entering text with invalid syntax leaves things the same
test('invalid_syntax', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "in:valid")
    .pressKey("enter")
})


// When you edit, stay in the same place after pressing Enter
test('editing_stays_in_same_place_with_enter', async t => {
  await t
    .doubleClick(Selector('.letvarname'))
    .pressKey("2")
    .pressKey("enter")
})

// When you edit, go to the next blank after pressing Tab
test('editing_goes_to_next_with_tab', async t => {
  await t
    .doubleClick(Selector('.letvarname'))
    .pressKey("2")
    .pressKey("tab")
})


// When you press shift+enter, start a thread
test('editing_starts_a_thread_with_shift_enter', async t => {
  await t
    .doubleClick(Selector('.letrhs'))
    .pressKey("2")
    .pressKey("shift+enter")
})

test('object_literals_work', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "{")
    .pressKey("enter")
    .typeText("#entry-box", "k1")
    .pressKey("tab")
    .pressKey("tab")
    .typeText("#entry-box", "k2")
    .pressKey("enter")
    .typeText("#entry-box", "2")
    .pressKey("tab")
    .typeText("#entry-box", "k3")
    .pressKey("enter")
    .typeText("#entry-box", "3")
    .pressKey("tab")
    .typeText("#entry-box", "k4") // Check that this opens a new row
    .pressKey("tab") // Skip the new stuff
    .pressKey("tab")
    .pressKey("tab")
    .pressKey("tab") // End right after it
})

test('rename_function', async t => {
  await t
    .click(Selector('.fnname'))
    .click(Selector('.fa-edit'))
    .click(Selector('.fn-name-content'))
    .pressKey('backspace')
    .typeText('#entry-box', 'hello')
    .pressKey('enter')

    // Return to main canvas to finish tests
    .click(".return-to-canvas")
})

test('rename_pattern_variable', async t => {
  await t
    .click(Selector('.letvarname'))
    .pressKey('backspace')
    .typeText('#entry-box', 'foo')
    .pressKey('enter')
    .doubleClick(Selector('.matchexpr .matchcase').nth(1).child(0))
    .pressKey('backspace')
    .pressKey('backspace')
    .pressKey('backspace')
    .typeText('#entry-box', 'bar')
    .pressKey('enter')
})

test('sending_to_rail_works', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "List::head_v1")
    .pressKey("enter")
    .pressKey("esc")
    .pressKey("shift+up")
    .pressKey("alt+e")
})

test('execute_function_works', async t => {

  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "Uuid::gen")
    .pressKey("enter")
    .click(Selector('.fa-play'))
    .click(Selector('.fncall'))
    ;

  let v1 = await Selector('.selected[data-live-value]').getAttribute('data-live-value');

  await t
    .click(Selector('.fa-redo'))
    ;

  let v2 = await Selector('.selected[data-live-value]').getAttribute('data-live-value');

  let re = /<UUID: [0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}>/;
  await t.expect(v1).match(re);
  await t.expect(v2).match(re);
  await t.expect(v1).notEql(v2);
})

test('function_version_renders', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "DB::del")
    .expect(
      Selector('.autocomplete-item.highlighted .versioned-function')
      .withText("DB::deleteAll1")
    ).ok()
})

test('only_backspace_out_of_strings_on_last_char', async t => {
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "\"t")
    .pressKey("backspace")
    .pressKey("tab") // moved selection to a different blank
    // test that it's an empty string. Note this needs to not be selected or
    // the livevalue will be in textcontent occasionally, breaking the test.
    .expect(Selector('.ast .tstr').textContent).eql('')
    .click(Selector('.ast .tstr'))
    .pressKey("enter")
    .pressKey("backspace")
    // we should have gone over the backspace - checking in client
    ;
})

test('delete_db_col', async t => {
  await t
    .click(Selector('.delete-col'))
  ;
})

test('cant_delete_locked_col', async t => {
  await t
    .click(Selector('.fncall')) // this click is required due to caching
    .click(Selector('.fa-play'))
  ;

  // the redo button only appears once the analysis has returned, so we
  // can use this as a totally not brittle proxy for the fact that
  // we want to wait until the data is in the DB
  await t.expect(Selector('.fa-redo', {timeout: 5000}).exists).ok();

  await t
    .click(Selector('.db')) // this click is required due to caching
    .expect(Selector('.delete-col').exists).notOk()
  ;
})

