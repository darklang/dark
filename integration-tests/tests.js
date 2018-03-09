import { Selector } from 'testcafe';

fixture `Integration Tests`
  .beforeEach( async t => {
    const testname = t.testRun.test.name;
    const host = process.env.TEST_HOST
    const url = "http://test_" + testname + "." + host + "/admin/integration_test";
    const pageLoaded = Selector('#finishIntegrationTest').exists;
    await t
      .navigateTo(url)
      .takeScreenshot()
      .expect(pageLoaded).ok()
      ;
  })
  .afterEach( async t => {
    const signal = Selector('#integrationTestSignal');
    await t
      .takeScreenshot()
      .click("#finishIntegrationTest")
      .takeScreenshot()
      .expect(signal.exists).ok()
      ;

    const { log, error } = await t.getBrowserConsoleMessages();
    await t.expect(error).eql([])


    if (await signal.hasClass("failure")) {
      console.log("msg/mod logs for: " + t.testRun.test.name);
      for (var l of log) {
        console.log(l)
      }
      await t.expect("test state").eql(await signal.textContent);
    }

    await t.expect(signal.hasClass("success")).eql(true)
  })

//********************************
// Avoiding test race conditions
//********************************

// If you're typing using .typeText, and the text is more than 3
// characters, using { speed: 0.4 } to get testCafe to slow down a bit.

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

// Allow us wait for a certain autocomplete entry to be selected
function acAvailable(content) {
  return Selector('.autocomplete-item.highlighted')
                 .withText(content).exists;
}





// ------------------------
// Tests below here. Don't forget to update client/IntegrationTest.elm
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
    .typeText("#entry-box", "req")
    .expect(acAvailable("request")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "bo")
    .expect(acAvailable("body")).ok()
    .pressKey("enter")
    ;
});

test('field_access_closes', async t => {
  await t
    .pressKey("enter")
    .typeText("#entry-box", "req")
    .expect(acAvailable("request")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "bo")
    .expect(acAvailable("body")).ok()
    .pressKey("enter")
    ;
});

// This has a race condition somewhere
test('field_access_pipes', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")

    .typeText("#entry-box", "req")
    .expect(acAvailable("request")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "bo")
    .expect(acAvailable("body")).ok()
    .pressKey("shift+enter")
    ;
});

test('field_access_nested', async t => {
  await t
    .pressKey("enter")

    .typeText("#entry-box", "req")
    .expect(acAvailable("request")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "bo")
    .expect(acAvailable("body")).ok()
    .typeText("#entry-box", ".")

    .typeText("#entry-box", "field.")
    .typeText("#entry-box", "field2")
    .pressKey("enter")
    ;
});


test('pipeline_let_equals', async t => {
  await t
    .pressKey("enter")
    .typeText("#entry-box", "3")
    .pressKey("shift+enter")
    .typeText("#entry-box", "= value", { speed: 0.4 })
    .pressKey("enter")
    ;
});

test('pipe_within_let', async t => {
  await t
    .pressKey("enter")
    .typeText("#entry-box", "3")
    .pressKey("shift+enter")
    .typeText("#entry-box", "= value", { speed: 0.4 })
    .pressKey("enter")
    .typeText("#entry-box", "value", { speed: 0.4 })
    .pressKey("shift+enter")
    .typeText("#entry-box", "assoc", { speed: 0.4 })
    .pressKey("enter")
    .pressKey("esc")
    ;
});

test('tabbing_works', async t => {
  // Fill in "then" box in if stmt
  await t
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

test('next_sibling_works', async t => {
  await t
    .click(".ast")
    .pressKey("down")
    .pressKey("right")
    ;
});

test('varbinds_are_editable', async t => {
  await t
    .click(".ast")
    .pressKey("down")
    .pressKey("enter")
    ;
});

test('editing_request_edits_request', async t => {
  await t
    .pressKey("enter")
    .typeText("#entry-box", "req")
    .expect(acAvailable("request")).ok()
    .typeText("#entry-box", ".")

    .pressKey("esc")
    .pressKey("up")
    .pressKey("down")
    .pressKey("enter")
    ;
});

test('autocomplete_highlights_on_partial_match', async t => {
  await t
    .pressKey("enter")
    .typeText("#entry-box", "nt::add")
    .expect(acAvailable("Int::add")).ok()
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
    .click(".ast")
    .pressKey("enter")
    .typeText("#entry-box", "req")
    .expect(acAvailable("request")).ok()
    .pressKey("enter")
});

test('hover_values_for_varnames', async t => {
  await t
    .pressKey("enter")
    .typeText("#entry-box", "let")
    .pressKey("enter")
    .typeText("#entry-box", "myvar", { speed: 0.4 })
    .pressKey("enter")
    .typeText("#entry-box", "5")
    .pressKey("enter")
});

