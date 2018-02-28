import { Selector } from 'testcafe';

fixture `Integration Tests`
  .beforeEach( async t => {
    const testname = t.testRun.test.name;
    const url = "http://test_" + testname + ".localhost:8000/admin/integration_test";
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


// ------------------------
// Tests below here. Don't forget to update client/IntegrationTest.elm
// ------------------------

test('enter_changes_state', async t => {
  const entryBoxAvailable = Selector('#entry-box').exists;
  await t
    .pressKey("enter")
    .expect(entryBoxAvailable).ok()
   ;
});

test('field_access', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entry-box", "req.")
    .typeText("#entry-box", "bo")
    .pressKey("enter")
    .expect(astAvailable).ok()
    ;
});

test('field_access_closes', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entry-box", "req.")
    .typeText("#entry-box", "bo")
    .pressKey("enter")
    .expect(astAvailable).ok()
    ;
});

// This has a race condition somewhere
test('field_access_pipes', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entry-box", "req.")
    .typeText("#entry-box", "bo")
    .pressKey("shift+enter")
    .expect(astAvailable).ok()
    ;
});

test('field_access_nested', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entry-box", "req.")
    .typeText("#entry-box", "bo.")
    .typeText("#entry-box", "field.")
    .typeText("#entry-box", "field2")
    .pressKey("enter")
    .expect(astAvailable).ok()
    ;
});


test('pipeline_let_equals', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entry-box", "3")
    .pressKey("shift+enter")
    .typeText("#entry-box", "= value")
    .pressKey("enter")
    .expect(astAvailable).ok()
    ;
});

test('pipe_within_let', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entry-box", "3")
    .pressKey("shift+enter")
    .typeText("#entry-box", "= value")
    .pressKey("enter")
    .typeText("#entry-box", "value")
    .pressKey("shift+enter")
    .typeText("#entry-box", "assoc")
    .pressKey("enter")
    .pressKey("esc")
    .expect(astAvailable).ok()
    ;
});

test('tabbing_works', async t => {
  // Fill in "then" box in if stmt
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entry-box", "if")
    .pressKey("enter")
    .pressKey("esc")
    .pressKey("tab")
    .pressKey("enter")
    .typeText("#entry-box", "5")
    .pressKey("enter")
    .expect(astAvailable).ok()
    ;
});

test('next_sibling_works', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .click(".ast")
    .pressKey("down")
    .pressKey("right")
    ;
  // TODO: this might be flaky. Maybe wait for a specific ID to be
  // selected?
});

test('varbinds_are_editable', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .click(".ast")
    .pressKey("down")
    .pressKey("enter")
    ;
});

test('editing_request_edits_request', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entry-box", "req.")
    .pressKey("esc")
    .pressKey("up")
    .pressKey("down")
    .pressKey("enter")
    ;
});

