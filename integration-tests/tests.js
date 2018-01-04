import { Selector } from 'testcafe';

fixture `Integration Tests`
  .beforeEach( async t => {
    const testname = t.testRun.test.name;
    const url = "http://test_" + testname + ".localhost:8000/admin/integration_test";
    const pageLoaded = Selector('#darkErrors').exists;
    await t
      .navigateTo(url)
      .expect(pageLoaded).ok()
      ;
  })
  .afterEach( async t => {
    const signal = Selector('#integrationTestSignal');
    await t
      .click("#finishIntegrationTest")
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
  const entryBoxAvailable = Selector('#entryBox').exists;
  await t
    .pressKey("enter")
    .expect(entryBoxAvailable).ok()
   ;
});

test('field_access', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entryBox", "req.bo", { speed: 0.6 })
    .pressKey("enter")
    .expect(astAvailable).ok()
    ;
});

test('field_access_closes', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entryBox", "req.bo")
    .pressKey("enter")
    .expect(astAvailable).ok()
    ;
});

test('field_access_pipes', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entryBox", "req.bo")
    .pressKey("shift+enter")
    .expect(astAvailable).ok()
    ;
});


test('pipeline_let_equals', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entryBox", "3")
    .pressKey("shift+enter")
    .typeText("#entryBox", "= value")
    .pressKey("enter")
    .expect(astAvailable).ok()
    ;
});

test('tabbing_works', async t => {
  // Fill in "then" box in if stmt
  const astAvailable = Selector('.ast').exists;
  await t
    .pressKey("enter")
    .typeText("#entryBox", "if")
    .pressKey("down")
    .pressKey("enter")
    .pressKey("esc")
    .pressKey("tab")
    .pressKey("enter")
    .typeText("#entryBox", "5")
    .pressKey("enter")
    .expect(astAvailable).ok()
    ;
});

test('next_sibling_works', async t => {
  const astAvailable = Selector('.ast').exists;
  await t
    .click(".ast")
    .pressKey("down")
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
    .pressKey("down")
    .pressKey("enter")
    ;
  // TODO: this might be flaky. Maybe wait for a specific ID to be
  // selected?
});

