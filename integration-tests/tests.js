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

    if (await signal.hasClass("failure")) {
      await t.expect("error message").eql(await signal.textContent);
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
    .typeText("#entryBox", "req.bo")
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

