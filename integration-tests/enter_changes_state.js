// function testFn(p) {
//   p.sendEvent('keypress', p.event.key.Enter);
//   waitFor(p, "#entryBox", finishTest)
// }
//

import { Selector } from 'testcafe';

fixture `Getting started`
  .beforeEach( async t => {
    const testname = t.testRun.test.name;
    const url = "http://test_" + testname + ".localhost:8000/admin/integration_test";
    const pageLoaded = Selector('#darkErrors').exists;
    await t
      .setTestSpeed(0.5)
      .setPageLoadTimeout(0)
      .navigateTo(url)
      .expect(pageLoaded).ok()
      ;
  })
  .afterEach( async t => {
    const signal = Selector('#integrationTestSignal');
    await t
      .click("#finishIntegrationTest")
      .expect(signal.exists).ok()
      .takeScreenshot("a.png")
      .expect(signal.hasClass("success")).eql(true)
  })




test('enter_changes_state', async t => {
  const entryBoxAvailable = Selector('#entryBox').exists;
  await t
    .pressKey("enter")
    .expect(entryBoxAvailable).ok()
   ;
});
