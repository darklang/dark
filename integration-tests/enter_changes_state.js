// function testFn(p) {
//   p.sendEvent('keypress', p.event.key.Enter);
//   waitFor(p, "#entryBox", finishTest)
// }
//

import { Selector } from 'testcafe';

fixture `Getting started`
  .page `http://test_enter_changes_state.localhost:8000/admin/integration_test`;


test('enter_changes_state', async t => {
  const pageLoaded = Selector('#darkErrors').exists;
  const entryBoxAvailable = Selector('#entryBox').exists;
  const signal = Selector('#integrationTestSignal');

  await t
    .setTestSpeed(0.5)
    .setPageLoadTimeout(0)
    .expect(pageLoaded).ok()
    .pressKey("enter")
    .expect(entryBoxAvailable).ok()
    .click("#finishIntegrationTest")
    .expect(signal.exists).ok()
    .takeScreenshot("a.png")
    .expect(signal.hasClass("success")).eql(true)
    ;
});
