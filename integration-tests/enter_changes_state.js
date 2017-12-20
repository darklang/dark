function testFn(p) {
  p.sendEvent('keypress', p.event.key.Enter);
  waitFor(p, "#entryBox", finishTest)
}
