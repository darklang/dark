function enterChangesState(p) {
  p.sendEvent('keypress', p.event.key.Enter);
  waitFor(p, "#entryBox", clickFinishTest)
}


