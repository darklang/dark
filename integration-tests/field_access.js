function testFn(p) {
  screenshot(p, "start");
  p.sendEvent('keypress', p.event.key.Enter);
  screenshot(p, "enter1");
  p.sendEvent('keypress', p.event.key.R);
  screenshot(p, "r");
  p.sendEvent('keypress', p.event.key.E);
  screenshot(p, "e");
  
  p.sendEvent('keypress', p.event.key.Q);
  screenshot(p, "q");
  p.sendEvent('keypress', p.event.key.Dot);
  screenshot(p, ".");
  p.sendEvent('keypress', p.event.key.B);
  screenshot(p, "b");
  p.sendEvent('keypress', p.event.key.O);
  screenshot(p, "o");
  p.sendEvent('keypress', p.event.key.Enter);
  screenshot(p, "enter2");
  waitFor(p, ".ast", finishTest)
}
