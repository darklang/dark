function depth(n) {
  return n <= 0 ? 0 : 1 + depth(n - 1);
}

require("../harness.js").run(() => {
  let acc = 0;
  for (let i = 0; i < 50; i++) acc += depth(5000);
  return acc;
});
