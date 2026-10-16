function work(n) {
  const d = new Map();
  for (let i = 1; i <= n; i++) d.set("k" + i, i);
  let acc = 0;
  for (let i = 1; i <= n; i++) acc += d.get("k" + i);
  return acc;
}

require("../harness.js").run(() => {
  let acc = 0;
  for (let i = 0; i < 1000; i++) acc += work(50);
  return acc;
});
