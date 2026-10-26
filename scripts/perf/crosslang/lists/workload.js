function work(n) {
  const xs = [];
  for (let i = 1; i <= n; i++) xs.push(i);
  const ys = xs.map(x => x + 1);
  const zs = ys.filter(x => x % 2 === 0);
  return zs.length;
}

require("../harness.js").run(() => {
  let acc = 0;
  for (let i = 0; i < 2000; i++) acc += work(50);
  return acc;
});
