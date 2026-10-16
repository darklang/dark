function step(p) {
  return { ...p, x: p.x + 1, y: p.y + 2 };
}

function work(i) {
  const p = { x: i, y: i + 1, label: "p" };
  const q = step(step(p));
  return q.x + q.y;
}

require("../harness.js").run(() => {
  let acc = 0;
  for (let i = 1; i <= 20000; i++) acc += work(i);
  return acc;
});
