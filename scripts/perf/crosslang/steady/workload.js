function work(n) {
  const xs = [];
  for (let i = 1; i <= n; i++) xs.push(i);
  return xs.map(x => x + 1).length;
}

function repeat(times, n, acc) {
  return times <= 0 ? acc : repeat(times - 1, n, acc + work(n));
}

require("../harness.js").run(() => repeat(200, 50, 0));
