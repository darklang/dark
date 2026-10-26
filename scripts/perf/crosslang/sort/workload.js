require("../harness.js").run(() => {
  const xs = [];
  let s = 1;
  for (let i = 0; i < 50000; i++) {
    s = (s * 48271) % 2147483647;
    xs.push(s);
  }
  xs.sort((a, b) => a - b);
  return xs[0] + xs[xs.length - 1] + xs.length;
});
