function work(n) {
  const xs = []; for (let i = 1; i <= n; i++) xs.push(i);
  return xs.map(x => x + 1).length;
}
function repeat(times, n, acc) { return times <= 0 ? acc : repeat(times - 1, n, acc + work(n)); }
repeat(20, 50, 0);
const t0 = process.hrtime.bigint(); const r = repeat(200, 50, 0); const t1 = process.hrtime.bigint();
console.log(`node    elapsed_ms=${(Number(t1 - t0) / 1e6).toFixed(2)}  result=${r}`);
