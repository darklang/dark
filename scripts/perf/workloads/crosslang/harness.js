// Run the work, print `result=<v>`. With CROSSLANG_ALLOC=1, also report allocation.
//
// The caller passes --expose-gc and --max-semi-space-size=512 in alloc mode: the first so we can
// collect before measuring, the second so nothing is collected *during* the measured section and
// the heapUsed delta is everything the work allocated.
function run(work) {
  if (process.env.CROSSLANG_ALLOC === "1") {
    global.gc();
    const before = process.memoryUsage().heapUsed;
    const r = work();
    const after = process.memoryUsage().heapUsed;
    process.stderr.write(`alloc_bytes=${after - before}\n`);
    console.log(`result=${r}`);
  } else {
    console.log(`result=${work()}`);
  }
}
module.exports = { run };
