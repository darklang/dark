// Cumulative allocation: give the young generation enough room that no GC runs during the
// measured section, so heapUsed growth == every byte allocated.
const v8 = require('v8');
function work(n){const xs=[];for(let i=1;i<=n;i++)xs.push(i);return xs.map(x=>x+1).length;}
function repeat(t,n,a){return t<=0?a:repeat(t-1,n,a+work(n));}
repeat(20,50,0);
global.gc();
const gcBefore = v8.getHeapStatistics().number_of_native_contexts; // unused, kept for parity
const before = process.memoryUsage().heapUsed;
const r = repeat(200,50,0);
const after = process.memoryUsage().heapUsed;
console.log(`node   cumulative=${((after-before)/1024).toFixed(1)} KB  per_iter=${((after-before)/200/1024).toFixed(2)} KB`);
