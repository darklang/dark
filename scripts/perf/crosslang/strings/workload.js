function work(i) {
  const base = "field-" + i;
  const parts = [];
  for (let j = 1; j <= 8; j++) parts.push(base + ":" + j);
  const joined = parts.join(",");
  const back = joined.split(",");
  return joined.length + back.length;
}

require("../harness.js").run(() => {
  let acc = 0;
  for (let i = 1; i <= 5000; i++) acc += work(i);
  return acc;
});
