function work(i) {
  const item = { id: i, name: "item", tags: ["a", "b", "c"], active: true };
  const s = JSON.stringify(item);
  const back = JSON.parse(s);
  return s.length + back.id;
}

require("../harness.js").run(() => {
  let acc = 0;
  for (let i = 1; i <= 2000; i++) acc += work(i);
  return acc;
});
