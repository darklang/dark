require("../harness.js").run(() => {
  let acc = 0;
  for (let i = 1; i <= 300000; i++) acc = (acc + i * i) % 1000003;
  return acc;
});
