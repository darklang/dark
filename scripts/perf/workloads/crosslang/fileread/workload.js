const fs = require("fs");

require("../harness.js").run(() => {
  const lines = fs.readFileSync("rundir/crosslang-lines.txt", "utf8").split("\n");
  let a = 0, b = 0;
  for (const l of lines) {
    if (l.includes("LEVEL=")) a++;
    if (l.includes("ERROR")) b++;
  }
  return a + b;
});
