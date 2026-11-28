// For headless-check.mjs against a deployed preview's /?cmd=status page: the runtime boots,
// the store loads, the command answers, and the build banner from host.sh is on the page.
// The driver wraps this in an async function body, so it returns rather than evaluates.
//
//   node backend/src/Wasm/headless-check.mjs https://wasm.darklang.com/latest/?cmd=status \
//        backend/src/Wasm/deploy/preview/check.js 180
const out = document.getElementById("out");
const text = out ? out.textContent : "";
const banner = document.getElementById("build-banner");
if (out && out.className === "err") return "FAIL: " + text.slice(0, 200);
if (!text || text === "loading" || /^loading /.test(text)) return "booting: " + text;
if (!banner) return "FAIL: command answered but no build banner (build.json missing?)";
return "DONE: " + banner.textContent + " | " + text.split("\n")[0].slice(0, 120);
