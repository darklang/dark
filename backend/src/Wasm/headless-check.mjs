#!/usr/bin/env node
// Drive a headless Chromium at the published page over CDP and run a small script in it.
// No playwright/puppeteer needed: Node 22 has a WebSocket global, and the browser binary
// playwright once downloaded is enough.
//
//   node headless-check.mjs <url> <script-file> [timeout-seconds]
//
// The script file is JS evaluated in the page; it must return (or resolve to) a string,
// which is printed. Console messages and page errors are echoed to stderr.

import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { homedir } from "node:os";

const [url, scriptFile, timeoutArg] = process.argv.slice(2);
if (!url || !scriptFile) {
  console.error("usage: headless-check.mjs <url> <script.js> [timeout-seconds]");
  process.exit(2);
}
const timeoutMs = 1000 * (Number(timeoutArg) || 120);
const script = readFileSync(scriptFile, "utf8");

const candidates = [
  process.env.CHROME,
  `${homedir()}/.cache/ms-playwright/chromium-1234/chrome-linux64/chrome`,
  "/usr/bin/chromium",
  "/usr/bin/chromium-browser",
  "/usr/bin/google-chrome",
].filter(Boolean);
const chrome = candidates.find((p) => {
  try { readFileSync(p, { flag: "r" }); return true; } catch { return false; }
});
if (!chrome) { console.error("no chromium found; set CHROME"); process.exit(2); }

const port = 9222 + Math.floor(Math.random() * 1000);
const proc = spawn(chrome, [
  "--headless=new", "--no-sandbox", "--disable-gpu", "--no-first-run",
  `--remote-debugging-port=${port}`, "--window-size=1200,800", "about:blank",
], { stdio: ["ignore", "ignore", "pipe"] });
proc.stderr.on("data", () => {});
const cleanup = () => { try { proc.kill("SIGKILL"); } catch {} };
process.on("exit", cleanup);

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
let target;
for (let i = 0; i < 100 && !target; i++) {
  try {
    const list = await (await fetch(`http://127.0.0.1:${port}/json/list`)).json();
    target = list.find((t) => t.type === "page");
  } catch { await sleep(100); }
}
if (!target) { console.error("chrome did not expose a page target"); process.exit(1); }

const ws = new WebSocket(target.webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });
let nextId = 1;
const pending = new Map();
ws.onmessage = (ev) => {
  const msg = JSON.parse(ev.data);
  if (msg.id && pending.has(msg.id)) {
    const { res, rej } = pending.get(msg.id); pending.delete(msg.id);
    msg.error ? rej(new Error(JSON.stringify(msg.error))) : res(msg.result);
  } else if (msg.method === "Runtime.consoleAPICalled") {
    const text = msg.params.args.map((a) => a.value ?? a.description ?? "").join(" ");
    console.error(`[console.${msg.params.type}] ${text}`);
  } else if (msg.method === "Runtime.exceptionThrown") {
    console.error(`[pageerror] ${msg.params.exceptionDetails.text} ${msg.params.exceptionDetails.exception?.description ?? ""}`);
  }
};
const send = (method, params = {}) => new Promise((res, rej) => {
  const id = nextId++; pending.set(id, { res, rej });
  ws.send(JSON.stringify({ id, method, params }));
});

await send("Runtime.enable");
await send("Page.enable");
await send("Page.navigate", { url });
const timer = setTimeout(() => { console.error("timeout"); cleanup(); process.exit(1); }, timeoutMs);
const result = await send("Runtime.evaluate", {
  expression: `(async () => { ${script} })()`,
  awaitPromise: true, returnByValue: true, timeout: timeoutMs,
});
clearTimeout(timer);
if (result.exceptionDetails) {
  console.error("script threw:", result.exceptionDetails.exception?.description ?? result.exceptionDetails.text);
  cleanup(); process.exit(1);
}
console.log(result.result.value);
cleanup();
process.exit(0);
