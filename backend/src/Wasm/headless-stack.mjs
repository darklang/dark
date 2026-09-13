#!/usr/bin/env node
// Open the page headless, wait N seconds, then interrupt the main thread with
// Debugger.pause and print the call stack. For finding where a wasm app is spinning.
//
//   node headless-stack.mjs <url> <seconds-before-pause>

import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { homedir } from "node:os";

const [url, secsArg] = process.argv.slice(2);
const chrome = [
  process.env.CHROME,
  `${homedir()}/.cache/ms-playwright/chromium-1234/chrome-linux64/chrome`,
].filter(Boolean).find((p) => { try { readFileSync(p, { flag: "r" }); return true; } catch { return false; } });
const port = 9222 + Math.floor(Math.random() * 1000);
const proc = spawn(chrome, ["--headless=new", "--no-sandbox", "--disable-gpu", "--no-first-run",
  `--remote-debugging-port=${port}`, "--window-size=1200,800", "about:blank"], { stdio: ["ignore", "ignore", "pipe"] });
proc.stderr.on("data", () => {});
const cleanup = () => { try { proc.kill("SIGKILL"); } catch {} };
process.on("exit", cleanup);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
let target;
for (let i = 0; i < 100 && !target; i++) {
  try { target = (await (await fetch(`http://127.0.0.1:${port}/json/list`)).json()).find((t) => t.type === "page"); }
  catch { await sleep(100); }
}
const ws = new WebSocket(target.webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });
let nextId = 1; const pending = new Map();
let paused = null;
ws.onmessage = (ev) => {
  const msg = JSON.parse(ev.data);
  if (msg.id && pending.has(msg.id)) { const { res } = pending.get(msg.id); pending.delete(msg.id); res(msg.result ?? msg.error); }
  else if (msg.method === "Debugger.paused") paused = msg.params;
  else if (msg.method === "Runtime.consoleAPICalled") console.error("[console] " + msg.params.args.map((a) => a.value ?? a.description ?? "").join(" "));
};
const send = (method, params = {}) => new Promise((res) => { const id = nextId++; pending.set(id, { res }); ws.send(JSON.stringify({ id, method, params })); });
await send("Runtime.enable");
await send("Debugger.enable");
await send("Page.navigate", { url });
await sleep(1000 * Number(secsArg || 60));
send("Debugger.pause");
for (let i = 0; i < 100 && !paused; i++) await sleep(200);
if (!paused) { console.log("could not pause"); cleanup(); process.exit(1); }
console.log("reason: " + paused.reason);
for (const f of paused.callFrames.slice(0, 40)) {
  console.log(`${f.functionName || "(anon)"}  ${f.url.split("/").pop()}:${f.location.lineNumber}`);
}
cleanup(); process.exit(0);
