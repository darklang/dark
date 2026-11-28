// Shared by the pages: how to call into the runtime, how to boot it, and the helpers every page
// needs to read a command's output.
//
// The runtime runs in a Web Worker (worker.js), so the page stays live while Dark code runs.
// `dark.invoke(name, ...args)` calls one of the exports in `Exports.cs` and resolves with its
// result. `dark.boot(onStatus)` resolves once the CLI can run. `dark.run(argv)` runs one command
// with its output captured and returns { output, code }; the argv splitter honours double quotes.
// `dark.takesTheScreen(argv)` is the list of commands that need a real terminal, which the
// one-shot pages refuse.
window.dark = (() => {
  const screenCommands = new Set(["workbench", "wb", "outliner", "tree-exp", "views", "apps", "text-editor", "agent", "ai"]);
  const worker = new Worker(new URL("worker.js", document.baseURI), { type: "module" });
  const pending = new Map();
  let nextId = 0, onStatus = () => {}, restoreOutcome = "", notice = "";

  worker.onmessage = ({ data }) => {
    if (data.status !== undefined) { onStatus(data.status); return; }
    if (data.event !== undefined) { window.dispatchEvent(new CustomEvent(data.event, { detail: data.detail })); return; }
    // The runtime's own messages pass through here too (a threaded build posts some), and
    // carry none of our ids.
    const p = pending.get(data.id);
    if (!p) return;
    pending.delete(data.id);
    if (data.error !== undefined) p.reject(new Error(data.error)); else p.resolve(data.value);
  };
  worker.onerror = (e) => {
    const err = new Error("the runtime's worker failed: " + (e.message || e));
    for (const p of pending.values()) p.reject(err);
    pending.clear();
  };

  const call = (name, ...args) => new Promise((resolve, reject) => {
    const id = nextId++;
    pending.set(id, { resolve, reject });
    worker.postMessage({ id, call: name, args });
  });

  // `?fresh` and `?restore=<name>` are taken off the address before anything else, so a reload never
  // acts on them twice; the worker does what they ask. `window.` on purpose: cli.html keeps its
  // command history in a top-level `history`, which hides the browser's.
  function takeAddressFlags() {
    const url = new URL(location.href);
    const fresh = url.searchParams.has("fresh"), back = url.searchParams.get("restore");
    if (fresh || back !== null) {
      url.searchParams.delete("fresh"); url.searchParams.delete("restore");
      window.history.replaceState(window.history.state, "", url);
    }
    return { fresh, back };
  }

  async function boot(status = () => {}) {
    onStatus = status;
    const r = await call("boot", { base: document.baseURI, ...takeAddressFlags() });
    restoreOutcome = r.restored; notice = r.notice;
    document.addEventListener("visibilitychange", () => { if (document.visibilityState === "hidden") call("save"); });
  }

  async function run(argv) {
    const res = await call("RunCommand", argv);
    call("save");
    const i = res.lastIndexOf("\n[exit ");
    return { output: res.slice(0, i), code: res.slice(i + 7, -1) };
  }

  const stripAnsi = (s) => s.replace(/\x1b\[[0-9;]*[A-Za-z]/g, "");

  function splitArgv(text) {
    const argv = []; let cur = ""; let quoted = false;
    for (const ch of text) {
      if (ch === '"') { quoted = !quoted; continue; }
      if (!quoted && /\s/.test(ch)) { if (cur) { argv.push(cur); cur = ""; } continue; }
      cur += ch;
    }
    if (cur) argv.push(cur);
    return argv;
  }

  // A site's optional JSON beside the page (showcase.json, build.json); null when absent.
  async function sideFile(name) {
    try { const r = await fetch(new URL(name, document.baseURI)); return r.ok ? await r.json() : null; }
    catch (e) { return null; }
  }

  // `restored`: what Boot did with work kept from an earlier visit ("", "kept", "upgraded",
  // "newer: <why>" or "refused: <why>"). `notice`: what `?fresh` or `?restore` did, or "". `save`:
  // keep the tab's work now, rather than at the next command.
  return { invoke: call, boot, run, save: () => call("save"), restored: () => restoreOutcome, notice: () => notice, stripAnsi, splitArgv, sideFile, takesTheScreen: (argv) => screenCommands.has(argv[0]) };
})();

// On the preview host, build.json says which branch and commit this is: a corner banner.
dark.sideFile("build.json").then((b) => {
  if (!b) return;
  const el = document.createElement("a");
  el.id = "build-banner"; el.href = "/"; el.title = "built " + b.built + " from " + b.sha;
  el.textContent = (b.pr ? "#" + b.pr + " " : "") + b.branch + " @ " + String(b.sha).slice(0, 7);
  document.body.appendChild(el);
});
