// Shared by the pages: how to call into the runtime, how to boot it and load the store,
// and the two helpers every page needs to read a command's output.
//
// `dark.boot(onStatus)` resolves once the CLI can run (`Cli.Boot` done). `dark.run(argv)`
// runs one command with its output captured and returns { output, code }; the argv
// splitter honours double quotes. `dark.takesTheScreen(argv)` is the list of commands that
// need a real terminal, which the one-shot pages refuse.
window.dark = (() => {
  const invoke = (name, ...args) => DotNet.invokeMethodAsync("Darklang.Wasm", name, ...args);
  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  const screenCommands = new Set(["workbench", "wb", "outliner", "tree-exp", "views", "apps", "text-editor", "agent", "ai"]);

  async function boot(onStatus = () => {}) {
    onStatus("loading the Darklang runtime");
    for (let i = 0; i < 80 && !(typeof Blazor !== "undefined" && Blazor.start); i++) await sleep(250);
    if (!(typeof Blazor !== "undefined" && Blazor.start)) throw new Error("_framework/blazor.webassembly.js did not load");
    await Blazor.start();
    // The JS-interop dispatcher isn't ready the instant Blazor.start() resolves.
    let ready = false;
    for (let i = 0; i < 120 && !ready; i++) { try { await invoke("Ready"); ready = true; } catch (e) { await sleep(500); } }
    if (!ready) throw new Error("runtime did not become ready");
    onStatus("loading the package store");
    // store.json names the store file and its inflated size (the one-shot brotli decoder needs it).
    const manifest = await (await fetch(new URL("store.json", document.baseURI))).json();
    await invoke("Boot", new URL(manifest.file, document.baseURI).href, manifest.size);
  }

  async function run(argv) {
    const res = await invoke("RunCommand", argv);
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

  return { invoke, boot, run, stripAnsi, splitArgv, takesTheScreen: (argv) => screenCommands.has(argv[0]) };
})();
