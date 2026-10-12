// Shared by the pages: how to call into the runtime, how to boot it and load the store,
// and the two helpers every page needs to read a command's output.
//
// `dark.boot(onStatus)` resolves once the CLI can run (`Cli.Boot` done). `dark.run(argv)`
// runs one command with its output captured and returns { output, code }; the argv
// splitter honours double quotes. `dark.takesTheScreen(argv)` is the list of commands that
// need a real terminal, which the one-shot pages refuse.
//
// The tab keeps its work across reloads: see "Keeping a tab's work" below.
window.dark = (() => {
  const invoke = (name, ...args) => DotNet.invokeMethodAsync("Darklang.Wasm", name, ...args);
  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  const screenCommands = new Set(["workbench", "wb", "outliner", "tree-exp", "views", "apps", "text-editor", "agent", "ai"]);

  // ---- Keeping a tab's work ----
  //
  // The store lives in emscripten's in-memory filesystem, under /dark, and is gone on reload. So the
  // page keeps a copy in the browser's IndexedDB, puts it back before Boot, and Boot decides what to
  // do with it (`Cli.Boot`, `RestoreOutcome`): use it, bring it up to this build first, or refuse it
  // and start fresh. No Dark code is involved and nothing new is granted; it is the host keeping its
  // own store, as the desktop CLI keeps ~/.darklang/data.db.
  //
  // Not kept: credentials.db (any script on this origin can read IndexedDB, and the terminal page
  // loads xterm from a CDN), SQLite's -shm (rebuilt on open), logs, backups, and the shipped store
  // Boot unpacks while upgrading.
  const SAVED = "store";
  const isKept = (name) => !(name === "logs" || name === "backups" || name === "credentials.db"
    || name === "incoming-store.db" || name.endsWith("-shm"));
  const fs = () => Blazor.runtime.Module.FS;
  let db = null;
  const openDb = () => db || (db = new Promise((res, rej) => {
    const r = indexedDB.open("darklang-tab", 1);
    r.onupgradeneeded = () => r.result.createObjectStore("files");
    r.onsuccess = () => res(r.result); r.onerror = () => rej(r.error);
  }));
  const request = async (mode, f) => {
    const d = await openDb();
    return new Promise((res, rej) => {
      const tx = d.transaction("files", mode); const r = f(tx.objectStore("files"));
      tx.oncomplete = () => res(r && r.result); tx.onerror = () => rej(tx.error); tx.onabort = () => rej(tx.error);
    });
  };

  // A snapshot is named ("store", "store-before-upgrade", "store-refused-<time>") and held as one
  // metadata record under its name, { savedAt, files: { path: { size, chunks } } }, plus one
  // record per 48 KB chunk of each file under "<name>|<path>|<index>". Saves write only the chunks
  // whose bytes changed: authoring changes a few chunks of the WAL, while the store around it is
  // 73 MB, nearly all of it the stdlib.
  //
  // Under 64 KB on purpose. Chromium keeps a value above about 64 KB as a separate blob, and reading
  // back a store's worth of those never finished: 73 MB as 64 KB or 256 KB chunks pinned the page for
  // minutes, where 48 KB chunks read back in 0.8 s (measured, headless Chromium).
  const CHUNK = 49152;
  const chunkKey = (name, path, i) => name + "|" + path + "|" + i;
  const chunksOf = (name) => IDBKeyRange.bound(name + "|", name + "|\uffff");

  // Every kept file under /dark, as its live bytes. A view, not a copy, where emscripten's in-memory
  // filesystem allows it: the store is 73 MB and this runs on every save.
  function liveFiles(dir, out) {
    for (const name of fs().readdir(dir)) {
      if (name === "." || name === ".." || !isKept(name)) continue;
      const path = dir + "/" + name; const node = fs().lookupPath(path).node;
      if (fs().isDir(node.mode)) { liveFiles(path, out); continue; }
      out[path] = node.contents instanceof Uint8Array ? node.contents.subarray(0, node.usedBytes) : fs().readFile(path);
    }
    return out;
  }

  // Whether the chunk at `off` differs between `a` and `b`: in length, or in any byte. Compared four
  // bytes at a time where the alignment allows.
  function chunkDiffers(a, b, off) {
    if (!b) return true;
    const end = Math.min(off + CHUNK, a.length);
    if (Math.min(off + CHUNK, b.length) !== end) return true;
    const len = end - off;
    if ((a.byteOffset + off) % 4 === 0 && (b.byteOffset + off) % 4 === 0 && len % 4 === 0) {
      const x = new Int32Array(a.buffer, a.byteOffset + off, len / 4), y = new Int32Array(b.buffer, b.byteOffset + off, len / 4);
      for (let k = 0; k < x.length; k++) if (x[k] !== y[k]) return true;
      return false;
    }
    for (let k = off; k < end; k++) if (a[k] !== b[k]) return true;
    return false;
  }

  // `prev` at `size` bytes: itself when the size is unchanged, else a copy cut or padded to fit.
  function resized(prev, size) {
    if (prev && prev.length === size) return prev;
    const b = new Uint8Array(size);
    if (prev) b.set(prev.subarray(0, Math.min(prev.length, size)));
    return b;
  }

  // Copy snapshot `from` to `to`, replacing whatever `to` held, in one transaction. Rare (an upgrade,
  // a refusal), so it copies every chunk rather than being clever.
  async function copySnapshot(from, to) {
    const meta = await request("readonly", (st) => st.get(from));
    if (!meta) return;
    const keys = await request("readonly", (st) => st.getAllKeys(chunksOf(from)));
    const values = await request("readonly", (st) => st.getAll(chunksOf(from)));
    await request("readwrite", (st) => {
      st.delete(chunksOf(to));
      keys.forEach((k, i) => st.put(values[i], to + k.slice(from.length)));
      return st.put(meta, to);
    });
  }
  async function deleteSnapshot(name) {
    await request("readwrite", (st) => { st.delete(chunksOf(name)); return st.delete(name); });
  }

  // `keptBytes` holds a copy of every file as last saved, which is what a save compares against. Not
  // size and mtime: SQLite's in-place writes in the tab do not reliably move emscripten's mtime, so a
  // change to data.db that leaves its size alone would be missed. It costs a second copy of the store
  // in memory, and the comparison is about 25 ms over 73 MB (AOT, headless Chromium).
  let persisting = false, savedMeta = null, keptBytes = new Map(), restoreOutcome = "", lastFailure = null, notice = "";

  // Save what changed since the last save, if anything did. Saves run one after another, so a caller
  // that awaits one knows the work it did before is kept. The chunks and the metadata naming them go
  // in one transaction, so a save that fails part way leaves the previous snapshot whole.
  let queue = Promise.resolve();
  function save() {
    queue = queue.then(async () => {
      if (!persisting) return;
      try {
        // One synchronous pass over every file, so the snapshot is one moment: the store and its WAL
        // as they stood together, with nothing from the runtime landing in between.
        const now = liveFiles("/dark", {});
        const old = (savedMeta && savedMeta.files) || {};
        const files = {}; const puts = []; const deletes = [];
        for (const [path, bytes] of Object.entries(now)) {
          const prev = keptBytes.get(path);
          const chunks = Math.max(1, Math.ceil(bytes.length / CHUNK));
          for (let i = 0; i < chunks; i++)
            if (chunkDiffers(bytes, prev, i * CHUNK)) puts.push([path, i, bytes.slice(i * CHUNK, (i + 1) * CHUNK)]);
          const was = old[path] ? old[path].chunks : 0;
          for (let i = chunks; i < was; i++) deletes.push(chunkKey(SAVED, path, i));
          files[path] = { size: bytes.length, chunks };
        }
        for (const [path, f] of Object.entries(old))
          if (!(path in now)) for (let i = 0; i < f.chunks; i++) deletes.push(chunkKey(SAVED, path, i));
        // A new file always has a differing chunk and a removed one always has chunks to delete, so
        // these two say everything that changed.
        if (puts.length > 0 || deletes.length > 0 || !savedMeta) {
          const meta = { savedAt: Date.now(), files };
          await request("readwrite", (st) => {
            for (const [path, i, part] of puts) st.put(part, chunkKey(SAVED, path, i));
            for (const k of deletes) st.delete(k);
            return st.put(meta, SAVED);
          });
          // Only now, so the copy never runs ahead of what IndexedDB holds. From the parts this save
          // wrote, which were taken in the same pass as everything else.
          const next = new Map();
          for (const [path, f] of Object.entries(files)) next.set(path, resized(keptBytes.get(path), f.size));
          for (const [path, i, part] of puts) next.get(path).set(part, i * CHUNK);
          savedMeta = meta;
          keptBytes = next;
        }
      } catch (e) {
        // Said where a person will see it, once per distinct failure: a quota reached or storage
        // cleared would otherwise leave them working on with nothing being kept.
        const why = String(e && e.name ? e.name + ": " + e.message : e);
        console.warn("[dark] could not keep this tab's work: " + why);
        if (why !== lastFailure) { lastFailure = why; window.dispatchEvent(new CustomEvent("dark-save-failed", { detail: why })); }
        return;
      }
      lastFailure = null;
    });
    return queue;
  }

  // Put back what an earlier visit kept, before Boot. True when there was something.
  //
  // A snapshot that cannot be put back whole (a missing chunk, storage half cleared) is set aside
  // under a name of its own before the tab starts fresh, so the first save cannot overwrite it.
  async function restore() {
    let meta = null;
    try {
      meta = await request("readonly", (st) => st.get(SAVED));
      if (!meta || !meta.files) return false;
      const keys = await request("readonly", (st) => st.getAllKeys(chunksOf(SAVED)));
      const values = await request("readonly", (st) => st.getAll(chunksOf(SAVED)));
      const byKey = new Map(keys.map((k, i) => [k, values[i]]));
      for (const [path, f] of Object.entries(meta.files)) {
        const bytes = new Uint8Array(f.size);
        let off = 0;
        for (let i = 0; i < f.chunks; i++) {
          const part = byKey.get(chunkKey(SAVED, path, i));
          if (!part) throw new Error("chunk " + i + " of " + path + " is missing");
          bytes.set(part, off); off += part.length;
        }
        if (off !== f.size) throw new Error(path + " is " + off + " bytes, expected " + f.size);
        let dir = "";
        for (const part of path.split("/").slice(1, -1)) { dir += "/" + part; try { fs().mkdir(dir); } catch (e) {} }
        fs().writeFile(path, bytes);
        keptBytes.set(path, bytes);
      }
      savedMeta = meta;
      return true;
    } catch (e) {
      console.warn("[dark] could not read this tab's kept work: " + e);
      if (meta) {
        try { await copySnapshot(SAVED, SAVED + "-unreadable-" + new Date().toISOString()); await deleteSnapshot(SAVED); } catch (e2) {}
        window.dispatchEvent(new CustomEvent("dark-save-failed", { detail: "your earlier work could not be read back (" + e.message + "); it is set aside, and this tab starts fresh" }));
      }
      for (const name of fs().readdir("/dark")) if (name !== "." && name !== ".." && isKept(name)) { try { fs().unlink("/dark/" + name); } catch (e3) {} }
      return false;
    }
  }

  // After Boot: act on what it did with the restored store, then keep saving.
  async function keepSaving(restored) {
    const outcome = restored ? await invoke("RestoreOutcome") : "";
    restoreOutcome = outcome;
    // Saved by a newer release than this page runs (an older deploy, or a cached page). The newer
    // build wants that store as it was, so this tab leaves it alone and saves nothing.
    if (outcome.startsWith("newer")) { console.log("[dark] kept work in this browser: " + outcome); return; }
    try {
      if (outcome.startsWith("refused")) {
        // Never overwritten: set aside under a name of its own, for whoever can read it later.
        await copySnapshot(SAVED, SAVED + "-refused-" + new Date().toISOString());
        await deleteSnapshot(SAVED);
        savedMeta = null; keptBytes = new Map();
      } else if (outcome === "upgraded") {
        // One copy of the store as the older build left it, the way the desktop backs up before an upgrade.
        await copySnapshot(SAVED, SAVED + "-before-upgrade");
      }
    } catch (e) { console.warn("[dark] could not set aside this tab's earlier work: " + e); }
    persisting = true;
    await save();
    // The workbench runs as one long command, so saving after commands alone would miss everything
    // done inside it.
    setInterval(save, 15000);
    document.addEventListener("visibilitychange", () => { if (document.visibilityState === "hidden") save(); });
    if (outcome) console.log("[dark] kept work in this browser: " + outcome);
  }

  // Start over, or go back, from the address. `?fresh` sets the saved work aside and the tab starts
  // from the shipped store; `?restore=<name>` puts a set-aside back, setting aside what it replaces.
  // Both run before anything is restored, so they work when the tab will not come up.
  //
  // Both are taken off the address first. Without that, a reload of a `?fresh` page would set aside,
  // again, everything done since the first one: the one way this could lose work.
  async function actOnAddress() {
    const url = new URL(location.href);
    const fresh = url.searchParams.has("fresh"), back = url.searchParams.get("restore");
    if (!fresh && back === null) return;
    url.searchParams.delete("fresh"); url.searchParams.delete("restore");
    // `window.` on purpose: cli.html keeps its command history in a top-level `history`, which hides
    // the browser's from every script on that page.
    window.history.replaceState(window.history.state, "", url);
    const exists = async (name) => !!(await request("readonly", (st) => st.get(name)));
    const setAside = async () => {
      if (!(await exists(SAVED))) return null;
      const name = SAVED + "-cleared-" + new Date().toISOString();
      await copySnapshot(SAVED, name);
      return name;
    };
    try {
      if (fresh) {
        const aside = await setAside();
        await deleteSnapshot(SAVED);
        notice = aside ? "Started fresh. Your earlier work is set aside; to go back, open this page with ?restore=" + aside : "Started fresh.";
      } else if (!back.startsWith(SAVED + "-") || back.includes("|") || !(await exists(back))) {
        notice = "Nothing set aside in this browser is called " + back + "; nothing was changed.";
      } else {
        const aside = await setAside();
        await copySnapshot(back, SAVED);
        notice = "Restored " + back + "." + (aside ? " What was here is set aside; to go back to it, open this page with ?restore=" + aside : "");
      }
    } catch (e) {
      notice = "Could not " + (fresh ? "start fresh" : "restore " + back) + " (" + e + "); nothing was changed.";
    }
    console.log("[dark] " + notice);
  }

  async function boot(onStatus = () => {}) {
    onStatus("loading the Darklang runtime");
    for (let i = 0; i < 80 && !(typeof Blazor !== "undefined" && Blazor.start); i++) await sleep(250);
    if (!(typeof Blazor !== "undefined" && Blazor.start)) throw new Error("_framework/blazor.webassembly.js did not load");
    await Blazor.start();
    // The JS-interop dispatcher isn't ready the instant Blazor.start() resolves.
    let ready = false;
    for (let i = 0; i < 120 && !ready; i++) { try { await invoke("Ready"); ready = true; } catch (e) { await sleep(500); } }
    if (!ready) throw new Error("runtime did not become ready");
    await actOnAddress();
    const restored = await restore();
    onStatus(restored ? "loading your work from this browser" : "loading the package store");
    // store.json names the store file and its inflated size (the one-shot brotli decoder needs it).
    const manifest = await (await fetch(new URL("store.json", document.baseURI))).json();
    await invoke("Boot", new URL(manifest.file, document.baseURI).href, manifest.size);
    await keepSaving(restored);
  }

  async function run(argv) {
    const res = await invoke("RunCommand", argv);
    save();
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

  // `restored`: what Boot did with work kept from an earlier visit ("", "kept", "upgraded",
  // "newer: <why>" or "refused: <why>"). `notice`: what `?fresh` or `?restore` did, or "". `save`:
  // keep the tab's work now, rather than at the next command.
  return { invoke, boot, run, save, restored: () => restoreOutcome, notice: () => notice, stripAnsi, splitArgv, takesTheScreen: (argv) => screenCommands.has(argv[0]) };
})();
