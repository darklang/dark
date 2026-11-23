"""The lsp gate's checks: `dark lsp` driven over stdio the way an editor drives it.

    python3 scripts/testing/_lsp-gate.py <cli>

Run by `scripts/testing/gates lsp`, which gives it a throwaway home with its own store.
"""

import json
import os
import sys
import queue
import subprocess
import threading
import time


class Server:
    def __init__(self, cli, env=None, args=()):
        self.proc = subprocess.Popen(
            [cli, "lsp", "--stdio", *args],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=env or os.environ.copy(),
        )
        self.messages = queue.Queue()
        self.seen = []
        self.next_id = 1
        self.stray = bytearray()
        self.log = bytearray()
        threading.Thread(target=self._read_stdout, daemon=True).start()
        threading.Thread(target=self._read_stderr, daemon=True).start()

    def _read_stdout(self):
        out = self.proc.stdout
        while True:
            header = out.readline()
            if not header:
                return
            if not header.startswith(b"Content-Length:"):
                self.stray += header
                continue
            length = int(header.split(b":")[1])
            if out.readline() not in (b"\r\n", b"\n"):
                self.stray += b"<no blank line after the header>"
            self.messages.put(json.loads(out.read(length)))

    def _read_stderr(self):
        for line in self.proc.stderr:
            self.log += line

    def send(self, message):
        message = {"jsonrpc": "2.0", **message}
        body = json.dumps(message).encode()
        self.proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.proc.stdin.flush()

    def notify(self, method, params=None):
        self.send({"method": method, "params": params or {}})

    def request(self, method, params=None, timeout=60):
        rid = self.next_id
        self.next_id += 1
        self.send({"id": rid, "method": method, "params": params or {}})
        return self.wait(lambda m: m.get("id") == rid and "method" not in m, timeout, f"a reply to {method}")

    def wait(self, pred, timeout, what):
        for m in self.seen:
            if pred(m):
                self.seen.remove(m)
                return m
        deadline = time.time() + timeout
        while time.time() < deadline:
            try:
                m = self.messages.get(timeout=max(0.01, deadline - time.time()))
            except queue.Empty:
                break
            if pred(m):
                return m
            self.seen.append(m)
        raise Failure(f"no {what} within {timeout}s")

    def diagnostics(self, uri, timeout=60):
        m = self.wait(
            lambda m: m.get("method") == "textDocument/publishDiagnostics" and m["params"]["uri"] == uri,
            timeout,
            f"diagnostics for {uri}",
        )
        return m["params"]["diagnostics"]

    def quiet(self, seconds):
        """Every message that arrives in the next <seconds>."""
        time.sleep(seconds)
        got = list(self.seen)
        self.seen.clear()
        while not self.messages.empty():
            got.append(self.messages.get())
        return got

    def open(self, uri, text):
        self.notify(
            "textDocument/didOpen",
            {"textDocument": {"uri": uri, "languageId": "darklang", "version": 1, "text": text}},
        )

    def change(self, uri, version, text):
        self.notify(
            "textDocument/didChange",
            {"textDocument": {"uri": uri, "version": version}, "contentChanges": [{"text": text}]},
        )

    def close_stdin(self):
        self.proc.stdin.close()

    def exit_code(self, timeout=30):
        try:
            return self.proc.wait(timeout)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            raise Failure(f"the server was still running {timeout}s after it should have stopped")

    def kill(self):
        if self.proc.poll() is None:
            self.proc.kill()


class Failure(Exception):
    pass


# --- the checks ---


def check(ok, message):
    if not ok:
        raise Failure(message)


def messages_of(diagnostics):
    return [d["message"] for d in diagnostics]


def run(cli):
    checked = []

    # The default policy a fresh install has: no grants, and the server must still answer.
    s = Server(cli)
    try:
        refresh = {"refreshSupport": True}
        init = s.request(
            "initialize",
            {"processId": None, "rootUri": None, "capabilities": {"workspace": {"inlayHint": refresh, "codeLens": refresh, "semanticTokens": refresh}}},
        )
        caps = init["result"]["capabilities"]
        check(caps["textDocumentSync"]["change"] == 2, f"unexpected sync kind: {caps}")
        s.notify("initialized")

        # A syntax error is an error on its line.
        uri = "file:///tmp/lsp-gate/syntax.dark"
        s.open(uri, "let f(x: Int64): Int64 =\n  x +\n")
        d = s.diagnostics(uri)
        check(any(x["severity"] == 1 for x in d), f"no error for a syntax error: {d}")
        checked.append("a syntax error is reported as an error")

        # A name that resolves to nothing is a warning at the name.
        uri = "file:///tmp/lsp-gate/names.dark"
        s.open(uri, "let f(): Int64 = LspGate.T.later()\n")
        d = s.diagnostics(uri)
        check(
            any(x["severity"] == 2 and "LspGate.T.later" in x["message"] for x in d),
            f"no warning for an unresolved name: {d}",
        )
        checked.append("an unresolved name is a warning")

        # Hot reload: another process defines the name, and the open document's warning goes,
        # with no edit to the document.
        defined = subprocess.run(
            [cli, "fn", "/LspGate.T.later", "(): Int64 = 1"], capture_output=True, text=True
        )
        check(defined.returncode == 0, f"could not define LspGate.T.later: {defined.stdout}{defined.stderr}")
        d = s.diagnostics(uri, timeout=60)
        check(
            not any("LspGate.T.later" in m for m in messages_of(d)),
            f"the warning stayed after the name was defined elsewhere: {d}",
        )
        s.wait(lambda m: m.get("method") == "dark/storeChanged", 30, "dark/storeChanged")
        checked.append("a name defined in another process clears its warning (store change)")

        # A type error comes from the at-rest checker.
        uri = "file:///tmp/lsp-gate/types.dark"
        s.open(uri, 'let f(x: Int64): String = x + 1\n')
        d = s.diagnostics(uri)
        check(any(x["severity"] == 1 and x.get("code") for x in d), f"no checker error: {d}")
        checked.append("a type error is reported with the checker's code")

        # Typing: ten keystrokes in a burst are one analysis, not ten.
        uri = "file:///tmp/lsp-gate/typing.dark"
        s.open(uri, "let double(x: Int64): Int64 = x * 2\n")
        s.diagnostics(uri)
        for v in range(2, 12):
            s.change(uri, v, "let double(x: Int64): Int64 = x * 2" + " " * v + "\n")
        publishes = [
            m
            for m in s.quiet(5)
            if m.get("method") == "textDocument/publishDiagnostics" and m["params"]["uri"] == uri
        ]
        check(len(publishes) == 1, f"ten keystrokes gave {len(publishes)} analyses, not one")
        checked.append("ten keystrokes, one analysis")

        # Non-ASCII text: Content-Length counts bytes, so the message after it must still parse.
        uri = "file:///tmp/lsp-gate/utf8.dark"
        s.open(uri, "// café -> naïve über\nlet x(): Int64 = 1\n")
        s.diagnostics(uri)
        reply = s.request("dark/noSuchMethod")
        check(reply.get("error", {}).get("code") == -32601, f"an unknown request was not refused: {reply}")
        checked.append("non-ASCII text keeps the stream in step; an unknown request gets -32601")

        # Closing clears the squiggles.
        s.notify("textDocument/didClose", {"textDocument": {"uri": uri}})
        check(s.diagnostics(uri) == [], "closing a document did not clear its diagnostics")

        # Hover: a package function's signature, its doc comment, and what it does to the world.
        uri = "file:///tmp/lsp-gate/hover.dark"
        s.open(uri, "let f(xs: List<Int64>): List<Int64> =\n  Stdlib.List.map(xs, fun x -> x + 1)\n\nlet g(): String = Stdlib.Cli.File.readText(\"a\") |> Stdlib.Result.withDefault(\"\")\nlet h(s: String): Int64 = Stdlib.String.length(s)\n")
        s.diagnostics(uri)
        pos = lambda line, ch: {"textDocument": {"uri": uri}, "position": {"line": line, "character": ch}}
        hover = s.request("textDocument/hover", pos(1, 16))["result"]
        check(hover is not None, "no hover on Stdlib.List.map")
        text = hover["contents"]["value"]
        check("Stdlib.List.map(" in text, f"the hover does not show the signature: {text}")
        check("`fn`" in text, f"the doc comment's markup was not turned into code: {text}")
        text = s.request("textDocument/hover", pos(4, 40))["result"]["contents"]["value"]
        check("effect-free" in text, f"the hover does not say String.length is effect-free: {text}")
        check(hover["range"]["start"] == {"line": 1, "character": 2}, f"hover range: {hover['range']}")
        text = s.request("textDocument/hover", pos(3, 40))["result"]["contents"]["value"]
        check("effects:" in text, f"the hover does not name readText's effects: {text}")
        check(s.request("textDocument/hover", pos(2, 0))["result"] is None, "a blank line has a hover")
        text = s.request("textDocument/hover", pos(1, 19))["result"]["contents"]["value"]
        check("(parameter) xs: List<Int64>" in text, f"hover on a parameter: {text}")
        text = s.request("textDocument/hover", pos(0, 32))["result"]["contents"]["value"]
        check("Stdlib.Int64" in text, f"hover on a built-in type: {text}")
        text = s.request("textDocument/hover", pos(1, 4))["result"]["contents"]["value"]
        check("module Darklang.Stdlib" in text and "functions" in text, f"hover on a module: {text}")
        checked.append("hover shows the signature, the docs, and the effects")

        # Definition: the item opens as a store document, and the server serves its source.
        loc = s.request("textDocument/definition", pos(1, 16))["result"]
        check(loc is not None and loc["uri"] == "darkfs:/Darklang/Stdlib/List/map.fn.dark", f"definition: {loc}")
        source = s.request("dark/fs/read", {"uri": loc["uri"]})["result"]
        check(source is not None and "let map(" in source, f"dark/fs/read: {source!r}"[:300])
        check(s.request("dark/fs/read", {"uri": "darkfs:/Nobody/Nothing/none.fn.dark"})["result"] is None, "a missing item read as something")

        # A store document is analysed in its own module, so its relative names resolve.
        s.open(loc["uri"], source)
        d = s.diagnostics(loc["uri"])
        check(not [x for x in d if x["severity"] == 2 and "not found" in x["message"]], f"List.map's own source has unresolved names: {d}")
        checked.append("definition opens the item as darkfs:, which serves its source and resolves its names")

        # Outline: the declarations of a document, modules nesting their members.
        uri = "file:///tmp/lsp-gate/outline.dark"
        s.open(uri, "type Point = { x: Int64 }\n\nmodule Geo =\n  let origin(): Point = Point { x = 0 }\n\nval limit = 3\n")
        s.diagnostics(uri)
        outline = s.request("textDocument/documentSymbol", {"textDocument": {"uri": uri}})["result"]
        names = [x["name"] for x in outline]
        check(names == ["Point", "Geo", "limit"], f"outline: {names}")
        geo = outline[1]
        check([c["name"] for c in geo["children"]] == ["origin"], f"module children: {geo['children']}")
        checked.append("the outline lists a document's declarations, modules nesting theirs")

        # Workspace symbols: package items by name, each opening as its store document.
        found = s.request("workspace/symbol", {"query": "padStart"})["result"]
        uris = [x["location"]["uri"] for x in found]
        check("darkfs:/Darklang/Stdlib/String/padStart.fn.dark" in uris, f"workspace/symbol padStart: {uris[:10]}")
        checked.append("workspace symbols find package items by name")

        # Locals: highlight, definition and references follow scope.
        uri = "file:///tmp/lsp-gate/locals.dark"
        s.open(uri, "let f(x: Int64): Int64 =\n  let y = x + 1\n  let g = fun x -> x * 2\n  g(y) + x\n")
        s.diagnostics(uri)
        lpos = lambda line, ch: {"textDocument": {"uri": uri}, "position": {"line": line, "character": ch}}
        marks = s.request("textDocument/documentHighlight", lpos(3, 9))["result"]
        spots = sorted((m["range"]["start"]["line"], m["range"]["start"]["character"], m["kind"]) for m in marks)
        check(spots == [(0, 6, 3), (1, 10, 2), (3, 9, 2)], f"highlights of x: {spots}")
        loc = s.request("textDocument/definition", lpos(3, 4))["result"]
        check(loc["uri"] == uri and loc["range"]["start"] == {"line": 1, "character": 6}, f"definition of y: {loc}")
        refs = s.request("textDocument/references", {**lpos(2, 19), "context": {"includeDeclaration": False}})["result"]
        check([r["range"]["start"] for r in refs] == [{"line": 2, "character": 19}], f"the lambda's x: {refs}")
        checked.append("locals: highlights, definition and references follow scope")

        # Folding and expand-selection come from the parsed declarations.
        uri = "file:///tmp/lsp-gate/folds.dark"
        s.open(uri, "// one\n// two\nlet f(x: Int64): String =\n  match x with\n  | 0 ->\n    \"zero\"\n  | _ -> \"other\"\n")
        s.diagnostics(uri)
        folds = s.request("textDocument/foldingRange", {"textDocument": {"uri": uri}})["result"]
        spans = {(f["startLine"], f["endLine"], f.get("kind")) for f in folds}
        check({(0, 1, "comment"), (2, 6, None), (3, 6, None), (4, 5, None)} <= spans, f"folds: {spans}")
        sel = s.request("textDocument/selectionRange", {"textDocument": {"uri": uri}, "positions": [{"line": 5, "character": 6}]})["result"][0]
        chain = []
        while sel:
            chain.append(sel["range"])
            sel = sel.get("parent")
        check(len(chain) >= 4 and chain[-1]["start"]["line"] == 2, f"selection chain: {chain}")
        check(chain[1]["start"] == {"line": 5, "character": 4}, f"the innermost range is not the string: {chain[:3]}")
        checked.append("folding covers comments, declarations and match cases; selection expands outward")

        # References, implementations and the call hierarchy come from the store.
        uri = "file:///tmp/lsp-gate/refs.dark"
        s.open(uri, "let a(s: String): Int64 = Stdlib.String.length(s)\nlet b(): Int64 = Stdlib.String.length(\"x\")\n")
        s.diagnostics(uri)
        rpos = lambda line, ch: {"textDocument": {"uri": uri}, "position": {"line": line, "character": ch}}
        refs = s.request("textDocument/references", {**rpos(0, 42), "context": {"includeDeclaration": True}})["result"]
        here = [r for r in refs if r["uri"] == uri]
        check(len(here) == 2, f"references in the document: {here}")
        check(any(r["uri"].startswith("darkfs:/Darklang/") and r["uri"].endswith(".fn.dark") for r in refs), f"no store callers: {refs[:5]}")
        check("darkfs:/Darklang/Stdlib/String/length.fn.dark" in [r["uri"] for r in refs], "the declaration is missing")
        [item] = s.request("textDocument/prepareCallHierarchy", rpos(0, 42))["result"]
        check(item["uri"] == "darkfs:/Darklang/Stdlib/String/length.fn.dark", f"call hierarchy item: {item}")
        callers = s.request("callHierarchy/incomingCalls", {"item": item})["result"]
        check(len(callers) > 0, "String.length has no callers")
        calls = s.request("callHierarchy/outgoingCalls", {"item": {**item, "uri": "darkfs:/Darklang/Stdlib/String/padStart.fn.dark"}})["result"]
        check(len(calls) > 0, "String.padStart calls nothing")
        s.open("file:///tmp/lsp-gate/impls.dark", "let x(): Int64 = 1\n// Stdlib.Add\n")
        impls = s.request("textDocument/implementation", {"textDocument": {"uri": "file:///tmp/lsp-gate/impls.dark"}, "position": {"line": 1, "character": 12}})["result"]
        check(any(i["uri"].endswith(".impl.dark") for i in impls or []), f"no impls of Stdlib.Add: {impls}")
        source = s.request("dark/fs/read", {"uri": impls[0]["uri"]})["result"]
        check(source is not None and "impl" in source, f"an impl document: {source!r}")
        checked.append("references, call hierarchy and implementations come from the store")

        # The package tree, one module at a time.
        top = s.request("dark/tree", {"path": []})["result"]
        check(any(e["name"] == "Darklang" and e["kind"] == "module" for e in top), f"tree root: {top}")
        lst = s.request("dark/tree", {"path": ["Darklang", "Stdlib", "List"]})["result"]
        mp = [e for e in lst if e["name"] == "map"]
        check(mp and mp[0]["uri"] == "darkfs:/Darklang/Stdlib/List/map.fn.dark", f"List.map in the tree: {mp}")
        check(not any(e["name"].startswith("__") for e in lst), "the tree lists internal helpers")
        whole = s.request("dark/fs/read", {"uri": "darkfs:/Darklang/Stdlib/List.dark"})["result"]
        check(whole is not None and "let map(" in whole and "let filter(" in whole, "a module document")
        checked.append("the package tree lists modules and items; a module opens as one document")

        # Incremental sync: a change sends only the edited range, in UTF-16 columns.
        uri = "file:///tmp/lsp-gate/incremental.dark"
        s.open(uri, "let s(): String = \"\U0001F600\"\nlet a(): Int64 = 1\n")
        s.diagnostics(uri)
        s.notify("textDocument/didChange", {
            "textDocument": {"uri": uri, "version": 2},
            "contentChanges": [
                {"range": {"start": {"line": 1, "character": 4}, "end": {"line": 1, "character": 5}}, "text": "abc"},
                {"range": {"start": {"line": 0, "character": 22}, "end": {"line": 0, "character": 22}}, "text": " + \"x\""},
            ],
        })
        outline = s.request("textDocument/documentSymbol", {"textDocument": {"uri": uri}})["result"]
        check([x["name"] for x in outline] == ["s", "abc"], f"after an incremental change: {outline}")
        d = s.diagnostics(uri)
        check(not [x for x in d if x["severity"] == 1], f"the edit after the emoji landed in the wrong place: {d}")
        checked.append("incremental changes land where the editor says, past non-ASCII text too")

        # Completion: a module's members, with signatures and snippets; locals; an enum's cases.
        uri = "file:///tmp/lsp-gate/complete.dark"
        s.open(uri, "let f(count: Int64): Int64 =\n  let total = count + 1\n  Stdlib.List.ma\n")
        s.diagnostics(uri)
        cpos = lambda line, ch: {"textDocument": {"uri": uri}, "position": {"line": line, "character": ch}}
        items = s.request("textDocument/completion", cpos(2, 16))["result"]["items"]
        mp = [i for i in items if i["label"] == "map"]
        check(mp and mp[0]["insertText"].startswith("map(${1:"), f"List.map completion: {mp}")
        check("labelDetails" in mp[0] and "List" in mp[0]["labelDetails"]["detail"], f"no signature beside map: {mp[0]}")
        resolved = s.request("completionItem/resolve", mp[0])["result"]
        check("Stdlib.List.map(" in resolved.get("documentation", {}).get("value", ""), f"resolve: {resolved}")
        items = s.request("textDocument/completion", cpos(2, 2))["result"]["items"]
        labels = {i["label"] for i in items}
        check({"count", "total", "Stdlib", "let"} <= labels, f"bare completions: {sorted(labels)[:40]}")
        s.change(uri, 2, "let f(count: Int64): Int64 =\n  Stdlib.Option.Option.\n")
        items = s.request("textDocument/completion", cpos(1, 23))["result"]["items"]
        check({"Some", "None"} <= {i["label"] for i in items}, f"enum cases: {[i['label'] for i in items][:20]}")
        checked.append("completion: module members with signatures, locals in scope, enum cases")

        # Signature help, with the piped argument counted.
        uri = "file:///tmp/lsp-gate/sig.dark"
        s.open(uri, "let f(s: String): String =\n  Stdlib.String.padStart(s, \n")
        help = s.request("textDocument/signatureHelp", {"textDocument": {"uri": uri}, "position": {"line": 1, "character": 28}})["result"]
        check(help and help["activeParameter"] == 1 and help["signatures"][0]["label"].startswith("padStart("), f"signature help: {help}")
        start, end = help["signatures"][0]["parameters"][1]["label"]
        check(help["signatures"][0]["label"][start:end].startswith("padWith"), f"parameter offsets: {help}")
        s.change(uri, 2, "let f(s: String): String =\n  s |> Stdlib.String.padStart(\n")
        help = s.request("textDocument/signatureHelp", {"textDocument": {"uri": uri}, "position": {"line": 1, "character": 30}})["result"]
        check(help and help["activeParameter"] == 1, f"a piped call starts at the second parameter: {help}")
        checked.append("signature help names the parameter being typed, a pipe's included")

        # Semantic tokens and formatting.
        legend = caps["semanticTokensProvider"]["legend"]["tokenTypes"]
        uri = "file:///tmp/lsp-gate/tokens.dark"
        s.open(uri, "let  f( x :Int64) : Int64 =\n    Stdlib.Int64.add(x,  1)\n")
        s.diagnostics(uri)
        data = s.request("textDocument/semanticTokens/full", {"textDocument": {"uri": uri}})["result"]["data"]
        check(data and len(data) % 5 == 0, f"semantic tokens: {data}")
        kinds = {legend[data[i + 3]] for i in range(0, len(data), 5)}
        check({"keyword", "function", "parameter", "namespace", "type"} <= kinds, f"token kinds: {kinds}")
        edits = s.request("textDocument/formatting", {"textDocument": {"uri": uri}, "options": {"tabSize": 2, "insertSpaces": True}})["result"]
        check(len(edits) == 1 and edits[0]["newText"].startswith("let f(x: Int64): Int64 ="), f"formatting: {edits}")
        checked.append("semantic tokens classify names; formatting lays a document out as dark format does")

        # Live values: a store function's document shows what each call gave in its newest run, and
        # follows a new run and an edit without being asked.
        def dark(*args):
            done = subprocess.run([cli, *args], capture_output=True, text=True)
            check(done.returncode == 0, f"dark {' '.join(args)}: {done.stdout}{done.stderr}")
            return done.stdout

        dark("config", "set", "trace.record", "on")
        dark("fn", "/LspGate.T.twice", "(x: Int64): Int64 =\n  let doubled = Stdlib.max(x, 0) * 2\n  doubled")
        dark("eval", "LspGate.T.twice(4)")
        uri = "darkfs:/LspGate/T/twice.fn.dark"
        s.open(uri, s.request("dark/fs/read", {"uri": uri})["result"])
        s.diagnostics(uri)

        def hints():
            return [h["label"].strip() for h in s.request("textDocument/inlayHint", {"textDocument": {"uri": uri}, "range": {"start": {"line": 0, "character": 0}, "end": {"line": 9, "character": 0}}})["result"] if h.get("kind") != 1]

        check(hints() == ["= 8"], f"live values: {hints()}")
        lenses = [l["command"]["title"] for l in s.request("textDocument/codeLens", {"textDocument": {"uri": uri}})["result"]]
        check(any(t.startswith("1 run · last ") for t in lenses) and "pure" in lenses, f"code lenses: {lenses}")
        [run] = s.request("dark/traces", {"uri": uri})["result"]
        shown = s.request("dark/fs/read", {"uri": f"{uri}?trace={run['id']}"})["result"]
        check(shown and "// = 8" in shown, f"a run's document: {shown!r}")
        checked.append("a store function shows its last run's values, its runs and timing, and its effects")

        s.quiet(0.5)
        dark("eval", "LspGate.T.twice(5)")
        s.wait(lambda m: m.get("method") == "workspace/inlayHint/refresh", 30, "a refresh after a new run")
        check(hints() == ["= 10"], f"after a new run: {hints()}")
        dark("fn", "/LspGate.T.twice", "(x: Int64): Int64 =\n  let doubled = Stdlib.max(x, 0) * 3\n  doubled")
        s.wait(lambda m: m.get("method") == "dark/storeChanged", 30, "dark/storeChanged after an edit")
        # What the editor does on dark/storeChanged: read the document again.
        s.change(uri, 2, s.request("dark/fs/read", {"uri": uri})["result"])
        s.diagnostics(uri)
        check(hints() == ["= 15"], f"values follow an edit to the code: {hints()}")
        checked.append("live values follow a new run and an edit, and the editor is asked to refresh")

        # Saving a store document applies it, with dark edit's checks.
        saved = s.request("dark/fs/write", {"uri": uri, "text": "let twice(x: Int64): Int64 =\n  x * 4\n"})["result"]
        check(saved["saved"], f"a good save: {saved}")
        check("x * 4" in s.request("dark/fs/read", {"uri": uri})["result"], "the save did not reach the store")
        refused = s.request("dark/fs/write", {"uri": uri, "text": "let twice(x: Int64): Int64 =\n  x +\n"})["result"]
        check(not refused["saved"] and refused["message"], f"a broken save: {refused}")
        check("x * 4" in s.request("dark/fs/read", {"uri": uri})["result"], "a refused save changed the store")
        checked.append("saving a store document applies it; a broken one is refused with the reason")

        # Quick fixes: qualify an unresolved name, add the case a match misses.
        uri = "file:///tmp/lsp-gate/fixes.dark"
        s.open(uri, "let pad(s: String): String = padStart(s, ' ', 3)\nlet f(o: Stdlib.Option.Option<Int64>): Int64 =\n  match o with\n  | Some(x) -> x\n")
        d = s.diagnostics(uri)
        whole = {"start": {"line": 0, "character": 0}, "end": {"line": 3, "character": 0}}
        acts = s.request("textDocument/codeAction", {"textDocument": {"uri": uri}, "range": {"start": {"line": 2, "character": 4}, "end": {"line": 2, "character": 4}}, "context": {"diagnostics": d}})["result"]
        titles = [a["title"] for a in acts]
        check("Use Stdlib.String.padStart" in titles, f"qualify: {titles} from {d}")
        add = [a for a in acts if a["title"] == "Add a case for None"]
        check(add, f"add a case: {titles} from {d}")
        [edit] = add[0]["edit"]["changes"][uri]
        check(edit["newText"] == "\n  | None -> ()" and edit["range"]["start"]["line"] == 3, f"the added case: {edit}")
        checked.append("quick fixes qualify an unresolved name and add a missing match case")

        # Rename: a local in its document; a package item in the store, its callers following.
        edit = s.request("textDocument/rename", {"textDocument": {"uri": uri}, "position": {"line": 3, "character": 15}, "newName": "value"})["result"]
        check(len(edit["changes"][uri]) == 2, f"renaming a local: {edit}")
        dark("fn", "/LspGate.T.renameMe", "(): Int64 = 7")
        uri = "file:///tmp/lsp-gate/rename.dark"
        s.open(uri, "let a(): Int64 = LspGate.T.renameMe() + LspGate.T.renameMe()\n")
        s.diagnostics(uri)
        span = s.request("textDocument/prepareRename", {"textDocument": {"uri": uri}, "position": {"line": 0, "character": 30}})["result"]
        check(span == {"start": {"line": 0, "character": 27}, "end": {"line": 0, "character": 35}}, f"prepareRename: {span}")
        edit = s.request("textDocument/rename", {"textDocument": {"uri": uri}, "position": {"line": 0, "character": 30}, "newName": "renamed"})["result"]
        check(sorted(e["range"]["start"]["character"] for e in edit["changes"][uri]) == [27, 50], f"renaming an item: {edit}")
        check(s.request("dark/fs/read", {"uri": "darkfs:/LspGate/T/renamed.fn.dark"})["result"], "the store did not rename it")
        checked.append("rename: a local in its document, a package item store-wide")

        # Links in doc comments open the item they name.
        uri = "file:///tmp/lsp-gate/links.dark"
        s.open(uri, "/// like <fn Stdlib.List.map>, for <type Stdlib.Option.Option>\nlet x(): Int64 = 1\n")
        links = s.request("textDocument/documentLink", {"textDocument": {"uri": uri}})["result"]
        check([l["target"] for l in links] == ["darkfs:/Darklang/Stdlib/List/map.fn.dark", "darkfs:/Darklang/Stdlib/Option/Option.type.dark"], f"links: {links}")
        check(links[0]["range"]["start"] == {"line": 0, "character": 13}, f"link range: {links[0]}")
        checked.append("names in doc comments link to their items")

        # Requests are answered beside the loop: many at once all come back, each once, and a
        # cancelled one is answered as cancelled or with its result.
        uri = "file:///tmp/lsp-gate/hover.dark"
        ids = []
        for i in range(10):
            ids.append(s.next_id)
            s.next_id += 1
            s.send({"id": ids[-1], "method": "textDocument/hover", "params": pos(1, 16)})
        slow = s.next_id
        s.next_id += 1
        s.send({"id": slow, "method": "textDocument/references", "params": {**pos(1, 16), "context": {"includeDeclaration": True}}})
        s.notify("$/cancelRequest", {"id": slow})
        for rid in ids:
            m = s.wait(lambda m, rid=rid: m.get("id") == rid and "method" not in m, 60, f"hover {rid}")
            check(m["result"] and "Stdlib.List.map(" in m["result"]["contents"]["value"], f"hover {rid}: {m}")
        m = s.wait(lambda m: m.get("id") == slow and "method" not in m, 60, "the cancelled request's reply")
        check("result" in m or m["error"]["code"] == -32800, f"a cancelled request: {m}")
        extra = [m for m in s.quiet(1) if m.get("id") in ids + [slow] and "method" not in m]
        check(not extra, f"a request was answered twice: {extra}")
        checked.append("concurrent requests are each answered once; a cancelled one says so")

        # The branch and the draft, for the status bar and the Changes view; a switch in a
        # terminal reaches the server.
        st = s.request("dark/status")["result"]
        check(st["branch"] == "main" and any(c["name"] == "LspGate.T.twice" and c["uri"] == "darkfs:/LspGate/T/twice.fn.dark" for c in st["changed"]), f"status: {st}")
        s.quiet(0.5)
        dark("branch", "create", "lsp-gate")
        s.wait(lambda m: m.get("method") == "dark/storeChanged", 30, "dark/storeChanged after a branch switch")
        check(s.request("dark/branches")["result"]["current"] == "lsp-gate", "the server did not follow the switch")
        dark("switch", "main")
        checked.append("status and branches; a branch switch elsewhere reaches the server")

        # Type hints: what the checker inferred for each `let` and lambda parameter.
        uri = "file:///tmp/lsp-gate/types-hints.dark"
        s.open(uri, "let f(xs: List<Int64>): String =\n  let doubled = Stdlib.List.map(xs, fun x -> x * 2)\n  let (n, label) = (Stdlib.List.length(doubled), \"items\")\n  match doubled with\n  | [ first, ..._ ] -> label\n  | [] -> \"none\"\n")
        s.diagnostics(uri)
        found = s.request("textDocument/inlayHint", {"textDocument": {"uri": uri}, "range": {"start": {"line": 0, "character": 0}, "end": {"line": 9, "character": 0}}})["result"]
        shown = sorted((h["position"]["line"], h["position"]["character"], h["label"]) for h in found if h.get("kind") == 1)
        check(shown == [(1, 13, ": List<Int64>"), (1, 41, ": Int64"), (2, 8, ": Int64"), (2, 15, ": String")], f"type hints: {shown}")
        checked.append("type hints show what the checker inferred for lets and lambda parameters")

        reply = s.request("shutdown")
        check("result" in reply and reply["result"] is None, f"shutdown: {reply}")
        s.notify("exit")
        check(s.exit_code() == 0, "exit after shutdown did not exit 0")
        check(not s.stray, f"stdout carried something other than the protocol: {bytes(s.stray)[:200]!r}")
        checked.append("shutdown then exit stops with 0; stdout carries only the protocol")
    finally:
        s.kill()

    # `--live` answers through the store's version of the request handler. (Following an edit
    # to it needs a dev build: the bundled `Darklang` packages cannot be edited from the CLI.)
    s = Server(cli, args=["--live"])
    try:
        s.request("initialize", {"capabilities": {}})
        uri = "file:///tmp/lsp-gate/live-server.dark"
        s.open(uri, "let h(s: String): Int64 = Stdlib.String.length(s)\n")
        s.diagnostics(uri)
        outline = [x["name"] for x in s.request("textDocument/documentSymbol", {"textDocument": {"uri": uri}})["result"]]
        check(outline == ["h"], f"--live outline: {outline}")
        hover = s.request("textDocument/hover", {"textDocument": {"uri": uri}, "position": {"line": 0, "character": 40}})["result"]
        check(hover and "length(" in hover["contents"]["value"], f"--live hover: {hover}")
        checked.append("--live answers through the store's request handler")
    finally:
        s.kill()

    # The editor going away without a word: stdin closes, and the server stops.
    s = Server(cli)
    try:
        s.request("initialize", {"capabilities": {}})
        s.close_stdin()
        check(s.exit_code() == 1, "closing stdin without shutdown did not stop the server with 1")
        checked.append("stdin closing stops the server")
    finally:
        s.kill()

    return checked


if __name__ == "__main__":
    try:
        done = run(sys.argv[1])
    except Failure as e:
        print(f"FAIL: {e}", file=sys.stderr)
        sys.exit(1)
    print(f"lsp: {len(done)} checks: " + "; ".join(done) + ".")
