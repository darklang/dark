import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

// Package items open as `darkfs:/Owner/Module/name.fn.dark`, and a module as
// `darkfs:/Owner/Module.dark`. The server reads them from the store, so the editor always shows
// what the store has now.
export class DarkFileSystem implements vscode.FileSystemProvider {
  private changed = new vscode.EventEmitter<vscode.FileChangeEvent[]>();
  readonly onDidChangeFile = this.changed.event;

  constructor(private client: () => LanguageClient | undefined) {}

  // The store moved: an open item may read differently now. VS Code reloads the ones that have
  // no unsaved edits.
  storeChanged() {
    const events = vscode.workspace.textDocuments
      .filter((d) => d.uri.scheme === "darkfs")
      .map((d) => ({ type: vscode.FileChangeType.Changed, uri: d.uri }));
    if (events.length > 0) this.changed.fire(events);
  }

  // A module is a directory, and every item and module a `.dark` document, so the store can be
  // opened as a folder.
  stat(uri: vscode.Uri): vscode.FileStat {
    const last = uri.path.split("/").pop() ?? "";
    if (last.endsWith(".dark")) return { type: vscode.FileType.File, ctime: 0, mtime: Date.now(), size: 0 };
    if (last.includes(".")) throw vscode.FileSystemError.FileNotFound(uri);
    return { type: vscode.FileType.Directory, ctime: 0, mtime: 0, size: 0 };
  }

  async readFile(uri: vscode.Uri): Promise<Uint8Array> {
    const c = this.client();
    if (!c) throw vscode.FileSystemError.Unavailable("the Darklang language server is not running");
    const text: string | null = await c.sendRequest("dark/fs/read", { uri: uri.toString(true) });
    if (text === null) throw vscode.FileSystemError.FileNotFound(uri);
    return new TextEncoder().encode(text);
  }

  watch(): vscode.Disposable {
    return new vscode.Disposable(() => {});
  }
  async readDirectory(uri: vscode.Uri): Promise<[string, vscode.FileType][]> {
    const c = this.client();
    if (!c) return [];
    const path = uri.path.split("/").filter((p) => p !== "");
    const entries: { name: string; kind: string; uri: string }[] = await c.sendRequest("dark/tree", { path });
    return entries.flatMap(({ name, kind, uri: item }): [string, vscode.FileType][] =>
      kind === "module"
        ? [[name, vscode.FileType.Directory], [`${name}.dark`, vscode.FileType.File]]
        : [[item.split("/").pop() ?? name, vscode.FileType.File]],
    );
  }
  createDirectory(): void {
    throw vscode.FileSystemError.NoPermissions("the store has no directories to make");
  }
  // Saving applies the document to the store, as `dark edit` does: the same checks, and what
  // they say when they refuse it.
  async writeFile(uri: vscode.Uri, content: Uint8Array): Promise<void> {
    const c = this.client();
    if (!c) throw vscode.FileSystemError.Unavailable("the Darklang language server is not running");
    const text = new TextDecoder().decode(content);
    const result: { saved: boolean; message: string } = await c.sendRequest("dark/fs/write", { uri: uri.toString(true), text });
    if (!result.saved) throw vscode.FileSystemError.NoPermissions(result.message || "the store refused it");
    vscode.window.setStatusBarMessage(result.message.split("\n")[0], 5000);
    this.changed.fire([{ type: vscode.FileChangeType.Changed, uri }]);
  }
  delete(uri: vscode.Uri): void {
    throw vscode.FileSystemError.NoPermissions(uri);
  }
  rename(uri: vscode.Uri): void {
    throw vscode.FileSystemError.NoPermissions(uri);
  }
}

// One recorded run of a function, as `darktrace:/Owner/Module/name (run 1a2b3c4d).dark`: the
// function with the value each call gave in that run beside it. Read-only, and not analysed. The
// query carries the function's document and the run.
export class TraceDocuments implements vscode.TextDocumentContentProvider {
  constructor(private client: () => LanguageClient | undefined) {}

  static uri(fnUri: string, runId: string): vscode.Uri {
    const fn = vscode.Uri.parse(fnUri);
    const path = fn.path.replace(/\.[a-z]+\.dark$/, ` (run ${runId.slice(0, 8)}).dark`);
    return vscode.Uri.from({ scheme: "darktrace", path, query: new URLSearchParams({ fn: fnUri, run: runId }).toString() });
  }

  async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    const c = this.client();
    if (!c) return "";
    const q = new URLSearchParams(uri.query);
    const text: string | null = await c.sendRequest("dark/fs/read", { uri: `${q.get("fn")}?trace=${q.get("run")}` });
    return text ?? "// this run is gone";
  }
}
