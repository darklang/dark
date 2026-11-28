import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions, State, TransportKind } from "vscode-languageclient/node";
import { DarkFileSystem, TraceDocuments } from "./fs";
import { Changes, pickBranch } from "./scm";
import { PackagesTree } from "./tree";

let client: LanguageClient | undefined;

function darkPath(): string {
  return vscode.workspace.getConfiguration("darklang").get<string>("path") || "dark";
}

function startClient(onStoreChanged: () => void): LanguageClient {
  const run = { command: darkPath(), args: ["lsp"], transport: TransportKind.stdio };
  const serverOptions: ServerOptions = { run, debug: run };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "darklang" },
      { scheme: "untitled", language: "darklang" },
      { scheme: "darkfs", language: "darklang" },
    ],
  };
  const c = new LanguageClient("darklang", "Darklang", serverOptions, clientOptions);
  c.onNotification("dark/storeChanged", onStoreChanged);
  c.start();
  return c;
}

// Runs a `dark` command in the Darklang terminal, where its output and any permission question
// show.
function inTerminal(args: string[]) {
  const terminal = vscode.window.terminals.find((t) => t.name === "Darklang") ?? vscode.window.createTerminal("Darklang");
  terminal.show(true);
  const quote = (a: string) => `'${a.replace(/'/g, `'\\''`)}'`;
  terminal.sendText([darkPath(), ...args].map(quote).join(" "));
}

interface Run {
  id: string;
  entry: string;
  created: string;
  inFunction: string;
  totalMs: number;
}

async function showTraces(fnUri: string) {
  if (!client) return;
  const runs: Run[] = await client.sendRequest("dark/traces", { uri: fnUri });
  if (runs.length === 0) {
    vscode.window.showInformationMessage("Nothing recorded has run this function yet.");
    return;
  }
  const picked = await vscode.window.showQuickPick(
    runs.map((r) => ({
      label: r.inFunction ? `${r.inFunction} in this function` : r.id.slice(0, 8),
      description: r.entry,
      detail: `${r.created} · ${r.totalMs} ms in all · ${r.id.slice(0, 8)}`,
      run: r,
    })),
    { placeHolder: "Open a run: the function with the value each call gave" },
  );
  if (!picked) return;
  const doc = await vscode.workspace.openTextDocument(TraceDocuments.uri(fnUri, picked.run.id));
  await vscode.languages.setTextDocumentLanguage(doc, "darklang");
  await vscode.window.showTextDocument(doc, { preview: true, viewColumn: vscode.ViewColumn.Beside });
}

export function activate(context: vscode.ExtensionContext) {
  const fs = new DarkFileSystem(() => client);
  const tree = new PackagesTree(() => client);
  const changes = new Changes(() => client);
  const onStoreChanged = () => {
    fs.storeChanged();
    tree.refresh();
    changes.refresh();
  };
  const start = () => {
    client = startClient(onStoreChanged);
    client.onDidChangeState((e) => {
      if (e.newState === State.Running) changes.refresh();
    });
  };
  const scmActions: [string, () => unknown][] = [
    ["Switch branch…", async () => {
      const b = await pickBranch(client);
      if (b) inTerminal(["switch", b]);
    }],
    ["New branch…", async () => {
      const name = await vscode.window.showInputBox({ prompt: "A name for the new branch, made from this one" });
      if (name) inTerminal(["branch", "create", name]);
    }],
    ["Commit…", async () => {
      const message = await vscode.window.showInputBox({ prompt: "What the commit does" });
      if (message) inTerminal(["commit", message]);
    }],
    ["Status", () => inTerminal(["status"])],
    ["Log", () => inTerminal(["log"])],
    ["Discard the draft…", () => inTerminal(["discard"])],
  ];
  context.subscriptions.push(
    vscode.workspace.registerFileSystemProvider("darkfs", fs, { isCaseSensitive: true }),
    vscode.workspace.registerTextDocumentContentProvider("darktrace", new TraceDocuments(() => client)),
    vscode.window.createTreeView("darklangPackages", { treeDataProvider: tree, showCollapseAll: true }),
    vscode.window.createTreeView("darklangChanges", { treeDataProvider: changes }),
    changes,
    vscode.workspace.onDidSaveTextDocument((d) => {
      if (d.uri.scheme === "darkfs") changes.refresh();
    }),
    vscode.commands.registerCommand("darklang.scm.refresh", () => changes.refresh()),
    vscode.commands.registerCommand("darklang.openStore", () => {
      const folders = vscode.workspace.workspaceFolders ?? [];
      if (folders.some((f) => f.uri.scheme === "darkfs")) return;
      vscode.workspace.updateWorkspaceFolders(folders.length, 0, { uri: vscode.Uri.parse("darkfs:/"), name: "Dark packages" });
    }),
    vscode.commands.registerCommand("darklang.scm.actions", async () => {
      const picked = await vscode.window.showQuickPick(scmActions.map(([label]) => label), { placeHolder: "Darklang" });
      scmActions.find(([label]) => label === picked)?.[1]();
    }),
    vscode.commands.registerCommand("darklang.branch.switch", () => scmActions[0][1]()),
    vscode.commands.registerCommand("darklang.scm.commit", () => scmActions[2][1]()),
    vscode.commands.registerCommand("darklang.restartServer", async () => {
      await client?.stop();
      start();
      tree.refresh();
    }),
    vscode.commands.registerCommand("darklang.packages.refresh", () => tree.refresh()),
    vscode.commands.registerCommand("darklang.packages.openModule", (node: { entry: { uri: string } }) =>
      vscode.window.showTextDocument(vscode.Uri.parse(node.entry.uri)),
    ),
    vscode.commands.registerCommand("darklang.run", (name: string) => inTerminal(["eval", `${name}()`])),
    vscode.commands.registerCommand("darklang.runTests", (...names: string[]) => inTerminal(["test", "run", ...names])),
    vscode.commands.registerCommand("darklang.showTraces", (fnUri?: string) => {
      const uri = fnUri ?? vscode.window.activeTextEditor?.document.uri.toString(true);
      if (uri) return showTraces(uri);
    }),
  );
  start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
