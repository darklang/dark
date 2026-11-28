import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

// One row of `dark/tree`: a module, or an item that opens as its `darkfs:` document.
interface Entry {
  name: string;
  kind: "module" | "fn" | "type" | "value" | "trait" | "impl" | "test";
  uri: string;
}

interface Node {
  path: string[];
  entry: Entry;
}

const icons: Record<Entry["kind"], string> = {
  module: "symbol-namespace",
  fn: "symbol-function",
  type: "symbol-struct",
  value: "symbol-constant",
  trait: "symbol-interface",
  impl: "symbol-class",
  test: "beaker",
};

export class PackagesTree implements vscode.TreeDataProvider<Node> {
  private changed = new vscode.EventEmitter<Node | undefined>();
  readonly onDidChangeTreeData = this.changed.event;

  constructor(private client: () => LanguageClient | undefined) {}

  refresh() {
    this.changed.fire(undefined);
  }

  getTreeItem(node: Node): vscode.TreeItem {
    const { entry } = node;
    const isModule = entry.kind === "module";
    const item = new vscode.TreeItem(
      entry.name,
      isModule ? vscode.TreeItemCollapsibleState.Collapsed : vscode.TreeItemCollapsibleState.None,
    );
    item.id = entry.uri;
    item.iconPath = new vscode.ThemeIcon(icons[entry.kind]);
    item.contextValue = entry.kind;
    item.tooltip = node.path.join(".");
    item.resourceUri = vscode.Uri.parse(entry.uri);
    if (!isModule) {
      item.command = { command: "vscode.open", title: "Open", arguments: [vscode.Uri.parse(entry.uri)] };
    }
    return item;
  }

  async getChildren(node?: Node): Promise<Node[]> {
    const c = this.client();
    if (!c) return [];
    const path = node ? node.path : [];
    const entries: Entry[] = await c.sendRequest("dark/tree", { path });
    return entries.map((entry) => ({ path: [...path, entry.name], entry }));
  }

  getParent(node: Node): Node | undefined {
    if (node.path.length <= 1) return undefined;
    const path = node.path.slice(0, -1);
    return { path, entry: { name: path[path.length - 1], kind: "module", uri: `darkfs:/${path.join("/")}.dark` } };
  }
}
