import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

interface Changed {
  name: string;
  kind: string;
  change: string;
  followed: boolean;
  uri: string;
}

interface Status {
  branch: string;
  draftOps: number;
  changed: Changed[];
  removed: string[];
  conflicts: number;
}

// The branch you are on and what your draft holds: the status bar, and the Changes view, whose
// items open as their store documents.
export class Changes implements vscode.TreeDataProvider<Changed | string>, vscode.Disposable {
  private changed = new vscode.EventEmitter<undefined>();
  readonly onDidChangeTreeData = this.changed.event;
  private status: Status | undefined;
  private bar = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);

  constructor(private client: () => LanguageClient | undefined) {
    this.bar.command = "darklang.scm.actions";
    this.bar.text = "$(git-branch) dark";
    this.bar.show();
  }

  async refresh() {
    const c = this.client();
    if (!c) return;
    try {
      this.status = await c.sendRequest<Status>("dark/status");
    } catch {
      return;
    }
    const n = this.status.changed.length + this.status.removed.length;
    this.bar.text = `$(git-branch) ${this.status.branch}${n ? ` · ${n} changed` : ""}${this.status.conflicts ? ` · $(warning) ${this.status.conflicts}` : ""}`;
    this.bar.tooltip = "Darklang: switch branch, commit, see what changed";
    this.changed.fire(undefined);
  }

  getTreeItem(item: Changed | string): vscode.TreeItem {
    if (typeof item === "string") {
      const t = new vscode.TreeItem(item);
      t.description = "removed";
      t.iconPath = new vscode.ThemeIcon("diff-removed");
      return t;
    }
    const t = new vscode.TreeItem(item.name);
    t.description = item.followed ? `${item.change}, followed an edit` : item.change;
    t.iconPath = new vscode.ThemeIcon(item.change === "added" ? "diff-added" : "diff-modified");
    if (item.uri) t.command = { command: "vscode.open", title: "Open", arguments: [vscode.Uri.parse(item.uri)] };
    return t;
  }

  getChildren(): (Changed | string)[] {
    if (!this.status) return [];
    return [...this.status.changed, ...this.status.removed];
  }

  dispose() {
    this.bar.dispose();
  }
}

export async function pickBranch(client: LanguageClient | undefined): Promise<string | undefined> {
  if (!client) return;
  const { current, branches }: { current: string; branches: string[] } = await client.sendRequest("dark/branches");
  return vscode.window.showQuickPick(
    branches.map((b) => ({ label: b, description: b === current ? "current" : undefined })),
    { placeHolder: "Switch to branch" },
  ).then((p) => p?.label);
}
