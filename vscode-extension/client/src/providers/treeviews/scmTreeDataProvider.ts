import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

// SCM tree node types
type ScmNodeType = "status-root" | "commits-root" | "wip-item" | "commit-item" | "commit-op";

interface ScmNode {
  id: string;
  label: string;
  type: ScmNodeType;
  description?: string;
  children?: ScmNode[];
  commitId?: string;
  opData?: any;
}

interface WipSummary {
  types: number;
  values: number;
  fns: number;
  renames: number;
  total: number;
}

interface CommitInfo {
  id: string;
  message: string;
  createdAt: string;
  opCount: string;
}

export class ScmTreeDataProvider implements vscode.TreeDataProvider<ScmNode>, vscode.Disposable {
  private _onDidChangeTreeData = new vscode.EventEmitter<ScmNode | undefined | null | void>();
  readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

  private _onStatusChanged = new vscode.EventEmitter<WipSummary>();
  readonly onStatusChanged = this._onStatusChanged.event;

  private cachedWipSummary: WipSummary | null = null;

  constructor(private client: LanguageClient) {}

  refresh(): void {
    this.cachedWipSummary = null;
    this._onDidChangeTreeData.fire(undefined);
  }

  dispose(): void {
    this._onDidChangeTreeData.dispose();
    this._onStatusChanged.dispose();
  }

  getTreeItem(element: ScmNode): vscode.TreeItem {
    const item = new vscode.TreeItem(element.label);

    if (element.type === "status-root" || element.type === "commits-root") {
      item.collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
    } else if (element.type === "commit-item") {
      item.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
    } else {
      item.collapsibleState = vscode.TreeItemCollapsibleState.None;
    }

    item.description = element.description;

    switch (element.type) {
      case "status-root":
        item.iconPath = new vscode.ThemeIcon("git-commit", new vscode.ThemeColor("charts.yellow"));
        item.contextValue = "scm-status-root";
        break;
      case "commits-root":
        item.iconPath = new vscode.ThemeIcon("history");
        item.contextValue = "scm-commits-root";
        break;
      case "wip-item":
        item.iconPath = new vscode.ThemeIcon("diff-added", new vscode.ThemeColor("charts.green"));
        item.contextValue = "scm-wip-item";
        break;
      case "commit-item":
        item.iconPath = new vscode.ThemeIcon("git-commit");
        item.contextValue = "scm-commit-item";
        item.tooltip = `${element.label}\n${element.description}`;
        if (element.commitId) {
          item.command = {
            command: "darklang.scm.showCommit",
            title: "Show Commit",
            arguments: [element.commitId],
          };
        }
        break;
      case "commit-op":
        item.iconPath = new vscode.ThemeIcon("symbol-method");
        item.contextValue = "scm-commit-op";
        break;
    }

    return item;
  }

  async getChildren(element?: ScmNode): Promise<ScmNode[]> {
    if (!element) {
      // Root level - return Status and Commits sections
      return this.getRootNodes();
    }

    if (element.type === "status-root") {
      return this.getWipItems();
    }

    if (element.type === "commits-root") {
      return this.getCommitItems();
    }

    if (element.type === "commit-item" && element.commitId) {
      return this.getCommitOps(element.commitId);
    }

    return element.children || [];
  }

  private async getRootNodes(): Promise<ScmNode[]> {
    const summary = await this.getWipSummary();
    const statusLabel = summary.total > 0
      ? `Uncommitted Changes (${summary.total})`
      : "Uncommitted Changes";

    return [
      {
        id: "__status__",
        label: statusLabel,
        type: "status-root",
        description: summary.total > 0 ? "ready to commit" : "none",
      },
      {
        id: "__commits__",
        label: "Recent Commits",
        type: "commits-root",
      },
    ];
  }

  private async getWipSummary(): Promise<WipSummary> {
    if (this.cachedWipSummary) {
      return this.cachedWipSummary;
    }

    try {
      const summary = await this.client.sendRequest<WipSummary>("dark/scm/getWipSummary", {});
      this.cachedWipSummary = summary;
      this._onStatusChanged.fire(summary);
      return summary;
    } catch (error) {
      console.error("Failed to get WIP summary:", error);
      const empty = { types: 0, values: 0, fns: 0, renames: 0, total: 0 };
      this._onStatusChanged.fire(empty);
      return empty;
    }
  }

  private async getWipItems(): Promise<ScmNode[]> {
    const summary = await this.getWipSummary();
    const items: ScmNode[] = [];

    if (summary.types > 0) {
      items.push({
        id: "wip-types",
        label: `${summary.types} type(s)`,
        type: "wip-item",
        description: "new or modified",
      });
    }
    if (summary.fns > 0) {
      items.push({
        id: "wip-fns",
        label: `${summary.fns} function(s)`,
        type: "wip-item",
        description: "new or modified",
      });
    }
    if (summary.values > 0) {
      items.push({
        id: "wip-values",
        label: `${summary.values} value(s)`,
        type: "wip-item",
        description: "new or modified",
      });
    }
    if (summary.renames > 0) {
      items.push({
        id: "wip-renames",
        label: `${summary.renames} rename(s)`,
        type: "wip-item",
      });
    }

    if (items.length === 0) {
      items.push({
        id: "wip-none",
        label: "No uncommitted changes",
        type: "wip-item",
        description: "all changes committed",
      });
    }

    return items;
  }

  private async getCommitItems(): Promise<ScmNode[]> {
    try {
      const commits = await this.client.sendRequest<CommitInfo[]>("dark/scm/getCommits", { limit: 10 });

      return commits.map(commit => ({
        id: `commit-${commit.id}`,
        label: commit.message,
        type: "commit-item" as ScmNodeType,
        description: `${commit.id.substring(0, 8)} · ${commit.opCount} ops`,
        commitId: commit.id,
      }));
    } catch (error) {
      console.error("Failed to get commits:", error);
      return [{
        id: "commits-error",
        label: "Failed to load commits",
        type: "commit-item",
      }];
    }
  }

  private async getCommitOps(commitId: string): Promise<ScmNode[]> {
    try {
      const ops = await this.client.sendRequest<string[]>("dark/scm/getCommitOps", { commitId });

      return ops.slice(0, 20).map((op, i) => ({
        id: `commit-${commitId}-op-${i}`,
        label: op,
        type: "commit-op" as ScmNodeType,
      }));
    } catch (error) {
      console.error("Failed to get commit ops:", error);
      return [];
    }
  }

  // Get cached status for status bar
  getCachedStatus(): WipSummary | null {
    return this.cachedWipSummary;
  }
}
