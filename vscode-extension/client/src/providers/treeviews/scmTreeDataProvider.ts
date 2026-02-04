import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

// SCM tree node types
type ScmNodeType = "branch-root" | "status-root" | "commits-root" | "wip-item" | "wip-op" | "commit-item" | "commit-op";

interface ScmNode {
  id: string;
  label: string;
  type: ScmNodeType;
  description?: string;
  children?: ScmNode[];
  commitId?: string;
  branchName?: string;
  isAncestor?: boolean;
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
  branchId?: string;
  branchName?: string;
}

interface BranchInfo {
  id: string;
  name: string;
  parentBranchId?: string;
  isActive?: boolean;
}

export class ScmTreeDataProvider implements vscode.TreeDataProvider<ScmNode>, vscode.Disposable {
  private _onDidChangeTreeData = new vscode.EventEmitter<ScmNode | undefined | null | void>();
  readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

  private _onStatusChanged = new vscode.EventEmitter<WipSummary>();
  readonly onStatusChanged = this._onStatusChanged.event;

  private cachedWipSummary: WipSummary | null = null;
  private currentBranch: BranchInfo | null = null;

  constructor(private client: LanguageClient) {}

  updateBranch(branch: BranchInfo): void {
    this.currentBranch = branch;
    this.refresh();
  }

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

    if (element.type === "branch-root") {
      item.collapsibleState = vscode.TreeItemCollapsibleState.None;
    } else if (element.type === "status-root" || element.type === "commits-root") {
      item.collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
    } else if (element.type === "commit-item") {
      item.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
    } else {
      item.collapsibleState = vscode.TreeItemCollapsibleState.None;
    }

    item.description = element.description;

    switch (element.type) {
      case "branch-root":
        item.iconPath = new vscode.ThemeIcon("git-branch", new vscode.ThemeColor("charts.blue"));
        item.contextValue = "scm-branch-root";
        item.command = {
          command: "darklang.branch.switch",
          title: "Switch Branch",
        };
        break;
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
      case "wip-op":
        item.iconPath = new vscode.ThemeIcon("symbol-method", new vscode.ThemeColor("charts.green"));
        item.contextValue = "scm-wip-op";
        break;
      case "commit-item":
        if (element.isAncestor) {
          item.iconPath = new vscode.ThemeIcon("git-commit", new vscode.ThemeColor("disabledForeground"));
        } else {
          item.iconPath = new vscode.ThemeIcon("git-commit");
        }
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
    // Fetch current branch if not cached
    if (!this.currentBranch) {
      try {
        this.currentBranch = await this.client.sendRequest<BranchInfo>("dark/getCurrentBranch", {});
      } catch {
        // ignore - will show fallback
      }
    }

    const branchName = this.currentBranch?.name ?? "main";

    const summary = await this.getWipSummary();
    const statusLabel = summary.total > 0
      ? `Uncommitted Changes (${summary.total})`
      : "Uncommitted Changes";

    return [
      {
        id: "__branch__",
        label: `Branch: ${branchName}`,
        type: "branch-root",
        description: "click to switch",
      },
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
    try {
      const ops = await this.client.sendRequest<string[]>("dark/scm/getWipOps", {});

      if (ops.length === 0) {
        return [{
          id: "wip-none",
          label: "No uncommitted changes",
          type: "wip-item",
          description: "all changes committed",
        }];
      }

      return ops.map((op, i) => ({
        id: `wip-op-${i}`,
        label: op,
        type: "wip-op" as ScmNodeType,
      }));
    } catch (error) {
      console.error("Failed to get WIP ops:", error);
      return [{
        id: "wip-error",
        label: "Failed to load changes",
        type: "wip-item",
      }];
    }
  }

  private async getCommitItems(): Promise<ScmNode[]> {
    try {
      const commits = await this.client.sendRequest<CommitInfo[]>("dark/scm/getCommits", { limit: 20 });
      const currentBranchId = this.currentBranch?.id;

      return commits.map(commit => {
        const isAncestor = currentBranchId != null && commit.branchId != null && commit.branchId !== currentBranchId;
        const branchLabel = isAncestor && commit.branchName ? ` · ${commit.branchName}` : "";
        return {
          id: `commit-${commit.id}`,
          label: commit.message,
          type: "commit-item" as ScmNodeType,
          description: `${commit.id.substring(0, 8)} · ${commit.opCount} ops${branchLabel}`,
          commitId: commit.id,
          branchName: commit.branchName,
          isAncestor,
        };
      });
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
