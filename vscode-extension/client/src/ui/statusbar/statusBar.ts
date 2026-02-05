import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

export interface StatusBarStatus {
  total: number;
  types: number;
  fns: number;
  values: number;
}

export interface BranchInfo {
  id: string;
  name: string;
  parentBranchId?: string;
  isActive?: boolean;
}

export class StatusBar implements vscode.Disposable {
  private statusBarItem: vscode.StatusBarItem;
  private currentStatus: StatusBarStatus = { total: 0, types: 0, fns: 0, values: 0 };
  private currentBranch: BranchInfo | null = null;
  private client: LanguageClient | null = null;

  constructor() {
    this.statusBarItem = vscode.window.createStatusBarItem(
      vscode.StatusBarAlignment.Left,
      100
    );
    this.statusBarItem.command = "darklang.branch.switch";
    this.updateDisplay();
    this.statusBarItem.show();
  }

  setClient(client: LanguageClient): void {
    this.client = client;
    this.fetchCurrentBranch();
  }

  async fetchCurrentBranch(): Promise<void> {
    if (!this.client) return;
    try {
      const branch = await this.client.sendRequest<BranchInfo>("dark/getCurrentBranch", {});
      this.currentBranch = branch;
      this.updateDisplay();
    } catch (error) {
      console.error("Failed to get current branch:", error);
    }
  }

  updateBranch(branch: BranchInfo): void {
    this.currentBranch = branch;
    this.updateDisplay();
  }

  updateStatus(status: StatusBarStatus): void {
    this.currentStatus = status;
    this.updateDisplay();
  }

  private updateDisplay(): void {
    const { total, types, fns, values } = this.currentStatus;
    const branchName = this.currentBranch?.name ?? "main";

    const parts: string[] = [];
    parts.push(`$(git-branch) ${branchName}`);

    if (total > 0) {
      parts.push(`$(git-commit) ${total} uncommitted`);
    }

    this.statusBarItem.text = parts.join(" · ");

    const tooltipLines: string[] = [`Branch: ${branchName}`];
    if (total === 0) {
      tooltipLines.push("No uncommitted changes");
    } else {
      const details: string[] = [];
      if (types > 0) details.push(`${types} type(s)`);
      if (fns > 0) details.push(`${fns} fn(s)`);
      if (values > 0) details.push(`${values} value(s)`);
      tooltipLines.push(`Uncommitted: ${details.join(", ")}`);
    }
    tooltipLines.push("Click to switch branch");

    this.statusBarItem.tooltip = tooltipLines.join("\n");

    if (total > 0) {
      this.statusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.warningBackground");
    } else {
      this.statusBarItem.backgroundColor = undefined;
    }
  }

  dispose(): void {
    this.statusBarItem.dispose();
  }
}
