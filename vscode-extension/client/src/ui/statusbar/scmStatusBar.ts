import * as vscode from "vscode";

export interface ScmStatus {
  total: number;
  types: number;
  fns: number;
  values: number;
}

export class ScmStatusBar implements vscode.Disposable {
  private statusBarItem: vscode.StatusBarItem;
  private currentStatus: ScmStatus = { total: 0, types: 0, fns: 0, values: 0 };

  constructor() {
    this.statusBarItem = vscode.window.createStatusBarItem(
      vscode.StatusBarAlignment.Left,
      100
    );
    this.statusBarItem.command = "darklang.scm.status";
    this.updateDisplay();
    this.statusBarItem.show();
  }

  updateStatus(status: ScmStatus): void {
    this.currentStatus = status;
    this.updateDisplay();
  }

  private updateDisplay(): void {
    const { total, types, fns, values } = this.currentStatus;

    if (total === 0) {
      this.statusBarItem.text = "$(check) Darklang";
      this.statusBarItem.tooltip = "No uncommitted changes";
      this.statusBarItem.backgroundColor = undefined;
    } else {
      this.statusBarItem.text = `$(git-commit) ${total} uncommitted`;

      const details: string[] = [];
      if (types > 0) details.push(`${types} type(s)`);
      if (fns > 0) details.push(`${fns} fn(s)`);
      if (values > 0) details.push(`${values} value(s)`);

      this.statusBarItem.tooltip = `Uncommitted: ${details.join(", ")}\nClick to show status`;
      this.statusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.warningBackground");
    }
  }

  dispose(): void {
    this.statusBarItem.dispose();
  }
}
