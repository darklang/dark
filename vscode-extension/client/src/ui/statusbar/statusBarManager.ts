import * as vscode from "vscode";

export interface StatusData {
  instance: { name: string };
  branch: { name: string };
  user: string;
}

export class StatusBarManager {
  private statusBarItems: vscode.StatusBarItem[] = [];
  private currentData: StatusData;

  constructor() {
    this.currentData = this.createDemoData();
    this.createStatusBarItems();
    this.updateDisplay();
  }

  private createDemoData(): StatusData {
    return {
      instance: { name: "Local" },
      branch: { name: "main" },
      user: "stachu"
    };
  }

  private createStatusBarItems(): void {
    this.statusBarItems = [
      vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100),
      vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 99),
      vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 98),
    ];
  }

  private updateDisplay(): void {
    const { instance, branch, user } = this.currentData;

    this.statusBarItems[0].text = `$(server) ${instance.name}`;
    this.statusBarItems[0].tooltip = "Click to switch Darklang instance";
    this.statusBarItems[0].command = "darklang.instance.switch";

    this.statusBarItems[1].text = `$(target) ${branch.name}`;
    this.statusBarItems[1].tooltip = "Current branch - click to manage";
    this.statusBarItems[1].command = "darklang.branch.switch";

    this.statusBarItems[2].text = `$(account) ${user}`;
    this.statusBarItems[2].tooltip = "Current user";
    this.statusBarItems[2].command = "darklang.user.switch";

    this.statusBarItems.forEach(item => item.show());
  }

  public updateData(newData: Partial<StatusData>): void {
    this.currentData = { ...this.currentData, ...newData };
    this.updateDisplay();
  }

  public updateBranch(branchName: string): void {
    this.updateData({ branch: { name: branchName } });
  }

  public dispose(): void {
    this.statusBarItems.forEach(item => item.dispose());
  }

  public getCurrentData(): StatusData {
    return { ...this.currentData };
  }
}