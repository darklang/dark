import * as vscode from "vscode";

export interface StatusData {
  instance: { name: string };
  branch: { name: string };
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
    // TODO: Get actual instance name from instance manager/state
    return {
      instance: { name: "Local" },
      branch: { name: "main" }
    };
  }

  private createStatusBarItems(): void {
    this.statusBarItems = [
      vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100),
      vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 99),
    ];
  }

  private updateDisplay(): void {
    const { instance, branch } = this.currentData;

    this.statusBarItems[0].text = `$(server) ${instance.name}`;
    this.statusBarItems[0].tooltip = "Current Darklang instance";
    // TODO: Add command to switch instances when implemented

    this.statusBarItems[1].text = `$(target) ${branch.name}`;
    this.statusBarItems[1].tooltip = "Current branch - click to manage";
    this.statusBarItems[1].command = "darklang.branch.switch";

    this.statusBarItems.forEach(item => item.show());
  }

  public updateData(newData: Partial<StatusData>): void {
    this.currentData = { ...this.currentData, ...newData };
    this.updateDisplay();
  }

  public updateBranch(branchName: string): void {
    this.updateData({ branch: { name: branchName } });
  }

  public updateInstance(instanceName: string): void {
    this.updateData({ instance: { name: instanceName } });
  }

  public dispose(): void {
    this.statusBarItems.forEach(item => item.dispose());
  }
}