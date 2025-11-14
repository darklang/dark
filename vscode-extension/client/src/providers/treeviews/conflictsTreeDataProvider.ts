import * as vscode from "vscode";

interface ConflictNode {
  id: string;
  label: string;
  description?: string;
  contextValue: string;
}

export class ConflictsTreeDataProvider implements vscode.TreeDataProvider<ConflictNode> {
  private _onDidChangeTreeData: vscode.EventEmitter<ConflictNode | undefined | null | void> =
    new vscode.EventEmitter<ConflictNode | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<ConflictNode | undefined | null | void> =
    this._onDidChangeTreeData.event;

  constructor() {}

  refresh(): void {
    this._onDidChangeTreeData.fire();
  }

  dispose(): void {
    this._onDidChangeTreeData.dispose();
  }

  getTreeItem(element: ConflictNode): vscode.TreeItem {
    const item = new vscode.TreeItem(element.label, vscode.TreeItemCollapsibleState.None);
    item.contextValue = element.contextValue;

    if (element.description) {
      item.description = element.description;
    }

    // Use issue-draft icon for conflicts
    item.iconPath = new vscode.ThemeIcon("issue-draft", new vscode.ThemeColor("descriptionForeground"));

    item.tooltip = element.label;

    return item;
  }

  async getChildren(element?: ConflictNode): Promise<ConflictNode[]> {
    if (!element) {
      // Root level - return fake conflicts
      return this.getFakeConflicts();
    }

    return [];
  }

  private getFakeConflicts(): ConflictNode[] {
    return [
      {
        id: "conflict-1",
        label: "Darklang.Internal.Test.WTTest",
        description: "pointing to two different definitions",
        contextValue: "conflict-item"
      },
      {
        id: "conflict-2",
        label: "Darklang.Stdlib.String.concat",
        description: "pointing to two different definitions",
        contextValue: "conflict-item"
      }
    ];
  }

  getConflictCount(): number {
    return 2; // Return the count of conflicts for the title badge
  }
}
