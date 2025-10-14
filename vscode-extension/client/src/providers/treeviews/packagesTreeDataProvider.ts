import * as vscode from "vscode";
import { PackageNode } from "../../types";
import { BranchStateManager } from "../../data/branchStateManager";

export class PackageTreeItem extends vscode.TreeItem {
  constructor(
    public readonly node: PackageNode,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState
  ) {
    super(node.label, collapsibleState);

    this.tooltip = this.getTooltip();
    this.contextValue = node.contextValue;
    this.setIcon();
    this.setCommand();
  }

  private getTooltip(): string {
    const { node } = this;

    return `${node.label} - Click to view or edit`;
  }

  private setIcon(): void {
    const { node } = this;

    // Set icons based on node type and status indicators
    if (node.type  === "owner") {
      this.iconPath = new vscode.ThemeIcon("symbol-package", new vscode.ThemeColor("charts.green"));

    } else if (node.type === "module" ) {
      this.iconPath = new vscode.ThemeIcon("folder", new vscode.ThemeColor("charts.blue"));

    } else if (node.type === "function") {
      this.iconPath = new vscode.ThemeIcon("symbol-function", new vscode.ThemeColor("charts.blue"));

    } else if (node.type === "type") {
      this.iconPath = new vscode.ThemeIcon("symbol-type-parameter", new vscode.ThemeColor("charts.purple"));

    } else  {
      this.iconPath = new vscode.ThemeIcon("symbol-structure");
    }
  }

  private setCommand(): void {
    const { node } = this;

    // Add command to open the definition when clicked on functions/types
    if (node.type === "function" || node.type === "type" || node.type === "constant") {
      if (node.packagePath) {
        // Open in read-only mode for normal items
        this.command = {
          command: "darklang.openPackageDefinition",
          title: "View Definition",
          arguments: [node.packagePath]
        };
      }
    }
    // Add commands for modules and namespaces
    else if (node.type === "module" && node.packagePath) {
      this.command = {
        command: "darklang.openFullModule",
        title: "View Module",
        arguments: [node]
      };
    }
  }
}

export class PackagesTreeDataProvider implements vscode.TreeDataProvider<PackageNode> {
  private _onDidChangeTreeData: vscode.EventEmitter<PackageNode | undefined | null | void> =
    new vscode.EventEmitter<PackageNode | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<PackageNode | undefined | null | void> =
    this._onDidChangeTreeData.event;

  private data: PackageNode[] = [];
  private branchStateManager = BranchStateManager.getInstance();

  constructor() {
    this.refresh();
    // Listen for branch changes
    this.branchStateManager.onBranchChanged(() => {
      this.refresh();
    });
  }

  refresh(): void {
    this.data = this.branchStateManager.getCurrentBranchState().packages;
    this._onDidChangeTreeData.fire();
  }

  getTreeItem(element: PackageNode): vscode.TreeItem {
    const collapsibleState = element.children && element.children.length > 0
      ? (element.collapsibleState === 1
          ? vscode.TreeItemCollapsibleState.Collapsed
          : vscode.TreeItemCollapsibleState.Expanded)
      : vscode.TreeItemCollapsibleState.None;

    return new PackageTreeItem(element, collapsibleState);
  }

  getChildren(element?: PackageNode): Thenable<PackageNode[]> {
    if (!element) {
      // Return root nodes
      return Promise.resolve(this.data);
    }

    // Return children of the element
    return Promise.resolve(element.children || []);
  }
}