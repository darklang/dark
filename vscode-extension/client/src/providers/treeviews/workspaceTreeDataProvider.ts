import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchNode } from "../../types";
import { BranchStateManager } from "../../data/branchStateManager";
import { InstanceDemoData } from "../../data/demo/instanceDemoData";

export class WorkspaceTreeDataProvider implements vscode.TreeDataProvider<BranchNode> {
  private _onDidChangeTreeData: vscode.EventEmitter<BranchNode | undefined | null | void> =
    new vscode.EventEmitter<BranchNode | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<BranchNode | undefined | null | void> =
    this._onDidChangeTreeData.event;

  private branchStateManager = BranchStateManager.getInstance();
  private client: LanguageClient | null = null;

  constructor() {
    console.log('🌳 WorkspaceTreeDataProvider constructor called');

    // Listen for branch changes and refresh the tree
    this.branchStateManager.onBranchChanged(() => {
      console.log('🔔 WorkspaceTreeDataProvider: Branch changed event received');
      this.refresh();
    });
  }

  setClient(client: LanguageClient): void {
    this.client = client;
  }

  addPendingOps(ops: any[]): void {
    console.log('WorkspaceTreeDataProvider.addPendingOps called - refreshing to show new ops from DB');
    // Ops are stored in the database, just refresh to show them
    this.refresh();
  }

  clearPendingOps(): void {
    // Ops are stored in DB, just refresh
    this.refresh();
  }

  refresh(): void {
    console.log('🔄 WorkspaceTreeDataProvider: refresh() called');
    this._onDidChangeTreeData.fire(undefined);
  }

  refreshItem(item: BranchNode): void {
    this._onDidChangeTreeData.fire(item);
  }

  getTreeItem(element: BranchNode): vscode.TreeItem {
    // Determine collapsible state
    let collapsibleState: vscode.TreeItemCollapsibleState;

    if (element.type === "changes-root") {
      // Always expandable to show pending ops
      collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
    } else if (element.children && element.children.length > 0) {
      collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
    } else {
      collapsibleState = vscode.TreeItemCollapsibleState.None;
    }

    const item = new vscode.TreeItem(element.label, collapsibleState);

    // Set context value for menu visibility
    item.contextValue = element.contextValue;

    // Handle Instance root node
    if (element.type === "instance-root") {
      item.iconPath = new vscode.ThemeIcon("home");
      item.tooltip = "Click to expand and switch instances";
      return item;
    }

    // Handle Branch root node (non-expandable, clickable)
    if (element.type === "branch-root") {
      item.iconPath = new vscode.ThemeIcon("git-branch");
      item.tooltip = "Click to switch branches";
      item.command = {
        command: "darklang.branch.quickSwitch",
        title: "Switch Branch",
        arguments: []
      };
      return item;
    }


    // Handle Pending Changes root node
    if (element.type === "changes-root") {
      item.iconPath = new vscode.ThemeIcon("git-commit", new vscode.ThemeColor("charts.yellow"));
      item.tooltip = "Recent package operations (last 50)";
      // Count will be shown when children are loaded
      return item;
    }

    // Handle individual instances under Instance node
    if (element.type === "instance-item") {
      const isCurrent = element.id === "local-instance"; // TODO: get from state
      if (isCurrent) {
        item.description = "●";
        item.iconPath = new vscode.ThemeIcon("home", new vscode.ThemeColor("charts.blue"));
      } else {
        item.iconPath = new vscode.ThemeIcon("cloud");
      }
      item.tooltip = element.instanceData?.url || element.instanceData?.path || "Switch to this instance";
      item.command = {
        command: "darklang.instance.switch",
        title: "Switch Instance",
        arguments: [element.id]
      };
      return item;
    }

    // Handle instance children (packages, branches, actions)
    if (element.type === "packages") {
      item.iconPath = new vscode.ThemeIcon("package");
      return item;
    }
    if (element.type === "branches") {
      item.iconPath = new vscode.ThemeIcon("git-branch");
      return item;
    }
    if (element.type === "category") {
      item.iconPath = new vscode.ThemeIcon("folder");
      return item;
    }

    // Handle pending op items
    if (element.type === "pending-op") {
      item.iconPath = new vscode.ThemeIcon("diff-added", new vscode.ThemeColor("charts.green"));
      item.tooltip = "Click to open module file";

      // Add command to show diff if we have location info
      if ((element as any).location) {
        item.command = {
          command: "darklang.showOpDiff",
          title: "Show Changes",
          arguments: [(element as any).opData, (element as any).location]
        };
      }

      return item;
    }

    return item;
  }

  async getChildren(element?: BranchNode): Promise<BranchNode[]> {
    if (!element) {
      // Root level: Instance, Branch, Pending Changes
      return this.getRootNodes();
    }

    // Handle pending changes node - fetch ops from LSP
    if (element.type === "changes-root") {
      return await this.getPendingChanges();
    }

    // Return children of the element
    return element.children || [];
  }

  private async getPendingChanges(): Promise<BranchNode[]> {
    console.log('getPendingChanges called - querying database');

    if (!this.client) {
      console.log('No LSP client available');
      return [];
    }

    try {
      const ops = await this.client.sendRequest<Array<{op: string, label: string}>>(
        'darklang/getPendingOps',
        {}
      );

      console.log('Received ops from database:', ops.length);

      const nodes = ops.map((opWithLabel, index) => {
        const location = this.extractLocationFromOp(opWithLabel.op);
        return {
          id: `op-${index}`,
          label: opWithLabel.label,  // Use the formatted label from backend
          type: "pending-op" as any,
          contextValue: "pending-op",
          location: location,  // Store location for the command
          opData: opWithLabel.op  // Store full op for diff view
        };
      });

      console.log('Returning nodes:', nodes);
      return nodes;
    } catch (error) {
      console.error('Failed to get pending ops:', error);
      return [];
    }
  }

  private extractLocationFromOp(op: any): { owner: string, modules: string[], name: string } | null {
    try {
      if (typeof op === 'string') {
        const parsed = JSON.parse(op);

        // SetTypeName has location
        if (parsed.SetTypeName && parsed.SetTypeName[1]) {
          const location = parsed.SetTypeName[1];
          return {
            owner: location.owner || '',
            modules: location.modules || [],
            name: location.name || ''
          };
        }

        // SetFnName has location
        if (parsed.SetFnName && parsed.SetFnName[1]) {
          const location = parsed.SetFnName[1];
          return {
            owner: location.owner || '',
            modules: location.modules || [],
            name: location.name || ''
          };
        }

        // SetValueName has location
        if (parsed.SetValueName && parsed.SetValueName[1]) {
          const location = parsed.SetValueName[1];
          return {
            owner: location.owner || '',
            modules: location.modules || [],
            name: location.name || ''
          };
        }
      }
    } catch (e) {
      // Ignore parse errors
    }
    return null;
  }


  private getRootNodes(): BranchNode[] {
    const branchName = this.branchStateManager.getCurrentBranchName();
    const branchId = this.branchStateManager.getCurrentBranchId();

    // Get instances for the Instance node
    const instances = InstanceDemoData.getInstancesData();
    const instanceChildren: BranchNode[] = instances.map(inst => ({
      id: inst.id,
      label: inst.label.replace("★ ", ""), // Remove star from label
      type: "instance-item" as any,
      contextValue: inst.contextValue,
      instanceData: inst.instanceData,
      children: inst.children?.map(child => ({
        id: child.id,
        label: child.label,
        type: child.type as any,
        contextValue: child.contextValue,
        instanceData: child.instanceData,
        children: child.children?.map(grandchild => ({
          id: grandchild.id,
          label: grandchild.label,
          type: grandchild.type as any,
          contextValue: grandchild.contextValue
        }))
      }))
    }));

    return [
      // Instance node (collapsed by default)
      {
        id: "workspace-instance",
        label: `Instance: Local`,  // TODO: get current instance name from state
        type: "instance-root" as any,
        contextValue: "workspace-instance-root",
        children: instanceChildren
      },
      // Branch node (non-expandable, clickable)
      {
        id: branchId || "no-branch",
        label: `Branch: ${branchName}`,
        type: "branch-root" as any,
        contextValue: "workspace-branch-root",
        children: []
      },
      // Pending Changes node (shows ops created from editing packages)
      {
        id: "pending-changes",
        label: "Pending Changes",
        type: "changes-root" as any,
        contextValue: "workspace-changes-root",
        children: [] // Will be populated async
      }
    ];
  }
}
