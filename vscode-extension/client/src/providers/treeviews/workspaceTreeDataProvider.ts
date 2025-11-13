import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchNode } from "../../types";
import { BranchStateManager } from "../../data/branchStateManager";

interface OpsFilterConfig {
  limit: number | null;  // null means no limit
  branch: 'current' | 'all' | string; // 'current', 'all', or a specific branch ID
  dateRange: 'all' | 'today' | 'week' | 'month' | 'custom';
  customStartDate?: Date;
  locationFilter?: string;
}

// Interface for pending op response from LSP
interface PendingOpResponse {
  op: string;
  label: string;
}

// Location information extracted from an op
interface OpLocation {
  owner: string;
  modules: string[];
  name: string;
}

// Extended BranchNode for pending ops with additional properties
interface PendingOpNode extends BranchNode {
  type: "pending-op";
  location?: OpLocation;
  opData?: string;
}

// Interface for branch quick pick items with custom properties
interface BranchQuickPickItem extends vscode.QuickPickItem {
  value?: string;
  branchID?: string;
}

// Type for package ops (from file system provider)
interface PackageOp {
  // TODO: Define proper structure based on actual op format
  [key: string]: unknown;
}

export class WorkspaceTreeDataProvider implements vscode.TreeDataProvider<BranchNode> {
  private _onDidChangeTreeData: vscode.EventEmitter<BranchNode | undefined | null | void> =
    new vscode.EventEmitter<BranchNode | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<BranchNode | undefined | null | void> =
    this._onDidChangeTreeData.event;

  private branchStateManager = BranchStateManager.getInstance();

  constructor(private client: LanguageClient) {
    // Listen for branch changes and refresh the tree
    this.branchStateManager.onBranchChanged(() => {
      this.refresh();
    });
  }

  // CLEANUP: These methods don't actually add/clear ops - they just refresh the tree
  // to re-fetch ops from the database. Consider renaming or removing.
  addPendingOps(_ops: PackageOp[]): void {
    // Ops are stored in the database, just refresh to show them
    this.refresh();
  }

  clearPendingOps(): void {
    // Ops are stored in DB, just refresh
    this.refresh();
  }

  refresh(): void {
    this._onDidChangeTreeData.fire(undefined);
  }

  dispose(): void {
    this._onDidChangeTreeData.dispose();
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
      item.tooltip = "TODO";
      item.contextValue = "workspace-changes-root"; // Enable context menu
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
      item.tooltip = element.instanceData?.url || element.instanceData?.path || element.label;
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

      const pendingOpNode = element as PendingOpNode;

      // Add command to show diff if we have location info
      if (pendingOpNode.location) {
        item.command = {
          command: "darklang.showOpDiff",
          title: "Show Changes",
          arguments: [pendingOpNode.opData, pendingOpNode.location]
        };
      }

      return item;
    }

    return item;
  }

  async getChildren(element?: BranchNode): Promise<BranchNode[]> {
    if (!element) {
      // Root level: Instance, Branch, Pending Changes
      return await this.getRootNodes();
    }

    // Handle pending changes node - fetch ops from LSP
    if (element.type === "changes-root") {
      return await this.getPendingChanges();
    }

    // Return children of the element
    return element.children || [];
  }

  private async getPendingChanges(): Promise<BranchNode[]> {
    try {
      const ops = []; // TODO: maybe show fake data here

      let nodes: PendingOpNode[] = ops.map((opWithLabel, index) => {
        const location = this.extractLocationFromOp(opWithLabel.op);
        return {
          id: `op-${index}`,
          label: opWithLabel.label,  // Use the formatted label from backend
          type: "pending-op",
          contextValue: "pending-op",
          location: location,  // Store location for the command
          opData: opWithLabel.op  // Store full op for diff view
        };
      });

      return nodes;
    } catch (error) {
      console.error('Failed to get pending ops:', error);
      return [];
    }
  }

  private extractLocationFromOp(op: string): OpLocation | null {
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


  private async getRootNodes(): Promise<BranchNode[]> {
    const branchName = this.branchStateManager.getCurrentBranchName();
    const branchID = this.branchStateManager.getCurrentBranchId();

    // Get instances from LSP
    let instanceChildren: BranchNode[] = [];
    try {
      const instances = await this.client.sendRequest<Array<{id: string, name: string, url: string}>>(
        "dark/listInstances",
        {}
      );

      instanceChildren = instances.map(inst => ({
        id: inst.id,
        label: inst.name,
        type: "instance-item" as const,
        contextValue: "remote-instance",
        instanceData: {
          url: inst.url,
          status: "connected" as const
        },
        children: []
      }));
    } catch (error) {
      console.error("Failed to fetch instances:", error);
    }

    return [
      // Instance node (collapsed by default)
      {
        id: "workspace-instance",
        label: `Instance: Local`,  // TODO: get current instance name from state
        type: "instance-root",
        contextValue: "workspace-instance-root",
        children: instanceChildren
      },
      // Branch node (non-expandable, clickable)
      {
        id: branchID || "no-branch",
        label: `Branch: ${branchName}`,
        type: "branch-root",
        contextValue: "workspace-branch-root",
        children: []
      },
      // Pending Changes node (shows ops created from editing packages)
      {
        id: "pending-changes",
        label: this.getPendingChangesLabel(),
        type: "changes-root",
        contextValue: "workspace-changes-root",
        children: [] // Will be populated async
      }
    ];
  }

  // Filter configuration methods
  private getPendingChangesLabel(): string {
    return 'Pending Changes';
  }
}
