import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchNode, BranchNodeType } from "../../types";
import { BranchStateManager } from "../../data/branchStateManager";

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
  type: BranchNodeType.PendingOp;
  location?: OpLocation;
  opData?: string;
}

// Group node for owner grouping
interface OwnerGroupNode extends BranchNode {
  type: BranchNodeType.OwnerGroup;
  owner: string;
  children: PendingOpNode[];
}

// Type for package ops (from file system provider)
interface PackageOp {
  // TODO: Define proper structure based on actual op format
  [key: string]: unknown;
}

export class WorkspaceTreeDataProvider
  implements vscode.TreeDataProvider<BranchNode>
{
  private _onDidChangeTreeData: vscode.EventEmitter<
    BranchNode | undefined | null | void
  > = new vscode.EventEmitter<BranchNode | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<
    BranchNode | undefined | null | void
  > = this._onDidChangeTreeData.event;

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

    if (
      element.type === BranchNodeType.ChangesRoot ||
      element.type === BranchNodeType.OwnerGroup
    ) {
      collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
    } else if (element.children && element.children.length > 0) {
      collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
    } else {
      collapsibleState = vscode.TreeItemCollapsibleState.None;
    }

    const item = new vscode.TreeItem(element.label, collapsibleState);

    // Set context value for menu visibility
    item.contextValue = element.contextValue;

    // Handle Instance root node
    if (element.type === BranchNodeType.InstanceRoot) {
      item.iconPath = new vscode.ThemeIcon("home");
      item.tooltip = "Click to expand and switch instances";
      return item;
    }

    // Handle Branch root node
    if (element.type === BranchNodeType.BranchRoot) {
      item.iconPath = new vscode.ThemeIcon("git-branch");
      item.tooltip = "Expand to see all branches";
      item.collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
      return item;
    }

    // Handle individual branch items
    if (element.type === BranchNodeType.BranchItem) {
      const branchData = element.branchData;
      if (branchData) {
        item.iconPath = branchData.isCurrent
          ? new vscode.ThemeIcon(
              "circle-filled",
              new vscode.ThemeColor("charts.green"),
            )
          : new vscode.ThemeIcon("git-branch");

        // Special handling for "main (no branch)" item
        if (branchData.isMain && branchData.branchId === "") {
          item.tooltip = "Click to clear branch selection";
          item.command = {
            command: "darklang.branch.clear",
            title: "Clear Branch",
          };
        } else {
          item.tooltip = `Click to switch to ${branchData.branchName}`;
          item.command = {
            command: "darklang.branch.switch",
            title: "Switch Branch",
            arguments: [
              {
                id: branchData.branchId,
                name: branchData.branchName,
              },
            ],
          };
        }
      }
      return item;
    }

    // Handle "Manage All Branches" action node
    if (element.type === BranchNodeType.BranchAction) {
      item.iconPath = new vscode.ThemeIcon("list-flat");
      item.tooltip = "Open branch management interface";
      item.command = {
        command: "darklang.branches.manageAll",
        title: "Manage All Branches",
      };
      return item;
    }

    // Handle Pending Changes root node
    if (element.type === BranchNodeType.ChangesRoot) {
      item.iconPath = new vscode.ThemeIcon(
        "git-commit",
        new vscode.ThemeColor("charts.yellow"),
      );
      item.tooltip = this.getOpsFilterTooltip();
      item.contextValue = "workspace-changes-root"; // Enable context menu
      // Count will be shown when children are loaded
      return item;
    }

    // Handle individual instances under Instance node
    if (element.type === BranchNodeType.InstanceItem) {
      const isCurrent = element.id === "local-instance"; // TODO: get from state
      if (isCurrent) {
        item.description = "●";
        item.iconPath = new vscode.ThemeIcon(
          "home",
          new vscode.ThemeColor("charts.blue"),
        );
      } else {
        item.iconPath = new vscode.ThemeIcon("cloud");
      }
      item.tooltip =
        element.instanceData?.url ||
        element.instanceData?.path ||
        element.label;
      return item;
    }

    // Handle owner group items
    if (element.type === BranchNodeType.OwnerGroup) {
      const ownerGroup = element as OwnerGroupNode;
      item.iconPath = new vscode.ThemeIcon(
        "person",
        new vscode.ThemeColor("charts.purple"),
      );
      item.tooltip = `Changes by ${ownerGroup.owner}`;
      item.description = `${ownerGroup.children.length} ops`;
      return item;
    }

    // Handle pending op items
    if (element.type === BranchNodeType.PendingOp) {
      const pendingOpNode = element as PendingOpNode;
      item.iconPath = new vscode.ThemeIcon(
        "diff-added",
        new vscode.ThemeColor("charts.green"),
      );
      item.tooltip = "Click to open module file";

      // Add command to show diff if we have location info
      if (pendingOpNode.location) {
        item.command = {
          command: "darklang.showOpDiff",
          title: "Show Changes",
          arguments: [pendingOpNode.opData, pendingOpNode.location],
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

    // Handle branch root node - show branch list
    if (element.type === BranchNodeType.BranchRoot) {
      return await this.getBranchChildren();
    }

    // Handle pending changes node - fetch ops from LSP
    if (element.type === BranchNodeType.ChangesRoot) {
      return await this.getPendingChanges();
    }

    // Handle owner group - return its children
    if (element.type === BranchNodeType.OwnerGroup) {
      const ownerGroup = element as OwnerGroupNode;
      return ownerGroup.children;
    }

    // Return children of the element
    return element.children || [];
  }

  private async getBranchChildren(): Promise<BranchNode[]> {
    const currentBranchId = this.branchStateManager.getCurrentBranchId();
    const allBranches = this.branchStateManager.getBranches();

    const children: BranchNode[] = [];

    // 1. Current branch (if any)
    if (currentBranchId) {
      const currentBranch = allBranches.find(b => b.id === currentBranchId);
      if (currentBranch) {
        children.push({
          id: `branch-${currentBranch.id}`,
          label: currentBranch.name,
          type: BranchNodeType.BranchItem,
          contextValue: "branch-item",
          branchData: {
            branchId: currentBranch.id,
            branchName: currentBranch.name,
            isCurrent: true,
            status: undefined,
            opsCount: undefined,
            lastSynced: undefined,
          },
        });
      }
    }

    // 2. Main/Root branch - show as "main (no branch)" when on a branch
    // Only add if we're currently on a specific branch
    if (currentBranchId && currentBranchId !== "") {
      const mainBranchNode: BranchNode = {
        id: "branch-main-clear",
        label: "main (no branch)",
        type: BranchNodeType.BranchItem,
        contextValue: "branch-item-main",
        branchData: {
          branchId: "",
          branchName: "main",
          isMain: true,
          status: undefined,
          opsCount: undefined,
        },
      };
      children.push(mainBranchNode);
    }

    // 3. All other branches (directly, no grouping)
    const otherBranches = allBranches.filter(b => b.id !== currentBranchId);

    otherBranches.forEach(branch => {
      children.push({
        id: `branch-${branch.id}`,
        label: branch.name,
        type: BranchNodeType.BranchItem,
        contextValue: "branch-item",
        branchData: {
          branchId: branch.id,
          branchName: branch.name,
          isCurrent: false,
          status: undefined,
          opsCount: undefined,
          lastSynced: undefined,
        },
      });
    });

    // 4. Add "Manage All" action node at the bottom
    children.push({
      id: "branch-manage-all",
      label: "Manage All Branches",
      type: BranchNodeType.BranchAction,
      contextValue: "branch-manage-all",
    });

    return children;
  }

  private async getPendingChanges(): Promise<BranchNode[]> {
    try {
      const requestParams = {
        limit: 20,
        branchFilter: "current",
        sinceDate: undefined,
      };

      const ops = await this.client.sendRequest<PendingOpResponse[]>(
        "dark/getPendingOps",
        requestParams,
      );

      let nodes: PendingOpNode[] = ops
        .filter(opWithLabel => {
          // Filter out Add* ops completely (AddFn, AddType, AddValue, etc.)
          try {
            const parsed = JSON.parse(opWithLabel.op);
            if (parsed.AddFn || parsed.AddType || parsed.AddValue) {
              return false; // Skip Add* ops
            }
          } catch (e) {
            // If parsing fails, keep the op
          }
          return true;
        })
        .map(opWithLabel => {
          const location = this.extractLocationFromOp(opWithLabel.op);
          // Use the op data itself as a stable ID (not index-based)
          // This ensures IDs don't change when ops are reordered
          const opId = opWithLabel.op;

          // Clean up label - remove "Set*Name →" prefix text, keep only the fqname
          let cleanLabel = opWithLabel.label;
          cleanLabel = cleanLabel.replace(/^SetFnName\s+→\s+/, "");
          cleanLabel = cleanLabel.replace(/^SetTypeName\s+→\s+/, "");
          cleanLabel = cleanLabel.replace(/^SetValueName\s+→\s+/, "");

          return {
            id: opId,
            label: cleanLabel,
            type: BranchNodeType.PendingOp,
            contextValue: "pending-op",
            location: location, // Store location for the command
            opData: opWithLabel.op, // Store full op for diff view
          };
        });

      // Add "see more" node if we hit the limit
      const hasMore = ops.length === 20;

      // Only group by owner when on a non-main branch
      const currentBranchId = this.branchStateManager.getCurrentBranchId();
      const shouldGroupByOwner =
        currentBranchId !== null && currentBranchId !== "";

      let result: BranchNode[];
      if (shouldGroupByOwner) {
        result = this.groupByOwner(nodes);
      } else {
        result = nodes;
      }

      // Add "see more" node at the end if there are more items
      if (hasMore) {
        const seeMoreNode: BranchNode = {
          id: "see-more-changes",
          label: "See more...",
          type: BranchNodeType.SeeMore,
          contextValue: "see-more",
        };
        result.push(seeMoreNode);
      }

      return result;
    } catch (error) {
      console.error("Failed to get pending ops:", error);
      return [];
    }
  }

  private groupByOwner(nodes: PendingOpNode[]): BranchNode[] {
    // Group nodes by owner
    const groupMap = new Map<string, PendingOpNode[]>();

    for (const node of nodes) {
      const owner = node.location?.owner || "Unknown";
      if (!groupMap.has(owner)) {
        groupMap.set(owner, []);
      }
      groupMap.get(owner)!.push(node);
    }

    // Convert to owner group nodes
    const ownerGroups: OwnerGroupNode[] = [];
    for (const [owner, opsInGroup] of groupMap.entries()) {
      ownerGroups.push({
        id: `owner-group-${owner}`,
        label: owner,
        type: BranchNodeType.OwnerGroup,
        contextValue: "owner-group",
        owner: owner,
        children: opsInGroup,
      });
    }

    // Sort by owner name
    return ownerGroups.sort((a, b) => a.owner.localeCompare(b.owner));
  }

  private extractLocationFromOp(op: string): OpLocation | null {
    try {
      if (typeof op === "string") {
        const parsed = JSON.parse(op);

        // SetTypeName has location
        if (parsed.SetTypeName && parsed.SetTypeName[1]) {
          const location = parsed.SetTypeName[1];
          return {
            owner: location.owner || "",
            modules: location.modules || [],
            name: location.name || "",
          };
        }

        // SetFnName has location
        if (parsed.SetFnName && parsed.SetFnName[1]) {
          const location = parsed.SetFnName[1];
          return {
            owner: location.owner || "",
            modules: location.modules || [],
            name: location.name || "",
          };
        }

        // SetValueName has location
        if (parsed.SetValueName && parsed.SetValueName[1]) {
          const location = parsed.SetValueName[1];
          return {
            owner: location.owner || "",
            modules: location.modules || [],
            name: location.name || "",
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
      const instances = await this.client.sendRequest<
        Array<{ id: string; name: string; url: string }>
      >("dark/listInstances", {});

      instanceChildren = instances.map(inst => ({
        id: inst.id,
        label: inst.name,
        type: BranchNodeType.InstanceItem,
        contextValue: "remote-instance",
        instanceData: {
          url: inst.url,
          status: "connected" as const,
        },
        children: [],
      }));
    } catch (error) {
      console.error("Failed to fetch instances:", error);
    }

    return [
      // Instance node (collapsed by default)
      {
        id: "workspace-instance",
        label: `Instance: Local`, // TODO: get current instance name from state
        type: BranchNodeType.InstanceRoot,
        contextValue: "workspace-instance-root",
        children: instanceChildren,
      },
      // Branch node (non-expandable, clickable)
      {
        id: branchID || "no-branch",
        label: `Branch: ${branchName}`,
        type: BranchNodeType.BranchRoot,
        contextValue: "workspace-branch-root",
        children: [],
      },
      // Pending Changes node
      {
        id: "workspace-changes",
        label: this.getPendingChangesLabel(),
        type: BranchNodeType.ChangesRoot,
        contextValue: "workspace-changes-root",
        children: [],
      },
    ];
  }

  private getPendingChangesLabel(): string {
    return "Changes";
  }

  private getOpsFilterTooltip(): string {
    return "Pending changes from the current branch";
  }
}
