import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchNode } from "../../types";
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
  type: "pending-op";
  location?: OpLocation;
  opData?: string;
  checked?: boolean;
}

// Group node for owner grouping
interface OwnerGroupNode extends BranchNode {
  type: "owner-group";
  owner: string;
  children: PendingOpNode[];
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


  // Track checked state of ops
  private checkedOps: Map<string, boolean> = new Map();
  // Cache of current ops for select all/deselect all
  private currentOpIds: string[] = [];

  constructor(private client: LanguageClient) {
    // Listen for branch changes and refresh the tree
    this.branchStateManager.onBranchChanged(() => {
      this.refresh();
    });
  }

  private formatRelativeTime(date: Date): string {
    const now = new Date();
    const diffMs = now.getTime() - date.getTime();
    const diffMinutes = Math.floor(diffMs / 60000);
    const diffHours = Math.floor(diffMs / 3600000);
    const diffDays = Math.floor(diffMs / 86400000);

    if (diffMinutes < 1) {
      return "just now";
    } else if (diffMinutes < 60) {
      return `${diffMinutes} min ago`;
    } else if (diffHours < 24) {
      return `${diffHours} hour${diffHours !== 1 ? 's' : ''} ago`;
    } else {
      return `${diffDays} day${diffDays !== 1 ? 's' : ''} ago`;
    }
  }

  toggleSelectAll(): void {
    // Check if all ops are currently selected
    const allSelected = this.areAllOpsSelected();

    if (allSelected) {
      // Deselect all
      for (const opId of this.currentOpIds) {
        this.checkedOps.set(opId, false);
      }
    } else {
      // Select all
      for (const opId of this.currentOpIds) {
        this.checkedOps.set(opId, true);
      }
    }

    this.updateSelectAllContext();
    this.refresh();
  }

  handleCheckboxChange(e: vscode.TreeCheckboxChangeEvent<BranchNode>): void {
    // Update the checkedOps map based on user's manual checkbox clicks
    for (const [item, state] of e.items) {
      const isChecked = state === vscode.TreeItemCheckboxState.Checked;
      this.checkedOps.set(item.id, isChecked);
    }

    this.updateSelectAllContext();
    this.refresh();
  }

  selectOwnerGroup(ownerGroupNode: OwnerGroupNode): void {
    // Toggle: check if all ops in this group are currently selected
    if (!ownerGroupNode.children || ownerGroupNode.children.length === 0) {
      return;
    }

    const allSelected = ownerGroupNode.children.every(child =>
      this.checkedOps.get(child.id) === true
    );

    if (allSelected) {
      // Deselect all ops in this owner group
      for (const child of ownerGroupNode.children) {
        this.checkedOps.set(child.id, false);
      }
    } else {
      // Select all ops in this owner group
      for (const child of ownerGroupNode.children) {
        this.checkedOps.set(child.id, true);
      }
    }

    this.updateSelectAllContext();
    this.refresh();
  }

  private areAllOpsSelected(): boolean {
    if (this.currentOpIds.length === 0) {
      return false;
    }

    return this.currentOpIds.every(opId => this.checkedOps.get(opId) === true);
  }

  private updateSelectAllContext(): void {
    const allSelected = this.areAllOpsSelected();
    const currentBranchId = this.branchStateManager.getCurrentBranchId();
    const isOnBranch = currentBranchId !== null && currentBranchId !== '';

    vscode.commands.executeCommand("setContext", "darklang.allOpsSelected", allSelected);
    vscode.commands.executeCommand("setContext", "darklang.isSpecificBranchSelected", isOnBranch);
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
    this.updateSelectAllContext();
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

    if (element.type === "changes-root" || element.type === "owner-group") {
      // Always expandable to show pending ops or grouped ops
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
    if (element.type === "instance-root") {
      item.iconPath = new vscode.ThemeIcon("home");
      item.tooltip = "Click to expand and switch instances";
      return item;
    }

    // Handle Branch root node (now expandable)
    if (element.type === "branch-root") {
      item.iconPath = new vscode.ThemeIcon("git-branch");
      item.tooltip = "Expand to see all branches";
      item.collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
      return item;
    }

    // Handle branch header (section label)
    if (element.type === "branch-header") {
      item.iconPath = new vscode.ThemeIcon("layers");
      item.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
      return item;
    }

    // Handle individual branch items
    if (element.type === "branch-item") {
      const branchData = element.branchData;
      if (branchData) {
        // Build description with status
        let descriptions: string[] = [];

        // Add last synced time first with "last synced" prefix
        if (branchData.lastSynced) {
          descriptions.push(`last synced ${this.formatRelativeTime(branchData.lastSynced)}`);
        }

        // Add status for all branches (always show status)
        if (branchData.status === "ahead" && branchData.opsCount !== undefined && branchData.opsCount > 0) {
          descriptions.push(`${branchData.opsCount} op${branchData.opsCount !== 1 ? 's' : ''} ahead`);
        } else if (branchData.status === "behind" && branchData.opsCount !== undefined && branchData.opsCount > 0) {
          descriptions.push(`${branchData.opsCount} op${branchData.opsCount !== 1 ? 's' : ''} behind`);
        } else {
          // Default to up-to-date if no specific status or count is 0
          descriptions.push("up-to-date");
        }

        item.description = descriptions.join(" • ");

        item.iconPath = branchData.isCurrent
          ? new vscode.ThemeIcon("circle-filled", new vscode.ThemeColor("charts.green"))
          : new vscode.ThemeIcon("git-branch");

        item.tooltip = `Click to switch to ${branchData.branchName}`;

        // Add command to switch to this branch
        item.command = {
          command: "darklang.branch.switch",
          title: "Switch Branch",
          arguments: [branchData.branchId]
        };
      }
      return item;
    }

    // Handle "Manage All Branches" action node
    if (element.type === "branch-action") {
      item.iconPath = new vscode.ThemeIcon("list-flat");
      item.tooltip = "Open branch management interface";
      item.command = {
        command: "darklang.branches.manageAll",
        title: "Manage All Branches"
      };
      return item;
    }


    // Handle Pending Changes root node
    if (element.type === "changes-root") {
      item.iconPath = new vscode.ThemeIcon("git-commit", new vscode.ThemeColor("charts.yellow"));
      item.tooltip = this.getOpsFilterTooltip();
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

    // Handle owner group items
    if (element.type === "owner-group") {
      const ownerGroup = element as OwnerGroupNode;
      item.iconPath = new vscode.ThemeIcon("person", new vscode.ThemeColor("charts.purple"));
      item.tooltip = `Changes by ${ownerGroup.owner}`;
      item.description = `${ownerGroup.children.length} ops`;

      // Set contextValue based on whether all ops in this group are selected
      const allSelected = ownerGroup.children.every(child =>
        this.checkedOps.get(child.id) === true
      );
      item.contextValue = allSelected ? "owner-group-allselected" : "owner-group-notallselected";

      return item;
    }

    // Handle pending op items
    if (element.type === "pending-op") {
      const pendingOpNode = element as PendingOpNode;

      // Only show checkboxes when on a non-main branch
      const currentBranchId = this.branchStateManager.getCurrentBranchId();
      const shouldShowCheckbox = currentBranchId !== null && currentBranchId !== '';

      if (shouldShowCheckbox) {
        // Add checkbox support
        const isChecked = this.checkedOps.get(pendingOpNode.id) || false;
        item.checkboxState = isChecked
          ? vscode.TreeItemCheckboxState.Checked
          : vscode.TreeItemCheckboxState.Unchecked;
      }

      item.iconPath = new vscode.ThemeIcon("diff-added", new vscode.ThemeColor("charts.green"));
      item.tooltip = "Click to open module file";

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

    // Handle branch root node - show branch list
    if (element.type === "branch-root") {
      return await this.getBranchChildren();
    }

    // Handle pending changes node - fetch ops from LSP
    if (element.type === "changes-root") {
      return await this.getPendingChanges();
    }

    // Handle owner group - return its children
    if (element.type === "owner-group") {
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
        // TODO: Get actual last synced time from LSP
        const lastSynced = new Date(Date.now() - 5 * 60 * 1000); // Mock: 5 minutes ago

        children.push({
          id: `branch-${currentBranch.id}`,
          label: currentBranch.name,
          type: "branch-item",
          contextValue: "branch-item",
          branchData: {
            branchId: currentBranch.id,
            branchName: currentBranch.name,
            isCurrent: true,
            status: "up-to-date",
            opsCount: 0,
            lastSynced: lastSynced
          }
        });
      }
    }

    // 2. Main/Root branch (always show, unless it's the current branch)
    // TODO: Get actual last synced time from LSP
    const mainLastSynced = new Date(Date.now() - 10 * 60 * 1000); // Mock: 10 minutes ago

    const mainBranchNode: BranchNode = {
      id: "branch-main",
      label: "main",
      type: "branch-item",
      contextValue: "branch-item-main",
      branchData: {
        branchId: "",
        branchName: "main",
        isMain: true,
        status: "up-to-date",
        opsCount: 0,
        lastSynced: mainLastSynced
      }
    };

    // Only add main if it's not the current branch
    if (!currentBranchId || currentBranchId === "") {
      // If no branch selected, don't show main as a separate item
    } else {
      children.push(mainBranchNode);
    }

    // 3. All other branches (directly, no grouping)
    const otherBranches = allBranches.filter(b => b.id !== currentBranchId);

    otherBranches.forEach((branch, index) => {
      // TODO: Get actual last synced time from LSP
      const lastSynced = new Date(Date.now() - (15 + index * 5) * 60 * 1000); // Mock: varying times

      children.push({
        id: `branch-${branch.id}`,
        label: branch.name,
        type: "branch-item",
        contextValue: "branch-item",
        branchData: {
          branchId: branch.id,
          branchName: branch.name,
          isCurrent: false,
          status: "up-to-date", // TODO: Calculate actual status
          opsCount: 0,
          lastSynced: lastSynced
        }
      });
    });

    // 4. Add "Manage All" action node at the bottom
    children.push({
      id: "branch-manage-all",
      label: "Manage All Branches",
      type: "branch-action",
      contextValue: "branch-manage-all"
    });

    return children;
  }

  private async getPendingChanges(): Promise<BranchNode[]> {
    try {
      const requestParams = {
        limit: 20,
        branchFilter: 'current',
        sinceDate: undefined,
      };

      const ops = await this.client.sendRequest<PendingOpResponse[]>(
        'dark/getPendingOps',
        requestParams
      );

      let nodes: PendingOpNode[] = ops
        .filter((opWithLabel) => {
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
        .map((opWithLabel, index) => {
          const location = this.extractLocationFromOp(opWithLabel.op);
          // Use the op data itself as a stable ID (not index-based)
          // This ensures IDs don't change when ops are reordered
          const opId = opWithLabel.op;

          // Clean up label - remove "Set*Name →" prefix text, keep only the fqname
          let cleanLabel = opWithLabel.label;
          cleanLabel = cleanLabel.replace(/^SetFnName\s+→\s+/, '');
          cleanLabel = cleanLabel.replace(/^SetTypeName\s+→\s+/, '');
          cleanLabel = cleanLabel.replace(/^SetValueName\s+→\s+/, '');

          return {
            id: opId,
            label: cleanLabel,
            type: "pending-op",
            contextValue: "pending-op",
            location: location,  // Store location for the command
            opData: opWithLabel.op,  // Store full op for diff view
            checked: this.checkedOps.get(opId) || false
          };
        });

      // Store current op IDs for select/deselect all
      this.currentOpIds = nodes.map(n => n.id);

      // Add "see more" node if we hit the limit
      const hasMore = ops.length === 20;

      // Only group by owner when on a non-main branch
      const currentBranchId = this.branchStateManager.getCurrentBranchId();
      const shouldGroupByOwner = currentBranchId !== null && currentBranchId !== '';

      let result: BranchNode[];
      if (shouldGroupByOwner) {
        result = this.groupByOwner(nodes);
      } else {
        result = nodes;
      }

      // Add "see more" node at the end if there are more items
      if (hasMore) {
        const seeMoreNode: BranchNode = {
          id: 'see-more-changes',
          label: 'See more...',
          type: 'section',
          contextValue: 'see-more',
        };
        result.push(seeMoreNode);
      }

      return result;
    } catch (error) {
      console.error('Failed to get pending ops:', error);
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
        type: "owner-group",
        contextValue: "owner-group",
        owner: owner,
        children: opsInGroup
      });
    }

    // Sort by owner name
    return ownerGroups.sort((a, b) => a.owner.localeCompare(b.owner));
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

  private getPendingChangesLabel(): string {
    return 'Changes';
  }

  private getOpsFilterTooltip(): string {
    return 'Pending changes from the current branch';
  }

}
