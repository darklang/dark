import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchNode } from "../../types";
import { BranchStateManager } from "../../data/branchStateManager";
import { InstanceDemoData } from "../../data/demo/instanceDemoData";

interface OpsFilterConfig {
  limit: number;
  branch: 'current' | 'all' | string; // 'current', 'all', or a specific branch ID
  dateRange: 'all' | 'today' | 'week' | 'month' | 'custom';
  customStartDate?: Date;
  locationFilter?: string;
}

export class WorkspaceTreeDataProvider implements vscode.TreeDataProvider<BranchNode> {
  private _onDidChangeTreeData: vscode.EventEmitter<BranchNode | undefined | null | void> =
    new vscode.EventEmitter<BranchNode | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<BranchNode | undefined | null | void> =
    this._onDidChangeTreeData.event;

  private branchStateManager = BranchStateManager.getInstance();
  private client: LanguageClient | null = null;

  // Filter configuration for ops display
  private opsFilter: OpsFilterConfig = {
    limit: 50,
    branch: 'current',
    dateRange: 'all',
  };

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
      // Calculate date filter if applicable
      let sinceDate: string | undefined;
      if (this.opsFilter.dateRange !== 'all') {
        const now = new Date();
        const since = new Date();

        switch (this.opsFilter.dateRange) {
          case 'today':
            since.setHours(0, 0, 0, 0);
            break;
          case 'week':
            since.setDate(now.getDate() - 7);
            break;
          case 'month':
            since.setMonth(now.getMonth() - 1);
            break;
          case 'custom':
            if (this.opsFilter.customStartDate) {
              since.setTime(this.opsFilter.customStartDate.getTime());
            }
            break;
        }

        sinceDate = since.toISOString();
      }

      const requestParams = {
        limit: this.opsFilter.limit,
        branchFilter: this.opsFilter.branch,
        sinceDate: sinceDate,
      };
      console.log('Sending getPendingOps request with params:', requestParams);

      const ops = await this.client.sendRequest<Array<{op: string, label: string}>>(
        'darklang/getPendingOps',
        requestParams
      );

      console.log('Received ops from database:', ops.length);
      console.log('Current filter config:', JSON.stringify(this.opsFilter));

      let nodes = ops.map((opWithLabel, index) => {
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

      // Apply client-side location filter if specified
      if (this.opsFilter.locationFilter && this.opsFilter.locationFilter.trim() !== '') {
        const filterLower = this.opsFilter.locationFilter.toLowerCase();
        console.log('Applying location filter:', filterLower);
        nodes = nodes.filter(node => {
          // Filter by label
          if (node.label.toLowerCase().includes(filterLower)) {
            return true;
          }
          // Filter by location if available
          if (node.location) {
            const locStr = `${node.location.owner}.${node.location.modules.join('.')}.${node.location.name}`.toLowerCase();
            return locStr.includes(filterLower);
          }
          // If no location available, don't match (only label matching works)
          return false;
        });
        console.log('After location filter:', nodes.length);
      }

      console.log('Returning nodes:', nodes.length);
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
        label: this.getPendingChangesLabel(),
        type: "changes-root" as any,
        contextValue: "workspace-changes-root",
        children: [] // Will be populated async
      }
    ];
  }

  // Filter configuration methods
  private getPendingChangesLabel(): string {
    const filters: string[] = [];

    // Show limit if not default
    if (this.opsFilter.limit !== 50) {
      if (this.opsFilter.limit >= 999999) {
        filters.push('all');
      } else {
        filters.push(`${this.opsFilter.limit}`);
      }
    }

    // Show date range if not all
    if (this.opsFilter.dateRange !== 'all') {
      filters.push(this.opsFilter.dateRange);
    }

    // Show branch filter
    if (this.opsFilter.branch === 'all') {
      filters.push('all branches');
    } else if (this.opsFilter.branch !== 'current') {
      // It's a specific branch - show the branch name
      const branchLabel = this.getBranchFilterLabel();
      filters.push(branchLabel);
    }

    // Show location filter
    if (this.opsFilter.locationFilter) {
      filters.push(`"${this.opsFilter.locationFilter}"`);
    }

    if (filters.length > 0) {
      return `Pending Changes (${filters.join(', ')})`;
    }
    return 'Pending Changes';
  }

  private getOpsFilterTooltip(): string {
    const parts: string[] = [];

    parts.push(`Showing up to ${this.opsFilter.limit} ops`);

    if (this.opsFilter.dateRange !== 'all') {
      const dateLabel = this.opsFilter.dateRange === 'custom'
        ? `since ${this.opsFilter.customStartDate?.toLocaleDateString()}`
        : `from ${this.opsFilter.dateRange}`;
      parts.push(dateLabel);
    }

    // Show branch filter information
    if (this.opsFilter.branch === 'current') {
      parts.push('current branch only');
    } else if (this.opsFilter.branch === 'all') {
      parts.push('all branches');
    } else {
      // It's a specific branch
      const branchLabel = this.getBranchFilterLabel();
      parts.push(`branch: ${branchLabel}`);
    }

    if (this.opsFilter.locationFilter) {
      parts.push(`filtered by "${this.opsFilter.locationFilter}"`);
    }

    return parts.join(', ');
  }

  async configureLimitFilter(): Promise<void> {
    console.log('Setting limit filter, current:', this.opsFilter.limit);
    const choice = await vscode.window.showQuickPick([
      { label: '10', value: 10 },
      { label: '25', value: 25 },
      { label: '50', value: 50 },
      { label: '100', value: 100 },
      { label: '500', value: 500 },
      { label: '1000', value: 1000 },
      { label: 'All (no limit)', value: 999999 },
    ], {
      placeHolder: `Current limit: ${this.opsFilter.limit}`
    });

    if (choice) {
      this.opsFilter.limit = choice.value;
      console.log('Limit changed to:', choice.value);
      vscode.window.showInformationMessage(`Ops limit set to ${choice.label}`);
      this.refresh();
    }
  }

  async configureDateFilter(): Promise<void> {
    const choice = await vscode.window.showQuickPick([
      { label: 'All time', value: 'all' },
      { label: 'Today', value: 'today' },
      { label: 'Last 7 days', value: 'week' },
      { label: 'Last 30 days', value: 'month' },
      { label: 'Custom date...', value: 'custom' },
    ], {
      placeHolder: `Current: ${this.opsFilter.dateRange}`
    });

    if (!choice) {
      return;
    }

    this.opsFilter.dateRange = choice.value as any;

    if (choice.value === 'custom') {
      const dateStr = await vscode.window.showInputBox({
        prompt: 'Enter start date (YYYY-MM-DD)',
        placeHolder: '2024-01-01'
      });

      if (dateStr) {
        const date = new Date(dateStr);
        if (!isNaN(date.getTime())) {
          this.opsFilter.customStartDate = date;
          vscode.window.showInformationMessage(`Filtering ops since ${dateStr}`);
        } else {
          vscode.window.showErrorMessage('Invalid date format');
          return;
        }
      }
    } else {
      vscode.window.showInformationMessage(`Date filter set to: ${choice.label}`);
    }

    this.refresh();
  }

  private getBranchFilterLabel(): string {
    if (this.opsFilter.branch === 'all') {
      return 'All branches';
    } else if (this.opsFilter.branch === 'current') {
      return 'Current branch';
    } else {
      // It's a specific branch ID - find the branch name
      const branch = this.branchStateManager.getBranches().find(b => b.id === this.opsFilter.branch);
      return branch ? branch.name : 'Unknown branch';
    }
  }

  async configureBranchFilter(): Promise<void> {
    const branches = this.branchStateManager.getBranches();
    const currentBranchId = this.branchStateManager.getCurrentBranchId();

    // Build the list of branch options
    const branchItems = [
      {
        label: '$(git-branch) Current branch only',
        value: 'current',
        description: currentBranchId ? this.branchStateManager.getCurrentBranchName() : 'No branch selected'
      },
      { label: '$(layers) All branches', value: 'all', description: 'Show ops from all branches' },
      { label: '', kind: vscode.QuickPickItemKind.Separator }
    ] as vscode.QuickPickItem[];

    // Add individual branches (only show non-merged branches)
    branches
      .filter(b => !b.mergedAt)
      .forEach(b => {
        const isCurrent = b.id === currentBranchId;
        branchItems.push({
          label: b.name,
          description: isCurrent ? '● Current' : undefined,
          detail: `Branch ID: ${b.id}`,
          // Store the branch ID as a custom property
          ...({ branchId: b.id } as any)
        });
      });

    const choice = await vscode.window.showQuickPick(branchItems, {
      placeHolder: `Current filter: ${this.getBranchFilterLabel()}`
    });

    if (choice) {
      const branchId = (choice as any).branchId;
      if (branchId) {
        this.opsFilter.branch = branchId;
        vscode.window.showInformationMessage(`Branch filter set to: ${choice.label}`);
      } else if ((choice as any).value) {
        this.opsFilter.branch = (choice as any).value;
        vscode.window.showInformationMessage(`Branch filter set to: ${choice.label}`);
      }
      this.refresh();
    }
  }

  async configureLocationFilter(): Promise<void> {
    const input = await vscode.window.showInputBox({
      prompt: 'Filter by module/function name (case-insensitive)',
      placeHolder: 'e.g., Stdlib.List or MyModule',
      value: this.opsFilter.locationFilter || ''
    });

    if (input !== undefined) {
      this.opsFilter.locationFilter = input.trim() || undefined;
      if (this.opsFilter.locationFilter) {
        vscode.window.showInformationMessage(`Filtering by location: ${this.opsFilter.locationFilter}`);
      } else {
        vscode.window.showInformationMessage('Location filter cleared');
      }
      this.refresh();
    }
  }

  async clearAllFilters(): Promise<void> {
    this.opsFilter = {
      limit: 50,
      branch: 'current',
      dateRange: 'all',
    };
    vscode.window.showInformationMessage('All filters cleared');
    this.refresh();
  }
}
