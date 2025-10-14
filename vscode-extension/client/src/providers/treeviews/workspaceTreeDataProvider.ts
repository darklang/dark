import * as vscode from "vscode";
import { BranchNode } from "../../types";
import { BranchStateManager, DemoBranchId } from "../../data/branchStateManager";
import { InstanceDemoData } from "../../data/demo/instanceDemoData";
import { buildPackageTree, change } from "../../utils/patchTreeBuilder";

export class WorkspaceTreeDataProvider implements vscode.TreeDataProvider<BranchNode> {
  private _onDidChangeTreeData: vscode.EventEmitter<BranchNode | undefined | null | void> =
    new vscode.EventEmitter<BranchNode | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<BranchNode | undefined | null | void> =
    this._onDidChangeTreeData.event;

  private branchStateManager = BranchStateManager.getInstance();

  constructor() {
    console.log('🌳 WorkspaceTreeDataProvider constructor called');

    // Listen for branch changes and refresh the tree
    this.branchStateManager.onBranchChanged(() => {
      console.log('🔔 WorkspaceTreeDataProvider: Branch changed event received');
      this.refresh();
    });
  }

  refresh(): void {
    console.log('🔄 WorkspaceTreeDataProvider: refresh() called');
    this._onDidChangeTreeData.fire(undefined);
  }

  refreshItem(item: BranchNode): void {
    this._onDidChangeTreeData.fire(item);
  }

  getTreeItem(element: BranchNode): vscode.TreeItem {
    const item = new vscode.TreeItem(
      element.label,
      element.children && element.children.length > 0
        ? (element.type === "instance-root" ? vscode.TreeItemCollapsibleState.Collapsed :
           element.type === "patches-root" ? vscode.TreeItemCollapsibleState.Expanded :
           vscode.TreeItemCollapsibleState.Collapsed)
        : vscode.TreeItemCollapsibleState.None
    );

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

    // Handle Patches root node
    if (element.type === "patches-root") {
      item.iconPath = new vscode.ThemeIcon("files");
      item.tooltip = "Patches in current branch";
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

    // Handle patches (reuse existing patch styling logic)
    if (element.contextValue === 'patch-quick-edits') {
      item.description = "●";
      item.tooltip = "Auto-created patch for quick edits (focused). Organize later if needed.";
      item.iconPath = new vscode.ThemeIcon("git-branch", new vscode.ThemeColor("charts.blue"));
      item.command = {
        command: "darklang.patch.view",
        title: "View Patch",
        arguments: [element]
      };
    } else if (element.contextValue === 'patch-local') {
      if (element.patchData?.isFocused) {
        item.description = "●";
      }
      item.tooltip = "Local patch (not yet pushed)";
      item.iconPath = new vscode.ThemeIcon("git-branch", new vscode.ThemeColor("charts.blue"));
      item.command = {
        command: "darklang.patch.view",
        title: "View Patch",
        arguments: [element]
      };
    } else if (element.contextValue === 'patch-shared') {
      item.description = element.patchData?.isFocused ? "● 👥" : "👥";
      item.tooltip = `Shared across ${element.patchData?.referenceCount || 0} branches`;
      item.iconPath = new vscode.ThemeIcon("git-branch", new vscode.ThemeColor("charts.purple"));
      item.command = {
        command: "darklang.patch.view",
        title: "View Patch",
        arguments: [element]
      };
    } else if (element.contextValue === 'patch-remote') {
      if (element.patchData?.isFocused) {
        item.description = "● 🌐";
      } else {
        item.description = "🌐";
      }
      item.tooltip = "Pushed to remote, not yet merged";
      item.iconPath = new vscode.ThemeIcon("cloud-upload", new vscode.ThemeColor("charts.orange"));
      item.command = {
        command: "darklang.patch.view",
        title: "View Patch",
        arguments: [element]
      };
    } else if (element.contextValue === 'patch-merged') {
      const mergedWhere = element.patchData?.isMergedUpstream ? "upstream" : "local";
      item.description = `✓ ${mergedWhere}`;
      item.tooltip = `Merged ${mergedWhere}`;
      item.iconPath = new vscode.ThemeIcon("check", new vscode.ThemeColor("charts.green"));
      item.resourceUri = vscode.Uri.parse(`dark://patch/${element.id}?merged=true`);
    }

    // Handle new package-centric patch tree nodes
    if (element.type === "patch-namespace") {
      item.iconPath = new vscode.ThemeIcon("symbol-namespace", new vscode.ThemeColor("charts.blue"));
      item.tooltip = `Namespace: ${element.label}`;
      return item;
    }

    if (element.type === "patch-module") {
      const count = element.changeCount || 0;
      const modulePath = element.changeData?.fullPath || "";
      item.iconPath = new vscode.ThemeIcon("symbol-module", new vscode.ThemeColor("charts.purple"));
      item.tooltip = `Module with ${count} change${count !== 1 ? 's' : ''} - Click to view package`;

      // Add command to open package view
      item.command = {
        command: "darklang.patch.viewPackage",
        title: "View Package",
        arguments: [modulePath]
      };

      // Collapse modules with multiple changes by default
      if (count > 1 && item.collapsibleState === vscode.TreeItemCollapsibleState.None) {
        item.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
      }
      return item;
    }

    if (element.type === "patch-change") {
      const changeType = element.changeData?.changeType || "modify";
      const fullPath = element.changeData?.fullPath || "";

      // Set icon based on change type
      switch (changeType) {
        case "add":
          item.iconPath = new vscode.ThemeIcon("add", new vscode.ThemeColor("charts.green"));
          item.tooltip = `Added: ${fullPath}`;
          break;
        case "modify":
          item.iconPath = new vscode.ThemeIcon("edit", new vscode.ThemeColor("charts.yellow"));
          item.tooltip = `Modified: ${fullPath}`;
          break;
        case "delete":
          item.iconPath = new vscode.ThemeIcon("trash", new vscode.ThemeColor("charts.red"));
          item.tooltip = `Deleted: ${fullPath}`;
          break;
        case "rename":
          item.iconPath = new vscode.ThemeIcon("symbol-field", new vscode.ThemeColor("charts.blue"));
          item.tooltip = `Renamed: ${fullPath}`;
          break;
      }

      // Add command to view/edit the change
      item.command = {
        command: "darklang.patch.viewChange",
        title: "View Change",
        arguments: [element]
      };

      return item;
    }

    return item;
  }

  getChildren(element?: BranchNode): Thenable<BranchNode[]> {
    if (!element) {
      // Root level: Instance, Branch, Patches
      return Promise.resolve(this.getRootNodes());
    }

    // Return children of the element
    return Promise.resolve(element.children || []);
  }

  private getRootNodes(): BranchNode[] {
    const branchState = this.branchStateManager.getCurrentBranchState();
    const branchName = branchState.statusBar.branch.name;

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

    // Get patches for the Patches node
    const patches = this.getBranchPatches();

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
        id: "workspace-branch",
        label: `Branch: ${branchName}`,
        type: "branch-root" as any,
        contextValue: "workspace-branch-root",
        children: []
      },
      // Patches node (expanded by default)
      {
        id: "workspace-patches",
        label: `Patches (${patches.length})`,
        type: "patches-root" as any,
        contextValue: "workspace-patches-root",
        children: patches
      }
    ];
  }

  private getBranchPatches(): BranchNode[] {
    // Get patches from branch state
    const currentBranch = this.branchStateManager.currentBranch;

    switch (currentBranch) {
      case DemoBranchId.Default:
        // Default branch - no patches
        return [];

      case DemoBranchId.FeatureFilterMap:
        return [
          {
            id: "patch-quick-edits-filtermap",
            label: "Quick edits",
            type: "patch",
            contextValue: "patch-quick-edits",
            patchData: { isFocused: true },
            children: buildPackageTree("patch-quick-edits-filtermap", [
              change("Darklang.Stdlib.List.map", "modify")
            ])
          },
          {
            id: "patch-filtermap",
            label: "Add List.filterMap function",
            type: "patch",
            contextValue: "patch-local",
            children: buildPackageTree("patch-filtermap", [
              change("Darklang.Stdlib.List.filterMap", "add"),
              change("Darklang.Stdlib.List.map", "modify")
            ])
          }
        ];

      case DemoBranchId.RefactorDbLayer:
        return [
          {
            id: "patch-quick-edits-db",
            label: "Quick edits",
            type: "patch",
            contextValue: "patch-quick-edits",
            patchData: { isFocused: true },
            children: buildPackageTree("patch-quick-edits-db", [
              change("MyApp.Database.query", "modify"),
              change("MyApp.Database.connect", "modify")
            ])
          },
          {
            id: "patch-db",
            label: "Database Layer: Connection pooling",
            type: "patch",
            contextValue: "patch-local",
            children: buildPackageTree("patch-db", [
              change("MyApp.Database.ConnectionPool", "add"),
              change("MyApp.Database.connect", "modify"),
              change("MyApp.Database.query", "modify")
            ])
          },
          {
            id: "patch-shared-auth",
            label: "Shared auth updates",
            type: "patch",
            contextValue: "patch-shared",
            patchData: { referenceCount: 3 },
            children: buildPackageTree("patch-shared-auth", [
              change("MyApp.Auth.validateToken", "modify"),
              change("MyApp.Auth.refreshToken", "add")
            ])
          },
          {
            id: "patch-remote-stdlib",
            label: "Stdlib improvements (Bob)",
            type: "patch",
            contextValue: "patch-remote",
            children: buildPackageTree("patch-remote-stdlib", [
              change("Darklang.Stdlib.List.head", "modify"),
              change("Darklang.Stdlib.List.tail", "modify"),
              change("Darklang.Stdlib.List.findFirst", "add")
            ])
          },
          {
            id: "patch-merged-base",
            label: "Base DB schema",
            type: "patch",
            contextValue: "patch-merged",
            patchData: { isMergedUpstream: true },
            children: buildPackageTree("patch-merged-base", [
              change("MyApp.Database.init", "add")
            ])
          }
        ];

      default:
        return [];
    }
  }
}
