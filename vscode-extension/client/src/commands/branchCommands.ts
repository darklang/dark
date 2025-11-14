import * as vscode from "vscode";
import { StatusBarManager } from "../ui/statusbar/statusBarManager";
import { WorkspaceTreeDataProvider } from "../providers/treeviews/workspaceTreeDataProvider";
import { BranchStateManager } from "../data/branchStateManager";

export class BranchCommands {
  private branchStateManager = BranchStateManager.getInstance();

  constructor(
    private statusBarManager: StatusBarManager,
    private workspaceProvider: WorkspaceTreeDataProvider
  ) {}

  register(): vscode.Disposable[] {
    return [
      // Quick branch switcher (for clicking Branch node)
      vscode.commands.registerCommand("darklang.branch.quickSwitch", async () => {
        const branches = this.branchStateManager.getBranches();
        const currentBranchId = this.branchStateManager.getCurrentBranchId();

        interface BranchQuickPickItem extends vscode.QuickPickItem {
          id?: string;
          action?: "manageAll" | "clear";
        }

        const activeBranches: BranchQuickPickItem[] = branches
          .filter(b => !b.mergedAt)
          .map(b => ({
            label: b.name,
            id: b.id,
            description: b.id === currentBranchId ? "● Current" : undefined
          }));

        // Add separator and options at the end
        const items: BranchQuickPickItem[] = [
          ...activeBranches,
          { label: "", kind: vscode.QuickPickItemKind.Separator },
          { label: "🚫 Clear Branch Selection", alwaysShow: true, action: "clear" },
          { label: "📋 Manage All Branches...", alwaysShow: true, action: "manageAll" }
        ];

        const selected = await vscode.window.showQuickPick(items, {
          placeHolder: "Switch to branch",
          matchOnDescription: true
        });

        if (selected) {
          if (selected.action === "manageAll") {
            vscode.commands.executeCommand("darklang.branches.manageAll");
          } else if (selected.action === "clear") {
            await this.branchStateManager.clearCurrentBranch();
            this.statusBarManager.updateBranch("No Branch");
            vscode.window.showInformationMessage("Branch selection cleared - viewing all branches");
          } else if (selected.id) {
            this.branchStateManager.setCurrentBranchById(selected.id);
            this.statusBarManager.updateBranch(selected.label);
            vscode.window.showInformationMessage(`Switched to branch: ${selected.label}`);
          }
        }
      }),

      vscode.commands.registerCommand("darklang.branch.new", async () => {
        const name = await vscode.window.showInputBox({
          prompt: "Enter branch name",
          placeHolder: "e.g., feature-user-auth"
        });

        if (name) {
          const newBranch = await this.branchStateManager.createBranch(name);
          if (newBranch) {
            this.statusBarManager.updateBranch(name);
            this.workspaceProvider.refresh();
            vscode.window.showInformationMessage(`Created and switched to branch: ${name}`);
          } else {
            vscode.window.showErrorMessage(`Failed to create branch: ${name}`);
          }
        }
      }),

      vscode.commands.registerCommand("darklang.branch.switch", (branchIdOrNode) => {
        console.log('🔀 Branch switch command called with:', branchIdOrNode);

        // Handle multiple formats:
        // 1. Direct branch ID string (from command argument in getTreeItem)
        // 2. Branch node with branchData
        // 3. Legacy format with id/label properties
        let branchID: string | undefined;
        let branchLabel: string;

        // Check type first - string takes priority (most common case from tree item command)
        if (typeof branchIdOrNode === 'string' || branchIdOrNode === '') {
          // Direct branch ID (could be empty string for main)
          branchID = branchIdOrNode;
          if (branchID === '' || !branchID) {
            branchLabel = "main";
          } else {
            const branch = this.branchStateManager.getBranches().find(b => b.id === branchID);
            branchLabel = branch?.name || "Unknown";
          }
        } else if (branchIdOrNode?.branchData) {
          // New format with branchData
          branchID = branchIdOrNode.branchData.branchId;
          branchLabel = branchIdOrNode.branchData.branchName;
        } else {
          // Legacy format
          branchID = branchIdOrNode?.id;
          branchLabel = branchIdOrNode?.label || branchIdOrNode?.name || branchIdOrNode?.title || "Unknown";
        }

        console.log('  branchID:', branchID, 'branchLabel:', branchLabel);

        if (branchID !== undefined) {
          this.branchStateManager.setCurrentBranchById(branchID);
          this.statusBarManager.updateBranch(branchLabel);
          this.workspaceProvider.refresh();
          vscode.window.showInformationMessage(`Switched to branch: ${branchLabel}`);
        } else {
          console.error('  ERROR: No branchID provided to switch command!');
        }
      }),

      vscode.commands.registerCommand("darklang.branch.view", (branch) => {
        const branchID = branch?.id || this.branchStateManager.getCurrentBranchId();
        const branchLabel = branch?.label || branch?.name || this.branchStateManager.getCurrentBranchName();

        if (!branchID) {
          vscode.window.showErrorMessage("No branch is currently selected");
          return;
        }

        const virtualUri = vscode.Uri.parse(`dark:///branch/${branchID}?label=${encodeURIComponent(branchLabel)}`);
        vscode.workspace.openTextDocument(virtualUri).then(doc => {
          vscode.window.showTextDocument(doc, {
            preview: false,
            preserveFocus: false
          });
          vscode.window.showInformationMessage(`Viewing branch: ${branchLabel}`);
        });
      }),

      vscode.commands.registerCommand("darklang.branch.rename", async () => {
        const currentName = this.branchStateManager.getCurrentBranchName();
        const newName = await vscode.window.showInputBox({
          prompt: "Enter new branch name",
          value: currentName,
          placeHolder: "e.g., helpful-owl-42: Add user validation"
        });

        if (newName && newName !== currentName) {
          // TODO: Implement actual branch rename API call
          this.statusBarManager.updateBranch(newName);
          vscode.window.showInformationMessage(`Branch rename not yet implemented`);
        }
      }),

      vscode.commands.registerCommand("darklang.branch.clear", async () => {
        await this.branchStateManager.clearCurrentBranch();
        this.statusBarManager.updateBranch("No Branch");
        this.workspaceProvider.refresh();
        vscode.window.showInformationMessage("Branch selection cleared - viewing all branches");
      }),

      vscode.commands.registerCommand("darklang.branch.sync", async (branchNode) => {
        // Extract branch ID from node
        const branchId = branchNode?.branchData?.branchId || branchNode?.id;
        const branchName = branchNode?.branchData?.branchName || branchNode?.label || "branch";

        // For now, just show a placeholder message
        vscode.window.showInformationMessage(`Sync functionality for ${branchName} not yet implemented`);

        // TODO: Implement actual sync logic
        // - Fetch ops from remote
        // - Compare with local
        // - Show sync status
      }),

      vscode.commands.registerCommand("darklang.branch.delete", async (branchNode) => {
        // Extract branch info from node
        const branchId = branchNode?.branchData?.branchId;
        const branchName = branchNode?.branchData?.branchName || branchNode?.label || "branch";

        if (!branchId) {
          vscode.window.showErrorMessage("Cannot delete: Invalid branch");
          return;
        }

        // Confirm deletion
        const confirm = await vscode.window.showWarningMessage(
          `Are you sure you want to delete branch "${branchName}"?`,
          { modal: true },
          "Delete"
        );

        if (confirm === "Delete") {
          // For now, just show a placeholder message
          vscode.window.showInformationMessage(`Delete functionality for ${branchName} not yet implemented`);

          // TODO: Implement actual delete logic
          // - Call LSP to delete branch
          // - Refresh branch list
          // - Switch to main if current branch was deleted
        }
      }),

      vscode.commands.registerCommand("darklang.branch.showMenu", async (branchNode) => {
        // Extract branch info
        const branchId = branchNode?.branchData?.branchId;
        const branchName = branchNode?.branchData?.branchName || branchNode?.label || "branch";
        const isMain = branchNode?.branchData?.isMain || false;

        interface MenuQuickPickItem extends vscode.QuickPickItem {
          action: "rename" | "delete";
        }

        const items: MenuQuickPickItem[] = [
          {
            label: "$(edit) Rename",
            action: "rename"
          }
        ];

        // Only show delete for non-main branches
        if (!isMain) {
          items.push({
            label: "$(trash) Delete",
            action: "delete"
          });
        }

        const selected = await vscode.window.showQuickPick(items, {
          placeHolder: `Actions for ${branchName}`
        });

        if (selected) {
          if (selected.action === "rename") {
            vscode.commands.executeCommand("darklang.branch.rename", branchNode);
          } else if (selected.action === "delete") {
            vscode.commands.executeCommand("darklang.branch.delete", branchNode);
          }
        }
      })
    ];
  }
}