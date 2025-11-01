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

      vscode.commands.registerCommand("darklang.branch.switch", (branch) => {
        console.log('🔀 Branch switch command called with:', branch);

        // Handle both {label: ...} and {id: ...} formats
        const branchID = branch?.id;
        // CLEANUP: do we need branch?.label || branch?.name
        const branchLabel = branch?.label || branch?.name || branch?.title || "Unknown";

        console.log('  branchID:', branchID, 'branchLabel:', branchLabel);

        if (branchID) {
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
      })
    ];
  }
}