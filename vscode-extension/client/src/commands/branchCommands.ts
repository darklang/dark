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

        const activeBranches = branches
          .filter(b => !b.mergedAt)
          .map(b => ({
            label: b.name,
            id: b.id,
            description: b.id === currentBranchId ? "● Current" : undefined
          }));

        // Add separator and options at the end
        const items: vscode.QuickPickItem[] = [
          ...activeBranches,
          { label: "", kind: vscode.QuickPickItemKind.Separator },
          { label: "🚫 Clear Branch Selection", alwaysShow: true },
          { label: "📋 Manage All Branches...", alwaysShow: true }
        ];

        const selected = await vscode.window.showQuickPick(items, {
          placeHolder: "Switch to branch",
          matchOnDescription: true
        });

        if (selected) {
          if (selected.label === "📋 Manage All Branches...") {
            vscode.commands.executeCommand("darklang.branches.manageAll");
          } else if (selected.label === "🚫 Clear Branch Selection") {
            await this.branchStateManager.clearCurrentBranch();
            this.statusBarManager.updateBranch("No Branch");
            vscode.window.showInformationMessage("Branch selection cleared - viewing all branches");
          } else {
            const branch = activeBranches.find(b => b.label === selected.label);
            if (branch) {
              this.branchStateManager.setCurrentBranchById(branch.id);
              this.statusBarManager.updateBranch(branch.label);
              vscode.window.showInformationMessage(`Switched to branch: ${branch.label}`);
            }
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
        const branchId = branch?.id;
        const branchLabel = branch?.label || branch?.name || "Unknown";

        console.log('  branchId:', branchId, 'branchLabel:', branchLabel);

        if (branchId) {
          this.branchStateManager.setCurrentBranchById(branchId);
          this.statusBarManager.updateBranch(branchLabel);
          vscode.window.showInformationMessage(`Switched to branch: ${branchLabel}`);
        }
      }),

      vscode.commands.registerCommand("darklang.branch.view", (branch) => {
        const branchId = branch?.id || this.branchStateManager.getCurrentBranchId();
        const branchLabel = branch?.label || branch?.name || this.branchStateManager.getCurrentBranchName();

        if (!branchId) {
          vscode.window.showErrorMessage("No branch is currently selected");
          return;
        }

        const virtualUri = vscode.Uri.parse(`dark:///branch/${branchId}?label=${encodeURIComponent(branchLabel)}`);
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