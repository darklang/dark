import * as vscode from "vscode";
import { StatusBarManager } from "../ui/statusbar/statusBarManager";
import { WorkspaceTreeDataProvider } from "../providers/treeviews/workspaceTreeDataProvider";
import { BranchStateManager, DemoBranchId } from "../data/branchStateManager";

// TODO: Rename file to branchCommands.ts
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
        const branches = [
          { label: "refactor-db-layer", id: DemoBranchId.RefactorDbLayer, description: "● Current" },
          { label: "feature-filtermap", id: DemoBranchId.FeatureFilterMap },
          { label: "default", id: DemoBranchId.Default },
        ];

        // Add separator and "Manage All Branches" at the end
        const items: vscode.QuickPickItem[] = [
          ...branches,
          { label: "", kind: vscode.QuickPickItemKind.Separator },
          { label: "📋 Manage All Branches...", alwaysShow: true }
        ];

        const selected = await vscode.window.showQuickPick(items, {
          placeHolder: "Switch to branch",
          matchOnDescription: true
        });

        if (selected) {
          if (selected.label === "📋 Manage All Branches...") {
            vscode.commands.executeCommand("darklang.branches.manageAll");
          } else {
            const branch = branches.find(b => b.label === selected.label);
            if (branch) {
              this.branchStateManager.switchBranch(branch.id);
              this.statusBarManager.updateBranch(branch.label);
              vscode.window.showInformationMessage(`Switched to branch: ${branch.label}`);
            }
          }
        }
      }),

      vscode.commands.registerCommand("darklang.branch.new", () => {
        vscode.window.showInputBox({ prompt: "Enter branch name/intent" }).then(name => {
          if (name) {
            this.statusBarManager.updateBranch(name);
            vscode.window.showInformationMessage(`Created new branch: ${name}`);
          }
        });
      }),

      vscode.commands.registerCommand("darklang.branch.switch", (branch) => {
        console.log('🔀 Branch switch command called with:', branch);

        // Handle both {label: ...} and {id: ...} formats
        const branchId = branch?.id || branch?.label || "default";
        const branchLabel = branch?.label || branchId;

        console.log('  branchId:', branchId, 'branchLabel:', branchLabel);

        // Map branch IDs to demo branches
        let demoBranchId = DemoBranchId.Default;
        if (branchId.includes("feature-filtermap") || branchId === "feature-filtermap") {
          demoBranchId = DemoBranchId.FeatureFilterMap;
        } else if (branchId.includes("refactor-db-layer") || branchId === "refactor-db-layer") {
          demoBranchId = DemoBranchId.RefactorDbLayer;
        }

        console.log('  Switching to branch:', demoBranchId);

        // Switch to the branch, which will trigger the tree refresh
        this.branchStateManager.switchBranch(demoBranchId);
        this.statusBarManager.updateBranch(branchLabel);
        vscode.window.showInformationMessage(`Switched to branch: ${branchLabel}`);
      }),

      vscode.commands.registerCommand("darklang.branch.view", (branch) => {
        const branchId = branch?.id || "current";
        const branchLabel = branch?.label || "Current Branch";

        // Clean URL - the central system handles title, badge, etc.
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
        const currentName = this.branchStateManager.getBranchName();
        const newName = await vscode.window.showInputBox({
          prompt: "Enter new branch name",
          value: currentName,
          placeHolder: "e.g., helpful-owl-42: Add user validation"
        });

        if (newName && newName !== currentName) {
          this.branchStateManager.setBranchName(newName);
          this.statusBarManager.updateBranch(newName);
          vscode.window.showInformationMessage(`Branch renamed to: ${newName}`);
        }
      })
    ];
  }
}