import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { ScmTreeDataProvider } from "../providers/treeviews/scmTreeDataProvider";
import { BranchInfo } from "../ui/statusbar/statusBar";

export class BranchCommands {
  private client: LanguageClient;
  private scmProvider: ScmTreeDataProvider;

  constructor(client: LanguageClient, scmProvider: ScmTreeDataProvider) {
    this.client = client;
    this.scmProvider = scmProvider;
  }

  register(): vscode.Disposable[] {
    return [
      vscode.commands.registerCommand("darklang.branch.switch", async () => {
        try {
          const branches = await this.client.sendRequest<BranchInfo[]>("dark/getBranches", {});
          const items = branches.map(b => ({
            label: b.name,
            description: b.isActive ? "(current)" : "",
            branchId: b.id,
          }));

          const selected = await vscode.window.showQuickPick(items, {
            placeHolder: "Select a branch to switch to",
          });

          if (selected && !branches.find(b => b.id === selected.branchId)?.isActive) {
            await this.client.sendRequest("dark/switchBranch", { branchId: selected.branchId });
          }
        } catch (error) {
          vscode.window.showErrorMessage(`Failed to switch branch: ${error}`);
        }
      }),

      vscode.commands.registerCommand("darklang.branch.create", async () => {
        const name = await vscode.window.showInputBox({
          prompt: "Branch name",
          placeHolder: "Enter new branch name",
        });
        if (name) {
          try {
            await this.client.sendRequest("dark/createBranch", { name });
            vscode.window.showInformationMessage(`Created and switched to branch "${name}"`);
          } catch (error) {
            vscode.window.showErrorMessage(`Failed to create branch: ${error}`);
          }
        }
      }),

      vscode.commands.registerCommand("darklang.branch.delete", async () => {
        try {
          const branches = await this.client.sendRequest<BranchInfo[]>("dark/getBranches", {});
          const deletable = branches.filter(b => !b.isActive && b.name !== "main");
          if (deletable.length === 0) {
            vscode.window.showInformationMessage("No branches available to delete");
            return;
          }

          const items = deletable.map(b => ({
            label: b.name,
            branchId: b.id,
          }));

          const selected = await vscode.window.showQuickPick(items, {
            placeHolder: "Select a branch to delete",
          });

          if (selected) {
            const confirm = await vscode.window.showWarningMessage(
              `Delete branch "${selected.label}"?`,
              { modal: true },
              "Delete"
            );
            if (confirm === "Delete") {
              await this.client.sendRequest("dark/deleteBranch", { branchId: selected.branchId });
              vscode.window.showInformationMessage(`Deleted branch "${selected.label}"`);
            }
          }
        } catch (error) {
          vscode.window.showErrorMessage(`Failed to delete branch: ${error}`);
        }
      }),

      vscode.commands.registerCommand("darklang.branch.rename", async () => {
        try {
          const branches = await this.client.sendRequest<BranchInfo[]>("dark/getBranches", {});
          const items = branches.filter(b => b.name !== "main").map(b => ({
            label: b.name,
            description: b.isActive ? "(current)" : "",
            branchId: b.id,
          }));

          if (items.length === 0) {
            vscode.window.showInformationMessage("No branches available to rename");
            return;
          }

          const selected = await vscode.window.showQuickPick(items, {
            placeHolder: "Select a branch to rename",
          });

          if (selected) {
            const newName = await vscode.window.showInputBox({
              prompt: "New branch name",
              placeHolder: "Enter new name",
              value: selected.label,
            });
            if (newName && newName !== selected.label) {
              await this.client.sendRequest("dark/renameBranch", { branchId: selected.branchId, newName });
              vscode.window.showInformationMessage(`Renamed branch to "${newName}"`);
              this.scmProvider.refresh();
            }
          }
        } catch (error) {
          vscode.window.showErrorMessage(`Failed to rename branch: ${error}`);
        }
      }),

      vscode.commands.registerCommand("darklang.branch.rebase", async () => {
        try {
          // Check for conflicts first
          const status = await this.client.sendRequest<{ conflicts: string[]; hasConflicts: boolean }>("dark/rebaseStatus", {});
          if (status.hasConflicts) {
            const conflicts = status.conflicts.join("\n  ");
            vscode.window.showWarningMessage(`Rebase has conflicts:\n  ${conflicts}`);
            return;
          }

          const result = await this.client.sendRequest<{ success: boolean; message?: string; conflicts?: string[] }>("dark/rebase", {});
          if (result.success) {
            vscode.window.showInformationMessage(result.message || "Rebase successful");
          } else if (result.conflicts && result.conflicts.length > 0) {
            const conflicts = result.conflicts.join("\n  ");
            vscode.window.showWarningMessage(`Rebase failed with conflicts:\n  ${conflicts}`);
          }
        } catch (error) {
          vscode.window.showErrorMessage(`Failed to rebase: ${error}`);
        }
      }),

      vscode.commands.registerCommand("darklang.branch.merge", async () => {
        try {
          // Check if merge is possible first
          const check = await this.client.sendRequest<{ canMerge: boolean; reason?: string }>("dark/canMerge", {});
          if (!check.canMerge) {
            vscode.window.showWarningMessage(`Cannot merge: ${check.reason}`);
            return;
          }

          const confirm = await vscode.window.showWarningMessage(
            "Merge current branch into its parent?",
            { modal: true },
            "Merge"
          );
          if (confirm === "Merge") {
            const result = await this.client.sendRequest<{ success: boolean; switchedTo?: { id: string; name: string } }>("dark/merge", {});
            if (result.success && result.switchedTo) {
              vscode.window.showInformationMessage(`Merged and switched to "${result.switchedTo.name}"`);
            }
          }
        } catch (error) {
          vscode.window.showErrorMessage(`Failed to merge: ${error}`);
        }
      }),
    ];
  }
}
