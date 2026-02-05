import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchInfo } from "../../ui/statusbar/statusBar";

export class BranchManagerPanel {
  public static currentPanel: BranchManagerPanel | undefined;
  public static readonly viewType = "darklangBranchManager";
  private static _client: LanguageClient | undefined;

  private readonly _panel: vscode.WebviewPanel;
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: vscode.Uri) {
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    if (BranchManagerPanel.currentPanel) {
      BranchManagerPanel.currentPanel._panel.reveal(column);
      BranchManagerPanel.currentPanel._update();
      return;
    }

    const panel = vscode.window.createWebviewPanel(
      BranchManagerPanel.viewType,
      "Manage Branches",
      column || vscode.ViewColumn.One,
      { enableScripts: true },
    );

    BranchManagerPanel.currentPanel = new BranchManagerPanel(panel);
  }

  public static revive(panel: vscode.WebviewPanel) {
    BranchManagerPanel.currentPanel = new BranchManagerPanel(panel);
  }

  public static setClient(client: LanguageClient): void {
    BranchManagerPanel._client = client;
  }

  /** Refresh the panel contents (called on branch/scm change notifications) */
  public static refresh(): void {
    if (BranchManagerPanel.currentPanel) {
      BranchManagerPanel.currentPanel._update();
    }
  }

  private constructor(panel: vscode.WebviewPanel) {
    this._panel = panel;

    this._update();

    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    this._panel.webview.onDidReceiveMessage(
      async (message) => {
        const client = BranchManagerPanel._client;
        if (!client) { return; }

        switch (message.type) {
          case "switchBranch":
            try {
              await client.sendRequest("dark/switchBranch", { branchId: message.branchId });
              this._update();
            } catch (error) {
              vscode.window.showErrorMessage(`Failed to switch branch: ${error}`);
            }
            break;

          case "createBranch": {
            const name = await vscode.window.showInputBox({
              prompt: "Branch name",
              placeHolder: "Enter new branch name",
            });
            if (name) {
              try {
                await client.sendRequest("dark/createBranch", { name });
                vscode.window.showInformationMessage(`Created and switched to branch "${name}"`);
                this._update();
              } catch (error) {
                vscode.window.showErrorMessage(`Failed to create branch: ${error}`);
              }
            }
            break;
          }

          case "deleteBranch": {
            const confirm = await vscode.window.showWarningMessage(
              `Delete branch "${message.branchName}"?`,
              { modal: true },
              "Delete",
            );
            if (confirm === "Delete") {
              try {
                await client.sendRequest("dark/deleteBranch", { branchId: message.branchId });
                vscode.window.showInformationMessage(`Deleted branch "${message.branchName}"`);
                this._update();
              } catch (error) {
                vscode.window.showErrorMessage(`Failed to delete branch: ${error}`);
              }
            }
            break;
          }

          case "renameBranch": {
            const newName = await vscode.window.showInputBox({
              prompt: "New branch name",
              placeHolder: "Enter new name",
              value: message.branchName,
            });
            if (newName && newName !== message.branchName) {
              try {
                await client.sendRequest("dark/renameBranch", { branchId: message.branchId, newName });
                vscode.window.showInformationMessage(`Renamed branch to "${newName}"`);
                this._update();
              } catch (error) {
                vscode.window.showErrorMessage(`Failed to rename branch: ${error}`);
              }
            }
            break;
          }

          case "rebase":
            try {
              const status = await client.sendRequest<{ conflicts: string[]; hasConflicts: boolean }>("dark/rebaseStatus", {});
              if (status.hasConflicts) {
                vscode.window.showWarningMessage(`Rebase has conflicts:\n  ${status.conflicts.join("\n  ")}`);
                return;
              }
              const result = await client.sendRequest<{ success: boolean; message?: string; conflicts?: string[] }>("dark/rebase", {});
              if (result.success) {
                vscode.window.showInformationMessage(result.message || "Rebase successful");
                this._update();
              } else if (result.conflicts && result.conflicts.length > 0) {
                vscode.window.showWarningMessage(`Rebase failed with conflicts:\n  ${result.conflicts.join("\n  ")}`);
              }
            } catch (error) {
              vscode.window.showErrorMessage(`Failed to rebase: ${error}`);
            }
            break;

          case "merge": {
            const check = await BranchManagerPanel._client?.sendRequest<{ canMerge: boolean; reason?: string }>("dark/canMerge", {});
            if (check && !check.canMerge) {
              vscode.window.showWarningMessage(`Cannot merge: ${check.reason}`);
              return;
            }
            const confirm = await vscode.window.showWarningMessage(
              "Merge current branch into its parent?",
              { modal: true },
              "Merge",
            );
            if (confirm === "Merge") {
              try {
                const result = await client.sendRequest<{ success: boolean; switchedTo?: { id: string; name: string } }>("dark/merge", {});
                if (result.success && result.switchedTo) {
                  vscode.window.showInformationMessage(`Merged and switched to "${result.switchedTo.name}"`);
                  this._update();
                }
              } catch (error) {
                vscode.window.showErrorMessage(`Failed to merge: ${error}`);
              }
            }
            break;
          }

          case "refresh":
            this._update();
            break;
        }
      },
      null,
      this._disposables,
    );
  }

  public dispose() {
    BranchManagerPanel.currentPanel = undefined;
    this._panel.dispose();
    while (this._disposables.length) {
      this._disposables.pop()?.dispose();
    }
  }

  private async _update() {
    const client = BranchManagerPanel._client;
    if (!client) { return; }

    let branches: BranchInfo[] = [];
    try {
      branches = await client.sendRequest<BranchInfo[]>("dark/getBranches", {});
    } catch {
      // If we can't fetch branches, show empty state
    }

    this._panel.webview.html = this._getHtml(branches);
  }

  private _getHtml(branches: BranchInfo[]): string {
    const currentBranch = branches.find(b => b.isActive);
    const otherBranches = branches.filter(b => !b.isActive);

    const renderBranch = (b: BranchInfo, isCurrent: boolean): string => {
      const currentTag = isCurrent ? '<span class="badge">current</span>' : "";
      const mainTag = b.name === "main" ? '<span class="badge badge-main">main</span>' : "";

      const switchBtn = !isCurrent
        ? `<button class="btn btn-primary" onclick="action('switchBranch', '${b.id}', '${b.name}')">Switch</button>`
        : "";
      const renameBtn = b.name !== "main"
        ? `<button class="btn btn-secondary" onclick="action('renameBranch', '${b.id}', '${b.name}')">Rename</button>`
        : "";
      const deleteBtn = !isCurrent && b.name !== "main"
        ? `<button class="btn btn-danger" onclick="action('deleteBranch', '${b.id}', '${b.name}')">Delete</button>`
        : "";

      // Show rebase/merge only for current non-main branch
      const scmBtns = isCurrent && b.name !== "main"
        ? `<button class="btn btn-secondary" onclick="action('rebase')">Rebase</button>
           <button class="btn btn-secondary" onclick="action('merge')">Merge</button>`
        : "";

      return `
        <div class="branch-item ${isCurrent ? "current" : ""}">
          <div class="branch-info">
            <div class="branch-name">${b.name} ${currentTag}${mainTag}</div>
          </div>
          <div class="branch-actions">
            ${scmBtns}${switchBtn}${renameBtn}${deleteBtn}
          </div>
        </div>`;
    };

    const currentHtml = currentBranch ? renderBranch(currentBranch, true) : "";
    const othersHtml = otherBranches.length > 0
      ? otherBranches.map(b => renderBranch(b, false)).join("")
      : '<div class="empty-state">No other branches</div>';

    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Manage Branches</title>
    <style>
        body {
            font-family: var(--vscode-font-family);
            font-size: var(--vscode-font-size);
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
            padding: 16px;
            margin: 0;
        }
        .header {
            display: flex;
            align-items: center;
            justify-content: space-between;
            margin-bottom: 16px;
        }
        h1 { font-size: 1.4em; font-weight: 600; margin: 0; }
        h2 { font-size: 1.1em; font-weight: 600; margin: 16px 0 8px 0; color: var(--vscode-descriptionForeground); }
        .branch-list { display: flex; flex-direction: column; gap: 8px; }
        .branch-item {
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 10px 12px;
            background: var(--vscode-list-inactiveSelectionBackground);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 4px;
        }
        .branch-item.current {
            border-color: var(--vscode-focusBorder);
        }
        .branch-info { flex: 1; }
        .branch-name { font-weight: 500; }
        .badge {
            display: inline-block;
            padding: 1px 6px;
            border-radius: 3px;
            font-size: 0.8em;
            margin-left: 8px;
            background: var(--vscode-badge-background);
            color: var(--vscode-badge-foreground);
        }
        .badge-main {
            background: var(--vscode-descriptionForeground);
            color: var(--vscode-editor-background);
        }
        .branch-actions { display: flex; gap: 6px; }
        .btn {
            padding: 4px 10px;
            border: none;
            border-radius: 3px;
            font-size: 0.85em;
            cursor: pointer;
            white-space: nowrap;
        }
        .btn-primary {
            background: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
        }
        .btn-primary:hover { background: var(--vscode-button-hoverBackground); }
        .btn-secondary {
            background: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
        }
        .btn-secondary:hover { background: var(--vscode-button-secondaryHoverBackground); }
        .btn-danger {
            background: var(--vscode-inputValidation-errorBackground);
            color: var(--vscode-inputValidation-errorForeground);
        }
        .btn-danger:hover { opacity: 0.8; }
        .empty-state {
            text-align: center;
            padding: 24px;
            color: var(--vscode-descriptionForeground);
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>Manage Branches</h1>
        <div style="display: flex; gap: 6px;">
            <button class="btn btn-secondary" onclick="action('refresh')">Refresh</button>
            <button class="btn btn-primary" onclick="action('createBranch')">+ New Branch</button>
        </div>
    </div>

    <h2>Current Branch</h2>
    <div class="branch-list">${currentHtml}</div>

    <h2>Other Branches</h2>
    <div class="branch-list">${othersHtml}</div>

    <script>
        const vscode = acquireVsCodeApi();
        function action(type, branchId, branchName) {
            vscode.postMessage({ type, branchId, branchName });
        }
    </script>
</body>
</html>`;
  }
}
