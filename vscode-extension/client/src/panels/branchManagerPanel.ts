import * as vscode from "vscode";
import { BranchStateManager } from "../data/branchStateManager";

/**
 * Branches Manager Panel - Manage all branches (active and inactive)
 */
export class BranchesManagerPanel {
  public static currentPanel: BranchesManagerPanel | undefined;
  public static readonly viewType = "darklangBranchesManager";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: vscode.Uri;
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: vscode.Uri) {
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    // If we already have a panel, show it
    if (BranchesManagerPanel.currentPanel) {
      BranchesManagerPanel.currentPanel._panel.reveal(column);
      return;
    }

    // Otherwise, create a new panel
    const panel = vscode.window.createWebviewPanel(
      BranchesManagerPanel.viewType,
      "Manage Branchs",
      column || vscode.ViewColumn.One,
      {
        enableScripts: true,
        localResourceRoots: [extensionUri],
      },
    );

    BranchesManagerPanel.currentPanel = new BranchesManagerPanel(
      panel,
      extensionUri,
    );
  }

  public static revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    BranchesManagerPanel.currentPanel = new BranchesManagerPanel(
      panel,
      extensionUri,
    );
  }

  private constructor(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    this._panel = panel;
    this._extensionUri = extensionUri;

    // Set the webview's initial html content
    this._update();

    // Listen for when the panel is disposed
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    // Handle messages from the webview
    this._panel.webview.onDidReceiveMessage(
      async message => {
        switch (message.type) {
          case "switchBranch":
            await vscode.commands.executeCommand("darklang.branch.switch", {
              id: message.branchId,
              title: message.branchName,
            });
            vscode.window.showInformationMessage(
              `Switched to branch: ${message.branchName}`,
            );
            this._update();
            break;

          case "viewBranch":
            const uri = vscode.Uri.parse(`dark:///branch/${message.branchId}`);
            const doc = await vscode.workspace.openTextDocument(uri);
            await vscode.window.showTextDocument(doc, { preview: false });
            break;

          case "toggleActive":
            // TODO: Implement branch active toggle
            vscode.window.showInformationMessage(
              `Toggled branch "${message.branchName}" to ${
                message.active ? "active" : "inactive"
              }`,
            );
            this._update();
            break;

          case "renameBranch":
            const newName = await vscode.window.showInputBox({
              prompt: "Enter new branch name",
              value: message.currentName,
              placeHolder: "Branch name",
            });
            if (newName && newName !== message.currentName) {
              // TODO: Implement branch rename
              vscode.window.showInformationMessage(
                `Renamed branch to: ${newName}`,
              );
              this._update();
            }
            break;

          case "createBranch":
            const branchName = await vscode.window.showInputBox({
              prompt: "Enter new branch name",
              placeHolder: "e.g., feature-user-auth",
            });
            if (branchName) {
              const branchStateManager = BranchStateManager.getInstance();
              const newBranch = await branchStateManager.createBranch(branchName);
              if (newBranch) {
                vscode.window.showInformationMessage(
                  `Created and switched to branch: ${branchName}`,
                );
                this._update();
              } else {
                vscode.window.showErrorMessage(
                  `Failed to create branch: ${branchName}`,
                );
              }
            }
            break;
        }
      },
      null,
      this._disposables,
    );
  }

  public dispose() {
    BranchesManagerPanel.currentPanel = undefined;

    this._panel.dispose();

    while (this._disposables.length) {
      const disposable = this._disposables.pop();
      if (disposable) {
        disposable.dispose();
      }
    }
  }

  private _update() {
    const webview = this._panel.webview;
    this._panel.webview.html = this._getHtmlForWebview(webview);
  }

  private _getHtmlForWebview(webview: vscode.Webview) {
    const branchStateManager = BranchStateManager.getInstance();
    const currentBranchId = branchStateManager.getCurrentBranchId();

    const branches = this._getAllBranches(currentBranchId);

    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Manage Branchs</title>
    <style>
        body {
            font-family: var(--vscode-font-family);
            font-size: var(--vscode-font-size);
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
            padding: 16px;
            margin: 0;
        }

        h1 {
            font-size: 1.5em;
            font-weight: 600;
            margin: 0 0 16px 0;
        }

        .search-box {
            width: 100%;
            padding: 8px;
            margin-bottom: 16px;
            background: var(--vscode-input-background);
            color: var(--vscode-input-foreground);
            border: 1px solid var(--vscode-input-border);
            border-radius: 4px;
            font-size: 1em;
        }

        .search-box:focus {
            outline: 1px solid var(--vscode-focusBorder);
        }

        .section-header {
            font-size: 1.1em;
            font-weight: 600;
            margin: 16px 0 8px 0;
            color: var(--vscode-textPreformat-foreground);
        }

        .branch-list {
            display: flex;
            flex-direction: column;
            gap: 8px;
        }

        .branch-item {
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 12px;
            background: var(--vscode-list-inactiveSelectionBackground);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 4px;
        }

        .branch-item.current {
            background: var(--vscode-list-activeSelectionBackground);
            border-color: var(--vscode-focusBorder);
        }

        .branch-info {
            flex: 1;
        }

        .branch-name {
            font-weight: 500;
            margin-bottom: 4px;
        }

        .branch-item.current .branch-name::before {
            content: "📍 ";
        }

        .branch-meta {
            font-size: 0.9em;
            color: var(--vscode-descriptionForeground);
        }

        .branch-actions {
            display: flex;
            gap: 8px;
        }

        .btn {
            padding: 4px 12px;
            border: none;
            border-radius: 3px;
            font-size: 0.9em;
            cursor: pointer;
            white-space: nowrap;
        }

        .btn-primary {
            background: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
        }

        .btn-primary:hover {
            background: var(--vscode-button-hoverBackground);
        }

        .btn-secondary {
            background: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
        }

        .btn-secondary:hover {
            background: var(--vscode-button-secondaryHoverBackground);
        }

        .btn-danger {
            background: var(--vscode-inputValidation-errorBackground);
            color: var(--vscode-inputValidation-errorForeground);
        }

        .btn-danger:hover {
            opacity: 0.8;
        }

        .empty-state {
            text-align: center;
            padding: 32px;
            color: var(--vscode-descriptionForeground);
        }

        .badge {
            display: inline-block;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 0.8em;
            margin-left: 8px;
        }

        .badge-active {
            background: var(--vscode-charts-green);
            color: var(--vscode-editor-background);
        }

        .badge-inactive {
            background: var(--vscode-descriptionForeground);
            color: var(--vscode-editor-background);
        }
    </style>
</head>
<body>
    <h1>Manage Branchs</h1>

    <div style="display: flex; gap: 8px; margin-bottom: 16px;">
        <input
            type="text"
            class="search-box"
            id="searchBox"
            placeholder="Search branches..."
            onkeyup="filterAndSort()"
            style="flex: 1; margin-bottom: 0;"
        />
        <select id="dateFilter" onchange="filterAndSort()" style="padding: 8px; background: var(--vscode-input-background); color: var(--vscode-input-foreground); border: 1px solid var(--vscode-input-border); border-radius: 4px;">
            <option value="all">All time</option>
            <option value="7">Last 7 days</option>
            <option value="30">Last 30 days</option>
            <option value="90">Last 90 days</option>
            <option value="older">Older than 90 days</option>
        </select>
        <select id="sortBy" onchange="filterAndSort()" style="padding: 8px; background: var(--vscode-input-background); color: var(--vscode-input-foreground); border: 1px solid var(--vscode-input-border); border-radius: 4px;">
            <option value="newest">Newest first</option>
            <option value="oldest">Oldest first</option>
            <option value="alpha">Alphabetical</option>
        </select>
        <button class="btn btn-primary" onclick="createNewBranch()" style="padding: 8px 16px; white-space: nowrap;">
            + New Branch
        </button>
    </div>

    <div id="activeBranches">
        <div class="section-header">Active Branches (${
          branches.active.length
        })</div>
        <div class="branch-list" id="activeList">
            ${branches.active
              .map(branch => this._renderBranch(branch, true))
              .join("")}
        </div>
    </div>

    <div id="inactiveBranches">
        <div class="section-header">Inactive Branches (${
          branches.inactive.length
        })</div>
        <div class="branch-list" id="inactiveList">
            ${branches.inactive
              .map(branch => this._renderBranch(branch, false))
              .join("")}
        </div>
    </div>

    <script>
        const vscode = acquireVsCodeApi();

        function filterAndSort() {
            const query = document.getElementById('searchBox').value.toLowerCase();
            const dateFilter = document.getElementById('dateFilter').value;
            const sortBy = document.getElementById('sortBy').value;

            const now = new Date();
            const msPerDay = 24 * 60 * 60 * 1000;

            // Get all branches from both lists
            const activeBranches = Array.from(document.querySelectorAll('#activeList .branch-item'));
            const inactiveBranches = Array.from(document.querySelectorAll('#inactiveList .branch-item'));
            const allBranches = [...activeBranches, ...inactiveBranches];

            // Filter and collect visible branches
            const visibleActive = [];
            const visibleInactive = [];

            allBranches.forEach(branch => {
                const name = branch.getAttribute('data-name').toLowerCase();
                const createdAt = new Date(branch.getAttribute('data-created'));
                const isActive = activeBranches.includes(branch);

                // Check name filter
                const matchesName = name.includes(query);

                // Check date filter
                const daysSinceCreated = (now - createdAt) / msPerDay;
                let matchesDate = true;

                if (dateFilter === '7') {
                    matchesDate = daysSinceCreated <= 7;
                } else if (dateFilter === '30') {
                    matchesDate = daysSinceCreated <= 30;
                } else if (dateFilter === '90') {
                    matchesDate = daysSinceCreated <= 90;
                } else if (dateFilter === 'older') {
                    matchesDate = daysSinceCreated > 90;
                }

                if (matchesName && matchesDate) {
                    if (isActive) {
                        visibleActive.push(branch);
                    } else {
                        visibleInactive.push(branch);
                    }
                }

                // Hide all initially
                branch.style.display = 'none';
            });

            // Sort function
            const sortBranches = (a, b) => {
                if (sortBy === 'alpha') {
                    return a.getAttribute('data-name').localeCompare(b.getAttribute('data-name'));
                } else if (sortBy === 'newest') {
                    return new Date(b.getAttribute('data-created')) - new Date(a.getAttribute('data-created'));
                } else if (sortBy === 'oldest') {
                    return new Date(a.getAttribute('data-created')) - new Date(b.getAttribute('data-created'));
                }
                return 0;
            };

            // Sort and display
            visibleActive.sort(sortBranches);
            visibleInactive.sort(sortBranches);

            const activeList = document.getElementById('activeList');
            const inactiveList = document.getElementById('inactiveList');

            // Clear and re-append in sorted order
            visibleActive.forEach(branch => {
                branch.style.display = 'flex';
                activeList.appendChild(branch);
            });

            visibleInactive.forEach(branch => {
                branch.style.display = 'flex';
                inactiveList.appendChild(branch);
            });
        }

        function switchBranch(branchId, branchName) {
            vscode.postMessage({
                type: 'switchBranch',
                branchId: branchId,
                branchName: branchName
            });
        }

        function viewBranch(branchId) {
            vscode.postMessage({
                type: 'viewBranch',
                branchId: branchId
            });
        }

        function toggleActive(branchId, branchName, currentActive) {
            vscode.postMessage({
                type: 'toggleActive',
                branchId: branchId,
                branchName: branchName,
                active: !currentActive
            });
        }

        function renameBranch(branchId, currentName) {
            vscode.postMessage({
                type: 'renameBranch',
                branchId: branchId,
                currentName: currentName
            });
        }

        function createNewBranch() {
            vscode.postMessage({
                type: 'createBranch'
            });
        }
    </script>
</body>
</html>`;
  }

  private _renderBranch(branch: any, isActive: boolean): string {
    const isCurrent = branch.isCurrent || false;
    const statusBadge = isActive
      ? '<span class="badge badge-active">Active</span>'
      : '<span class="badge badge-inactive">Inactive</span>';

    return `
      <div class="branch-item ${isCurrent ? "current" : ""}"
           data-name="${branch.name}"
           data-description="${branch.description || ""}"
           data-created="${branch.createdAt || ""}">
        <div class="branch-info">
          <div class="branch-name">${branch.name}${statusBadge}</div>
          <div class="branch-meta">
            ${branch.description ? branch.description + " · " : ""}
            ${branch.conflicts ? " · " + branch.conflicts + " conflicts" : ""}
          </div>
        </div>
        <div class="branch-actions">
          ${
            !isCurrent
              ? `<button class="btn btn-primary" onclick="switchBranch('${branch.id}', '${branch.name}')">Switch</button>`
              : ""
          }
          <button class="btn btn-secondary" onclick="viewBranch('${
            branch.id
          }')">View</button>
          <button class="btn btn-secondary" onclick="toggleActive('${
            branch.id
          }', '${branch.name}', ${isActive})">${
      isActive ? "Deactivate" : "Activate"
    }</button>
        </div>
      </div>
    `;
  }

  private _getAllBranches(currentBranchId: string | null): {
    active: any[];
    inactive: any[];
  } {
    const branchStateManager = BranchStateManager.getInstance();
    const realBranches = branchStateManager.getBranches();

    const allBranches = realBranches.map(branch => ({
      id: branch.id,
      name: branch.title,
      description: `Created ${new Date(branch.createdAt).toLocaleDateString()}`,
      active: branch.state === "active",
      isCurrent: branch.id === currentBranchId,
      createdAt: branch.createdAt,
    }));

    return {
      active: allBranches.filter(b => b.active),
      inactive: allBranches.filter(b => !b.active),
    };
  }
}
