import * as vscode from "vscode";

export class ReviewChangesPanel {
  public static currentPanel: ReviewChangesPanel | undefined;
  public static readonly viewType = "darklangReviewChanges";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: vscode.Uri;
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: vscode.Uri, ops: any[]) {
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    // If we already have a panel, show it
    if (ReviewChangesPanel.currentPanel) {
      ReviewChangesPanel.currentPanel._panel.reveal(column);
      ReviewChangesPanel.currentPanel._update(ops);
      return;
    }

    // Otherwise, create a new panel
    const panel = vscode.window.createWebviewPanel(
      ReviewChangesPanel.viewType,
      "Review Changes",
      column || vscode.ViewColumn.One,
      {
        enableScripts: true,
        localResourceRoots: [vscode.Uri.joinPath(extensionUri, "media")],
        retainContextWhenHidden: true
      }
    );

    ReviewChangesPanel.currentPanel = new ReviewChangesPanel(panel, extensionUri, ops);
  }

  public static revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    ReviewChangesPanel.currentPanel = new ReviewChangesPanel(panel, extensionUri, []);
  }

  private constructor(panel: vscode.WebviewPanel, extensionUri: vscode.Uri, ops: any[]) {
    this._panel = panel;
    this._extensionUri = extensionUri;

    // Set the webview's initial html content
    this._update(ops);

    // Listen for when the panel is disposed
    // This happens when the user closes the panel or when the panel is closed programmatically
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    // Handle messages from the webview
    this._panel.webview.onDidReceiveMessage(
      message => {
        switch (message.type) {
          case "toggleOp":
            this._handleToggleOp(message.opId);
            return;
          case "applyChanges":
            this._handleApplyChanges(message.selectedOps);
            return;
        }
      },
      null,
      this._disposables
    );
  }

  private _handleToggleOp(opId: string) {
    // TODO: Handle selection logic and dependencies
    console.log(`Toggled op: ${opId}`);
  }

  private _handleApplyChanges(selectedOps: string[]) {
    vscode.window.showInformationMessage(
      `Applying ${selectedOps.length} changes (not yet implemented)`
    );
  }

  public dispose() {
    ReviewChangesPanel.currentPanel = undefined;

    // Clean up our resources
    this._panel.dispose();

    while (this._disposables.length) {
      const x = this._disposables.pop();
      if (x) {
        x.dispose();
      }
    }
  }

  private _update(ops: any[]) {
    const webview = this._panel.webview;
    this._panel.webview.html = this._getHtmlForWebview(webview, ops);
  }

  private _getHtmlForWebview(webview: vscode.Webview, ops: any[]) {
    // Mock ops data if empty
    if (ops.length === 0) {
      ops = this._getMockOps();
    }

    // Categorize ops
    const { explicit, implicit, unselected } = this._categorizeOps(ops);

    return `<!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Review Changes</title>
      <style>
        body {
          padding: 20px;
          font-family: var(--vscode-font-family);
          color: var(--vscode-foreground);
          background-color: var(--vscode-editor-background);
        }

        h1 {
          font-size: 24px;
          margin-bottom: 10px;
        }

        .header {
          margin-bottom: 30px;
          border-bottom: 1px solid var(--vscode-panel-border);
          padding-bottom: 15px;
        }

        .summary {
          color: var(--vscode-descriptionForeground);
          margin-bottom: 10px;
        }

        .section {
          margin-bottom: 30px;
        }

        .section-title {
          font-size: 18px;
          font-weight: 600;
          margin-bottom: 15px;
          color: var(--vscode-foreground);
        }

        .section-explicit .section-title {
          color: var(--vscode-terminal-ansiGreen);
        }

        .section-implicit .section-title {
          color: var(--vscode-terminal-ansiYellow);
        }

        .section-unselected .section-title {
          color: var(--vscode-descriptionForeground);
        }

        .op-item {
          display: flex;
          align-items: flex-start;
          padding: 12px;
          margin-bottom: 8px;
          background-color: var(--vscode-list-inactiveSelectionBackground);
          border-radius: 4px;
          border: 1px solid var(--vscode-panel-border);
          cursor: pointer;
          transition: background-color 0.1s;
        }

        .op-item:hover {
          background-color: var(--vscode-list-hoverBackground);
        }

        .op-item.selected {
          border-color: var(--vscode-focusBorder);
          background-color: var(--vscode-list-activeSelectionBackground);
        }

        .op-item.implicit {
          opacity: 0.8;
          border-style: dashed;
        }

        .checkbox {
          margin-right: 12px;
          margin-top: 2px;
          width: 16px;
          height: 16px;
          flex-shrink: 0;
        }

        .checkbox:disabled {
          opacity: 0.5;
          cursor: not-allowed;
        }

        .op-content {
          flex: 1;
        }

        .op-type {
          font-size: 11px;
          text-transform: uppercase;
          color: var(--vscode-descriptionForeground);
          margin-bottom: 4px;
        }

        .op-name {
          font-weight: 500;
          margin-bottom: 4px;
        }

        .op-details {
          font-size: 13px;
          color: var(--vscode-descriptionForeground);
        }

        .op-dependency {
          font-size: 12px;
          color: var(--vscode-terminal-ansiYellow);
          margin-top: 4px;
        }

        .actions {
          position: sticky;
          bottom: 0;
          background-color: var(--vscode-editor-background);
          padding: 20px 0;
          border-top: 1px solid var(--vscode-panel-border);
          display: flex;
          gap: 10px;
          margin-top: 20px;
        }

        button {
          padding: 8px 16px;
          border: none;
          border-radius: 4px;
          cursor: pointer;
          font-size: 13px;
          font-family: var(--vscode-font-family);
        }

        .btn-primary {
          background-color: var(--vscode-button-background);
          color: var(--vscode-button-foreground);
        }

        .btn-primary:hover {
          background-color: var(--vscode-button-hoverBackground);
        }

        .btn-secondary {
          background-color: var(--vscode-button-secondaryBackground);
          color: var(--vscode-button-secondaryForeground);
        }

        .btn-secondary:hover {
          background-color: var(--vscode-button-secondaryHoverBackground);
        }

        .empty-state {
          text-align: center;
          padding: 40px;
          color: var(--vscode-descriptionForeground);
        }
      </style>
    </head>
    <body>
      <div class="header">
        <h1>Review Changes</h1>
        <div class="summary">
          <strong>${explicit.length}</strong> explicitly selected •
          <strong>${implicit.length}</strong> required dependencies •
          <strong>${unselected.length}</strong> not selected
        </div>
      </div>

      ${explicit.length > 0 ? `
        <div class="section section-explicit">
          <div class="section-title">✓ Explicitly Selected (${explicit.length})</div>
          ${explicit.map(op => this._renderOp(op, true, false)).join('')}
        </div>
      ` : ''}

      ${implicit.length > 0 ? `
        <div class="section section-implicit">
          <div class="section-title">⚠ Required Dependencies (${implicit.length})</div>
          <div style="margin-bottom: 15px; color: var(--vscode-descriptionForeground); font-size: 13px;">
            These changes are required by your selected changes and will be included automatically.
          </div>
          ${implicit.map(op => this._renderOp(op, true, true)).join('')}
        </div>
      ` : ''}

      ${unselected.length > 0 ? `
        <div class="section section-unselected">
          <div class="section-title">○ Not Selected (${unselected.length})</div>
          ${unselected.map(op => this._renderOp(op, false, false)).join('')}
        </div>
      ` : ''}

      ${explicit.length === 0 && implicit.length === 0 && unselected.length === 0 ? `
        <div class="empty-state">
          <p>No changes to review</p>
        </div>
      ` : ''}

      <div class="actions">
        <button class="btn-primary" onclick="applyChanges()">
          Apply ${explicit.length + implicit.length} Change${explicit.length + implicit.length !== 1 ? 's' : ''}
        </button>
        <button class="btn-secondary" onclick="selectAll()">Select All</button>
        <button class="btn-secondary" onclick="deselectAll()">Deselect All</button>
      </div>

      <script>
        const vscode = acquireVsCodeApi();
        let selectedOps = new Set(${JSON.stringify(explicit.map(op => op.id))});
        let implicitOps = new Set(${JSON.stringify(implicit.map(op => op.id))});

        function toggleOp(opId) {
          const checkbox = document.getElementById('cb-' + opId);
          if (checkbox.disabled) return;

          if (selectedOps.has(opId)) {
            selectedOps.delete(opId);
          } else {
            selectedOps.add(opId);
          }

          // TODO: Calculate dependencies and update implicitOps
          vscode.postMessage({ type: 'toggleOp', opId: opId });
          updateUI();
        }

        function updateUI() {
          // Update checkboxes and styling
          document.querySelectorAll('.op-item').forEach(item => {
            const opId = item.dataset.opId;
            const checkbox = document.getElementById('cb-' + opId);
            const isSelected = selectedOps.has(opId);
            const isImplicit = implicitOps.has(opId);

            checkbox.checked = isSelected || isImplicit;
            item.classList.toggle('selected', isSelected || isImplicit);
          });

          // Update summary
          const explicitCount = selectedOps.size;
          const implicitCount = implicitOps.size;
          const total = explicitCount + implicitCount;

          document.querySelector('.actions .btn-primary').textContent =
            \`Apply \${total} Change\${total !== 1 ? 's' : ''}\`;
        }

        function applyChanges() {
          const allSelected = [...selectedOps, ...implicitOps];
          vscode.postMessage({ type: 'applyChanges', selectedOps: allSelected });
        }

        function selectAll() {
          document.querySelectorAll('.checkbox:not(:disabled)').forEach(cb => {
            const opId = cb.id.replace('cb-', '');
            selectedOps.add(opId);
          });
          updateUI();
        }

        function deselectAll() {
          selectedOps.clear();
          updateUI();
        }
      </script>
    </body>
    </html>`;
  }

  private _renderOp(op: any, selected: boolean, implicit: boolean): string {
    return `
      <div class="op-item ${selected ? 'selected' : ''} ${implicit ? 'implicit' : ''}"
           data-op-id="${op.id}"
           onclick="toggleOp('${op.id}')">
        <input type="checkbox"
               class="checkbox"
               id="cb-${op.id}"
               ${selected ? 'checked' : ''}
               ${implicit ? 'disabled' : ''}
               onclick="event.stopPropagation()">
        <div class="op-content">
          <div class="op-type">${op.type}</div>
          <div class="op-name">${op.name}</div>
          <div class="op-details">${op.details}</div>
          ${implicit ? '<div class="op-dependency">Required by selected changes</div>' : ''}
        </div>
      </div>
    `;
  }

  private _getMockOps(): any[] {
    return [
      {
        id: "op1",
        type: "SetFnName",
        name: "Darklang.Stdlib.String.concat",
        details: "Rename function",
        selected: true,
        dependencies: ["op3"]
      },
      {
        id: "op2",
        type: "SetFnName",
        name: "Darklang.Internal.Test.WTTest",
        details: "Rename test function",
        selected: true,
        dependencies: []
      },
      {
        id: "op3",
        type: "UpdateFnParameter",
        name: "Darklang.Stdlib.String.concat",
        details: "Add parameter 'separator'",
        selected: false,
        dependencies: []
      },
      {
        id: "op4",
        type: "SetTypeName",
        name: "Darklang.Stdlib.Option",
        details: "Rename type",
        selected: false,
        dependencies: []
      },
      {
        id: "op5",
        type: "CreateFn",
        name: "Darklang.Stdlib.List.filter",
        details: "Create new function",
        selected: false,
        dependencies: []
      }
    ];
  }

  private _categorizeOps(ops: any[]): { explicit: any[], implicit: any[], unselected: any[] } {
    const explicit = ops.filter(op => op.selected);
    const implicitSet = new Set<string>();

    // Collect all dependencies of explicitly selected ops
    explicit.forEach(op => {
      if (op.dependencies && op.dependencies.length > 0) {
        op.dependencies.forEach((depId: string) => implicitSet.add(depId));
      }
    });

    const implicit = ops.filter(op => implicitSet.has(op.id) && !op.selected);
    const unselected = ops.filter(op => !op.selected && !implicitSet.has(op.id));

    return { explicit, implicit, unselected };
  }
}
