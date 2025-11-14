import * as vscode from "vscode";

export class ApprovalRequestsPanel {
  public static currentPanel: ApprovalRequestsPanel | undefined;

  public static readonly viewType = "darklang.approvalRequestsV2";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: vscode.Uri;
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: vscode.Uri) {
    console.log('ApprovalRequestsPanel.createOrShow called');
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    // If we already have a panel, dispose it and create a new one
    if (ApprovalRequestsPanel.currentPanel) {
      console.log('ApprovalRequestsPanel: Disposing existing panel');
      ApprovalRequestsPanel.currentPanel.dispose();
    }

    // Otherwise, create a new panel
    console.log('ApprovalRequestsPanel: Creating new webview panel');
    const panel = vscode.window.createWebviewPanel(
      ApprovalRequestsPanel.viewType,
      "Approval Requests",
      column || vscode.ViewColumn.One,
      {
        enableScripts: true,
        localResourceRoots: [extensionUri],
      },
    );
    console.log('ApprovalRequestsPanel: Webview panel created');

    ApprovalRequestsPanel.currentPanel = new ApprovalRequestsPanel(
      panel,
      extensionUri,
    );
    console.log('ApprovalRequestsPanel: Panel instance created');
  }

  public static revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    ApprovalRequestsPanel.currentPanel = new ApprovalRequestsPanel(
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
      (message) => {
        switch (message.command) {
          case "selectRequest":
            console.log("Selected request:", message.requestId);
            return;
          case "openRequest":
            console.log("Open request:", message.requestId);
            return;
        }
      },
      null,
      this._disposables,
    );
  }

  public dispose() {
    ApprovalRequestsPanel.currentPanel = undefined;

    this._panel.dispose();

    while (this._disposables.length) {
      const disposable = this._disposables.pop();
      if (disposable) {
        disposable.dispose();
      }
    }
  }

  private _update() {
    console.log('ApprovalRequestsPanel: Updating HTML');
    const webview = this._panel.webview;
    const html = this._getHtmlForWebview(webview);
    console.log('ApprovalRequestsPanel: HTML length:', html.length);
    console.log('ApprovalRequestsPanel: HTML contains alert:', html.includes('alert'));
    this._panel.webview.html = html;
    console.log('ApprovalRequestsPanel: HTML set');
  }

  private getNonce() {
    let text = '';
    const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    for (let i = 0; i < 32; i++) {
      text += possible.charAt(Math.floor(Math.random() * possible.length));
    }
    return text;
  }

  private _getHtmlForWebview(webview: vscode.Webview) {
    // Generate a nonce for the script
    const nonce = this.getNonce();

    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'nonce-${nonce}';">
    <title>Approval Requests</title>
    <style>
        * {
            box-sizing: border-box;
            margin: 0;
            padding: 0;
        }

        body {
            font-family: var(--vscode-font-family);
            font-size: var(--vscode-font-size);
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
            display: flex;
            flex-direction: column;
            min-height: 100vh;
        }

        .header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 20px 60px;
            border-bottom: 1px solid var(--vscode-panel-border);
        }

        .header-left {
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .header-icon {
            font-size: 18px;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .header-icon svg {
            width: 24px;
            height: 24px;
        }

        .header-title {
            font-size: 18px;
            font-weight: 500;
        }

        .header-right {
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .header-btn {
            padding: 8px 16px;
            border-radius: 4px;
            border: 1px solid var(--vscode-button-border);
            background-color: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
            cursor: pointer;
            font-size: 13px;
            display: flex;
            align-items: center;
            gap: 6px;
        }

        .header-btn:hover {
            background-color: var(--vscode-button-secondaryHoverBackground);
        }

        .header-btn.primary {
            background-color: #8B5CF6;
            color: white;
            border-color: #8B5CF6;
        }

        .header-btn.primary:hover {
            background-color: #7C3AED;
        }

        .user-avatar {
            width: 32px;
            height: 32px;
            border-radius: 50%;
            background: linear-gradient(135deg, #FF6B6B, #FFA500);
        }

        .content {
            flex: 1;
            display: flex;
            flex-direction: column;
            overflow: hidden;
        }

        .tabs-container {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 0 60px;
            border-bottom: 1px solid var(--vscode-panel-border);
        }

        .tabs {
            display: flex;
            gap: 32px;
        }

        .tabs-search {
            padding: 8px 12px;
            background: var(--vscode-input-background);
            border: 1px solid var(--vscode-input-border);
            border-radius: 4px;
            color: var(--vscode-input-foreground);
            font-size: 13px;
            outline: none;
            width: 250px;
        }

        .tabs-search:focus {
            border-color: var(--vscode-focusBorder);
        }

        .tab {
            padding: 16px 0;
            cursor: pointer;
            color: var(--vscode-descriptionForeground);
            border-bottom: 2px solid transparent;
            transition: all 0.2s;
            font-size: 14px;
            display: flex;
            align-items: center;
            gap: 8px;
        }

        .tab:hover {
            color: var(--vscode-foreground);
        }

        .tab.active {
            color: var(--vscode-foreground);
            border-bottom-color: var(--vscode-focusBorder);
        }

        .tab-count {
            background: var(--vscode-badge-background);
            color: var(--vscode-badge-foreground);
            padding: 2px 8px;
            border-radius: 10px;
            font-size: 11px;
            font-weight: 600;
        }

        .main-content {
            flex: 1;
            display: flex;
            overflow: hidden;
        }

        .table-container {
            flex: 1;
            overflow-y: auto;
            padding: 20px 60px;
        }

        table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 20px;
        }

        thead {
            position: sticky;
            top: 0;
            background: var(--vscode-editor-background);
            z-index: 1;
        }

        th {
            text-align: left;
            padding: 12px 16px;
            color: var(--vscode-descriptionForeground);
            font-weight: 500;
            font-size: 12px;
            text-transform: uppercase;
            border-bottom: 1px solid var(--vscode-panel-border);
        }

        tbody tr {
            border-bottom: 1px solid var(--vscode-panel-border);
            cursor: pointer;
            transition: background 0.2s;
        }

        tbody tr:hover {
            background: var(--vscode-list-hoverBackground);
        }

        tbody tr.selected {
            background: var(--vscode-list-activeSelectionBackground);
        }

        td {
            padding: 16px;
            font-size: 13px;
        }

        .status-badge {
            padding: 4px 10px;
            border-radius: 4px;
            font-size: 11px;
            font-weight: 600;
            text-transform: uppercase;
        }

        .status-badge.changes-requested {
            background: rgba(251, 191, 36, 0.2);
            color: #FCD34D;
        }

        .status-badge.approved {
            background: rgba(34, 197, 94, 0.2);
            color: #86EFAC;
        }

        .status-badge.draft {
            background: rgba(156, 163, 175, 0.2);
            color: #D1D5DB;
        }

        .detail-panel {
            width: 400px;
            background: var(--vscode-sideBar-background);
            border-left: 1px solid var(--vscode-panel-border);
            padding: 24px;
            overflow-y: auto;
        }

        .detail-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 24px;
            padding-bottom: 16px;
            border-bottom: 1px solid var(--vscode-panel-border);
        }

        .detail-title {
            font-size: 16px;
            font-weight: 600;
        }

        .detail-section {
            margin-bottom: 24px;
        }

        .detail-label {
            font-size: 11px;
            text-transform: uppercase;
            color: var(--vscode-descriptionForeground);
            margin-bottom: 8px;
            font-weight: 600;
        }

        .detail-value {
            font-size: 13px;
            color: var(--vscode-foreground);
            line-height: 1.5;
        }

        .ops-list {
            margin-top: 12px;
        }

        .op-item {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 8px;
            margin-bottom: 8px;
            background: var(--vscode-editor-inactiveSelectionBackground);
            border-radius: 4px;
        }

        .op-checkbox {
            width: 16px;
            height: 16px;
        }

        .op-label {
            font-size: 13px;
            flex: 1;
        }

        .op-diff {
            font-size: 11px;
            color: var(--vscode-textLink-foreground);
            cursor: pointer;
        }

        .op-diff:hover {
            text-decoration: underline;
        }

        .checks {
            margin-top: 12px;
        }

        .check-item {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 8px 0;
            font-size: 13px;
        }

        .check-icon {
            color: #86EFAC;
        }

        .select-all {
            color: var(--vscode-textLink-foreground);
            cursor: pointer;
            font-size: 12px;
            margin-top: 8px;
            display: inline-block;
        }

        .select-all:hover {
            text-decoration: underline;
        }

        .action-btn {
            width: 100%;
            padding: 10px;
            background: #8B5CF6;
            color: white;
            border: none;
            border-radius: 4px;
            cursor: pointer;
            font-size: 13px;
            font-weight: 500;
            margin-top: 16px;
        }

        .action-btn:hover {
            background: #7C3AED;
        }
    </style>
</head>
<body>
    <div class="header">
        <div class="header-left">
            <span class="header-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 10 10" fill="none"><path d="M7.23775 7.74442L6.96484 7.47109C6.92873 7.43525 6.88609 7.41734 6.83692 7.41734C6.78748 7.41734 6.74484 7.43525 6.709 7.47109C6.67317 7.50664 6.65525 7.54873 6.65525 7.59734C6.65525 7.64595 6.67317 7.68817 6.709 7.724L7.06734 8.08234C7.119 8.13428 7.1772 8.16025 7.24192 8.16025C7.30664 8.16025 7.36484 8.13428 7.4165 8.08234L8.29067 7.22109C8.3265 7.18525 8.34581 7.144 8.34859 7.09734C8.35137 7.05039 8.33206 7.00636 8.29067 6.96525C8.25484 6.92942 8.21095 6.9115 8.159 6.9115C8.10706 6.9115 8.06303 6.92956 8.02692 6.96567L7.23775 7.74442ZM3.029 3.63734H6.9715C7.03067 3.63734 7.08011 3.61748 7.11984 3.57775C7.15956 3.53803 7.17956 3.48845 7.17984 3.429C7.17984 3.36428 7.15984 3.31345 7.11984 3.2765C7.08011 3.23928 7.03067 3.22067 6.9715 3.22067H3.02859C2.96942 3.22067 2.91984 3.24053 2.87984 3.28025C2.83984 3.31998 2.81998 3.36956 2.82025 3.429C2.82053 3.48845 2.84039 3.53803 2.87984 3.57775C2.91928 3.61748 2.96928 3.63734 3.029 3.63734ZM7.49984 9.21484C7.03567 9.21484 6.64192 9.05303 6.31859 8.72942C5.99498 8.40609 5.83317 8.01234 5.83317 7.54817C5.83317 7.084 5.99498 6.69011 6.31859 6.3665C6.6422 6.04289 7.03595 5.88123 7.49984 5.8815C7.96373 5.88178 8.35762 6.04345 8.6815 6.3665C9.00539 6.68956 9.16706 7.08345 9.1665 7.54817C9.1665 8.01206 9.00484 8.40581 8.6815 8.72942C8.35762 9.05303 7.96373 9.21484 7.49984 9.21484ZM1.6665 2.33984C1.6665 2.15317 1.73206 1.99428 1.86317 1.86317C1.99428 1.73206 2.15317 1.6665 2.33984 1.6665H7.66025C7.84636 1.6665 8.00511 1.73206 8.1365 1.86317C8.26761 1.99428 8.33317 2.15317 8.33317 2.33984V4.55525C8.33317 4.61442 8.31331 4.664 8.27359 4.704C8.23387 4.744 8.18428 4.76386 8.12484 4.76359C8.06539 4.76331 8.01581 4.74345 7.97609 4.704C7.93636 4.66456 7.9165 4.61498 7.9165 4.55525V2.33984C7.9165 2.27567 7.88984 2.21678 7.8365 2.16317C7.78317 2.10956 7.72428 2.08289 7.65984 2.08317H2.33984C2.27567 2.08317 2.21678 2.10984 2.16317 2.16317C2.10956 2.2165 2.08289 2.27539 2.08317 2.33984V7.93734H4.7265C4.73511 7.99345 4.74525 8.04956 4.75692 8.10567C4.76886 8.16178 4.78414 8.21706 4.80275 8.2715C4.82137 8.32123 4.80887 8.36192 4.76525 8.39359C4.72137 8.42553 4.6797 8.42692 4.64025 8.39775L4.5365 8.32692C4.50567 8.30664 4.47317 8.2965 4.439 8.2965C4.40484 8.2965 4.3722 8.30664 4.34109 8.32692L3.97567 8.57984C3.94484 8.60039 3.91234 8.61067 3.87817 8.61067C3.844 8.61067 3.81136 8.60039 3.78025 8.57984L3.41484 8.32692C3.38373 8.30664 3.35109 8.2965 3.31692 8.2965C3.28275 8.2965 3.25025 8.30664 3.21942 8.32692L2.854 8.57984C2.82317 8.60039 2.79067 8.61067 2.7565 8.61067C2.72234 8.61067 2.6897 8.60039 2.65859 8.57984L2.29317 8.32692C2.26206 8.30664 2.22942 8.2965 2.19525 8.2965C2.16109 8.2965 2.12859 8.30664 2.09775 8.32692L1.6665 8.6615V2.33984ZM3.029 6.77859H4.64317C4.70261 6.77859 4.7522 6.75873 4.79192 6.719C4.83164 6.67928 4.8515 6.6297 4.8515 6.57025C4.8515 6.51081 4.83164 6.46136 4.79192 6.42192C4.7522 6.38248 4.70261 6.36248 4.64317 6.36192H3.029C2.96984 6.36192 2.92025 6.38192 2.88025 6.42192C2.84025 6.46192 2.82039 6.51136 2.82067 6.57025C2.82095 6.62914 2.84081 6.67873 2.88025 6.719C2.9197 6.75928 2.96928 6.77914 3.029 6.77859ZM3.029 5.20817H6.03067C6.08984 5.20817 6.13942 5.18831 6.17942 5.14859C6.21942 5.10887 6.23928 5.05928 6.239 4.99984C6.23873 4.94039 6.21887 4.89081 6.17942 4.85109C6.13998 4.81136 6.09039 4.7915 6.03067 4.7915H3.02859C2.96942 4.7915 2.91984 4.81136 2.87984 4.85109C2.83984 4.89081 2.82011 4.94039 2.82067 4.99984C2.82123 5.05928 2.84109 5.10887 2.88025 5.14859C2.91942 5.18831 2.969 5.20817 3.029 5.20817Z" fill="#CACACA"/></svg></span>
            <h1 class="header-title">Approval requests</h1>
        </div>
        <div class="header-right">
            <button class="header-btn">
                <span>📄</span>
                <span>Docs</span>
            </button>
            <button class="header-btn primary">
                <span>+</span>
                <span>New</span>
            </button>
            <div class="user-avatar"></div>
        </div>
    </div>

    <div class="content">
            <div class="tabs-container">
                <div class="tabs">
                    <div class="tab active">
                        To approve<span class="tab-count">3</span>
                    </div>
                    <div class="tab">
                        Contributions<span class="tab-count">2</span>
                    </div>
                    <div class="tab">
                        Approved<span class="tab-count">14</span>
                    </div>
                    <div class="tab">
                        Rejected<span class="tab-count">2</span>
                    </div>
                </div>
                <input type="text" class="tabs-search" placeholder="Search...">
            </div>

            <div class="main-content">
                <div class="table-container">
                <table>
                    <thead>
                        <tr>
                            <th>ID</th>
                            <th>title</th>
                            <th>module</th>
                            <th>submitted</th>
                            <th>by</th>
                            <th>status</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr data-id="1">
                            <td>#1</td>
                            <td>Add memory mcp server</td>
                            <td>Darklang.Mcp</td>
                            <td>2 days ago</td>
                            <td>John Doe</td>
                            <td><span class="status-badge changes-requested">changes requested</span></td>
                        </tr>
                        <tr data-id="22" class="selected">
                            <td>#22</td>
                            <td>delete unused search command</td>
                            <td>Darklang.CLI</td>
                            <td>2 hours ago</td>
                            <td>Mike Ross</td>
                            <td><span class="status-badge approved">Approved</span></td>
                        </tr>
                        <tr data-id="41">
                            <td>#41</td>
                            <td>update history command to ...</td>
                            <td>Darklang.CLI</td>
                            <td>3 days ago</td>
                            <td>Rachel Zane</td>
                            <td><span class="status-badge draft">Draft</span></td>
                        </tr>
                    </tbody>
                </table>
                </div>

                <div class="detail-panel">
                <div class="detail-header">
                    <div class="detail-title">#22 - Mike Ross</div>
                </div>
                <div class="detail-section">
                    <div class="detail-label">description</div>
                    <div class="detail-value">
                        the search command in Darklang.CLI isn't used and the code seems broken
                    </div>
                </div>

                <div class="detail-section">
                    <div class="detail-label">Ops</div>
                    <div class="ops-list">
                        <div class="op-item">
                            <input type="checkbox" class="op-checkbox" checked>
                            <span class="op-label">Delete Darklang.CLI.search</span>
                            <span class="op-diff">see diff</span>
                        </div>
                        <div class="op-item">
                            <input type="checkbox" class="op-checkbox">
                            <span class="op-label">some other op (the idea is you can select ops you wanna merge)</span>
                        </div>
                    </div>
                    <span class="select-all">select-all</span>
                </div>

                <div class="detail-section">
                    <div class="detail-label">Checks</div>
                    <div class="checks">
                        <div class="check-item">
                            <span class="check-icon">✓</span>
                            <span>All tests have passed</span>
                        </div>
                        <div class="check-item">
                            <span class="check-icon">✓</span>
                            <span>Formatting</span>
                        </div>
                    </div>
                </div>

                <button class="action-btn">open</button>
                </div>
            </div>
        </div>

    <script nonce="${nonce}">
        // TODO: Add interactivity later
    </script>
</body>
</html>`;
  }
}
