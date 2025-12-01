import * as vscode from "vscode";

/** Homepage Panel - Display Darklang homepage with account info */
export class HomepagePanel {
  public static currentPanel: HomepagePanel | undefined;
  public static readonly viewType = "darklangHomepage";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: vscode.Uri;
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: vscode.Uri) {
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    // If we already have a panel, show it
    if (HomepagePanel.currentPanel) {
      HomepagePanel.currentPanel._panel.reveal(column);
      return;
    }

    // Otherwise, create a new panel
    const panel = vscode.window.createWebviewPanel(
      HomepagePanel.viewType,
      "Darklang Homepage",
      column || vscode.ViewColumn.One,
      {
        enableScripts: true,
        localResourceRoots: [
          extensionUri,
          vscode.Uri.joinPath(extensionUri, "static"),
        ],
      },
    );

    HomepagePanel.currentPanel = new HomepagePanel(panel, extensionUri);
  }

  public static revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    HomepagePanel.currentPanel = new HomepagePanel(panel, extensionUri);
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
      message => {
        switch (message.command) {
          case "navigate":
            this._handleNavigation(message.page);
            return;
          case "createNew":
            this._handleCreateNew(message.type);
            return;
        }
      },
      null,
      this._disposables,
    );
  }

  private _handleNavigation(page: string) {
    const ApprovalRequestsPanel =
      require("./approvalRequestsPanel").ApprovalRequestsPanel;

    switch (page) {
      case "approval-requests":
        ApprovalRequestsPanel.createOrShow(this._extensionUri);
        break;
      // Add other navigation cases here
    }
  }

  private _handleCreateNew(type: string) {
    switch (type) {
      case "httphandler":
        const HttpHandlerPanel =
          require("./httpHandlerPanel").HttpHandlerPanel;
        HttpHandlerPanel.createOrShow(this._extensionUri);
        break;
      case "db":
        const DatabasePanel = require("./databasePanel").DatabasePanel;
        DatabasePanel.createOrShow(this._extensionUri);
        break;
    }
  }

  public dispose() {
    HomepagePanel.currentPanel = undefined;

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
    // Get URI for the logo
    const logoUri = webview.asWebviewUri(
      vscode.Uri.joinPath(
        this._extensionUri,
        "static",
        "wordmark-dark-transparent.svg",
      ),
    );

    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Darklang Home</title>
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
            min-height: 100vh;
        }

        .sidebar {
            width: 280px;
            background-color: var(--vscode-sideBar-background);
            border-right: 1px solid var(--vscode-sideBar-border);
            display: flex;
            flex-direction: column;
            padding: 40px 20px;
            transition: width 0.3s ease, padding 0.3s ease;
        }

        .sidebar.collapsed {
            width: 80px;
            padding: 40px 10px;
        }

        .sidebar.collapsed .logo-image {
            display: none;
        }

        .sidebar.collapsed .nav-label,
        .sidebar.collapsed .nav-badge,
        .sidebar.collapsed .nav-tag {
            display: none;
        }

        .sidebar.collapsed .nav-item {
            justify-content: center;
            padding: 14px 10px;
        }

        .sidebar.collapsed .logo-section {
            justify-content: center;
            padding: 0 10px;
        }

        .toggle-btn {
            background: none;
            border: none;
            color: var(--vscode-foreground);
            cursor: pointer;
            padding: 8px;
            border-radius: 4px;
            display: flex;
            align-items: center;
            justify-content: center;
            transition: background-color 0.2s;
        }

        .toggle-btn:hover {
            background-color: var(--vscode-list-hoverBackground);
        }

        .toggle-btn svg {
            width: 16px;
            height: 16px;
            transition: transform 0.3s ease;
        }

        .sidebar.collapsed .toggle-btn svg {
            transform: rotate(180deg);
        }

        .logo-section {
            margin-bottom: 60px;
            padding: 0 20px;
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .logo-image {
            width: 100%;
            height: auto;
            max-width: 150px;
            flex: 1;
        }

        .nav-menu {
            flex: 1;
            display: flex;
            flex-direction: column;
            gap: 8px;
        }

        .nav-item {
            display: flex;
            align-items: center;
            gap: 16px;
            padding: 14px 20px;
            border-radius: 6px;
            cursor: pointer;
            transition: all 0.2s;
            position: relative;
            color: var(--vscode-foreground);
            text-decoration: none;
        }

        .nav-item:hover {
            background-color: var(--vscode-list-hoverBackground);
        }

        .nav-item.active {
            background-color: rgba(100, 100, 200, 0.3);
        }

        .nav-icon {
            font-size: 18px;
            width: 24px;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .nav-icon svg {
            width: 20px;
            height: 20px;
        }

        .nav-item[data-page="branches"] .nav-icon svg,
        .nav-item[data-page="settings"] .nav-icon svg {
            width: 24px;
            height: 24px;
        }

        .nav-label {
            font-size: 15px;
            font-weight: 400;
        }

        .nav-badge {
            margin-left: auto;
            background-color: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
            border-radius: 10px;
            padding: 2px 8px;
            font-size: 11px;
            font-weight: 600;
        }

        .nav-tag {
            margin-left: auto;
            background-color: #0080FF;
            color: white;
            border-radius: 3px;
            padding: 2px 6px;
            font-size: 10px;
            font-weight: 700;
            text-transform: uppercase;
        }

        .main-content {
            flex: 1;
            overflow-y: auto;
        }

        .dashboard-header {
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 20px 60px;
            border-bottom: 1px solid var(--vscode-panel-border);
            background-color: var(--vscode-editor-background);
        }

        .header-left {
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .header-icon {
            font-size: 18px;
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
            transition: all 0.2s;
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

        .new-btn-container {
            position: relative;
        }

        .new-dropdown {
            position: absolute;
            top: 100%;
            right: 0;
            margin-top: 4px;
            background-color: var(--vscode-dropdown-background);
            border: 1px solid var(--vscode-dropdown-border);
            border-radius: 6px;
            box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
            z-index: 1000;
            min-width: 160px;
            display: none;
        }

        .new-dropdown.show {
            display: block;
        }

        .dropdown-item {
            padding: 10px 16px;
            cursor: pointer;
            display: flex;
            align-items: center;
            gap: 10px;
            color: var(--vscode-foreground);
            transition: background-color 0.15s;
        }

        .dropdown-item:first-child {
            border-radius: 6px 6px 0 0;
        }

        .dropdown-item:last-child {
            border-radius: 0 0 6px 6px;
        }

        .dropdown-item:hover {
            background-color: var(--vscode-list-hoverBackground);
        }

        .dropdown-item svg {
            width: 16px;
            height: 16px;
        }

        .user-avatar {
            width: 32px;
            height: 32px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            background: linear-gradient(135deg, #FF6B6B, #FFA500);
            cursor: pointer;
            overflow: hidden;
        }

        .user-avatar img {
            width: 100%;
            height: 100%;
            object-fit: cover;
        }

        .dashboard-content {
            padding: 40px 60px;
        }

        .search-container {
            margin-bottom: 48px;
        }

        .search-box {
            position: relative;
            width: 100%;
            max-width: 1200px;
        }

        .search-icon {
            position: absolute;
            left: 20px;
            top: 50%;
            transform: translateY(-50%);
            color: var(--vscode-input-placeholderForeground);
            font-size: 18px;
        }

        .search-input {
            width: 100%;
            padding: 16px 20px 16px 56px;
            background: var(--vscode-input-background);
            border: 1px solid var(--vscode-input-border);
            border-radius: 8px;
            color: var(--vscode-input-foreground);
            font-size: 14px;
            outline: none;
        }

        .search-input:focus {
            border-color: var(--vscode-focusBorder);
        }

        .search-input::placeholder {
            color: var(--vscode-input-placeholderForeground);
        }

        .section {
            margin-bottom: 48px;
        }

        .section-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 24px;
        }

        .section-title {
            font-size: 24px;
            font-weight: 600;
            color: var(--vscode-foreground);
        }

        .section-action {
            padding: 6px 14px;
            background: transparent;
            border: 1px solid var(--vscode-button-border);
            border-radius: 4px;
            color: var(--vscode-foreground);
            cursor: pointer;
            font-size: 13px;
            display: flex;
            align-items: center;
            gap: 6px;
            transition: all 0.2s;
        }

        .section-action:hover {
            background: var(--vscode-button-secondaryHoverBackground);
        }

        .cards-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(360px, 1fr));
            gap: 20px;
        }

        .project-card {
            background: var(--vscode-sideBar-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 8px;
            padding: 24px;
            cursor: pointer;
            transition: all 0.2s ease;
            display: flex;
            align-items: center;
            gap: 20px;
            position: relative;
        }

        .project-card:hover {
            background: var(--vscode-list-hoverBackground);
            border-color: var(--vscode-focusBorder);
            transform: translateY(-2px);
        }

        .card-icon-wrapper {
            width: 56px;
            height: 56px;
            background: var(--vscode-button-secondaryBackground);
            border-radius: 10px;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 28px;
            flex-shrink: 0;
        }

        .card-content {
            flex: 1;
            min-width: 0;
        }

        .card-title {
            font-size: 16px;
            font-weight: 600;
            margin-bottom: 8px;
            color: var(--vscode-foreground);
        }

        .card-meta {
            display: flex;
            align-items: center;
            gap: 8px;
            font-size: 13px;
            color: var(--vscode-descriptionForeground);
            flex-wrap: wrap;
        }

        .card-meta-item {
            display: flex;
            align-items: center;
        }

        .meta-separator {
            margin: 0 4px;
        }

        .meta-dot {
            width: 5px;
            height: 5px;
            background-color: #86EFAC;
            border-radius: 50%;
            margin: 0 6px;
        }

        .card-arrow {
            position: absolute;
            right: 24px;
            top: 50%;
            transform: translateY(-50%);
            color: var(--vscode-descriptionForeground);
            font-size: 20px;
            opacity: 0.6;
        }

        .project-card:hover .card-arrow {
            opacity: 1;
            transform: translateY(-50%) translateX(4px);
        }

        .status-tag {
            padding: 4px 10px;
            border-radius: 4px;
            font-size: 11px;
            font-weight: 600;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .status-tag.deployed {
            background: rgba(138, 43, 226, 0.25);
            color: #C084FC;
        }

        .status-tag.running {
            background: rgba(34, 197, 94, 0.25);
            color: #86EFAC;
        }

        .status-tag.stopped {
            background: rgba(239, 68, 68, 0.25);
            color: #FCA5A5;
        }

        .status-tag.draft {
            background: rgba(156, 163, 175, 0.25);
            color: #D1D5DB;
        }
    </style>
</head>
<body>
    <div class="sidebar">
        <div class="logo-section">
            <img src="${logoUri}" alt="Darklang" class="logo-image" />
            <button class="toggle-btn" id="sidebarToggle">
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" fill="currentColor">
                    <path d="M9.78 12.78a.75.75 0 01-1.06 0L4.47 8.53a.75.75 0 010-1.06l4.25-4.25a.75.75 0 011.06 1.06L6.06 8l3.72 3.72a.75.75 0 010 1.06z"/>
                </svg>
            </button>
        </div>

        <nav class="nav-menu">
            <div class="nav-item active" data-page="dashboard">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="10" height="10" viewBox="0 0 10 10" fill="none"><path d="M5.62484 3.40942V2.00692C5.62484 1.90775 5.65734 1.82623 5.72234 1.76234C5.78734 1.69845 5.86789 1.6665 5.964 1.6665H7.99734C8.09373 1.6665 8.17373 1.69845 8.23734 1.76234C8.30095 1.82623 8.33289 1.90775 8.33317 2.00692V3.40942C8.33317 3.50886 8.30067 3.59039 8.23567 3.654C8.17067 3.71789 8.09012 3.74984 7.994 3.74984H5.96067C5.86456 3.74984 5.78456 3.71789 5.72067 3.654C5.65678 3.59012 5.62484 3.50859 5.62484 3.40942ZM1.6665 4.6665V1.99984C1.6665 1.90539 1.699 1.82623 1.764 1.76234C1.829 1.69845 1.90956 1.6665 2.00567 1.6665H4.039C4.13539 1.6665 4.21539 1.69845 4.279 1.76234C4.34262 1.82623 4.37456 1.90539 4.37484 1.99984V4.6665C4.37484 4.76095 4.34234 4.84012 4.27734 4.904C4.21234 4.96789 4.13178 4.99984 4.03567 4.99984H2.00234C1.90623 4.99984 1.82623 4.96789 1.76234 4.904C1.69845 4.84012 1.6665 4.76095 1.6665 4.6665ZM5.62484 7.99984V5.33317C5.62484 5.23873 5.65734 5.15956 5.72234 5.09567C5.78734 5.03178 5.86789 4.99984 5.964 4.99984H7.99734C8.09373 4.99984 8.17373 5.03178 8.23734 5.09567C8.30095 5.15956 8.33289 5.23873 8.33317 5.33317V7.99984C8.33317 8.09428 8.30067 8.17345 8.23567 8.23734C8.17067 8.30123 8.09012 8.33317 7.994 8.33317H5.96067C5.86456 8.33317 5.78456 8.30123 5.72067 8.23734C5.65678 8.17345 5.62484 8.09428 5.62484 7.99984ZM1.6665 7.99275V6.59025C1.6665 6.49109 1.699 6.40956 1.764 6.34567C1.829 6.28178 1.90956 6.24984 2.00567 6.24984H4.039C4.13539 6.24984 4.21539 6.28178 4.279 6.34567C4.34262 6.40956 4.37456 6.49109 4.37484 6.59025V7.99275C4.37484 8.0922 4.34234 8.17373 4.27734 8.23734C4.21234 8.30123 4.13178 8.33317 4.03567 8.33317H2.00234C1.90623 8.33317 1.82623 8.30123 1.76234 8.23734C1.69845 8.17345 1.6665 8.09192 1.6665 7.99275ZM2.08317 4.58317H3.95817V2.08317H2.08317V4.58317ZM6.0415 7.9165H7.9165V5.4165H6.0415V7.9165ZM6.0415 3.33317H7.9165V2.08317H6.0415V3.33317ZM2.08317 7.9165H3.95817V6.6665H2.08317V7.9165Z" fill="#CACACA"/></svg></span>
                <span class="nav-label">Dashboard</span>
            </div>

            <div class="nav-item" data-page="packages">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="8" height="8" viewBox="0 0 8 8" fill="none">
<g clip-path="url(#clip0_163_21713)">
<path d="M1.99984 5.33333V4.33333M3.99984 2V1M5.99984 5.33333V4.33333M3.33317 5V6.33333C3.33317 6.51014 3.26293 6.67971 3.13791 6.80474C3.01288 6.92976 2.84331 7 2.6665 7H1.33317C1.15636 7 0.98679 6.92976 0.861766 6.80474C0.736742 6.67971 0.666504 6.51014 0.666504 6.33333V5C0.666504 4.82319 0.736742 4.65362 0.861766 4.5286C0.98679 4.40357 1.15636 4.33333 1.33317 4.33333H2.6665C2.84331 4.33333 3.01288 4.40357 3.13791 4.5286C3.26293 4.65362 3.33317 4.82319 3.33317 5ZM5.33317 1.66667V3C5.33317 3.17681 5.26293 3.34638 5.13791 3.4714C5.01288 3.59643 4.84331 3.66667 4.6665 3.66667H3.33317C3.15636 3.66667 2.98679 3.59643 2.86177 3.4714C2.73674 3.34638 2.6665 3.17681 2.6665 3V1.66667C2.6665 1.48986 2.73674 1.32029 2.86177 1.19526C2.98679 1.07024 3.15636 1 3.33317 1H4.6665C4.84331 1 5.01288 1.07024 5.13791 1.19526C5.26293 1.32029 5.33317 1.48986 5.33317 1.66667ZM7.33317 5V6.33333C7.33317 6.51014 7.26293 6.67971 7.13791 6.80474C7.01288 6.92976 6.84331 7 6.6665 7H5.33317C5.15636 7 4.98679 6.92976 4.86177 6.80474C4.73674 6.67971 4.6665 6.51014 4.6665 6.33333V5C4.6665 4.82319 4.73674 4.65362 4.86177 4.5286C4.98679 4.40357 5.15636 4.33333 5.33317 4.33333H6.6665C6.84331 4.33333 7.01288 4.40357 7.13791 4.5286C7.26293 4.65362 7.33317 4.82319 7.33317 5Z" stroke="#CACACA" stroke-width="0.375" stroke-linecap="round" stroke-linejoin="round"/>
</g>
<defs>
<clipPath id="clip0_163_21713">
<rect width="8" height="8" fill="white"/>
</clipPath>
</defs>
</svg></span>
                <span class="nav-label">Packages</span>
            </div>

            <div class="nav-item" data-page="branches">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 15 15" fill="none"><path fill-rule="evenodd" clip-rule="evenodd" d="M10.7996 8.49938C10.6936 8.49938 10.5918 8.53889 10.5168 8.60921C10.4418 8.67954 10.3997 8.77491 10.3997 8.87437C10.3997 8.97382 10.4418 9.06919 10.5168 9.13952C10.5918 9.20984 10.6936 9.24935 10.7996 9.24935C10.9057 9.24935 11.0074 9.20984 11.0824 9.13952C11.1574 9.06919 11.1996 8.97382 11.1996 8.87437C11.1996 8.77491 11.1574 8.67954 11.0824 8.60921C11.0074 8.53889 10.9057 8.49938 10.7996 8.49938ZM10.3997 7.81342C10.1327 7.90188 9.90763 8.076 9.76432 8.30499C9.62101 8.53398 9.56866 8.8031 9.61652 9.06478C9.66439 9.32647 9.80938 9.56386 10.0259 9.735C10.2424 9.90613 10.5164 10 10.7996 10C11.0828 10 11.3569 9.90613 11.5734 9.735C11.7899 9.56386 11.9349 9.32647 11.9827 9.06478C12.0306 8.8031 11.9782 8.53398 11.8349 8.30499C11.6916 8.076 11.4666 7.90188 11.1996 7.81342V7.62443C11.1996 7.22662 11.031 6.84511 10.731 6.56382C10.431 6.28253 10.0241 6.1245 9.59979 6.1245H8.39995V5.18555C8.6668 5.09709 8.8917 4.92304 9.03491 4.69415C9.17813 4.46525 9.23042 4.19626 9.18256 3.93471C9.1347 3.67317 8.98976 3.4359 8.77336 3.26486C8.55696 3.09381 8.28304 3 8 3C7.71696 3 7.44304 3.09381 7.22664 3.26486C7.01024 3.4359 6.8653 3.67317 6.81744 3.93471C6.76958 4.19626 6.82187 4.46525 6.96509 4.69415C7.1083 4.92304 7.3332 5.09709 7.60005 5.18555V6.1245H6.40021C5.97592 6.1245 5.56901 6.28253 5.269 6.56382C4.96898 6.84511 4.80043 7.22662 4.80043 7.62443V7.81342C4.53343 7.90188 4.30838 8.076 4.16507 8.30499C4.02176 8.53398 3.96941 8.8031 4.01727 9.06478C4.06514 9.32647 4.21013 9.56386 4.42663 9.735C4.64313 9.90613 4.9172 10 5.20038 10C5.48355 10 5.75762 9.90613 5.97412 9.735C6.19062 9.56386 6.33561 9.32647 6.38348 9.06478C6.43134 8.8031 6.37899 8.53398 6.23568 8.30499C6.09237 8.076 5.86732 7.90188 5.60032 7.81342V7.62443C5.60032 7.42553 5.6846 7.23477 5.8346 7.09412C5.98461 6.95348 6.18807 6.87447 6.40021 6.87447H7.60005V7.81342C7.33306 7.90188 7.10801 8.076 6.9647 8.30499C6.82139 8.53398 6.76903 8.8031 6.8169 9.06478C6.86476 9.32647 7.00976 9.56386 7.22626 9.735C7.44276 9.90613 7.71682 10 8 10C8.28318 10 8.55724 9.90613 8.77374 9.735C8.99024 9.56386 9.13524 9.32647 9.1831 9.06478C9.23097 8.8031 9.17861 8.53398 9.0353 8.30499C8.89199 8.076 8.66694 7.90188 8.39995 7.81342V6.87447H9.59979C9.81193 6.87447 10.0154 6.95348 10.1654 7.09412C10.3154 7.23477 10.3997 7.42553 10.3997 7.62443V7.81342ZM5.20038 8.49938C5.0943 8.49938 4.99257 8.53889 4.91757 8.60921C4.84257 8.67954 4.80043 8.77491 4.80043 8.87437C4.80043 8.97382 4.84257 9.06919 4.91757 9.13952C4.99257 9.20984 5.0943 9.24935 5.20038 9.24935C5.30645 9.24935 5.40818 9.20984 5.48318 9.13952C5.55818 9.06919 5.60032 8.97382 5.60032 8.87437C5.60032 8.77491 5.55818 8.67954 5.48318 8.60921C5.40818 8.53889 5.30645 8.49938 5.20038 8.49938ZM7.60005 8.87437C7.60005 8.77491 7.64219 8.67954 7.7172 8.60921C7.7922 8.53889 7.89393 8.49938 8 8.49938C8.10607 8.49938 8.2078 8.53889 8.2828 8.60921C8.35781 8.67954 8.39995 8.77491 8.39995 8.87437C8.39995 8.97382 8.35781 9.06919 8.2828 9.13952C8.2078 9.20984 8.10607 9.24935 8 9.24935C7.89393 9.24935 7.7922 9.20984 7.7172 9.13952C7.64219 9.06919 7.60005 8.97382 7.60005 8.87437ZM8 3.74962C7.89393 3.74962 7.7922 3.78913 7.7172 3.85945C7.64219 3.92977 7.60005 4.02515 7.60005 4.1246C7.60005 4.22406 7.64219 4.31943 7.7172 4.38976C7.7922 4.46008 7.89393 4.49959 8 4.49959C8.10607 4.49959 8.2078 4.46008 8.2828 4.38976C8.35781 4.31943 8.39995 4.22406 8.39995 4.1246C8.39995 4.02515 8.35781 3.92977 8.2828 3.85945C8.2078 3.78913 8.10607 3.74962 8 3.74962Z" fill="#CACACA"/></svg></span>
                <span class="nav-label">Branches</span>
            </div>

            <div class="nav-item" data-page="apps">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="6" height="6" viewBox="0 0 6 6" fill="none"><g clip-path="url(#clip0_163_21717)"><path d="M0.75 0.75H2.5V2.5H0.75V0.75ZM3.5 3.5H5.25V5.25H3.5V3.5ZM0.75 3.5H2.5V5.25H0.75V3.5ZM5.375 1.625C5.375 1.89022 5.26964 2.14457 5.08211 2.33211C4.89457 2.51964 4.64022 2.625 4.375 2.625C4.10978 2.625 3.85543 2.51964 3.66789 2.33211C3.48036 2.14457 3.375 1.89022 3.375 1.625C3.375 1.35978 3.48036 1.10543 3.66789 0.917893C3.85543 0.730357 4.10978 0.625 4.375 0.625C4.64022 0.625 4.89457 0.730357 5.08211 0.917893C5.26964 1.10543 5.375 1.35978 5.375 1.625Z" stroke="#CACACA" stroke-width="0.5"/></g><defs><clipPath id="clip0_163_21717"><rect width="6" height="6" fill="white"/></clipPath></defs></svg></span>
                <span class="nav-label">Apps</span>
            </div>

            <div class="nav-item" data-page="approval-requests">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="10" height="10" viewBox="0 0 10 10" fill="none"><path d="M7.23775 7.74442L6.96484 7.47109C6.92873 7.43525 6.88609 7.41734 6.83692 7.41734C6.78748 7.41734 6.74484 7.43525 6.709 7.47109C6.67317 7.50664 6.65525 7.54873 6.65525 7.59734C6.65525 7.64595 6.67317 7.68817 6.709 7.724L7.06734 8.08234C7.119 8.13428 7.1772 8.16025 7.24192 8.16025C7.30664 8.16025 7.36484 8.13428 7.4165 8.08234L8.29067 7.22109C8.3265 7.18525 8.34581 7.144 8.34859 7.09734C8.35137 7.05039 8.33206 7.00636 8.29067 6.96525C8.25484 6.92942 8.21095 6.9115 8.159 6.9115C8.10706 6.9115 8.06303 6.92956 8.02692 6.96567L7.23775 7.74442ZM3.029 3.63734H6.9715C7.03067 3.63734 7.08011 3.61748 7.11984 3.57775C7.15956 3.53803 7.17956 3.48845 7.17984 3.429C7.17984 3.36428 7.15984 3.31345 7.11984 3.2765C7.08011 3.23928 7.03067 3.22067 6.9715 3.22067H3.02859C2.96942 3.22067 2.91984 3.24053 2.87984 3.28025C2.83984 3.31998 2.81998 3.36956 2.82025 3.429C2.82053 3.48845 2.84039 3.53803 2.87984 3.57775C2.91928 3.61748 2.96928 3.63734 3.029 3.63734ZM7.49984 9.21484C7.03567 9.21484 6.64192 9.05303 6.31859 8.72942C5.99498 8.40609 5.83317 8.01234 5.83317 7.54817C5.83317 7.084 5.99498 6.69011 6.31859 6.3665C6.6422 6.04289 7.03595 5.88123 7.49984 5.8815C7.96373 5.88178 8.35762 6.04345 8.6815 6.3665C9.00539 6.68956 9.16706 7.08345 9.1665 7.54817C9.1665 8.01206 9.00484 8.40581 8.6815 8.72942C8.35762 9.05303 7.96373 9.21484 7.49984 9.21484ZM1.6665 2.33984C1.6665 2.15317 1.73206 1.99428 1.86317 1.86317C1.99428 1.73206 2.15317 1.6665 2.33984 1.6665H7.66025C7.84636 1.6665 8.00511 1.73206 8.1365 1.86317C8.26761 1.99428 8.33317 2.15317 8.33317 2.33984V4.55525C8.33317 4.61442 8.31331 4.664 8.27359 4.704C8.23387 4.744 8.18428 4.76386 8.12484 4.76359C8.06539 4.76331 8.01581 4.74345 7.97609 4.704C7.93636 4.66456 7.9165 4.61498 7.9165 4.55525V2.33984C7.9165 2.27567 7.88984 2.21678 7.8365 2.16317C7.78317 2.10956 7.72428 2.08289 7.65984 2.08317H2.33984C2.27567 2.08317 2.21678 2.10984 2.16317 2.16317C2.10956 2.2165 2.08289 2.27539 2.08317 2.33984V7.93734H4.7265C4.73511 7.99345 4.74525 8.04956 4.75692 8.10567C4.76886 8.16178 4.78414 8.21706 4.80275 8.2715C4.82137 8.32123 4.80887 8.36192 4.76525 8.39359C4.72137 8.42553 4.6797 8.42692 4.64025 8.39775L4.5365 8.32692C4.50567 8.30664 4.47317 8.2965 4.439 8.2965C4.40484 8.2965 4.3722 8.30664 4.34109 8.32692L3.97567 8.57984C3.94484 8.60039 3.91234 8.61067 3.87817 8.61067C3.844 8.61067 3.81136 8.60039 3.78025 8.57984L3.41484 8.32692C3.38373 8.30664 3.35109 8.2965 3.31692 8.2965C3.28275 8.2965 3.25025 8.30664 3.21942 8.32692L2.854 8.57984C2.82317 8.60039 2.79067 8.61067 2.7565 8.61067C2.72234 8.61067 2.6897 8.60039 2.65859 8.57984L2.29317 8.32692C2.26206 8.30664 2.22942 8.2965 2.19525 8.2965C2.16109 8.2965 2.12859 8.30664 2.09775 8.32692L1.6665 8.6615V2.33984ZM3.029 6.77859H4.64317C4.70261 6.77859 4.7522 6.75873 4.79192 6.719C4.83164 6.67928 4.8515 6.6297 4.8515 6.57025C4.8515 6.51081 4.83164 6.46136 4.79192 6.42192C4.7522 6.38248 4.70261 6.36248 4.64317 6.36192H3.029C2.96984 6.36192 2.92025 6.38192 2.88025 6.42192C2.84025 6.46192 2.82039 6.51136 2.82067 6.57025C2.82095 6.62914 2.84081 6.67873 2.88025 6.719C2.9197 6.75928 2.96928 6.77914 3.029 6.77859ZM3.029 5.20817H6.03067C6.08984 5.20817 6.13942 5.18831 6.17942 5.14859C6.21942 5.10887 6.23928 5.05928 6.239 4.99984C6.23873 4.94039 6.21887 4.89081 6.17942 4.85109C6.13998 4.81136 6.09039 4.7915 6.03067 4.7915H3.02859C2.96942 4.7915 2.91984 4.81136 2.87984 4.85109C2.83984 4.89081 2.82011 4.94039 2.82067 4.99984C2.82123 5.05928 2.84109 5.10887 2.88025 5.14859C2.91942 5.18831 2.969 5.20817 3.029 5.20817Z" fill="#CACACA"/></svg></span>
                <span class="nav-label">Approval Requests</span>
                <span class="nav-badge">3</span>
            </div>

            <div class="nav-item" data-page="contributions">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="6" height="6" viewBox="0 0 8 8" fill="none"><g clip-path="url(#clip0_163_21707)"><path d="M7.98209 0.342903C8.00028 0.297471 8.00473 0.247701 7.9949 0.199763C7.98506 0.151825 7.96138 0.107827 7.92677 0.0732234C7.89217 0.0386202 7.84817 0.0149334 7.80023 0.00509968C7.75229 -0.00473405 7.70252 -0.000282297 7.65709 0.017903L0.383593 2.9274C0.319538 2.95304 0.263798 2.99586 0.222509 3.05114C0.181219 3.10641 0.155981 3.17201 0.149571 3.24071C0.143161 3.30941 0.155829 3.37854 0.186179 3.44051C0.21653 3.50247 0.263387 3.55486 0.321593 3.5919L2.81909 5.1809L3.58459 6.3839C3.62075 6.43849 3.67687 6.47671 3.74091 6.49036C3.80494 6.50401 3.87178 6.49199 3.92705 6.45689C3.98232 6.42179 4.02161 6.36641 4.03648 6.30265C4.05136 6.23889 4.04063 6.17184 4.00659 6.1159L3.31859 5.0349L7.06559 1.2879L6.11809 3.6569C6.10508 3.68752 6.0983 3.72042 6.09816 3.75369C6.09802 3.78696 6.10453 3.81992 6.11729 3.85064C6.13005 3.88136 6.14882 3.90923 6.17249 3.9326C6.19616 3.95598 6.22426 3.9744 6.25514 3.98678C6.28602 3.99915 6.31906 4.00525 6.35232 4.00469C6.38558 4.00414 6.4184 3.99695 6.44885 3.98355C6.4793 3.97015 6.50677 3.95081 6.52965 3.92666C6.55253 3.90251 6.57036 3.87403 6.58209 3.8429L7.98209 0.342903ZM6.71209 0.934403L2.96509 4.6814L0.795593 3.3009L6.71209 0.934403Z" fill="#CACACA"/><path d="M8 6.25C8 6.71413 7.81563 7.15925 7.48744 7.48744C7.15925 7.81563 6.71413 8 6.25 8C5.78587 8 5.34075 7.81563 5.01256 7.48744C4.68437 7.15925 4.5 6.71413 4.5 6.25C4.5 5.78587 4.68437 5.34075 5.01256 5.01256C5.34075 4.68437 5.78587 4.5 6.25 4.5C6.71413 4.5 7.15925 4.68437 7.48744 5.01256C7.81563 5.34075 8 5.78587 8 6.25ZM6.25 5.25C6.1837 5.25 6.12011 5.27634 6.07322 5.32322C6.02634 5.37011 6 5.4337 6 5.5V6H5.5C5.4337 6 5.37011 6.02634 5.32322 6.07322C5.27634 6.12011 5.25 6.1837 5.25 6.25C5.25 6.3163 5.27634 6.37989 5.32322 6.42678C5.37011 6.47366 5.4337 6.5 5.5 6.5H6V7C6 7.0663 6.02634 7.12989 6.07322 7.17678C6.12011 7.22366 6.1837 7.25 6.25 7.25C6.3163 7.25 6.37989 7.22366 6.42678 7.17678C6.47366 7.12989 6.5 7.0663 6.5 7V6.5H7C7.0663 6.5 7.12989 6.47366 7.17678 6.42678C7.22366 6.37989 7.25 6.3163 7.25 6.25C7.25 6.1837 7.22366 6.12011 7.17678 6.07322C7.12989 6.02634 7.0663 6 7 6H6.5V5.5C6.5 5.4337 6.47366 5.37011 6.42678 5.32322C6.37989 5.27634 6.3163 5.25 6.25 5.25Z" fill="#CACACA"/></g><defs><clipPath id="clip0_163_21707"><rect width="8" height="8" fill="white"/></clipPath></defs></svg></span>
                <span class="nav-label">Contributions</span>
                <span class="nav-badge">2</span>
            </div>

            <div class="nav-item" data-page="traces">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="6" height="6" viewBox="0 0 6 6" fill="none"><path d="M4.6875 3.84375C4.51308 3.84399 4.343 3.89818 4.20059 3.99889C4.05818 4.09959 3.9504 4.24188 3.89203 4.40625H1.6875C1.51345 4.40625 1.34653 4.33711 1.22346 4.21404C1.10039 4.09097 1.03125 3.92405 1.03125 3.75C1.03125 3.57595 1.10039 3.40903 1.22346 3.28596C1.34653 3.16289 1.51345 3.09375 1.6875 3.09375H3.9375C4.211 3.09375 4.47331 2.9851 4.6667 2.7917C4.8601 2.59831 4.96875 2.336 4.96875 2.0625C4.96875 1.789 4.8601 1.52669 4.6667 1.3333C4.47331 1.1399 4.211 1.03125 3.9375 1.03125H1.6875C1.61291 1.03125 1.54137 1.06088 1.48863 1.11363C1.43588 1.16637 1.40625 1.23791 1.40625 1.3125C1.40625 1.38709 1.43588 1.45863 1.48863 1.51137C1.54137 1.56412 1.61291 1.59375 1.6875 1.59375H3.9375C4.06182 1.59375 4.18105 1.64314 4.26896 1.73104C4.35686 1.81895 4.40625 1.93818 4.40625 2.0625C4.40625 2.18682 4.35686 2.30605 4.26896 2.39396C4.18105 2.48186 4.06182 2.53125 3.9375 2.53125H1.6875C1.36427 2.53125 1.05427 2.65965 0.825714 2.88821C0.597154 3.11677 0.46875 3.42677 0.46875 3.75C0.46875 4.07323 0.597154 4.38323 0.825714 4.61179C1.05427 4.84035 1.36427 4.96875 1.6875 4.96875H3.89203C3.94359 5.11459 4.03426 5.24345 4.15411 5.34124C4.27396 5.43904 4.41839 5.502 4.57161 5.52325C4.72484 5.5445 4.88095 5.52322 5.02289 5.46174C5.16484 5.40025 5.28715 5.30093 5.37645 5.17463C5.46576 5.04833 5.51863 4.8999 5.52928 4.74558C5.53992 4.59126 5.50794 4.43698 5.43683 4.29961C5.36572 4.16224 5.25821 4.04706 5.12605 3.96666C4.9939 3.88627 4.84219 3.84375 4.6875 3.84375ZM4.6875 4.96875C4.63187 4.96875 4.5775 4.95225 4.53125 4.92135C4.48499 4.89045 4.44895 4.84652 4.42766 4.79513C4.40637 4.74374 4.4008 4.68719 4.41165 4.63263C4.42251 4.57807 4.44929 4.52796 4.48863 4.48863C4.52796 4.44929 4.57807 4.42251 4.63263 4.41165C4.68719 4.4008 4.74374 4.40637 4.79513 4.42766C4.84652 4.44895 4.89045 4.48499 4.92135 4.53125C4.95226 4.5775 4.96875 4.63187 4.96875 4.6875C4.96875 4.76209 4.93912 4.83363 4.88637 4.88637C4.83363 4.93912 4.76209 4.96875 4.6875 4.96875Z" fill="#CACACA"/></svg></span>
                <span class="nav-label">Traces</span>
            </div>

            <div class="nav-item" data-page="settings">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="22" height="22" viewBox="0 0 22 22" fill="none">
<path d="M12.0214 12.3305C12.8453 12.3305 13.5131 11.6627 13.5131 10.8388C13.5131 10.015 12.8453 9.34717 12.0214 9.34717C11.1976 9.34717 10.5298 10.015 10.5298 10.8388C10.5298 11.6627 11.1976 12.3305 12.0214 12.3305Z" stroke="#CACACA" stroke-width="0.5"/>
<path d="M12.899 5.94228C12.7165 5.8667 12.4848 5.8667 12.0214 5.8667C11.558 5.8667 11.3263 5.8667 11.1438 5.94228C11.0231 5.99225 10.9134 6.06554 10.821 6.15793C10.7286 6.25033 10.6553 6.36003 10.6053 6.48077C10.5596 6.59165 10.5412 6.72142 10.5342 6.90987C10.531 7.04607 10.4932 7.17923 10.4245 7.29688C10.3558 7.41454 10.2584 7.51284 10.1414 7.58261C10.0225 7.64912 9.8886 7.68438 9.75233 7.68507C9.61607 7.68577 9.48185 7.65188 9.36225 7.58659C9.19518 7.49808 9.07436 7.44935 8.95453 7.43344C8.69315 7.39907 8.42882 7.46989 8.21964 7.63034C8.06351 7.75117 7.94716 7.95155 7.71545 8.3528C7.48375 8.75406 7.3674 8.95444 7.34204 9.15084C7.32495 9.28035 7.33355 9.41195 7.36733 9.53813C7.40112 9.66431 7.45943 9.78259 7.53894 9.88623C7.61253 9.9817 7.71545 10.0618 7.87506 10.1622C8.11025 10.3099 8.2614 10.5615 8.2614 10.8389C8.2614 11.1164 8.11025 11.368 7.87506 11.5151C7.71545 11.6161 7.61203 11.6961 7.53894 11.7916C7.45943 11.8952 7.40112 12.0135 7.36733 12.1397C7.33355 12.2659 7.32495 12.3975 7.34204 12.527C7.3679 12.7229 7.48375 12.9238 7.71496 13.325C7.94716 13.7263 8.06301 13.9267 8.21964 14.0475C8.32327 14.127 8.44156 14.1853 8.56774 14.2191C8.69392 14.2529 8.82552 14.2615 8.95503 14.2444C9.07436 14.2285 9.19518 14.1797 9.36225 14.0912C9.48185 14.0259 9.61607 13.992 9.75233 13.9927C9.8886 13.9934 10.0225 14.0287 10.1414 14.0952C10.3816 14.2344 10.5243 14.4905 10.5342 14.7679C10.5412 14.9569 10.5591 15.0862 10.6053 15.197C10.6553 15.3178 10.7286 15.4275 10.821 15.5199C10.9134 15.6123 11.0231 15.6856 11.1438 15.7355C11.3263 15.8111 11.558 15.8111 12.0214 15.8111C12.4848 15.8111 12.7165 15.8111 12.899 15.7355C13.0197 15.6856 13.1294 15.6123 13.2218 15.5199C13.3142 15.4275 13.3875 15.3178 13.4375 15.197C13.4832 15.0862 13.5016 14.9569 13.5086 14.7679C13.5185 14.4905 13.6612 14.2339 13.9014 14.0952C14.0203 14.0287 14.1542 13.9934 14.2904 13.9927C14.4267 13.992 14.5609 14.0259 14.6805 14.0912C14.8476 14.1797 14.9684 14.2285 15.0877 14.2444C15.2173 14.2615 15.3489 14.2529 15.475 14.2191C15.6012 14.1853 15.7195 14.127 15.8231 14.0475C15.9798 13.9271 16.0956 13.7263 16.3273 13.325C16.559 12.9238 16.6754 12.7234 16.7007 12.527C16.7178 12.3975 16.7092 12.2659 16.6754 12.1397C16.6417 12.0135 16.5833 11.8952 16.5038 11.7916C16.4302 11.6961 16.3273 11.6161 16.1677 11.5156C16.0513 11.4447 15.9548 11.3454 15.8873 11.2271C15.8197 11.1087 15.7833 10.9752 15.7814 10.8389C15.7814 10.5615 15.9325 10.3099 16.1677 10.1627C16.3273 10.0618 16.4307 9.9817 16.5038 9.88623C16.5833 9.78259 16.6417 9.66431 16.6754 9.53813C16.7092 9.41195 16.7178 9.28035 16.7007 9.15084C16.6749 8.95494 16.559 8.75406 16.3278 8.3528C16.0956 7.95155 15.9798 7.75117 15.8231 7.63034C15.7195 7.55083 15.6012 7.49252 15.475 7.45873C15.3489 7.42495 15.2173 7.41636 15.0877 7.43344C14.9684 7.44935 14.8476 7.49808 14.68 7.58659C14.5605 7.65179 14.4264 7.68563 14.2902 7.68493C14.154 7.68424 14.0202 7.64903 13.9014 7.58261C13.7844 7.51284 13.6869 7.41454 13.6182 7.29688C13.5495 7.17923 13.5118 7.04607 13.5086 6.90987C13.5016 6.72092 13.4837 6.59165 13.4375 6.48077C13.3875 6.36003 13.3142 6.25033 13.2218 6.15793C13.1294 6.06554 13.0197 5.99225 12.899 5.94228Z" stroke="#CACACA" stroke-width="0.5"/>
</svg></span>
                <span class="nav-label">Settings</span>
            </div>

            <div class="nav-item" data-page="changelog">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="7" height="7" viewBox="0 0 7 7" fill="none"><g clip-path="url(#clip0_163_21839)"><path d="M1.74984 3.49984V2.9165C1.74984 2.45238 1.93421 2.00726 2.2624 1.67907C2.59059 1.35088 3.03571 1.1665 3.49984 1.1665C3.96397 1.1665 4.40909 1.35088 4.73727 1.67907C5.06546 2.00726 5.24984 2.45238 5.24984 2.9165V3.49984M1.1665 5.24984C1.1665 5.40455 1.22796 5.55292 1.33736 5.66232C1.44675 5.77171 1.59513 5.83317 1.74984 5.83317C1.90455 5.83317 2.05292 5.77171 2.16232 5.66232C2.27171 5.55292 2.33317 5.40455 2.33317 5.24984C2.33317 5.09513 2.27171 4.94675 2.16232 4.83736C2.05292 4.72796 1.90455 4.6665 1.74984 4.6665C1.59513 4.6665 1.44675 4.72796 1.33736 4.83736C1.22796 4.94675 1.1665 5.09513 1.1665 5.24984ZM4.6665 5.24984C4.6665 5.40455 4.72796 5.55292 4.83736 5.66232C4.94675 5.77171 5.09513 5.83317 5.24984 5.83317C5.40455 5.83317 5.55292 5.77171 5.66232 5.66232C5.77171 5.55292 5.83317 5.40455 5.83317 5.24984C5.83317 5.09513 5.77171 4.94675 5.66232 4.83736C5.55292 4.72796 5.40455 4.6665 5.24984 4.6665C5.09513 4.6665 4.94675 4.72796 4.83736 4.83736C4.72796 4.94675 4.6665 5.09513 4.6665 5.24984Z" stroke="#CACACA" stroke-width="0.75" stroke-linecap="round" stroke-linejoin="round"/><path d="M4.375 2.625L5.25 3.5L6.125 2.625" stroke="#CACACA" stroke-width="0.75" stroke-linecap="round" stroke-linejoin="round"/></g><defs><clipPath id="clip0_163_21839"><rect width="7" height="7" fill="white"/></clipPath></defs></svg></span>
                <span class="nav-label">changelog</span>
                <span class="nav-tag">NEW</span>
            </div>

            <div class="nav-item" data-page="logout">
                <span class="nav-icon"><svg xmlns="http://www.w3.org/2000/svg" width="10" height="10" viewBox="0 0 10 10" fill="none"><path d="M2.33984 8.33317C2.14789 8.33317 1.98775 8.269 1.85942 8.14067C1.73109 8.01234 1.66678 7.85206 1.6665 7.65984V2.33984C1.6665 2.14789 1.73081 1.98775 1.85942 1.85942C1.98803 1.73109 2.14817 1.66678 2.33984 1.6665H5.00775V2.08317H2.33984C2.27567 2.08317 2.21678 2.10984 2.16317 2.16317C2.10956 2.2165 2.08289 2.27539 2.08317 2.33984V7.66025C2.08317 7.72414 2.10984 7.78289 2.16317 7.8365C2.2165 7.89012 2.27525 7.91678 2.33942 7.9165H5.00775V8.33317H2.33984ZM6.859 6.47442L6.5665 6.17442L7.53275 5.20817H3.82984V4.7915H7.53275L6.56609 3.82484L6.85859 3.52567L8.33317 4.99984L6.859 6.47442Z" fill="#CACACA"/></svg></span>
                <span class="nav-label">Logout</span>
            </div>
        </nav>
    </div>

    <div class="main-content">
        <div class="dashboard-header">
            <div class="header-left">
                <span class="header-icon">⊞</span>
                <h1 class="header-title">Dashboard</h1>
            </div>
            <div class="header-right">
                <button class="header-btn">
                    <span>📄</span>
                    <span>Docs</span>
                </button>
                <div class="new-btn-container">
                    <button class="header-btn primary" id="newBtn">
                        <span>+</span>
                        <span>New</span>
                    </button>
                    <div class="new-dropdown" id="newDropdown">
                        <div class="dropdown-item" data-type="httphandler">
                            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                <path d="M22 12h-4l-3 9L9 3l-3 9H2"/>
                            </svg>
                            <span>HTTP Handler</span>
                        </div>
                        <div class="dropdown-item" data-type="db">
                            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                <ellipse cx="12" cy="5" rx="9" ry="3"/>
                                <path d="M21 12c0 1.66-4 3-9 3s-9-1.34-9-3"/>
                                <path d="M3 5v14c0 1.66 4 3 9 3s9-1.34 9-3V5"/>
                            </svg>
                            <span>Database</span>
                        </div>
                    </div>
                </div>
                <div class="user-avatar"></div>
            </div>
        </div>

        <div class="dashboard-content">
            <div class="search-container">
                <div class="search-box">
                    <span class="search-icon">🔍</span>
                    <input
                        type="text"
                        class="search-input"
                        placeholder="Search packages, functions, apps, docs ..."
                    />
                </div>
            </div>

            <div class="section">
                <div class="section-header">
                    <h2 class="section-title">Recent</h2>
                </div>
                <div class="cards-grid">
                    <div class="project-card" data-project="feature-x">
                        <div class="card-icon-wrapper"><svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18" fill="none">
<path d="M12.9377 6.7501C12.2925 6.7502 11.6669 6.97215 11.1659 7.37874C10.6649 7.78533 10.3189 8.35183 10.186 8.98322C8.06873 8.8426 6.14948 7.83347 5.71636 6.67247C6.39026 6.51088 6.98078 6.10617 7.37465 5.53597C7.76852 4.96578 7.938 4.27024 7.85059 3.58276C7.76318 2.89529 7.42504 2.26429 6.90101 1.8108C6.37699 1.3573 5.70398 1.11325 5.01108 1.12544C4.31818 1.13763 3.65418 1.40521 3.14643 1.87685C2.63868 2.3485 2.32294 2.991 2.25976 3.68112C2.19659 4.37124 2.39043 5.06039 2.80411 5.61638C3.2178 6.17237 3.82218 6.55606 4.50136 6.69385V11.3063C3.81846 11.4457 3.21164 11.8337 2.79856 12.3951C2.38547 12.9565 2.19555 13.6513 2.2656 14.3447C2.33565 15.0382 2.66069 15.6809 3.17771 16.1483C3.69472 16.6157 4.36687 16.8745 5.06386 16.8745C5.76084 16.8745 6.43299 16.6157 6.95 16.1483C7.46702 15.6809 7.79206 15.0382 7.86211 14.3447C7.93217 13.6513 7.74224 12.9565 7.32915 12.3951C6.91607 11.8337 6.30925 11.4457 5.62636 11.3063V8.39935C6.92555 9.43543 8.5192 10.034 10.1792 10.1093C10.2828 10.632 10.5328 11.1146 10.9 11.5007C11.2672 11.8869 11.7365 12.1607 12.2533 12.2905C12.7702 12.4202 13.3132 12.4004 13.8193 12.2335C14.3253 12.0665 14.7735 11.7593 15.1116 11.3475C15.4498 10.9357 15.664 10.4362 15.7294 9.9074C15.7947 9.37856 15.7084 8.84204 15.4806 8.36031C15.2529 7.87859 14.8929 7.47149 14.4427 7.18642C13.9925 6.90135 13.4706 6.75003 12.9377 6.7501ZM3.37523 3.9376C3.37523 3.49005 3.55302 3.06082 3.86949 2.74436C4.18596 2.42789 4.61518 2.2501 5.06273 2.2501C5.51028 2.2501 5.93951 2.42789 6.25597 2.74436C6.57244 3.06082 6.75023 3.49005 6.75023 3.9376C6.75023 4.38515 6.57244 4.81437 6.25597 5.13084C5.93951 5.44731 5.51028 5.6251 5.06273 5.6251C4.61518 5.6251 4.18596 5.44731 3.86949 5.13084C3.55302 4.81437 3.37523 4.38515 3.37523 3.9376ZM6.75023 14.0626C6.75605 14.2878 6.71671 14.5119 6.63455 14.7216C6.55239 14.9314 6.42906 15.1226 6.27184 15.2839C6.11462 15.4452 5.92669 15.5735 5.71912 15.661C5.51156 15.7486 5.28857 15.7937 5.06329 15.7937C4.83802 15.7937 4.61503 15.7486 4.40746 15.661C4.1999 15.5735 4.01197 15.4452 3.85475 15.2839C3.69752 15.1226 3.5742 14.9314 3.49203 14.7216C3.40987 14.5119 3.37054 14.2878 3.37636 14.0626C3.37636 13.615 3.55415 13.1858 3.87061 12.8694C4.18708 12.5529 4.6163 12.3751 5.06386 12.3751C5.51141 12.3751 5.94063 12.5529 6.2571 12.8694C6.57357 13.1858 6.75023 13.615 6.75023 14.0626ZM12.9389 11.2501C12.4913 11.2501 12.0621 11.0723 11.7456 10.7558C11.4291 10.4394 11.2514 10.0102 11.2514 9.5626C11.2514 9.11505 11.4291 8.68582 11.7456 8.36936C12.0621 8.05289 12.4913 7.8751 12.9389 7.8751C13.3864 7.8751 13.8156 8.05289 14.1321 8.36936C14.4486 8.68582 14.6264 9.11505 14.6264 9.5626C14.6264 10.0102 14.4486 10.4394 14.1321 10.7558C13.8156 11.0723 13.3864 11.2501 12.9389 11.2501Z" fill="#8599FA"/>
</svg></div>
                        <div class="card-content">
                            <div class="card-title">feature-x</div>
                            <div class="card-meta">
                                <span class="card-meta-item">Branch</span>
                                <span class="meta-separator">.</span>
                                <span class="card-meta-item">22 ops</span>
                                <span class="meta-separator">.</span>
                                <span class="card-meta-item">2h ago</span>
                            </div>
                        </div>
                        <span class="card-arrow">→</span>
                    </div>

                    <div class="project-card" data-project="portfolio-site">
                        <div class="card-icon-wrapper"><svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18" fill="none">
<g clip-path="url(#clip0_160_18180)">
<path d="M4.125 9C4.125 10.2929 4.63861 11.5329 5.55285 12.4471C6.46709 13.3614 7.70707 13.875 9 13.875C10.2929 13.875 11.5329 13.3614 12.4471 12.4471C13.3614 11.5329 13.875 10.2929 13.875 9C13.875 7.70707 13.3614 6.46709 12.4471 5.55285C11.5329 4.63861 10.2929 4.125 9 4.125C7.70707 4.125 6.46709 4.63861 5.55285 5.55285C4.63861 6.46709 4.125 7.70707 4.125 9Z" stroke="#8599FA" stroke-width="1.125" stroke-linecap="round" stroke-linejoin="round"/>
<path d="M4.125 9H13.875M9 4.125C11.4375 7.375 11.4375 10.625 9 13.875C6.5625 10.625 6.5625 7.375 9 4.125ZM8.4375 1.125C8.4375 1.27418 8.49676 1.41726 8.60225 1.52275C8.70774 1.62824 8.85082 1.6875 9 1.6875C9.14918 1.6875 9.29226 1.62824 9.39775 1.52275C9.50324 1.41726 9.5625 1.27418 9.5625 1.125C9.5625 0.975816 9.50324 0.832742 9.39775 0.727252C9.29226 0.621763 9.14918 0.5625 9 0.5625C8.85082 0.5625 8.70774 0.621763 8.60225 0.727252C8.49676 0.832742 8.4375 0.975816 8.4375 1.125ZM14.0062 3.43125C14.0062 3.58043 14.0655 3.72351 14.171 3.829C14.2765 3.93449 14.4196 3.99375 14.5687 3.99375C14.7179 3.99375 14.861 3.93449 14.9665 3.829C15.072 3.72351 15.1312 3.58043 15.1312 3.43125C15.1312 3.28207 15.072 3.13899 14.9665 3.0335C14.861 2.92801 14.7179 2.86875 14.5687 2.86875C14.4196 2.86875 14.2765 2.92801 14.171 3.0335C14.0655 3.13899 14.0062 3.28207 14.0062 3.43125ZM16.3125 9C16.3125 9.14918 16.3718 9.29226 16.4773 9.39775C16.5827 9.50324 16.7258 9.5625 16.875 9.5625C17.0242 9.5625 17.1673 9.50324 17.2727 9.39775C17.3782 9.29226 17.4375 9.14918 17.4375 9C17.4375 8.85082 17.3782 8.70774 17.2727 8.60225C17.1673 8.49676 17.0242 8.4375 16.875 8.4375C16.7258 8.4375 16.5827 8.49676 16.4773 8.60225C16.3718 8.70774 16.3125 8.85082 16.3125 9ZM14.0062 14.5687C14.0062 14.7179 14.0655 14.861 14.171 14.9665C14.2765 15.072 14.4196 15.1312 14.5687 15.1312C14.7179 15.1312 14.861 15.072 14.9665 14.9665C15.072 14.861 15.1312 14.7179 15.1312 14.5687C15.1312 14.4196 15.072 14.2765 14.9665 14.171C14.861 14.0655 14.7179 14.0062 14.5687 14.0062C14.4196 14.0062 14.2765 14.0655 14.171 14.171C14.0655 14.2765 14.0062 14.4196 14.0062 14.5687ZM8.4375 16.875C8.4375 17.0242 8.49676 17.1673 8.60225 17.2727C8.70774 17.3782 8.85082 17.4375 9 17.4375C9.14918 17.4375 9.29226 17.3782 9.39775 17.2727C9.50324 17.1673 9.5625 17.0242 9.5625 16.875C9.5625 16.7258 9.50324 16.5827 9.39775 16.4773C9.29226 16.3718 9.14918 16.3125 9 16.3125C8.85082 16.3125 8.70774 16.3718 8.60225 16.4773C8.49676 16.5827 8.4375 16.7258 8.4375 16.875ZM2.86875 14.5687C2.86875 14.7179 2.92801 14.861 3.0335 14.9665C3.13899 15.072 3.28207 15.1312 3.43125 15.1312C3.58043 15.1312 3.72351 15.072 3.829 14.9665C3.93449 14.861 3.99375 14.7179 3.99375 14.5687C3.99375 14.4196 3.93449 14.2765 3.829 14.171C3.72351 14.0655 3.58043 14.0062 3.43125 14.0062C3.28207 14.0062 3.13899 14.0655 3.0335 14.171C2.92801 14.2765 2.86875 14.4196 2.86875 14.5687ZM0.5625 9C0.5625 9.14918 0.621763 9.29226 0.727252 9.39775C0.832742 9.50324 0.975816 9.5625 1.125 9.5625C1.27418 9.5625 1.41726 9.50324 1.52275 9.39775C1.62824 9.29226 1.6875 9.14918 1.6875 9C1.6875 8.85082 1.62824 8.70774 1.52275 8.60225C1.41726 8.49676 1.27418 8.4375 1.125 8.4375C0.975816 8.4375 0.832742 8.49676 0.727252 8.60225C0.621763 8.70774 0.5625 8.85082 0.5625 9ZM2.86875 3.43125C2.86875 3.58043 2.92801 3.72351 3.0335 3.829C3.13899 3.93449 3.28207 3.99375 3.43125 3.99375C3.58043 3.99375 3.72351 3.93449 3.829 3.829C3.93449 3.72351 3.99375 3.58043 3.99375 3.43125C3.99375 3.28207 3.93449 3.13899 3.829 3.0335C3.72351 2.92801 3.58043 2.86875 3.43125 2.86875C3.28207 2.86875 3.13899 2.92801 3.0335 3.0335C2.92801 3.13899 2.86875 3.28207 2.86875 3.43125Z" stroke="#8599FA" stroke-width="1.125" stroke-linecap="round" stroke-linejoin="round"/>
</g>
<defs>
<clipPath id="clip0_160_18180">
<rect width="18" height="18" fill="white"/>
</clipPath>
</defs>
</svg></div>
                        <div class="card-content">
                            <div class="card-title">portfolio-site</div>
                            <div class="card-meta">
                                <span class="card-meta-item">App</span>
                                <span class="meta-separator">.</span>
                                <span class="card-meta-item">Deployed</span>
                                <span class="meta-separator">.</span>
                                <span class="card-meta-item">200req/day</span>
                            </div>
                        </div>
                        <span class="card-arrow">→</span>
                    </div>

                    <div class="project-card" data-project="darklang-cli">
                        <div class="card-icon-wrapper"><svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18" fill="none">
<path d="M16.2529 4.18945L9.50287 0.81445C9.34672 0.736422 9.17456 0.695801 9 0.695801C8.82544 0.695801 8.65327 0.736422 8.49712 0.81445L1.74712 4.18945C1.56028 4.28281 1.40312 4.42635 1.29325 4.60398C1.18337 4.78162 1.12511 4.98633 1.125 5.1952V12.8047C1.12511 13.0136 1.18337 13.2183 1.29325 13.3959C1.40312 13.5736 1.56028 13.7171 1.74712 13.8105L8.49712 17.1855C8.65327 17.2635 8.82544 17.3041 9 17.3041C9.17456 17.3041 9.34672 17.2635 9.50287 17.1855L16.2529 13.8105C16.4397 13.7171 16.5969 13.5736 16.7068 13.3959C16.8166 13.2183 16.8749 13.0136 16.875 12.8047V5.1952C16.8749 4.98633 16.8166 4.78162 16.7068 4.60398C16.5969 4.42635 16.4397 4.28281 16.2529 4.18945ZM6.60487 6.63408L12.6157 3.62808L15.0435 4.84195L9 7.81083L6.60487 6.63408ZM9 1.8202L11.358 2.9992L5.33588 6.01083L2.9565 4.84195L9 1.8202ZM2.25 5.7487L4.78125 6.99183V10.125C4.78125 10.2741 4.84051 10.4172 4.946 10.5227C5.05149 10.6282 5.19457 10.6875 5.34375 10.6875C5.49293 10.6875 5.63601 10.6282 5.7415 10.5227C5.84699 10.4172 5.90625 10.2741 5.90625 10.125V7.5442L8.4375 8.78733V15.8985L2.25 12.8047V5.7487ZM9.5625 15.8985V8.78845L15.75 5.7487V12.8047L9.5625 15.8985Z" fill="#BF9F85"/>
</svg></div>
                        <div class="card-content">
                            <div class="card-title">Darklang.CLI</div>
                            <div class="card-meta">
                                <span class="card-meta-item">Package</span>
                                <span class="meta-separator">.</span>
                                <span class="card-meta-item">22fns</span>
                                <span class="meta-separator">|</span>
                                <span class="card-meta-item">2types</span>
                                <span class="meta-separator">|</span>
                                <span class="card-meta-item">3vals</span>
                            </div>
                        </div>
                        <span class="card-arrow">→</span>
                    </div>
                </div>
            </div>

            <div class="section">
                <div class="section-header">
                    <h2 class="section-title">Pinned</h2>
                    <button class="section-action">+ Add pin</button>
                </div>
                <div class="cards-grid">
                    <div class="project-card" data-project="portfolio-site">
                        <div class="card-content">
                            <div class="card-title">portfolio-site</div>
                            <div class="card-meta">
                                <span class="status-tag deployed">Deployed</span>
                                <span class="card-meta-item">200req/day</span>
                                <span class="meta-dot"></span>
                                <span class="card-meta-item">john.dev</span>
                            </div>
                        </div>
                        <span class="card-arrow">→</span>
                    </div>

                    <div class="project-card" data-project="darklang-cli">
                        <div class="card-content">
                            <div class="card-title">darklang-cli</div>
                            <div class="card-meta">
                                <span class="status-tag running">Running</span>
                                <span class="card-meta-item">200req/day</span>
                            </div>
                        </div>
                        <span class="card-arrow">→</span>
                    </div>

                    <div class="project-card" data-project="api-service">
                        <div class="card-content">
                            <div class="card-title">api-service</div>
                            <div class="card-meta">
                                <span class="status-tag running">Running</span>
                                <span class="card-meta-item">1.2k req/day</span>
                                <span class="meta-dot"></span>
                                <span class="card-meta-item">alice.dev</span>
                            </div>
                        </div>
                        <span class="card-arrow">→</span>
                    </div>

                    <div class="project-card" data-project="String.toUpperCase">
                        <div class="card-content">
                            <div class="card-title">String.toUpperCase</div>
                            <div class="card-meta">
                                <span class="card-meta-item">Function</span>
                                <span class="meta-separator">.</span>
                                <span class="card-meta-item">Stdlib.String</span>
                            </div>
                        </div>
                        <span class="card-arrow">→</span>
                    </div>

                    <div class="project-card" data-project="blog-site">
                        <div class="card-content">
                            <div class="card-title">blog-site</div>
                            <div class="card-meta">
                                <span class="status-tag stopped">Stopped</span>
                                <span class="card-meta-item">App</span>
                                <span class="meta-dot"></span>
                                <span class="card-meta-item">jane.dev</span>
                            </div>
                        </div>
                        <span class="card-arrow">→</span>
                    </div>

                    <div class="project-card" data-project="Http">
                        <div class="card-content">
                            <div class="card-title">Http</div>
                            <div class="card-meta">
                                <span class="card-meta-item">Module</span>
                                <span class="meta-separator">.</span>
                                <span class="card-meta-item">15 fns</span>
                                <span class="meta-separator">.</span>
                                <span class="card-meta-item">Stdlib</span>
                            </div>
                        </div>
                        <span class="card-arrow">→</span>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script>
        const vscode = acquireVsCodeApi();

        // Handle sidebar toggle
        const toggleBtn = document.getElementById('sidebarToggle');
        const sidebar = document.querySelector('.sidebar');

        toggleBtn?.addEventListener('click', () => {
            sidebar?.classList.toggle('collapsed');
        });

        // Handle navigation item clicks
        document.querySelectorAll('.nav-item').forEach(item => {
            item.addEventListener('click', () => {
                // Update active state
                document.querySelectorAll('.nav-item').forEach(i => i.classList.remove('active'));
                item.classList.add('active');

                // Send message to extension
                const page = item.getAttribute('data-page');
                vscode.postMessage({
                    command: 'navigate',
                    page: page
                });
            });
        });

        // Handle project card clicks
        document.querySelectorAll('.project-card').forEach(card => {
            card.addEventListener('click', () => {
                const project = card.getAttribute('data-project');
                vscode.postMessage({
                    command: 'openProject',
                    project: project
                });
            });
        });

        // Handle header button clicks
        document.querySelectorAll('.header-btn').forEach(btn => {
            btn.addEventListener('click', (e) => {
                e.stopPropagation();
                const text = btn.textContent.trim();
                if (text.includes('Docs')) {
                    vscode.postMessage({ command: 'openDocs' });
                }
            });
        });

        // Handle New button dropdown
        const newBtn = document.getElementById('newBtn');
        const newDropdown = document.getElementById('newDropdown');

        newBtn?.addEventListener('click', (e) => {
            e.stopPropagation();
            newDropdown?.classList.toggle('show');
        });

        // Close dropdown when clicking outside
        document.addEventListener('click', () => {
            newDropdown?.classList.remove('show');
        });

        // Handle dropdown item clicks
        document.querySelectorAll('.dropdown-item').forEach(item => {
            item.addEventListener('click', (e) => {
                e.stopPropagation();
                const type = item.getAttribute('data-type');
                newDropdown?.classList.remove('show');
                vscode.postMessage({
                    command: 'createNew',
                    type: type
                });
            });
        });

        // Handle search input
        const searchInput = document.querySelector('.search-input');
        searchInput?.addEventListener('input', (e) => {
            vscode.postMessage({
                command: 'search',
                query: e.target.value
            });
        });

        // Handle "Add pin" button
        document.querySelector('.section-action')?.addEventListener('click', (e) => {
            e.stopPropagation();
            vscode.postMessage({
                command: 'addPin'
            });
        });
    </script>
</body>
</html>`;
  }
}
