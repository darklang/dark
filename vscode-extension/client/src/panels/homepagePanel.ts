import * as vscode from "vscode";
import { renderDashboard, getDefaultDashboardData } from "./pages/dashboardPage";
import { renderSidebar, navIcons } from "./components/sidebar";
import { renderHeader } from "./components/header";

export type PageName = "dashboard" | "packages" | "branches" | "apps" | "approval-requests" | "contributions" | "traces" | "settings" | "changelog" | "logout";

/** Homepage Panel - Display Darklang homepage with account info */
export class HomepagePanel {
  public static currentPanel: HomepagePanel | undefined;
  public static readonly viewType = "darklangHomepage";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: vscode.Uri;
  private _disposables: vscode.Disposable[] = [];
  private _currentPage: PageName = "dashboard";

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
        }
      },
      null,
      this._disposables,
    );
  }

  private _handleNavigation(page: string) {
    const validPages: PageName[] = ["dashboard", "packages", "branches", "apps", "approval-requests", "contributions", "traces", "settings", "changelog", "logout"];

    if (!validPages.includes(page as PageName)) {
      return;
    }

    if (page === "logout") {
      vscode.window.showInformationMessage("Logout functionality coming soon!");
      return;
    }

    if (page === "dashboard") {
      this._currentPage = "dashboard";
      this._update();
      return;
    }

    // For pages not yet implemented
    vscode.window.showInformationMessage(`${page} page coming soon!`);
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

  private _renderHeader(): string {
    // Map page names to display titles
    const pageTitles: Record<PageName, string> = {
      dashboard: "Dashboard",
      packages: "Packages",
      branches: "Branches",
      apps: "Apps",
      "approval-requests": "Approval Requests",
      contributions: "Contributions",
      traces: "Traces",
      settings: "Settings",
      changelog: "Changelog",
      logout: "Logout",
    };

    return renderHeader({
      title: pageTitles[this._currentPage] || "Dashboard",
      icon: navIcons[this._currentPage] || navIcons.dashboard,
    });
  }

  private _renderCurrentPage(): string {
    switch (this._currentPage) {
      case "dashboard":
        return renderDashboard(getDefaultDashboardData());
      default:
        return `<div class="dashboard-content"><p>Page not implemented: ${this._currentPage}</p></div>`;
    }
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
            width: 220px;
            background-color: var(--vscode-sideBar-background);
            border-right: 1px solid var(--vscode-sideBar-border);
            display: flex;
            flex-direction: column;
            padding: 28px 14px;
            transition: width 0.3s ease, padding 0.3s ease;
        }

        .sidebar.collapsed {
            width: 60px;
            padding: 28px 8px;
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
            padding: 10px 8px;
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
            margin-bottom: 36px;
            padding: 0 14px;
            display: flex;
            align-items: center;
            gap: 10px;
        }

        .logo-image {
            width: 100%;
            height: auto;
            max-width: 120px;
            flex: 1;
        }

        .nav-menu {
            flex: 1;
            display: flex;
            flex-direction: column;
            gap: 4px;
        }

        .nav-item {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 10px 14px;
            border-radius: 5px;
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
            font-size: 14px;
            width: 18px;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .nav-icon svg {
            width: 16px;
            height: 16px;
        }

        .nav-item[data-page="branches"] .nav-icon svg,
        .nav-item[data-page="settings"] .nav-icon svg {
            width: 18px;
            height: 18px;
        }

        .nav-label {
            font-size: 13px;
            font-weight: 400;
        }

        .nav-badge {
            margin-left: auto;
            background-color: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
            border-radius: 8px;
            padding: 1px 6px;
            font-size: 10px;
            font-weight: 600;
        }

        .nav-tag {
            margin-left: auto;
            background-color: #0080FF;
            color: white;
            border-radius: 3px;
            padding: 1px 5px;
            font-size: 9px;
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
            padding: 14px 36px;
            border-bottom: 1px solid var(--vscode-panel-border);
            background-color: var(--vscode-editor-background);
        }

        .header-left {
            display: flex;
            align-items: center;
            gap: 10px;
        }

        .header-icon {
            font-size: 14px;
        }

        .header-title {
            font-size: 15px;
            font-weight: 500;
        }

        .header-right {
            display: flex;
            align-items: center;
            gap: 10px;
        }

        .header-btn {
            padding: 6px 12px;
            border-radius: 4px;
            border: 1px solid var(--vscode-button-border);
            background-color: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
            cursor: pointer;
            font-size: 12px;
            display: flex;
            align-items: center;
            gap: 5px;
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

        .user-avatar {
            width: 28px;
            height: 28px;
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
            padding: 28px 36px;
        }

        .search-container {
            margin-bottom: 32px;
        }

        .search-box {
            position: relative;
            width: 100%;
            max-width: 600px;
            margin: 0 auto;
        }

        .search-icon {
            position: absolute;
            left: 14px;
            top: 50%;
            transform: translateY(-50%);
            color: var(--vscode-input-placeholderForeground);
            font-size: 14px;
        }

        .search-input {
            width: 100%;
            padding: 12px 16px 12px 42px;
            background: var(--vscode-input-background);
            border: 1px solid var(--vscode-input-border);
            border-radius: 6px;
            color: var(--vscode-input-foreground);
            font-size: 13px;
            outline: none;
        }

        .search-input:focus {
            border-color: var(--vscode-focusBorder);
        }

        .search-input::placeholder {
            color: var(--vscode-input-placeholderForeground);
        }

        .section {
            margin-bottom: 32px;
        }

        .section-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 16px;
        }

        .section-title {
            font-size: 17px;
            font-weight: 600;
            color: var(--vscode-foreground);
        }

        .section-action {
            padding: 4px 10px;
            background: transparent;
            border: 1px solid var(--vscode-button-border);
            border-radius: 4px;
            color: var(--vscode-foreground);
            cursor: pointer;
            font-size: 11px;
            display: flex;
            align-items: center;
            gap: 4px;
            transition: all 0.2s;
        }

        .section-action:hover {
            background: var(--vscode-button-secondaryHoverBackground);
        }

        .cards-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
            gap: 14px;
        }

        .project-card {
            background: var(--vscode-sideBar-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 6px;
            padding: 16px;
            cursor: pointer;
            transition: all 0.2s ease;
            display: flex;
            align-items: center;
            gap: 14px;
            position: relative;
        }

        .project-card:hover {
            background: var(--vscode-list-hoverBackground);
            border-color: var(--vscode-focusBorder);
            transform: translateY(-1px);
        }

        .card-icon-wrapper {
            width: 40px;
            height: 40px;
            background: var(--vscode-button-secondaryBackground);
            border-radius: 8px;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 20px;
            flex-shrink: 0;
        }

        .card-content {
            flex: 1;
            min-width: 0;
        }

        .card-title {
            font-size: 13px;
            font-weight: 600;
            margin-bottom: 4px;
            color: var(--vscode-foreground);
        }

        .card-meta {
            display: flex;
            align-items: center;
            gap: 6px;
            font-size: 11px;
            color: var(--vscode-descriptionForeground);
            flex-wrap: wrap;
        }

        .card-meta-item {
            display: flex;
            align-items: center;
        }

        .meta-separator {
            margin: 0 2px;
        }

        .meta-dot {
            width: 4px;
            height: 4px;
            background-color: #86EFAC;
            border-radius: 50%;
            margin: 0 4px;
        }

        .card-arrow {
            position: absolute;
            right: 16px;
            top: 50%;
            transform: translateY(-50%);
            color: var(--vscode-descriptionForeground);
            font-size: 14px;
            opacity: 0.6;
        }

        .project-card:hover .card-arrow {
            opacity: 1;
            transform: translateY(-50%) translateX(3px);
        }

        .status-tag {
            padding: 2px 7px;
            border-radius: 3px;
            font-size: 9px;
            font-weight: 600;
            text-transform: uppercase;
            letter-spacing: 0.4px;
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

        /* Responsive styles for smaller screens */
        @media (max-width: 1400px) {
            .sidebar {
                width: 190px;
                padding: 22px 12px;
            }

            .logo-section {
                margin-bottom: 28px;
                padding: 0 12px;
            }

            .logo-image {
                max-width: 100px;
            }

            .nav-item {
                padding: 8px 12px;
                gap: 10px;
            }

            .nav-label {
                font-size: 12px;
            }

            .dashboard-header {
                padding: 12px 28px;
            }

            .dashboard-content {
                padding: 22px 28px;
            }

            .section-title {
                font-size: 15px;
            }

            .cards-grid {
                grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
                gap: 12px;
            }

            .project-card {
                padding: 14px;
                gap: 12px;
            }

            .card-icon-wrapper {
                width: 36px;
                height: 36px;
                font-size: 18px;
            }

            .card-title {
                font-size: 12px;
            }

            .card-meta {
                font-size: 10px;
            }

            .search-input {
                padding: 10px 14px 10px 38px;
                font-size: 12px;
            }

            .search-icon {
                left: 12px;
                font-size: 13px;
            }

            .search-container {
                margin-bottom: 24px;
            }

            .section {
                margin-bottom: 24px;
            }
        }

        @media (max-width: 1200px) {
            .sidebar {
                width: 170px;
                padding: 18px 10px;
            }

            .sidebar.collapsed {
                width: 50px;
            }

            .logo-section {
                margin-bottom: 22px;
                padding: 0 10px;
            }

            .logo-image {
                max-width: 90px;
            }

            .nav-item {
                padding: 7px 10px;
                gap: 8px;
            }

            .nav-label {
                font-size: 11px;
            }

            .nav-icon svg {
                width: 14px;
                height: 14px;
            }

            .nav-item[data-page="branches"] .nav-icon svg,
            .nav-item[data-page="settings"] .nav-icon svg {
                width: 16px;
                height: 16px;
            }

            .nav-badge {
                padding: 1px 5px;
                font-size: 9px;
            }

            .nav-tag {
                padding: 1px 4px;
                font-size: 8px;
            }

            .dashboard-header {
                padding: 10px 22px;
            }

            .header-title {
                font-size: 14px;
            }

            .header-btn {
                padding: 5px 10px;
                font-size: 11px;
            }

            .user-avatar {
                width: 24px;
                height: 24px;
            }

            .dashboard-content {
                padding: 18px 22px;
            }

            .search-container {
                margin-bottom: 20px;
            }

            .section {
                margin-bottom: 20px;
            }

            .section-header {
                margin-bottom: 12px;
            }

            .section-title {
                font-size: 14px;
            }

            .section-action {
                padding: 3px 8px;
                font-size: 10px;
            }

            .cards-grid {
                grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
                gap: 10px;
            }

            .project-card {
                padding: 12px;
                gap: 10px;
            }

            .card-icon-wrapper {
                width: 32px;
                height: 32px;
                font-size: 16px;
                border-radius: 6px;
            }

            .card-title {
                font-size: 11px;
                margin-bottom: 3px;
            }

            .card-meta {
                font-size: 9px;
                gap: 4px;
            }

            .card-arrow {
                right: 12px;
                font-size: 12px;
            }

            .status-tag {
                padding: 2px 5px;
                font-size: 8px;
            }

            .search-input {
                padding: 9px 12px 9px 34px;
                font-size: 11px;
            }

            .search-icon {
                left: 10px;
                font-size: 12px;
            }
        }

        @media (max-width: 1024px) {
            .sidebar {
                width: 150px;
                padding: 16px 8px;
            }

            .sidebar.collapsed {
                width: 44px;
            }

            .logo-section {
                margin-bottom: 18px;
                padding: 0 8px;
                gap: 6px;
            }

            .logo-image {
                max-width: 80px;
            }

            .nav-item {
                padding: 6px 8px;
                gap: 6px;
            }

            .nav-label {
                font-size: 10px;
            }

            .nav-icon svg {
                width: 12px;
                height: 12px;
            }

            .nav-item[data-page="branches"] .nav-icon svg,
            .nav-item[data-page="settings"] .nav-icon svg {
                width: 14px;
                height: 14px;
            }

            .nav-badge {
                padding: 1px 4px;
                font-size: 8px;
            }

            .nav-tag {
                padding: 1px 3px;
                font-size: 7px;
            }

            .dashboard-header {
                padding: 8px 18px;
            }

            .header-title {
                font-size: 13px;
            }

            .header-btn {
                padding: 4px 8px;
                font-size: 10px;
            }

            .user-avatar {
                width: 22px;
                height: 22px;
            }

            .dashboard-content {
                padding: 14px 18px;
            }

            .search-container {
                margin-bottom: 16px;
            }

            .section {
                margin-bottom: 16px;
            }

            .section-header {
                margin-bottom: 10px;
            }

            .section-title {
                font-size: 13px;
            }

            .section-action {
                padding: 3px 6px;
                font-size: 9px;
            }

            .cards-grid {
                grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
                gap: 8px;
            }

            .project-card {
                padding: 10px;
                gap: 8px;
            }

            .card-icon-wrapper {
                width: 28px;
                height: 28px;
                font-size: 14px;
                border-radius: 5px;
            }

            .card-title {
                font-size: 10px;
                margin-bottom: 2px;
            }

            .card-meta {
                font-size: 8px;
                gap: 3px;
            }

            .card-arrow {
                right: 10px;
                font-size: 10px;
            }

            .status-tag {
                padding: 1px 4px;
                font-size: 7px;
            }

            .search-input {
                padding: 8px 10px 8px 30px;
                font-size: 10px;
            }

            .search-icon {
                left: 8px;
                font-size: 10px;
            }

            .meta-dot {
                width: 3px;
                height: 3px;
                margin: 0 3px;
            }
        }
    </style>
</head>
<body>
    ${renderSidebar(logoUri.toString(), this._currentPage)}

    <div class="main-content">
        ${this._renderHeader()}
        ${this._renderCurrentPage()}
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
                vscode.postMessage({
                    command: text.includes('New') ? 'createNew' : 'openDocs'
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
