import * as vscode from "vscode";
import { renderDashboard, getDefaultDashboardData, DashboardData, DashboardPinnedItem, DashboardRecentItem } from "./pages/dashboardPage";
import { renderSidebar, navIcons } from "./components/sidebar";
import { renderHeader } from "./components/header";
import { homepageStyles } from "./styles/homepageStyles";
import { PinnedItemsService } from "../services/pinnedItemsService";
import { RecentItemsService } from "../services/recentItemsService";

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

    // Subscribe to pinned items changes
    this._disposables.push(
      PinnedItemsService.onDidChange(() => this._update())
    );

    // Subscribe to recent items changes
    this._disposables.push(
      RecentItemsService.onDidChange(() => this._update())
    );

    // Load pinned items and then update the view
    PinnedItemsService.load()
      .then(() => this._update())
      .catch(err => {
        console.error("Failed to load pinned items:", err);
        this._update();
      });

    // Listen for when the panel is disposed
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    // Handle messages from the webview
    this._panel.webview.onDidReceiveMessage(
      message => {
        switch (message.command) {
          case "navigate":
            this._handleNavigation(message.page);
            return;
          case "unpin":
            this._handleUnpin(message.treeId);
            return;
          case "openPackage":
            this._handleOpenPackage(message.treeId, message.kind);
            return;
          case "openProject":
            this._handleOpenPackage(message.project, message.itemType);
            return;
        }
      },
      null,
      this._disposables,
    );
  }

  private _getPinnedItems(): DashboardPinnedItem[] {
    return PinnedItemsService.getAll().map(item => {
      // Map kind string to the expected type
      let kind: "function" | "type" | "value" | "package" = "package";
      const normalizedKind = item.kind.toLowerCase();
      if (normalizedKind === "function") kind = "function";
      else if (normalizedKind === "type") kind = "type";
      else if (normalizedKind === "value" || normalizedKind === "constant") kind = "value";

      return {
        id: item.itemId,
        title: item.name,
        treeId: item.treeId,
        kind,
        meta: [item.kind, item.modules || item.owner],
      };
    });
  }

  private _getRecentItems(): DashboardRecentItem[] {
    return RecentItemsService.getAll().map(item => ({
      id: item.id,
      title: item.title,
      type: item.type,
      meta: item.meta,
    }));
  }

  private async _handleUnpin(treeId: string): Promise<void> {
    await PinnedItemsService.unpin(treeId);
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

    // Pages that are implemented
    const pages: PageName[] = ["dashboard", "packages"];
    if (pages.includes(page as PageName)) {
      this._currentPage = page as PageName;
      this._update();
      return;
    }

    // For pages not yet implemented
    vscode.window.showInformationMessage(`${page} page coming soon!`);
  }

  private async _handleOpenPackage(treeId: string, itemType?: string): Promise<void> {
    if (!treeId) return;

    // Check if this is a top-level owner (no dots = owner like "Darklang", "Stachu")
    const isOwner = !treeId.includes(".");

    try {
      if (isOwner) {
        // For owners, reveal and expand in tree view instead of opening file
        await vscode.commands.executeCommand("darklang.revealInPackagesTree", treeId);
      } else {
        await vscode.commands.executeCommand("darklang.openPackageDefinition", treeId, itemType);
      }
    } catch (error) {
      console.error("Failed to open package:", error);
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
        const pinnedItems = this._getPinnedItems();
        const recentItems = this._getRecentItems();
        const data: DashboardData = {
          ...getDefaultDashboardData(),
          pinnedItems,
          recentItems,
        };
        return renderDashboard(data);
      case "packages":
        return `<iframe src="https://wip.darklang.com/packages" style="width: 100%; height: calc(100vh - 60px); border: none;"></iframe>`;
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
    <style>${homepageStyles}</style>
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
                const itemType = card.getAttribute('data-type');
                vscode.postMessage({
                    command: 'openProject',
                    project: project,
                    itemType: itemType
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

        // Handle unpin button clicks
        document.querySelectorAll('.unpin-btn').forEach(btn => {
            btn.addEventListener('click', (e) => {
                e.stopPropagation();
                const treeId = btn.getAttribute('data-tree-id');
                vscode.postMessage({
                    command: 'unpin',
                    treeId: treeId
                });
            });
        });

        // Handle pinned card clicks - reveal in tree view
        document.querySelectorAll('.pinned-card').forEach(card => {
            card.addEventListener('click', (e) => {
                // Don't trigger if clicking the unpin button
                if (e.target.closest('.unpin-btn')) return;

                const treeId = card.getAttribute('data-tree-id');
                const kind = card.getAttribute('data-kind');
                vscode.postMessage({
                    command: 'openPackage',
                    treeId: treeId,
                    kind: kind
                });
            });
        });
    </script>
</body>
</html>`;
  }
}
