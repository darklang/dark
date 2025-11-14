import * as vscode from "vscode";
import { SemanticTokensFeature } from "vscode-languageclient/lib/common/semanticTokens";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  Trace,
  TransportKind,
} from "vscode-languageclient/node";

import { PackagesTreeDataProvider } from "./providers/treeviews/packagesTreeDataProvider";
import { WorkspaceTreeDataProvider } from "./providers/treeviews/workspaceTreeDataProvider";
import { ConflictsTreeDataProvider } from "./providers/treeviews/conflictsTreeDataProvider";
import { BranchesManagerPanel } from "./panels/branchManagerPanel";
import { HomepagePanel } from "./panels/homepagePanel";
import { ReviewChangesPanel } from "./panels/reviewChangesPanel";
import { ApprovalRequestsPanel } from "./panels/approvalRequestsPanel";

import { BranchCommands } from "./commands/branchCommands";
import { PackageCommands } from "./commands/packageCommands";
import { InstanceCommands } from "./commands/instanceCommands";
import { SyncCommands } from "./commands/syncCommands";
import { ScriptCommands } from "./commands/scriptCommands";

import { StatusBarManager } from "./ui/statusbar/statusBarManager";
import { BranchStateManager } from "./data/branchStateManager";

import { DarkFileSystemProvider } from "./providers/darkFileSystemProvider";
import { DarkContentProvider } from "./providers/darkContentProvider";
import { DarklangFileDecorationProvider } from "./providers/fileDecorationProvider";
import { PackageContentProvider } from "./providers/content/packageContentProvider";

let client: LanguageClient;

function createLSPClient(): LanguageClient {
  const isDebug = process.env.VSCODE_DEBUG_MODE === "true";
  const cwd = "/home/dark/app";
  const cli = isDebug ? "./scripts/run-cli" : "darklang";
  const args = [cli, "run", "@Darklang.LanguageTools.LspServer.runServerCli", "()" ];

  const baseRun = {
    options: { cwd },
    command: "bash",
    args,
    transport: TransportKind.stdio as const,
  };

  const serverOptions: ServerOptions = { run: baseRun, debug: baseRun };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "darklang" },
      { scheme: "darkfs", language: "darklang" },
    ],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.dark"),
    },
    traceOutputChannel: vscode.window.createOutputChannel("Darklang LSP - Client"),
    connectionOptions: { cancellationStrategy: null, maxRestartCount: 0 },
  };

  const lsp = new LanguageClient("darklangLsp", "Darklang LSP - Server", serverOptions, clientOptions);
  lsp.registerFeature(new SemanticTokensFeature(lsp));
  lsp.trace = Trace.Verbose;
  return lsp;
}

export async function activate(context: vscode.ExtensionContext) {
  client = createLSPClient();
  client.start();
  await client.onReady();

  BranchStateManager.initialize(client);

  // Auto-sync state
  let autoSyncEnabled = context.globalState.get<boolean>("autoSyncEnabled", false);

  // Set context for menu visibility
  const updateAutoSyncContext = () => {
    vscode.commands.executeCommand("setContext", "darklang.autoSyncEnabled", autoSyncEnabled);
  };
  updateAutoSyncContext();

  const statusBar = new StatusBarManager();
  const fsProvider = new DarkFileSystemProvider(client);
  const contentProvider = new DarkContentProvider(client);
  const decorationProvider = new DarklangFileDecorationProvider();

  PackageContentProvider.setClient(client);

  // Webview panel serializer (if supported)
  vscode.window.registerWebviewPanelSerializer?.(
    BranchesManagerPanel.viewType,
    {
      async deserializeWebviewPanel(panel) {
        BranchesManagerPanel.revive(panel, context.extensionUri);
      },
    },
  );

  vscode.window.registerWebviewPanelSerializer?.(
    HomepagePanel.viewType,
    {
      async deserializeWebviewPanel(panel) {
        HomepagePanel.revive(panel, context.extensionUri);
      },
    },
  );

  vscode.window.registerWebviewPanelSerializer?.(
    ReviewChangesPanel.viewType,
    {
      async deserializeWebviewPanel(panel) {
        ReviewChangesPanel.revive(panel, context.extensionUri);
      },
    },
  );

  vscode.window.registerWebviewPanelSerializer?.(
    ApprovalRequestsPanel.viewType,
    {
      async deserializeWebviewPanel(panel) {
        ApprovalRequestsPanel.revive(panel, context.extensionUri);
      },
    },
  );

  const packagesProvider = new PackagesTreeDataProvider(client);
  const workspaceProvider = new WorkspaceTreeDataProvider(client);
  const conflictsProvider = new ConflictsTreeDataProvider();

  fsProvider.setPackagesProvider(packagesProvider);
  fsProvider.setWorkspaceProvider(workspaceProvider);

  const createView = (id: string, provider: vscode.TreeDataProvider<any>, showCollapseAll: boolean) =>
    vscode.window.createTreeView(id, { treeDataProvider: provider, showCollapseAll });

  const packagesView = createView("darklangPackages", packagesProvider, true);
  const workspaceView = createView("darklangWorkspace", workspaceProvider, false);
  const conflictsView = createView("darklangConflicts", conflictsProvider, false);
  (workspaceView as any).title = "Workspace"; // keep existing behavior

  // Set conflicts title with count
  const updateConflictsTitle = () => {
    const count = conflictsProvider.getConflictCount();
    // Use circled number characters or format with parentheses
    const countDisplay = count < 10 ? String.fromCharCode(0x2460 + count - 1) : `(${count})`;
    (conflictsView as any).description = countDisplay;
  };
  updateConflictsTitle();

  // Listen for checkbox changes in workspace view
  workspaceView.onDidChangeCheckboxState((e) => {
    workspaceProvider.handleCheckboxChange(e);
  });

  BranchStateManager.getInstance().onBranchChanged(() => {
    packagesProvider.refresh();
    fsProvider.refreshAllOpenFiles();
  });

  const packageCommands = new PackageCommands();
  const branchCommands = new BranchCommands(statusBar, workspaceProvider);
  const instanceCommands = new InstanceCommands(client, statusBar, workspaceProvider);
  const syncCommands = new SyncCommands(client);
  const scriptCommands = new ScriptCommands();

  instanceCommands.setPackagesProvider(packagesProvider);

  const reg = (d: vscode.Disposable) => context.subscriptions.push(d);

  const ops = [
    ["darklang.ops.toggleSelectAll", () => workspaceProvider.toggleSelectAll()],
    ["darklang.ops.toggleDeselectAll", () => workspaceProvider.toggleSelectAll()],
  ] as const;

  // Register owner group selection commands separately (need parameter)
  context.subscriptions.push(
    vscode.commands.registerCommand("darklang.ops.toggleSelectOwnerGroup", (node: any) => {
      workspaceProvider.selectOwnerGroup(node);
    }),
    vscode.commands.registerCommand("darklang.ops.toggleDeselectOwnerGroup", (node: any) => {
      workspaceProvider.selectOwnerGroup(node);
    })
  );

  // Core registrations
  [
    statusBar,
    vscode.workspace.registerFileSystemProvider("darkfs", fsProvider, { isCaseSensitive: true, isReadonly: false }),
    vscode.workspace.registerTextDocumentContentProvider("dark", contentProvider),
    vscode.window.registerFileDecorationProvider(decorationProvider),
    vscode.commands.registerCommand("darklang.branches.manageAll", () => {
      BranchesManagerPanel.createOrShow(context.extensionUri);
    }),
    vscode.commands.registerCommand("darklang.openHomepage", () => {
      HomepagePanel.createOrShow(context.extensionUri);
    }),
    vscode.commands.registerCommand("darklang.changes.review", () => {
      // TODO: Get actual ops from workspace provider
      const ops = [];
      ReviewChangesPanel.createOrShow(context.extensionUri, ops);
    }),
    vscode.commands.registerCommand("darklang.packages.search", async () => {
      const query = await vscode.window.showInputBox({
        prompt: "Search packages",
        placeHolder: "Type to filter packages, modules, and functions...",
        value: packagesProvider.getSearchQuery(),
      });
      if (query !== undefined) {
        packagesProvider.setSearchQuery(query);
      }
    }),
    vscode.commands.registerCommand("darklang.packages.clearSearch", () => {
      packagesProvider.clearSearch();
    }),
    vscode.commands.registerCommand("darklang.workspace.toggleAutoSync", async () => {
      autoSyncEnabled = true;
      await context.globalState.update("autoSyncEnabled", autoSyncEnabled);
      updateAutoSyncContext();
      vscode.window.showInformationMessage("Auto-sync enabled");
    }),
    vscode.commands.registerCommand("darklang.workspace.toggleAutoSyncOn", async () => {
      autoSyncEnabled = false;
      await context.globalState.update("autoSyncEnabled", autoSyncEnabled);
      updateAutoSyncContext();
      vscode.window.showInformationMessage("Auto-sync disabled");
    }),
    ...ops.map(([cmd, fn]) => vscode.commands.registerCommand(cmd, fn)),
    packagesView,
    packagesProvider,
    workspaceView,
    conflictsView,
    conflictsProvider,
    ...packageCommands.register(),
    ...branchCommands.register(),
    ...instanceCommands.register(),
    ...syncCommands.register(),
    ...scriptCommands.register(),
  ].forEach(reg);
}

export function deactivate(): Thenable<void> | undefined {
  return client ? client.stop() : undefined;
}
