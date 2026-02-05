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
import { ScmTreeDataProvider } from "./providers/treeviews/scmTreeDataProvider";
import { PackageCommands } from "./commands/packageCommands";
import { ScmCommands } from "./commands/scmCommands";
import { BranchCommands } from "./commands/branchCommands";
import { ScriptCommands } from "./commands/scriptCommands";

import { DarkFileSystemProvider } from "./providers/darkFileSystemProvider";
import { DarkContentProvider } from "./providers/darkContentProvider";
import { DiffContentProvider } from "./providers/diffContentProvider";
import { DarklangFileDecorationProvider } from "./providers/fileDecorationProvider";
import { PackageContentProvider } from "./providers/content/packageContentProvider";
import { RecentItemsService } from "./services/recentItemsService";
import { StatusBar, BranchInfo } from "./ui/statusbar/statusBar";
import { HomepagePanel } from "./panels/homepage/homepagePanel";

let client: LanguageClient;

// Shared debug mode state, set during activation
export let isDebugMode = false;

function createLSPClient(isDebug: boolean): LanguageClient {
  const cwd = isDebug ? "/home/dark/app" : (process.env.HOME || process.env.USERPROFILE || ".");
  const cli = "./scripts/run-cli";

  const command = isDebug ? "bash" : "dark";
  const args = isDebug
    ? [cli, "--no-log", "run", "@Darklang.LanguageTools.LspServer.runServerCli", "()"]
    : ["run", "@Darklang.LanguageTools.LspServer.runServerCli", "()"];

  const baseRun = {
    options: { cwd },
    command,
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
  // Use VS Code's ExtensionMode to detect development mode
  isDebugMode = context.extensionMode === vscode.ExtensionMode.Development;

  client = createLSPClient(isDebugMode);
  client.start();
  await client.onReady();

  RecentItemsService.initialize(context);

  const fsProvider = new DarkFileSystemProvider(client);
  const contentProvider = new DarkContentProvider(client);
  const diffContentProvider = DiffContentProvider.getInstance();
  const decorationProvider = new DarklangFileDecorationProvider();
  const statusBar = new StatusBar();

  PackageContentProvider.setClient(client);
  HomepagePanel.setClient(client);

  // Tree providers
  const packagesProvider = new PackagesTreeDataProvider(client);
  const scmProvider = new ScmTreeDataProvider(client);

  fsProvider.setPackagesProvider(packagesProvider);

  // Update status bar when SCM status changes
  scmProvider.onStatusChanged(status => {
    statusBar.updateStatus(status);
  });

  // Tree views
  const packagesView = vscode.window.createTreeView("darklangPackages", {
    treeDataProvider: packagesProvider,
    showCollapseAll: true,
  });

  const scmView = vscode.window.createTreeView("darklangScm", {
    treeDataProvider: scmProvider,
    showCollapseAll: false,
  });

  // Homepage panel serializer
  vscode.window.registerWebviewPanelSerializer?.(HomepagePanel.viewType, {
    async deserializeWebviewPanel(panel) {
      HomepagePanel.revive(panel, context.extensionUri);
    },
  });

  // Commands
  const packageCommands = new PackageCommands();
  packageCommands.setPackagesProvider(packagesProvider);
  packageCommands.setPackagesView(packagesView);
  const scmCommands = new ScmCommands(client, scmProvider);
  const branchCommands = new BranchCommands(client, scmProvider);
  const scriptCommands = new ScriptCommands();
  scriptCommands.setClient(client);

  const reg = (d: vscode.Disposable) => context.subscriptions.push(d);

  [
    statusBar,
    vscode.workspace.registerFileSystemProvider("darkfs", fsProvider, { isCaseSensitive: true, isReadonly: false }),
    vscode.workspace.registerTextDocumentContentProvider("dark", contentProvider),
    vscode.workspace.registerTextDocumentContentProvider(DiffContentProvider.scheme, diffContentProvider),
    vscode.window.registerFileDecorationProvider(decorationProvider),

    // Homepage command
    vscode.commands.registerCommand("darklang.openHomepage", () => {
      HomepagePanel.createOrShow(context.extensionUri);
    }),

    // Utility commands
    vscode.commands.registerCommand("darklang.clearRecentItems", () => {
      RecentItemsService.clear();
      vscode.window.showInformationMessage("Recent items cleared");
    }),

    packagesView,
    packagesProvider,
    scmView,
    scmProvider,
    ...packageCommands.register(),
    ...scmCommands.register(),
    ...branchCommands.register(),
    ...scriptCommands.register(),
  ].forEach(reg);

  // Wire up status bar to LSP client
  statusBar.setClient(client);

  // Listen for branch change notifications from the LSP server
  client.onNotification("dark/branchChanged", (params: any) => {
    const branchData = Array.isArray(params) ? params[0] : params;
    if (branchData && branchData.id && branchData.name) {
      const branch: BranchInfo = { id: branchData.id, name: branchData.name };
      statusBar.updateBranch(branch);
      scmProvider.updateBranch(branch);
    }
  });

  // Listen for SCM change notifications (refresh views when branch changes)
  client.onNotification("dark/scm/changed", () => {
    scmProvider.refresh();
  });

  // Initial SCM refresh to populate status bar
  scmProvider.refresh();

  // Auto-open homepage on activation
  HomepagePanel.createOrShow(context.extensionUri);
}

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
  }
}
