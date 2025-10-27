import * as os from "os";

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

import { BranchesManagerPanel } from "./panels/branchManagerPanel";

import { BranchCommands } from "./commands/branchCommands";
import { PackageCommands } from "./commands/packageCommands";
import { InstanceCommands } from "./commands/instanceCommands";
import { SyncCommands } from "./commands/syncCommands";

import { StatusBarManager } from "./ui/statusbar/statusBarManager";
import { BranchStateManager } from "./data/branchStateManager";

import { DarkFileSystemProvider } from "./providers/darkFileSystemProvider";
import { DarklangFileDecorationProvider } from "./providers/fileDecorationProvider";

let client: LanguageClient;

let statusBarManager: StatusBarManager;

let workspaceProvider: WorkspaceTreeDataProvider;
let packagesProvider: PackagesTreeDataProvider;

let fileSystemProvider: DarkFileSystemProvider;
let fileDecorationProvider: DarklangFileDecorationProvider;

interface LSPFileResponse {
  content: string;
}

export function activate(context: vscode.ExtensionContext) {
  console.log("🚀 Darklang VS Code extension is activating...");

  const isDebugMode = () => process.env.VSCODE_DEBUG_MODE === "true";

  const sharedServerOptions = {
    options: { cwd: "/home/dark/app" },
    command: "bash",
    args: [
      isDebugMode ? "./scripts/run-cli" : "darklang",
      "run",
      "@Darklang.LanguageTools.LspServer.runServerCli",
      "()", // 'parses' to () - TODO clean this up once we switch over to new parser
    ],
    transport: TransportKind.stdio,
  };
  const serverOptions: ServerOptions = {
    run: sharedServerOptions,
    debug: sharedServerOptions,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "darklang" },
      { scheme: "darkfs", language: "darklang" },
    ],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.dark"),
    },

    // in the window that has the extension loaded, go to the Output tab,
    // and select this option in the dropdown to find corresponding logs
    traceOutputChannel: vscode.window.createOutputChannel(
      "Darklang LSP - Client",
    ),

    // without this, VS Code will try to restart our extension 5 times,
    // which can be really annoying while debugging
    connectionOptions: {
      cancellationStrategy: null,
      maxRestartCount: 0,
    },
  };

  // Initialize LSP client
  client = new LanguageClient(
    "darklangLsp",
    "Darklang LSP - Server",
    serverOptions,
    clientOptions,
  );
  client.registerFeature(new SemanticTokensFeature(client));
  client.trace = Trace.Verbose;

  // Initialize status bar manager
  statusBarManager = new StatusBarManager();
  context.subscriptions.push(statusBarManager);

  // Initialize file system provider for darkfs:// URLs (editable package modules)
  fileSystemProvider = new DarkFileSystemProvider();
  const fileSystemProviderRegistration =
    vscode.workspace.registerFileSystemProvider("darkfs", fileSystemProvider, {
      isCaseSensitive: true,
      isReadonly: false,
    });
  context.subscriptions.push(fileSystemProviderRegistration);

  // Pass the LSP client to providers once it's ready
  client.onReady().then(() => {
    fileSystemProvider.setClient(client);
    workspaceProvider.setClient(client);
    BranchStateManager.getInstance().setClient(client);
  });

  // Initialize file decoration provider for badges
  fileDecorationProvider = new DarklangFileDecorationProvider();
  const decorationProviderRegistration =
    vscode.window.registerFileDecorationProvider(fileDecorationProvider);
  context.subscriptions.push(decorationProviderRegistration);

  // Register branches manager command
  context.subscriptions.push(
    vscode.commands.registerCommand("darklang.branches.manageAll", () => {
      BranchesManagerPanel.createOrShow(context.extensionUri);
    }),
  );

  // Register webview panel serializer for branches manager
  if (vscode.window.registerWebviewPanelSerializer) {
    vscode.window.registerWebviewPanelSerializer(
      BranchesManagerPanel.viewType,
      {
        async deserializeWebviewPanel(
          webviewPanel: vscode.WebviewPanel,
          state: any,
        ) {
          BranchesManagerPanel.revive(webviewPanel, context.extensionUri);
        },
      },
    );
  }

  // Initialize tree data providers
  // Wait for LSP client to be ready before initializing tree provider
  client.onReady().then(() => {
    packagesProvider = new PackagesTreeDataProvider(client);
    const packagesView = vscode.window.createTreeView("darklangPackages", {
      treeDataProvider: packagesProvider,
      showCollapseAll: true,
    });

    context.subscriptions.push(packagesView);

    // Wire up packages provider to file system provider
    fileSystemProvider.setPackagesProvider(packagesProvider);

    // Refresh packages tree and open files when branch changes
    BranchStateManager.getInstance().onBranchChanged(() => {
      console.log('🔄 Branch changed - refreshing packages tree and open files');
      packagesProvider.refresh();
      fileSystemProvider.refreshAllOpenFiles();
    });

    // Pass packages provider to instance commands for sync refresh
    instanceCommands.setPackagesProvider(packagesProvider);
  });

  workspaceProvider = new WorkspaceTreeDataProvider();

  // Wire up providers now that workspaceProvider is created
  fileSystemProvider.setWorkspaceProvider(workspaceProvider);

  // Create tree views
  console.log("📊 Creating tree views...");

  // Workspace tree view (unified view: Instance + Branch)
  const workspaceView = vscode.window.createTreeView("darklangWorkspace", {
    treeDataProvider: workspaceProvider,
    showCollapseAll: false,
  });
  workspaceView.title = "Workspace";

  console.log("✅ Tree views created successfully");

  context.subscriptions.push(workspaceView);

  // Start LSP client
  client.start();

  // Initialize command handlers
  const packageCommands = new PackageCommands();
  const branchCommands = new BranchCommands(
    statusBarManager,
    workspaceProvider,
  );
  const instanceCommands = new InstanceCommands(
    statusBarManager,
    workspaceProvider,
  );
  const syncCommands = new SyncCommands();

  // Pass LSP client to commands when ready
  client.onReady().then(() => {
    syncCommands.setClient(client);
    instanceCommands.setClient(client);
  });

  // Register all commands
  const allBranchCommands = branchCommands.register();
  const allInstanceCommands = instanceCommands.register();
  const allPackageCommands = packageCommands.register();
  const allSyncCommands = syncCommands.register();

  // Register all commands with context
  context.subscriptions.push(
    ...allPackageCommands,
    ...allBranchCommands,
    ...allInstanceCommands,
    ...allSyncCommands,
  );

  console.log("🎉 Darklang VS Code extension fully activated!");
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
