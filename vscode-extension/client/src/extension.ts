import { workspace, ExtensionContext, commands, window, Uri } from "vscode";
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

import { ServerBackedTreeDataProvider } from "./providers/treeviews/ServerBackedTreeDataProvider";
import { ComprehensiveDarkContentProvider } from "./providers/comprehensiveDarkContentProvider";
import { StatusBarManager } from "./ui/statusbar/statusBarManager";
import { PackagesTreeDataProvider } from "./providers/treeviews/packagesTreeDataProvider";
import { WorkspaceTreeDataProvider } from "./providers/treeviews/workspaceTreeDataProvider";
import { BranchesManagerPanel } from "./panels";
import {
  BranchCommands,
  PackageCommands,
  InstanceCommands
} from "./commands";
import { BranchStateManager } from "./data/branchStateManager";
import { DarklangFileDecorationProvider } from "./providers/fileDecorationProvider";

let client: LanguageClient;
let statusBarManager: StatusBarManager;
let workspaceProvider: WorkspaceTreeDataProvider;
let packagesProvider: PackagesTreeDataProvider;
let contentProvider: ComprehensiveDarkContentProvider;
let fileDecorationProvider: DarklangFileDecorationProvider;

interface LSPFileResponse {
  content: string;
}

export function activate(context: ExtensionContext) {
  console.log('🚀 Darklang VS Code extension is activating...');

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
      { scheme: "dark", language: "darklang" },
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.dark"),
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

  // DISABLED: LSP client to avoid crashes during UI development
  // client = new LanguageClient(
  //   "darklangLsp",
  //   "Darklang LSP - Server",
  //   serverOptions,
  //   clientOptions,
  // );
  // client.registerFeature(new SemanticTokensFeature(client));
  // client.trace = Trace.Verbose;

  // Initialize status bar manager
  statusBarManager = new StatusBarManager();
  context.subscriptions.push(statusBarManager);

  // Initialize content provider for dark:// URLs
  contentProvider = new ComprehensiveDarkContentProvider();
  const contentProviderRegistration = workspace.registerTextDocumentContentProvider("dark", contentProvider);
  context.subscriptions.push(contentProviderRegistration);

  // Initialize file decoration provider for badges
  fileDecorationProvider = new DarklangFileDecorationProvider();
  const decorationProviderRegistration = vscode.window.registerFileDecorationProvider(fileDecorationProvider);
  context.subscriptions.push(decorationProviderRegistration);


  // // Register webview panel serializer for home page
  // if (vscode.window.registerWebviewPanelSerializer) {
  //   vscode.window.registerWebviewPanelSerializer(DarklangHomePanel.viewType, {
  //     async deserializeWebviewPanel(webviewPanel: vscode.WebviewPanel, state: any) {
  //       DarklangHomePanel.revive(webviewPanel, context.extensionUri);
  //     }
  //   });
  // }

  // Register branches manager command
  context.subscriptions.push(
    vscode.commands.registerCommand('darklang.branches.manageAll', () => {
      BranchesManagerPanel.createOrShow(context.extensionUri);
    })
  );

  // Register webview panel serializer for branches manager
  if (vscode.window.registerWebviewPanelSerializer) {
    vscode.window.registerWebviewPanelSerializer(BranchesManagerPanel.viewType, {
      async deserializeWebviewPanel(webviewPanel: vscode.WebviewPanel, state: any) {
        BranchesManagerPanel.revive(webviewPanel, context.extensionUri);
      }
    });
  }

  // Initialize branch state manager first (needed by tree views)
  const branchStateManager = BranchStateManager.getInstance();

  // Initialize tree data providers
  packagesProvider = new PackagesTreeDataProvider();
  workspaceProvider = new WorkspaceTreeDataProvider();

  // Create tree views
  console.log('📊 Creating tree views...');

  // Workspace tree view (unified view: Instance + Branch + Patches)
  const workspaceView = vscode.window.createTreeView("darklangWorkspace", {
    treeDataProvider: workspaceProvider,
    showCollapseAll: false,
  });
  workspaceView.title = "Workspace";

  const packagesView = vscode.window.createTreeView("darklangPackages", {
    treeDataProvider: packagesProvider,
    showCollapseAll: true,
  });

  console.log('✅ Tree views created successfully');

  context.subscriptions.push(workspaceView, packagesView);

  // DISABLED: LSP client start
  // client.start();

  // Initialize command handlers
  const packageCommands = new PackageCommands();
  const branchCommands = new BranchCommands(statusBarManager, workspaceProvider);
  const instanceCommands = new InstanceCommands(statusBarManager, workspaceProvider);

  // Register all commands
  const allBranchCommands = branchCommands.register();
  const allInstanceCommands = instanceCommands.register();
  const allPackageCommands = packageCommands.register();

  // Register all commands with context
  context.subscriptions.push(
    ...allPackageCommands,
    ...allBranchCommands,
    ...allInstanceCommands,
  );

  console.log('🎉 Darklang VS Code extension fully activated!');
}

export function deactivate(): Thenable<void> | undefined {
  // DISABLED: LSP client deactivation
  // if (!client) {
  //   return undefined;
  // }
  // return client.stop();
  return undefined;
}
