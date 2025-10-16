import * as os from "os";

import * as vscode from "vscode";
import { SemanticTokensFeature } from "vscode-languageclient/lib/common/semanticTokens";
import { LanguageClient, LanguageClientOptions, ServerOptions, Trace, TransportKind } from "vscode-languageclient/node";

import { PackagesTreeDataProvider } from "./providers/treeviews/packagesTreeDataProvider";
import { WorkspaceTreeDataProvider } from "./providers/treeviews/workspaceTreeDataProvider";

import { BranchesManagerPanel } from "./panels/branchManagerPanel";

import { BranchCommands } from "./commands/branchCommands";
import { PackageCommands } from "./commands/packageCommands";
import { InstanceCommands } from "./commands/instanceCommands";

import { StatusBarManager } from "./ui/statusbar/statusBarManager";

import { DarkContentProvider } from "./providers/darkContentProvider";
import { DarklangFileDecorationProvider } from "./providers/fileDecorationProvider";


let client: LanguageClient;

let statusBarManager: StatusBarManager;

let workspaceProvider: WorkspaceTreeDataProvider;
let packagesProvider: PackagesTreeDataProvider;

let contentProvider: DarkContentProvider;
let fileDecorationProvider: DarklangFileDecorationProvider;

interface LSPFileResponse {
  content: string;
}

export function activate(context: vscode.ExtensionContext) {
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

  // Initialize content provider for dark:// URLs
  contentProvider = new DarkContentProvider();
  const contentProviderRegistration = vscode.workspace.registerTextDocumentContentProvider("dark", contentProvider);
  context.subscriptions.push(contentProviderRegistration);

  // Pass the LSP client to the content provider once it's ready
  client.onReady().then(() => {
    contentProvider.setClient(client);
  });

  // Initialize file decoration provider for badges
  fileDecorationProvider = new DarklangFileDecorationProvider();
  const decorationProviderRegistration = vscode.window.registerFileDecorationProvider(fileDecorationProvider);
  context.subscriptions.push(decorationProviderRegistration);


  // Per-URI view mode tracking
  type ViewMode = 'source' | 'ast';
  const viewModeByUri = new Map<string, ViewMode>();

  function getModeFor(uri?: vscode.Uri): ViewMode {
    if (!uri) return 'source';
    return viewModeByUri.get(uri.toString()) ?? 'source';
  }

  async function updateContextForActiveEditor() {
    const uri = vscode.window.activeTextEditor?.document?.uri;
    await vscode.commands.executeCommand('setContext', 'darklang.viewMode', getModeFor(uri));
  }

  // Inject mode getter into content provider
  contentProvider.getModeForUri = (uri) => getModeFor(uri);

  // Initialize context and keep it in sync when tabs/editors change
  updateContextForActiveEditor();
  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(updateContextForActiveEditor),
    vscode.window.tabGroups.onDidChangeTabs(updateContextForActiveEditor),
    vscode.workspace.onDidCloseTextDocument(doc => {
      if (doc.uri.scheme === 'dark') {
        viewModeByUri.delete(doc.uri.toString());
      }
    })
  );

  // Register branches manager command
  context.subscriptions.push(
    vscode.commands.registerCommand('darklang.branches.manageAll', () => {
      BranchesManagerPanel.createOrShow(context.extensionUri);
    }),
    vscode.commands.registerCommand('darklang.switchToAST', async () => {
      const uri = vscode.window.activeTextEditor?.document?.uri;
      if (!uri || uri.scheme !== 'dark') return;
      viewModeByUri.set(uri.toString(), 'ast');
      await updateContextForActiveEditor();
      contentProvider.bump(uri);
    }),
    vscode.commands.registerCommand('darklang.switchToSource', async () => {
      const uri = vscode.window.activeTextEditor?.document?.uri;
      if (!uri || uri.scheme !== 'dark') return;
      viewModeByUri.set(uri.toString(), 'source');
      await updateContextForActiveEditor();
      contentProvider.bump(uri);
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

  // Initialize tree data providers
  // Wait for LSP client to be ready before initializing tree provider
  client.onReady().then(() => {
    packagesProvider = new PackagesTreeDataProvider(client);
    const packagesView = vscode.window.createTreeView("darklangPackages", {
      treeDataProvider: packagesProvider,
      showCollapseAll: true,
    });

    context.subscriptions.push(packagesView);
  });

  workspaceProvider = new WorkspaceTreeDataProvider();

  // Create tree views
  console.log('📊 Creating tree views...');

  // Workspace tree view (unified view: Instance + Branch + Patches)
  const workspaceView = vscode.window.createTreeView("darklangWorkspace", {
    treeDataProvider: workspaceProvider,
    showCollapseAll: false,
  });
  workspaceView.title = "Workspace";

  console.log('✅ Tree views created successfully');

  context.subscriptions.push(workspaceView);

  // Start LSP client
  client.start();

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
  if (!client) {
    return undefined;
  }
  return client.stop();
}
