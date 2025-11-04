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
import { ScriptCommands } from "./commands/scriptCommands";

import { StatusBarManager } from "./ui/statusbar/statusBarManager";
import { BranchStateManager } from "./data/branchStateManager";

import { DarkFileSystemProvider } from "./providers/darkFileSystemProvider";
import { DarkContentProvider } from "./providers/darkContentProvider";
import { DarklangFileDecorationProvider } from "./providers/fileDecorationProvider";

let client: LanguageClient;

function createLSPClient(): LanguageClient {
  const isDebugMode = process.env.VSCODE_DEBUG_MODE === "true";
  const serverOptions: ServerOptions = {
    run: {
      options: { cwd: "/home/dark/app" },
      command: "bash",
      args: [
        isDebugMode ? "./scripts/run-cli" : "darklang",
        "run",
        "@Darklang.LanguageTools.LspServer.runServerCli",
        "()",
      ],
      transport: TransportKind.stdio,
    },
    debug: {
      options: { cwd: "/home/dark/app" },
      command: "bash",
      args: [
        isDebugMode ? "./scripts/run-cli" : "darklang",
        "run",
        "@Darklang.LanguageTools.LspServer.runServerCli",
        "()",
      ],
      transport: TransportKind.stdio,
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "darklang" },
      { scheme: "darkfs", language: "darklang" },
    ],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.dark"),
    },
    traceOutputChannel: vscode.window.createOutputChannel("Darklang LSP - Client"),
    connectionOptions: {
      cancellationStrategy: null,
      maxRestartCount: 0,
    },
  };

  const lspClient = new LanguageClient(
    "darklangLsp",
    "Darklang LSP - Server",
    serverOptions,
    clientOptions,
  );
  lspClient.registerFeature(new SemanticTokensFeature(lspClient));
  lspClient.trace = Trace.Verbose;
  return lspClient;
}

export async function activate(context: vscode.ExtensionContext) {
  client = createLSPClient();
  client.start();
  await client.onReady();

  BranchStateManager.initialize(client);

  const statusBarManager = new StatusBarManager();
  const fileSystemProvider = new DarkFileSystemProvider(client);
  const contentProvider = new DarkContentProvider(client);
  const fileDecorationProvider = new DarklangFileDecorationProvider();

  const { PackageContentProvider } = await import("./providers/content/packageContentProvider");
  PackageContentProvider.setClient(client);

  // Register webview panel serializer for branches manager
  if (vscode.window.registerWebviewPanelSerializer) {
    vscode.window.registerWebviewPanelSerializer(
      BranchesManagerPanel.viewType,
      {
        async deserializeWebviewPanel(
          webviewPanel: vscode.WebviewPanel,
          _state: any,
        ) {
          BranchesManagerPanel.revive(webviewPanel, context.extensionUri);
        },
      },
    );
  }

  const packagesProvider = new PackagesTreeDataProvider(client);
  const workspaceProvider = new WorkspaceTreeDataProvider(client);

  fileSystemProvider.setPackagesProvider(packagesProvider);
  fileSystemProvider.setWorkspaceProvider(workspaceProvider);

  const packagesView = vscode.window.createTreeView("darklangPackages", {
    treeDataProvider: packagesProvider,
    showCollapseAll: true,
  });

  const workspaceView = vscode.window.createTreeView("darklangWorkspace", {
    treeDataProvider: workspaceProvider,
    showCollapseAll: false,
  });
  workspaceView.title = "Workspace";

  BranchStateManager.getInstance().onBranchChanged(() => {
    packagesProvider.refresh();
    fileSystemProvider.refreshAllOpenFiles();
  });

  const packageCommands = new PackageCommands();
  const branchCommands = new BranchCommands(statusBarManager, workspaceProvider);
  const instanceCommands = new InstanceCommands(client, statusBarManager, workspaceProvider);
  const syncCommands = new SyncCommands(client);
  const scriptCommands = new ScriptCommands();

  instanceCommands.setPackagesProvider(packagesProvider);

  const opsCommands = [
    ["darklang.ops.setLimit", () => workspaceProvider.configureLimitFilter()],
    ["darklang.ops.setDateRange", () => workspaceProvider.configureDateFilter()],
    ["darklang.ops.setBranch", () => workspaceProvider.configureBranchFilter()],
    ["darklang.ops.setLocation", () => workspaceProvider.configureLocationFilter()],
    ["darklang.ops.clearFilters", () => workspaceProvider.clearAllFilters()],
  ].map(([cmd, handler]) =>
    vscode.commands.registerCommand(cmd as string, handler as () => void)
  );

  context.subscriptions.push(
    statusBarManager,
    vscode.workspace.registerFileSystemProvider("darkfs", fileSystemProvider, {
      isCaseSensitive: true,
      isReadonly: false,
    }),
    vscode.workspace.registerTextDocumentContentProvider("dark", contentProvider),
    vscode.window.registerFileDecorationProvider(fileDecorationProvider),
    vscode.commands.registerCommand("darklang.branches.manageAll", () => {
      BranchesManagerPanel.createOrShow(context.extensionUri);
    }),
    ...opsCommands,
    packagesView,
    packagesProvider,
    workspaceView,
    ...packageCommands.register(),
    ...branchCommands.register(),
    ...instanceCommands.register(),
    ...syncCommands.register(),
    ...scriptCommands.register(),
  );
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
