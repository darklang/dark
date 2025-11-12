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

  const packagesProvider = new PackagesTreeDataProvider(client);
  const workspaceProvider = new WorkspaceTreeDataProvider(client);

  fsProvider.setPackagesProvider(packagesProvider);
  fsProvider.setWorkspaceProvider(workspaceProvider);

  const createView = (id: string, provider: vscode.TreeDataProvider<any>, showCollapseAll: boolean) =>
    vscode.window.createTreeView(id, { treeDataProvider: provider, showCollapseAll });

  const packagesView = createView("darklangPackages", packagesProvider, true);
  const workspaceView = createView("darklangWorkspace", workspaceProvider, false);
  (workspaceView as any).title = "Workspace"; // keep existing behavior

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
    ["darklang.ops.setLimit", () => workspaceProvider.configureLimitFilter()],
    ["darklang.ops.setDateRange", () => workspaceProvider.configureDateFilter()],
    ["darklang.ops.setBranch", () => workspaceProvider.configureBranchFilter()],
    ["darklang.ops.setLocation", () => workspaceProvider.configureLocationFilter()],
    ["darklang.ops.clearFilters", () => workspaceProvider.clearAllFilters()],
  ] as const;

  // Core registrations
  [
    statusBar,
    vscode.workspace.registerFileSystemProvider("darkfs", fsProvider, { isCaseSensitive: true, isReadonly: false }),
    vscode.workspace.registerTextDocumentContentProvider("dark", contentProvider),
    vscode.window.registerFileDecorationProvider(decorationProvider),
    vscode.commands.registerCommand("darklang.branches.manageAll", () => {
      BranchesManagerPanel.createOrShow(context.extensionUri);
    }),
    ...ops.map(([cmd, fn]) => vscode.commands.registerCommand(cmd, fn)),
    packagesView,
    packagesProvider,
    workspaceView,
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
