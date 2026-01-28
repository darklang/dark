import * as vscode from "vscode";
import { spawn } from "child_process";
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
import { ApprovalsTreeDataProvider } from "./providers/treeviews/approvalsTreeDataProvider";
import { TodosTreeDataProvider } from "./providers/treeviews/todosTreeDataProvider";
import { PendingChangesTreeDataProvider } from "./providers/treeviews/pendingChangesTreeDataProvider";
import { BranchNode } from "./types";
import { BranchesManagerPanel } from "./panels/branchManagerPanel";
import { HomepagePanel } from "./panels/homepage/homepagePanel";

import { BranchCommands } from "./commands/branchCommands";
import { PackageCommands } from "./commands/packageCommands";
import { ScriptCommands } from "./commands/scriptCommands";
import { ApprovalCommands } from "./commands/approvalCommands";

import { StatusBarManager } from "./ui/statusbar/statusBarManager";
import { BranchStateManager } from "./data/branchStateManager";
import { AccountService } from "./services/accountService";

import { DarkFileSystemProvider } from "./providers/darkFileSystemProvider";
import { DarkContentProvider } from "./providers/darkContentProvider";
import { DiffContentProvider } from "./providers/diffContentProvider";
import { DarklangFileDecorationProvider } from "./providers/fileDecorationProvider";
import { PackageContentProvider } from "./providers/content/packageContentProvider";
import { RecentItemsService } from "./services/recentItemsService";

let client: LanguageClient;

const isDebug = process.env.VSCODE_DEBUG_MODE === "true";
const homeDir = process.env.HOME || process.env.USERPROFILE || ".";
const cwd = isDebug ? "/home/dark/app" : homeDir;
const cli = isDebug ? "./scripts/run-cli" : "darklang";

function startSyncService(): void {
  const command = isDebug ? "bash" : cli;
  const args = isDebug
    ? [cli, "run", "@Darklang.Cli.SyncService.startInBackground", "()"]
    : ["run", "@Darklang.Cli.SyncService.startInBackground", "()"];
  const child = spawn(command, args, {
    cwd,
    detached: true,
    stdio: "ignore",
  });
  child.unref();
  console.log("Sync service start requested via CLI");
}

function stopSyncService(): void {
  const command = isDebug ? "bash" : cli;
  const args = isDebug
    ? [cli, "run", "@Darklang.Cli.SyncService.stop", "()"]
    : ["run", "@Darklang.Cli.SyncService.stop", "()"];
  const child = spawn(command, args, {
    cwd,
    stdio: "ignore",
  });
  child.unref();
  console.log("Sync service stop requested via CLI");
}

function createLSPClient(): LanguageClient {
  const command = isDebug ? "bash" : cli;
  const args = isDebug
    ? [cli, "run", "@Darklang.LanguageTools.LspServer.runServerCli", "()"]
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
  client = createLSPClient();
  client.start();
  await client.onReady();

  // Sync default account to LSP before initializing BranchStateManager
  // (branches are filtered by account, so account must be set first)
  await client.sendRequest("dark/setCurrentAccount", {
    accountID: AccountService.getCurrentAccountId(),
  });

  BranchStateManager.initialize(client);
  RecentItemsService.initialize(context);

  // Auto-start sync service via CLI
  startSyncService();

  const statusBar = new StatusBarManager();
  const fsProvider = new DarkFileSystemProvider(client);
  const contentProvider = new DarkContentProvider(client);
  const diffContentProvider = DiffContentProvider.getInstance();
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

  const packagesProvider = new PackagesTreeDataProvider(client);
  const workspaceProvider = new WorkspaceTreeDataProvider(client);
  const approvalsProvider = new ApprovalsTreeDataProvider(client);
  const todosProvider = new TodosTreeDataProvider(client);
  const pendingChangesProvider = new PendingChangesTreeDataProvider(client);

  fsProvider.setPackagesProvider(packagesProvider);
  fsProvider.setWorkspaceProvider(workspaceProvider);
  fsProvider.setApprovalsProvider(approvalsProvider);

  const createView = (id: string, provider: vscode.TreeDataProvider<any>, showCollapseAll: boolean) =>
    vscode.window.createTreeView(id, { treeDataProvider: provider, showCollapseAll });

  const packagesView = createView("darklangPackages", packagesProvider, true);
  const workspaceView = createView("darklangWorkspace", workspaceProvider, false);
  const pendingChangesView = createView("darklangPendingChanges", pendingChangesProvider, false);

  // Todos view with checkbox support for available updates
  const todosView = vscode.window.createTreeView("darklangTodos", {
    treeDataProvider: todosProvider,
    showCollapseAll: false,
    manageCheckboxStateManually: true,
  });

  // Handle checkbox changes for available updates
  todosView.onDidChangeCheckboxState(e => {
    todosProvider.handleCheckboxChange(e.items);
  });

  const approvalsView = vscode.window.createTreeView("darklangApprovals", {
    treeDataProvider: approvalsProvider,
    showCollapseAll: false,
    manageCheckboxStateManually: true,
  });

  // Handle checkbox changes for pending locations
  approvalsView.onDidChangeCheckboxState(e => {
    approvalsProvider.handleCheckboxChange(e.items);
  });

  (workspaceView as any).title = "Workspace"; // keep existing behavior

  BranchStateManager.getInstance().onBranchChanged(() => {
    packagesProvider.refresh();
    fsProvider.refreshAllOpenFiles();
  });

  // Refresh open files when account changes (caches are cleared server-side)
  const accountChangeListener = AccountService.onDidChange(async (accountID) => {
    await client.sendRequest("dark/setCurrentAccount", { accountID });
    packagesProvider.refresh();
    approvalsProvider.refresh();
    todosProvider.refresh();
    pendingChangesProvider.refresh();
    fsProvider.refreshAllOpenFiles();
  });
  context.subscriptions.push(accountChangeListener);

  const packageCommands = new PackageCommands();
  packageCommands.setPackagesProvider(packagesProvider);
  packageCommands.setPackagesView(packagesView);
  const branchCommands = new BranchCommands(statusBar, workspaceProvider);
  const scriptCommands = new ScriptCommands();
  const approvalCommands = new ApprovalCommands();
  approvalCommands.setClient(client);
  approvalCommands.setApprovalsProvider(approvalsProvider);
  approvalCommands.setPackagesProvider(packagesProvider);

  // Set client on HomepagePanel for account fetching
  HomepagePanel.setClient(client);

  const reg = (d: vscode.Disposable) => context.subscriptions.push(d);

  // Core registrations
  [
    statusBar,
    vscode.workspace.registerFileSystemProvider("darkfs", fsProvider, { isCaseSensitive: true, isReadonly: false }),
    vscode.workspace.registerTextDocumentContentProvider("dark", contentProvider),
    vscode.workspace.registerTextDocumentContentProvider(DiffContentProvider.scheme, diffContentProvider),
    vscode.window.registerFileDecorationProvider(decorationProvider),
    vscode.commands.registerCommand("darklang.branches.manageAll", () => {
      BranchesManagerPanel.createOrShow(context.extensionUri);
    }),
    vscode.commands.registerCommand("darklang.openHomepage", () => {
      HomepagePanel.createOrShow(context.extensionUri);
    }),
    vscode.commands.registerCommand("darklang.clearRecentItems", () => {
      RecentItemsService.clear();
      vscode.window.showInformationMessage("Recent items cleared");
    }),
    packagesView,
    packagesProvider,
    workspaceView,
    approvalsView,
    approvalsProvider,
    todosView,
    todosProvider,
    vscode.commands.registerCommand("darklang.todos.refresh", () => {
      todosProvider.refresh();
    }),
    vscode.commands.registerCommand("darklang.todos.dismiss", async (node: BranchNode) => {
      if (node.todoData?.todoId) {
        try {
          await client.sendRequest("dark/dismissTodo", { todoId: node.todoData.todoId });
          todosProvider.refresh();
        } catch (error) {
          vscode.window.showErrorMessage(`Failed to dismiss: ${error}`);
        }
      }
    }),
    vscode.commands.registerCommand("darklang.todos.viewItem", async (node: BranchNode) => {
      if (node.todoData?.itemName) {
        // itemName is the full display name like "Owner.Module.Name"
        const uri = vscode.Uri.parse(`dark:///package/${node.todoData.itemName}`);
        await vscode.commands.executeCommand("vscode.open", uri);
      }
    }),
    vscode.commands.registerCommand("darklang.todos.applySelectedUpdates", async () => {
      const checkedUpdates = todosProvider.getCheckedUpdates();
      if (checkedUpdates.length === 0) {
        vscode.window.showInformationMessage("No updates selected to apply");
        return;
      }

      try {
        const result = await client.sendRequest<{ results?: Array<{ todoId: string; success: boolean; error?: string; newItemId?: string }>; success?: boolean; error?: string }>(
          "dark/applyUpdates",
          { todoIds: checkedUpdates }
        );

        // Handle error response format
        if (result.success === false && result.error) {
          vscode.window.showErrorMessage(`Failed to apply updates: ${result.error}`);
          return;
        }

        // Handle results array format
        if (!result.results || !Array.isArray(result.results)) {
          vscode.window.showErrorMessage(`Unexpected response format from server`);
          console.error("Unexpected response:", result);
          return;
        }

        const succeeded = result.results.filter(r => r.success).length;
        const failed = result.results.filter(r => !r.success).length;

        if (failed > 0) {
          const errors = result.results.filter(r => !r.success).map(r => r.error).join(", ");
          vscode.window.showWarningMessage(`Applied ${succeeded} updates, ${failed} failed: ${errors}`);
        } else {
          vscode.window.showInformationMessage(`Applied ${succeeded} update(s)`);
        }

        todosProvider.refresh();
        packagesProvider.refresh();
        fsProvider.refreshAllOpenFiles();
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to apply updates: ${error}`);
      }
    }),
    vscode.commands.registerCommand("darklang.todos.revertUpdate", async (node: BranchNode) => {
      if (!node.todoData?.todoId) {
        return;
      }

      const confirm = await vscode.window.showWarningMessage(
        `Revert applied update to ${node.label}? This will restore the previous version.`,
        { modal: true },
        "Revert"
      );

      if (confirm !== "Revert") {
        return;
      }

      try {
        const result = await client.sendRequest<{ results: Array<{ todoId: string; success: boolean; error?: string }> }>(
          "dark/revertUpdates",
          { todoIds: [node.todoData.todoId] }
        );

        const r = result.results[0];
        if (r?.success) {
          vscode.window.showInformationMessage(`Reverted update to ${node.label}`);
        } else {
          vscode.window.showErrorMessage(`Failed to revert: ${r?.error || "Unknown error"}`);
        }

        todosProvider.refresh();
        packagesProvider.refresh();
        fsProvider.refreshAllOpenFiles();
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to revert update: ${error}`);
      }
    }),
    vscode.commands.registerCommand("darklang.todos.selectAllUpdates", () => {
      todosProvider.selectAllUpdates();
    }),
    vscode.commands.registerCommand("darklang.todos.deselectAllUpdates", () => {
      todosProvider.deselectAllUpdates();
    }),
    vscode.commands.registerCommand("darklang.todos.revertAllUpdates", async () => {
      // First get the list of applied updates
      const appliedUpdates = await client.sendRequest<Array<{ todoId: string }>>(
        "dark/listAppliedUpdates",
        { accountID: AccountService.getCurrentAccountId() }
      );

      if (!appliedUpdates || appliedUpdates.length === 0) {
        vscode.window.showInformationMessage("No applied updates to revert");
        return;
      }

      const confirm = await vscode.window.showWarningMessage(
        `Revert all ${appliedUpdates.length} applied update(s)? This will restore previous versions.`,
        { modal: true },
        "Revert All"
      );

      if (confirm !== "Revert All") {
        return;
      }

      try {
        const todoIds = appliedUpdates.map(u => u.todoId);
        const result = await client.sendRequest<{ results: Array<{ todoId: string; success: boolean; error?: string }> }>(
          "dark/revertUpdates",
          { todoIds }
        );

        const succeeded = result.results?.filter(r => r.success).length ?? 0;
        const failed = result.results?.filter(r => !r.success).length ?? 0;

        if (failed > 0) {
          const errors = result.results?.filter(r => !r.success).map(r => r.error).join(", ");
          vscode.window.showWarningMessage(`Reverted ${succeeded} updates, ${failed} failed: ${errors}`);
        } else {
          vscode.window.showInformationMessage(`Reverted ${succeeded} update(s)`);
        }

        todosProvider.refresh();
        packagesProvider.refresh();
        fsProvider.refreshAllOpenFiles();
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to revert updates: ${error}`);
      }
    }),
    pendingChangesView,
    pendingChangesProvider,
    vscode.commands.registerCommand("darklang.pendingChanges.refresh", () => {
      pendingChangesProvider.refresh();
    }),
    vscode.commands.registerCommand("darklang.pendingChanges.revert", async (node: BranchNode) => {
      if (node.pendingChangeData?.locationId) {
        const confirm = await vscode.window.showWarningMessage(
          `Revert pending change to ${node.label}? This will discard your uncommitted changes.`,
          { modal: true },
          "Revert"
        );
        if (confirm === "Revert") {
          try {
            await client.sendRequest("dark/revertPendingChange", {
              locationId: node.pendingChangeData.locationId,
              accountId: AccountService.getCurrentAccountId()
            });
            pendingChangesProvider.refresh();
            packagesProvider.refresh();
            fsProvider.refreshAllOpenFiles();
            vscode.window.showInformationMessage(`Reverted ${node.label}`);
          } catch (error) {
            vscode.window.showErrorMessage(`Failed to revert: ${error}`);
          }
        }
      }
    }),
    vscode.commands.registerCommand("darklang.pendingChanges.viewItem", async (node: BranchNode) => {
      if (node.pendingChangeData) {
        const data = node.pendingChangeData;
        // Build full path: Owner.Module.Name
        const fullPath = data.modules
          ? `${data.owner}.${data.modules}.${data.name}`
          : `${data.owner}.${data.name}`;
        const uri = vscode.Uri.parse(`dark:///package/${fullPath}`);
        await vscode.commands.executeCommand("vscode.open", uri);
      }
    }),
    ...packageCommands.register(),
    ...branchCommands.register(),
    ...scriptCommands.register(),
    ...approvalCommands.register(),
  ].forEach(reg);
}

export async function deactivate(): Promise<void> {
  // Stop sync service via CLI (not LSP)
  stopSyncService();

  if (client) {
    await client.stop();
  }
}
