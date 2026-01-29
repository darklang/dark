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
import { ScriptCommands } from "./commands/scriptCommands";

import { DarkFileSystemProvider } from "./providers/darkFileSystemProvider";
import { DarkContentProvider } from "./providers/darkContentProvider";
import { DiffContentProvider } from "./providers/diffContentProvider";
import { DarklangFileDecorationProvider } from "./providers/fileDecorationProvider";
import { PackageContentProvider } from "./providers/content/packageContentProvider";
import { RecentItemsService } from "./services/recentItemsService";
import { ScmStatusBar } from "./ui/statusbar/scmStatusBar";
import { HomepagePanel } from "./panels/homepage/homepagePanel";

let client: LanguageClient;

// Default = prod mode (uses 'dark' CLI)
// DARKLANG_CLI_MODE=debug uses './scripts/run-cli' with --no-log
const isDebug = process.env.DARKLANG_CLI_MODE === "debug";
const cwd = isDebug ? "/home/dark/app" : (process.env.HOME || process.env.USERPROFILE || ".");
const cli = isDebug ? "./scripts/run-cli" : "dark";

function createLSPClient(): LanguageClient {
  const command = isDebug ? "bash" : cli;
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
  client = createLSPClient();
  client.start();
  await client.onReady();

  RecentItemsService.initialize(context);

  const fsProvider = new DarkFileSystemProvider(client);
  const contentProvider = new DarkContentProvider(client);
  const diffContentProvider = DiffContentProvider.getInstance();
  const decorationProvider = new DarklangFileDecorationProvider();
  const scmStatusBar = new ScmStatusBar();

  PackageContentProvider.setClient(client);
  HomepagePanel.setClient(client);

  // Tree providers
  const packagesProvider = new PackagesTreeDataProvider(client);
  const scmProvider = new ScmTreeDataProvider(client);

  fsProvider.setPackagesProvider(packagesProvider);

  // Update status bar when SCM status changes
  scmProvider.onStatusChanged(status => {
    scmStatusBar.updateStatus(status);
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
  const scriptCommands = new ScriptCommands();

  const reg = (d: vscode.Disposable) => context.subscriptions.push(d);

  [
    scmStatusBar,
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

    // SCM commands
    vscode.commands.registerCommand("darklang.scm.refresh", () => {
      scmProvider.refresh();
    }),

    vscode.commands.registerCommand("darklang.scm.status", async () => {
      const terminal = vscode.window.createTerminal("Darklang SCM");
      terminal.sendText(`${cli} status`);
      terminal.show();
    }),

    vscode.commands.registerCommand("darklang.scm.commit", async () => {
      const message = await vscode.window.showInputBox({
        prompt: "Commit message",
        placeHolder: "Enter commit message",
      });
      if (message) {
        const terminal = vscode.window.createTerminal("Darklang SCM");
        terminal.sendText(`${cli} commit "${message}" --yes`);
        terminal.show();
        // Refresh SCM view after a delay
        setTimeout(() => scmProvider.refresh(), 2000);
      }
    }),

    vscode.commands.registerCommand("darklang.scm.log", async () => {
      const terminal = vscode.window.createTerminal("Darklang SCM");
      terminal.sendText(`${cli} log`);
      terminal.show();
    }),

    vscode.commands.registerCommand("darklang.scm.discard", async () => {
      const confirm = await vscode.window.showWarningMessage(
        "Discard all uncommitted changes?",
        { modal: true },
        "Discard"
      );
      if (confirm === "Discard") {
        const terminal = vscode.window.createTerminal("Darklang SCM");
        terminal.sendText(`${cli} discard --yes`);
        terminal.show();
        // Refresh SCM view after a delay
        setTimeout(() => scmProvider.refresh(), 2000);
      }
    }),

    vscode.commands.registerCommand("darklang.scm.showCommit", async (commitId: string) => {
      try {
        const ops = await client.sendRequest<string[]>("dark/scm/getCommitOps", { commitId });
        const content = ops.length > 0
          ? ops.map((op, i) => `${i + 1}. ${op}`).join("\n")
          : "No operations in this commit.";

        const doc = await vscode.workspace.openTextDocument({
          content: `Commit: ${commitId}\n${"=".repeat(50)}\n\n${content}`,
          language: "plaintext"
        });
        await vscode.window.showTextDocument(doc, { preview: true });
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to load commit: ${error}`);
      }
    }),

    packagesView,
    packagesProvider,
    scmView,
    scmProvider,
    ...packageCommands.register(),
    ...scriptCommands.register(),
  ].forEach(reg);

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
