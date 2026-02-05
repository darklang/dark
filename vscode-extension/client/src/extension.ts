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

    // SCM commands
    vscode.commands.registerCommand("darklang.scm.refresh", () => {
      scmProvider.refresh();
    }),

    vscode.commands.registerCommand("darklang.scm.status", async () => {
      try {
        const summary = await client.sendRequest<{ types: number; fns: number; values: number; renames: number; total: number }>("dark/scm/getWipSummary", {});
        const ops = await client.sendRequest<string[]>("dark/scm/getWipOps", {});

        const lines: string[] = ["Darklang SCM Status", "=".repeat(40), ""];
        if (summary.total === 0) {
          lines.push("No uncommitted changes.");
        } else {
          lines.push(`${summary.total} uncommitted change(s):`);
          if (summary.types > 0) lines.push(`  ${summary.types} type(s)`);
          if (summary.fns > 0) lines.push(`  ${summary.fns} function(s)`);
          if (summary.values > 0) lines.push(`  ${summary.values} value(s)`);
          if (summary.renames > 0) lines.push(`  ${summary.renames} rename(s)`);
          lines.push("", "Operations:", ...ops.map((op, i) => `  ${i + 1}. ${op}`));
        }

        const doc = await vscode.workspace.openTextDocument({ content: lines.join("\n"), language: "plaintext" });
        await vscode.window.showTextDocument(doc, { preview: true });
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to get status: ${error}`);
      }
    }),

    vscode.commands.registerCommand("darklang.scm.commit", async () => {
      const message = await vscode.window.showInputBox({
        prompt: "Commit message",
        placeHolder: "Enter commit message",
      });
      if (message) {
        try {
          const result = await client.sendRequest<{ success: boolean; commitId: string }>("dark/scm/commit", { message });
          if (result.success) {
            vscode.window.showInformationMessage(`Committed: ${result.commitId.substring(0, 8)}`);
          }
        } catch (error) {
          vscode.window.showErrorMessage(`Commit failed: ${error}`);
        }
      }
    }),

    vscode.commands.registerCommand("darklang.scm.log", async () => {
      try {
        const commits = await client.sendRequest<{ id: string; message: string; createdAt: string; opCount: string; branchName?: string }[]>("dark/scm/getCommits", { limit: 20 });

        const lines: string[] = ["Darklang Commit Log", "=".repeat(40), ""];
        if (commits.length === 0) {
          lines.push("No commits yet.");
        } else {
          for (const c of commits) {
            const branch = c.branchName ? `  [${c.branchName}]` : "";
            lines.push(`${c.id.substring(0, 8)}  ${c.message}  (${c.opCount} ops, ${c.createdAt})${branch}`);
          }
        }

        const doc = await vscode.workspace.openTextDocument({ content: lines.join("\n"), language: "plaintext" });
        await vscode.window.showTextDocument(doc, { preview: true });
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to get log: ${error}`);
      }
    }),

    vscode.commands.registerCommand("darklang.scm.discard", async () => {
      const confirm = await vscode.window.showWarningMessage(
        "Discard all uncommitted changes?",
        { modal: true },
        "Discard"
      );
      if (confirm === "Discard") {
        try {
          const result = await client.sendRequest<{ success: boolean; discardedCount: number }>("dark/scm/discard", {});
          if (result.success) {
            vscode.window.showInformationMessage(`Discarded ${result.discardedCount} change(s)`);
          }
        } catch (error) {
          vscode.window.showErrorMessage(`Discard failed: ${error}`);
        }
      }
    }),

    vscode.commands.registerCommand("darklang.scm.showCommit", async (commitIdOrNode: string | { commitId?: string }) => {
      try {
        const commitId = typeof commitIdOrNode === "string" ? commitIdOrNode : commitIdOrNode?.commitId;
        if (!commitId) { return; }
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

    // Branch commands
    vscode.commands.registerCommand("darklang.branch.switch", async () => {
      try {
        const branches = await client.sendRequest<BranchInfo[]>("dark/getBranches", {});
        const items = branches.map(b => ({
          label: b.name,
          description: b.isActive ? "(current)" : "",
          branchId: b.id,
        }));

        const selected = await vscode.window.showQuickPick(items, {
          placeHolder: "Select a branch to switch to",
        });

        if (selected && !branches.find(b => b.id === selected.branchId)?.isActive) {
          await client.sendRequest("dark/switchBranch", { branchId: selected.branchId });
        }
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to switch branch: ${error}`);
      }
    }),

    vscode.commands.registerCommand("darklang.branch.create", async () => {
      const name = await vscode.window.showInputBox({
        prompt: "Branch name",
        placeHolder: "Enter new branch name",
      });
      if (name) {
        try {
          await client.sendRequest("dark/createBranch", { name });
          vscode.window.showInformationMessage(`Created and switched to branch "${name}"`);
        } catch (error) {
          vscode.window.showErrorMessage(`Failed to create branch: ${error}`);
        }
      }
    }),

    vscode.commands.registerCommand("darklang.branch.delete", async () => {
      try {
        const branches = await client.sendRequest<BranchInfo[]>("dark/getBranches", {});
        const deletable = branches.filter(b => !b.isActive && b.name !== "main");
        if (deletable.length === 0) {
          vscode.window.showInformationMessage("No branches available to delete");
          return;
        }

        const items = deletable.map(b => ({
          label: b.name,
          branchId: b.id,
        }));

        const selected = await vscode.window.showQuickPick(items, {
          placeHolder: "Select a branch to delete",
        });

        if (selected) {
          const confirm = await vscode.window.showWarningMessage(
            `Delete branch "${selected.label}"?`,
            { modal: true },
            "Delete"
          );
          if (confirm === "Delete") {
            await client.sendRequest("dark/deleteBranch", { branchId: selected.branchId });
            vscode.window.showInformationMessage(`Deleted branch "${selected.label}"`);
          }
        }
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to delete branch: ${error}`);
      }
    }),

    vscode.commands.registerCommand("darklang.branch.rename", async () => {
      try {
        const branches = await client.sendRequest<BranchInfo[]>("dark/getBranches", {});
        const items = branches.filter(b => b.name !== "main").map(b => ({
          label: b.name,
          description: b.isActive ? "(current)" : "",
          branchId: b.id,
        }));

        if (items.length === 0) {
          vscode.window.showInformationMessage("No branches available to rename");
          return;
        }

        const selected = await vscode.window.showQuickPick(items, {
          placeHolder: "Select a branch to rename",
        });

        if (selected) {
          const newName = await vscode.window.showInputBox({
            prompt: "New branch name",
            placeHolder: "Enter new name",
            value: selected.label,
          });
          if (newName && newName !== selected.label) {
            await client.sendRequest("dark/renameBranch", { branchId: selected.branchId, newName });
            vscode.window.showInformationMessage(`Renamed branch to "${newName}"`);
            scmProvider.refresh();
          }
        }
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to rename branch: ${error}`);
      }
    }),

    vscode.commands.registerCommand("darklang.branch.rebase", async () => {
      try {
        // Check for conflicts first
        const status = await client.sendRequest<{ conflicts: string[]; hasConflicts: boolean }>("dark/rebaseStatus", {});
        if (status.hasConflicts) {
          const conflicts = status.conflicts.join("\n  ");
          vscode.window.showWarningMessage(`Rebase has conflicts:\n  ${conflicts}`);
          return;
        }

        const result = await client.sendRequest<{ success: boolean; message?: string; conflicts?: string[] }>("dark/rebase", {});
        if (result.success) {
          vscode.window.showInformationMessage(result.message || "Rebase successful");
        } else if (result.conflicts && result.conflicts.length > 0) {
          const conflicts = result.conflicts.join("\n  ");
          vscode.window.showWarningMessage(`Rebase failed with conflicts:\n  ${conflicts}`);
        }
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to rebase: ${error}`);
      }
    }),

    vscode.commands.registerCommand("darklang.branch.merge", async () => {
      try {
        // Check if merge is possible first
        const check = await client.sendRequest<{ canMerge: boolean; reason?: string }>("dark/canMerge", {});
        if (!check.canMerge) {
          vscode.window.showWarningMessage(`Cannot merge: ${check.reason}`);
          return;
        }

        const confirm = await vscode.window.showWarningMessage(
          "Merge current branch into its parent?",
          { modal: true },
          "Merge"
        );
        if (confirm === "Merge") {
          const result = await client.sendRequest<{ success: boolean; switchedTo?: { id: string; name: string } }>("dark/merge", {});
          if (result.success && result.switchedTo) {
            vscode.window.showInformationMessage(`Merged and switched to "${result.switchedTo.name}"`);
          }
        }
      } catch (error) {
        vscode.window.showErrorMessage(`Failed to merge: ${error}`);
      }
    }),

    packagesView,
    packagesProvider,
    scmView,
    scmProvider,
    ...packageCommands.register(),
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
