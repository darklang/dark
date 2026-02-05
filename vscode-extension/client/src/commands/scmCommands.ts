import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { ScmTreeDataProvider } from "../providers/treeviews/scmTreeDataProvider";

export class ScmCommands {
  private client: LanguageClient;
  private scmProvider: ScmTreeDataProvider;

  constructor(client: LanguageClient, scmProvider: ScmTreeDataProvider) {
    this.client = client;
    this.scmProvider = scmProvider;
  }

  register(): vscode.Disposable[] {
    return [
      vscode.commands.registerCommand("darklang.scm.refresh", () => {
        this.scmProvider.refresh();
      }),

      vscode.commands.registerCommand("darklang.scm.status", async () => {
        try {
          const summary = await this.client.sendRequest<{ types: number; fns: number; values: number; renames: number; total: number }>("dark/scm/getWipSummary", {});
          const ops = await this.client.sendRequest<string[]>("dark/scm/getWipOps", {});

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
            const result = await this.client.sendRequest<{ success: boolean; commitId: string }>("dark/scm/commit", { message });
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
          const commits = await this.client.sendRequest<{ id: string; message: string; createdAt: string; opCount: string; branchName?: string }[]>("dark/scm/getCommits", { limit: 20 });

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
            const result = await this.client.sendRequest<{ success: boolean; discardedCount: number }>("dark/scm/discard", {});
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
          const ops = await this.client.sendRequest<string[]>("dark/scm/getCommitOps", { commitId });
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
    ];
  }
}
