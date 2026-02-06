import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { isDebugMode } from "../extension";

export class ScriptCommands {
  private client: LanguageClient | null = null;

  public setClient(client: LanguageClient): void {
    this.client = client;
  }

  public register(): vscode.Disposable[] {
    return [
      vscode.commands.registerCommand(
        "darklang.runScript",
        () => this.runScript()
      )
    ];
  }

  private async runScript(): Promise<void> {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showWarningMessage(
        "No active editor! Please open a file to run the script."
      );
      return;
    }

    const filePath = editor.document.uri.fsPath;

    // Get current branch name from LSP
    let branchArg = "";
    if (this.client) {
      try {
        const branches = await this.client.sendRequest<{ id: string; name: string; isActive: boolean }[]>("dark/getBranches", {});
        const activeBranch = branches.find(b => b.isActive);
        if (activeBranch && activeBranch.name !== "main") {
          branchArg = ` --branch "${activeBranch.name}"`;
        }
      } catch {
        // If we can't get branch info, proceed without it
      }
    }

    let terminal = vscode.window.terminals.find(
      (t) => t.name === "darklang-terminal"
    );

    if (!terminal) {
      terminal = vscode.window.createTerminal("darklang-terminal");
    }

    terminal.show();

    const command = isDebugMode
      ? `cd /home/dark/app && ./scripts/run-cli${branchArg} run "${filePath}" --skip-self-update`
      : `darklang${branchArg} run "${filePath}" --skip-self-update`;

    terminal.sendText(command);
  }
}
