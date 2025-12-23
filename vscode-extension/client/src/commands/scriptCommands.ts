import * as vscode from "vscode";
import { BranchStateManager } from "../data/branchStateManager";

export class ScriptCommands {
  private isDebugMode(): boolean {
    return process.env.VSCODE_DEBUG_MODE === "true";
  }

  public register(): vscode.Disposable[] {
    return [
      vscode.commands.registerCommand(
        "darklang.runScript",
        () => this.runScript()
      )
    ];
  }

  private runScript(): void {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showWarningMessage(
        "No active editor! Please open a file to run the script."
      );
      return;
    }

    const filePath = editor.document.uri.fsPath;

    let terminal = vscode.window.terminals.find(
      (t) => t.name === "darklang-terminal"
    );

    if (!terminal) {
      terminal = vscode.window.createTerminal("darklang-terminal");
    }

    terminal.show();

    // Get current branch from extension state
    let branchEnv = "";
    try {
      const branchManager = BranchStateManager.getInstance();
      const branchId = branchManager.getCurrentBranchId();
      if (branchId) {
        branchEnv = `DARK_BRANCH="${branchId}" `;
      }
    } catch {
      // BranchStateManager not initialized yet, run without branch
    }

    const command = this.isDebugMode()
      ? `cd /home/dark/app && ${branchEnv}./scripts/run-cli run "${filePath}" --skip-self-update`
      : `${branchEnv}darklang run "${filePath}" --skip-self-update`;

    terminal.sendText(command);
  }
}
