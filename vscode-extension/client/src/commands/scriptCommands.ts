import * as os from "os";
import * as vscode from "vscode";

export class ScriptCommands {
  private isDebugMode(): boolean {
    return process.env.VSCODE_DEBUG_MODE === "true";
  }

  public register(): vscode.Disposable[] {
    const runScriptCommand = vscode.commands.registerCommand(
      "darklang.runScript",
      () => this.runScript()
    );

    return [runScriptCommand];
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
      terminal = vscode.window.createTerminal(`darklang-terminal`);
    }

    terminal.show(true);

    if (this.isDebugMode()) {
      const changeDirCommand = `cd /home/dark/app`;
      const scriptCommand = `./scripts/run-cli run "${filePath}" --skip-self-update`;
      terminal.sendText(changeDirCommand, true);
      terminal.sendText(scriptCommand, true);
    } else {
      const changeDirCommand = `cd ${os.homedir()}`;
      const scriptCommand = `darklang run "${filePath}" --skip-self-update`;
      terminal.sendText(changeDirCommand, true);
      terminal.sendText(scriptCommand, true);
    }
  }
}
