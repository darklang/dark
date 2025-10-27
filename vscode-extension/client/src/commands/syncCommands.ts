import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

export class SyncCommands {
  private client: LanguageClient | null = null;

  setClient(client: LanguageClient): void {
    this.client = client;
  }

  register(): vscode.Disposable[] {
    return [
      vscode.commands.registerCommand("darklang.sync.execute", async () => {
        if (!this.client) {
          vscode.window.showErrorMessage("LSP client not ready");
          return;
        }

        // Ask for instance name
        const instanceName = await vscode.window.showInputBox({
          prompt: "Enter instance name to sync with",
          placeHolder: "e.g., local, origin, production",
          value: "local"
        });

        if (!instanceName) return;

        try {
          vscode.window.showInformationMessage(`Syncing with ${instanceName}...`);

          const response = await this.client.sendRequest<{
            success: boolean;
            message: string;
          }>("darklang/sync", {
            instanceId: instanceName,
            remoteUrl: "" // Not used anymore, kept for backwards compatibility
          });

          if (response.success) {
            vscode.window.showInformationMessage(response.message);
          } else {
            vscode.window.showErrorMessage(`Sync failed: ${response.message}`);
          }
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : String(error);
          vscode.window.showErrorMessage(`Sync error: ${errorMessage}`);
        }
      }),

      vscode.commands.registerCommand("darklang.sync.quickSync", async () => {
        if (!this.client) {
          vscode.window.showErrorMessage("LSP client not ready");
          return;
        }

        // Quick sync with default local instance
        const instanceName = "local";

        try {
          vscode.window.showInformationMessage(`Syncing with ${instanceName}...`);

          const response = await this.client.sendRequest<{
            success: boolean;
            message: string;
          }>("darklang/sync", {
            instanceId: instanceName,
            remoteUrl: "" // Not used anymore, kept for backwards compatibility
          });

          if (response.success) {
            vscode.window.showInformationMessage(response.message);
          } else {
            vscode.window.showErrorMessage(`Sync failed: ${response.message}`);
          }
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : String(error);
          vscode.window.showErrorMessage(`Sync error: ${errorMessage}`);
        }
      })
    ];
  }
}
